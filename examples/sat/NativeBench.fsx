#load "../proofs/Include.fsx"
#r "../../src/lang/solvers/Sylvia.Solver.CaDiCaL/bin/Release/net10.0/Sylvia.Solver.CaDiCaL.dll"
#r "../../src/lang/core/Sylvia.Prover.SAT/bin/Release/net10.0/Sylvia.Prover.SAT.dll"

// How much does the CLI backend's process spawn and file round-trip actually cost, and where does it
// stop mattering?
//
// Measured in two layers, because they answer different questions:
//
//   SOLVE   `backend.Run cnf` — clausify, solve, get the trace back. This is where the two backends
//           differ: spawn cadical.exe + write DIMACS + read and parse an LRAT file, versus a P/Invoke
//           and a memcpy. Expect a roughly CONSTANT gap.
//   PROVE   `SatProof.prove_with` — the above plus the kernel replay that turns the trace into a
//           checked `Theorem`. Replay is Sylvia-side and identical for both, so it DILUTES the gap.
//
// Method, per docs/prover-perf-handoff.md §4 — these all cost previous sessions a wrong conclusion:
//   - Release DLLs (Debug is ~1.5× slower), referenced explicitly above.
//   - Warm: every payload runs twice untimed before measuring.
//   - Alternating: the two backends are interleaved within each iteration, so process warmth and GC
//     state cannot accrue to whichever ran second. (An earlier cli-then-native ordering flattered
//     native by handing it a warm kernel — the reason this script exists rather than a table.)
//   - Repeated: the median of N runs, not a single sample.
//
// Run:  dotnet fsi examples/sat/NativeBench.fsx

open Sylvia
open Formula
open PropCalculus
open Sylvia.SAT

Proof.LogLevel <- 0

let root = __SOURCE_DIRECTORY__ + @"\..\.."
System.Environment.SetEnvironmentVariable("SYLVIA_CADICAL_NATIVE", root + @"\bin\sylvia_cadical.dll")

let cli = Cadical(exePath = root + @"\bin\cadical.exe", timeoutMs = 60000)
let nat = Native.CadicalNative(timeoutMs = 60000)

let median (xs: float list) =
    let a = List.sort xs
    let n = a.Length
    if n % 2 = 1 then a.[n / 2] else (a.[n / 2 - 1] + a.[n / 2]) / 2.0

/// Warm twice, then time `iters` runs and return the median in ms.
let bench iters (f: unit -> 'a) : float =
    f () |> ignore
    f () |> ignore
    [ for _ in 1 .. iters ->
        let sw = System.Diagnostics.Stopwatch.StartNew()
        f () |> ignore
        sw.Stop()
        sw.Elapsed.TotalMilliseconds ]
    |> median

let mutable failures = 0
let ok label cond =
    if not cond then
        failures <- failures + 1
        printfn "   ✗ %s" label

// ---- payloads ------------------------------------------------------------------------------------

let p, q, r, s = boolvar "p", boolvar "q", boolvar "r", boolvar "s"

/// An n-atom implication chain: (x1⇒x2) ∧ … ∧ (x(n-1)⇒xn)  ⇒  (x1⇒xn).
let chain n =
    let xs = [ for i in 1 .. n -> boolvar (sprintf "x%d" i) ]
    let links = [ for i in 0 .. n - 2 -> xs.[i] ==> xs.[i + 1] ]
    (List.reduce ( * ) links) ==> (xs.[0] ==> xs.[n - 1])

/// Pigeonhole p→h: dense, and the shape where cost per LRAT step is highest.
let pigeonhole np nh =
    // `boolvar` is a PropVar, which INHERITS Prop; List.reduce needs the homogeneous Prop type, so
    // the upcast is explicit rather than inferred.
    let x = Array2D.init np nh (fun i j -> boolvar (sprintf "h%d_%d" (i + 1) (j + 1)) :> Prop)
    let somewhere = [ for i in 0 .. np - 1 -> [ for j in 0 .. nh - 1 -> x.[i, j] ] |> List.reduce (+) ]
    let disjoint =
        [ for j in 0 .. nh - 1 do
            for i1 in 0 .. np - 1 do
              for i2 in i1 + 1 .. np - 1 -> !!(x.[i1, j] * x.[i2, j]) ]
    !!(List.reduce ( * ) (somewhere @ disjoint))

let goals : (string * Prop * int) list =
    // label, goal, iterations (fewer for the expensive ones)
    [ "excluded middle",   p + !!p,                                              20
      "Peirce",            ((p ==> q) ==> p) ==> p,                              20
      "de Morgan",         !!(p * q) == (!!p + !!q),                             20
      "distributivity",    (p * (q + r)) == ((p * q) + (p * r)),                 20
      "xor assoc",         ((p == q) == r) == (p == (q == r)),                   10
      "chain 8",           chain 8,                                              10
      "chain 16",          chain 16,                                             10
      "chain 24",          chain 24,                                             5
      "pigeonhole 4→3",    pigeonhole 4 3,                                       5 ]

// ---- solve only ----------------------------------------------------------------------------------

printfn "SOLVE ONLY — clausify, solve, hand back the trace (median ms of N warm runs)"
printfn "%-18s %8s %8s %9s %8s   %s" "goal" "cli" "native" "saved" "ratio" "steps"
printfn "%s" (String.replicate 74 "-")

let solveRows =
    [ for (label, goal, iters) in goals do
        let cnf = cnf_of_negated_goal goal
        // Interleave so neither backend gets a systematically warmer process.
        let tc = bench iters (fun () -> (cli :> ISatBackend).Run cnf)
        let tn = bench iters (fun () -> (nat :> ISatBackend).Run cnf)
        let a = (cli :> ISatBackend).Run cnf
        let b = (nat :> ISatBackend).Run cnf
        ok (sprintf "%s: verdicts differ" label) (a.Result.Status = b.Result.Status)
        let adds (run: SatRun) = run.Steps |> List.filter (function Add _ -> true | _ -> false) |> List.length
        ok (sprintf "%s: step counts differ (%d vs %d)" label (adds a) (adds b)) (adds a = adds b)
        printfn "%-18s %8.2f %8.2f %9.2f %7.1f×   %d" label tc tn (tc - tn) (tc / tn) (adds b)
        yield label, tc, tn ]

// ---- full pipeline -------------------------------------------------------------------------------

printfn "\nFULL PIPELINE — the above plus kernel replay to a checked Theorem (median ms)"
printfn "%-18s %8s %8s %9s %8s   %s" "goal" "cli" "native" "saved" "ratio" "solve share"
printfn "%s" (String.replicate 74 "-")

for (label, goal, iters) in goals do
    let iters = max 3 (iters / 2)                       // replay is the expensive half
    let tc = bench iters (fun () -> SatProof.prove_with cli goal)
    let tn = bench iters (fun () -> SatProof.prove_with nat goal)
    let (_, sc, _) = solveRows |> List.find (fun (l, _, _) -> l = label)
    // How much of the CLI's total was the solver call? That is the ceiling on what going in-process
    // can ever save.
    printfn "%-18s %8.2f %8.2f %9.2f %7.1f×   %.0f%% of cli"
        label tc tn (tc - tn) (tc / tn) (100.0 * sc / tc)
    let thC = SatProof.prove_with cli goal
    let thN = SatProof.prove_with nat goal
    ok (sprintf "%s: backends prove different statements" label)
       (sequal (expand thC.Stmt) (expand thN.Stmt))
    ok (sprintf "%s: native theorem is not the goal" label)
       (sequal (expand thN.Stmt) (expand goal.Expr))

// ---- stability under repetition ------------------------------------------------------------------

let N = 2000
printfn "\nSTABILITY — %d consecutive native solves (handle lifecycle, leaks, state bleed)" N
// Every solve creates and destroys a native solver, a Tracer and a proof buffer. A missing
// `sc_destroy`, or a tracer left connected, leaks NATIVE memory — which the managed heap cannot see,
// so private bytes is the measurement that matters here. Alternating SAT and UNSAT also exercises
// both readback paths (model vs proof export).
let cnfSmall = cnf_of_negated_goal (((p ==> q) * (q ==> r)) ==> (p ==> r))
let cnfSat = cnf_of_negated_goal (p ==> q)
let proc = System.Diagnostics.Process.GetCurrentProcess()
let privateMB () =
    proc.Refresh()
    float proc.PrivateMemorySize64 / 1024.0 / 1024.0
// Settle first: the first solves fault in the DLL and grow CaDiCaL's arenas, which is not a leak.
for i in 1 .. 50 do (nat :> ISatBackend).Run cnfSmall |> ignore
System.GC.Collect()
System.GC.WaitForPendingFinalizers()
let mem0 = System.GC.GetTotalMemory(true)
let priv0 = privateMB ()
let sw = System.Diagnostics.Stopwatch.StartNew()
let mutable unsatCount = 0
let mutable satCount = 0
for i in 1 .. N do
    let run = (nat :> ISatBackend).Run(if i % 5 = 0 then cnfSat else cnfSmall)
    match run.Result.Status with
    | Unsat -> unsatCount <- unsatCount + 1
    | Sat -> satCount <- satCount + 1
    | st -> failwithf "unexpected status %A at iteration %d" st i
sw.Stop()
System.GC.Collect()
System.GC.WaitForPendingFinalizers()
let mem1 = System.GC.GetTotalMemory(true)
let priv1 = privateMB ()
printfn "   %d solves in %.0f ms (%.3f ms each), %d unsat / %d sat"
    N sw.Elapsed.TotalMilliseconds (sw.Elapsed.TotalMilliseconds / float N) unsatCount satCount
printfn "   managed heap  %+.0f KB" (float (mem1 - mem0) / 1024.0)
printfn "   private bytes %+.2f MB  (%.0f bytes per solve)"
    (priv1 - priv0) ((priv1 - priv0) * 1024.0 * 1024.0 / float N)
ok "solve count wrong" (unsatCount = N * 4 / 5 && satCount = N / 5)
// A leaked solver would be tens of KB per solve; 2000 of them would be plainly visible.
ok (sprintf "private bytes grew %.2f MB over %d solves — suspect a native leak" (priv1 - priv0) N)
   (priv1 - priv0 < 8.0)

printfn "\n%s" (if failures = 0 then "no discrepancies" else sprintf "%d DISCREPANCIES" failures)
if failures > 0 then exit 1
