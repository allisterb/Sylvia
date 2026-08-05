#load "../proofs/Include.fsx"
#r "../../src/lang/solvers/Sylvia.Solver.CaDiCaL/bin/Debug/net10.0/Sylvia.Solver.CaDiCaL.dll"
#r "../../src/lang/core/Sylvia.Prover.SAT/bin/Debug/net10.0/Sylvia.Prover.SAT.dll"

// The two SAT backends must be interchangeable. This is the gate for that.
//
//   Cadical              spawns cadical.exe, writes DIMACS, reads back a text LRAT file
//   Native.CadicalNative calls sylvia_cadical.dll in process, receives the proof from CaDiCaL's
//                        Tracer with no file involved
//
// Both implement `ISatBackend`, so `SatProof.prove_with` takes either. What this script checks is
// that choosing one over the other cannot change an answer: same verdicts, same DERIVATION, and the
// same kernel-checked `Theorem` at the end.
//
// THE ONE THING THAT LEGITIMATELY DIFFERS IS CLAUSE NUMBERING, and it is a trap. CaDiCaL numbers
// input clauses 1..m only because its DIMACS *parser* reserves that range from the `p cnf` header.
// Nothing reserves it when clauses are added through the API, so the native backend's ids differ —
// on de Morgan below, whose CNF has two unit clauses, the CLI's first derived clause is id 6 and the
// native one's is id 3. `refute` used to assume 1..m; seeding the wrong clause against an id does
// not fail loudly, it just replays against a formula the solver never used. The native backend
// therefore reports its own ids in `SatRun.Originals`, and that is what the replay seeds from.
//
// Run:  dotnet fsi examples/sat/NativeBackend.fsx
//   needs bin/cadical.exe (CLI backend) and bin/sylvia_cadical.dll (native backend)

open Sylvia
open Formula
open PropCalculus
open Sylvia.SAT

Proof.LogLevel <- 0

let root = __SOURCE_DIRECTORY__ + @"\..\.."
System.Environment.SetEnvironmentVariable("SYLVIA_CADICAL_NATIVE", root + @"\bin\sylvia_cadical.dll")

let cli = Cadical(exePath = root + @"\bin\cadical.exe", timeoutMs = 30000)
let nat = Native.CadicalNative(timeoutMs = 30000)

let mutable failures = 0
let ok label cond =
    if not cond then failures <- failures + 1
    printfn "  %s  %s" (if cond then "✓" else "✗") label

printfn "CLI backend available:    %b" (cli :> ISatBackend).IsAvailable
printfn "native backend available: %b  (%s)\n" (nat :> ISatBackend).IsAvailable (Native.signature ())

let p, q, r, s = boolvar "p", boolvar "q", boolvar "r", boolvar "s"

let goals : (string * Prop) list =
    [ "excluded middle",  p + !!p
      "Peirce",           ((p ==> q) ==> p) ==> p
      "de Morgan",        !!(p * q) == (!!p + !!q)          // two unit clauses: ids diverge here
      "distributivity",   (p * (q + r)) == ((p * q) + (p * r))
      "chain",            ((p ==> q) * (q ==> r)) ==> (p ==> r)
      "xor assoc",        ((p == q) == r) == (p == (q == r))
      "4-var chain",      ((p ==> q) * (q ==> r) * (r ==> s)) ==> (p ==> s) ]

// Clause ids are internal to a backend, so compare the derivation by CONTENT: which clause each step
// concludes, and which formulas it was derived from.
let derivation (plan: ResolutionStep list) =
    plan |> List.map (fun st ->
        List.sort st.Literals,
        st.IsEmpty,
        st.Premises |> List.map (fun (_, prem) -> prem.ToString()) |> List.sort)

printfn "── the two backends agree on verdict and derivation ──"
for (label, goal) in goals do
    let cnf = cnf_of_negated_goal goal
    let a = (cli :> ISatBackend).Run cnf
    let b = (nat :> ISatBackend).Run cnf
    ok (sprintf "%s: same verdict (%A)" label a.Result.Status) (a.Result.Status = b.Result.Status)
    if a.Result.Status = Unsat then
        // `reconstruction_plan_of` seeds from whichever ids that backend reported — this is the call
        // that makes the two comparable at all.
        let pa = reconstruction_plan_of cnf a
        let pb = reconstruction_plan_of cnf b
        ok (sprintf "%s: same derivation (%d steps)" label pa.Length) (derivation pa = derivation pb)

printfn "\n── the native ids really do differ, and are handled ──"
let dm = cnf_of_negated_goal (!!(p * q) == (!!p + !!q))
let dmNat = (nat :> ISatBackend).Run dm
let dmCli = (cli :> ISatBackend).Run dm
let firstDerived (run: SatRun) =
    run.Steps |> List.pick (function Add(i, _, _) -> Some i | _ -> None)
printfn "   CLI first derived id = %d, native = %d" (firstDerived dmCli) (firstDerived dmNat)
ok "native reports its own input-clause ids" (not (List.isEmpty dmNat.Originals))
ok "CLI reports none (its parser reserved 1..m)" (List.isEmpty dmCli.Originals)
ok "the ids genuinely differ — so the mapping is load-bearing"
   (firstDerived dmCli <> firstDerived dmNat)

printfn "\n── both backends yield the same kernel-checked Theorem ──"
for (label, goal) in goals do
    let tc = SatProof.prove_with cli goal
    let tn = SatProof.prove_with nat goal
    ok (sprintf "%s: native proves exactly the goal" label)
       (sequal (expand tn.Stmt) (expand goal.Expr))
    ok (sprintf "%s: both prove the same statement" label)
       (sequal (expand tc.Stmt) (expand tn.Stmt))

printfn "\n── a non-theorem is still refused ──"
match SatProof.try_prove_with nat (p ==> q) with
| Ok _ -> ok "p ⇒ q is refused" false
| Error _ -> ok "p ⇒ q is refused (¬goal is satisfiable)" true

printfn "\n── the native backend can be installed as the default decider ──"
SatProof.install_with nat
let goal4 = ((p ==> q) * (q ==> r) * (r ==> s)) ==> (p ==> s)
let viaDecide = PropCalculus.decide goal4
SatProof.uninstall ()
ok "PropCalculus.decide routes through it" (sequal (expand viaDecide.Stmt) (expand goal4.Expr))

printfn "\n%s"
    (if failures = 0 then "ALL GREEN  (0 failed)" else sprintf "%d CHECK(S) FAILED" failures)
if failures > 0 then exit 1
