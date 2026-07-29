module Sylvia.Tests.Perf.Program

open System
open System.Diagnostics

open Sylvia
open Sylvia.Tests.Perf

/// Quick Stopwatch/GC harness for before/after comparisons while optimizing.
/// For rigorous numbers use the BenchmarkDotNet project in tests/Sylvia.Benchmarks.

let timeOnce name (f: unit -> 'a) =
    let a0 = GC.GetAllocatedBytesForCurrentThread()
    let sw = Stopwatch.StartNew()
    f () |> ignore
    sw.Stop()
    let a = GC.GetAllocatedBytesForCurrentThread() - a0
    printfn "%-36s %12.3f ms %14.1f KB" name sw.Elapsed.TotalMilliseconds (float a / 1024.)

let time name iters (f: unit -> 'a) =
    f () |> ignore // warmup / JIT
    let a0 = GC.GetAllocatedBytesForCurrentThread()
    let sw = Stopwatch.StartNew()
    for _ in 1 .. iters do
        f () |> ignore
    sw.Stop()
    let a = GC.GetAllocatedBytesForCurrentThread() - a0
    printfn "%-36s %12.3f us/op %12d B/op   (%d iters)" name (sw.Elapsed.TotalMilliseconds * 1000. / float iters) (a / int64 iters) iters

let runMicro () =
    timeOnce "corpus init (module cctor)" (fun () -> Payloads.largeA)
    timeOnce "trans_implies (first call)" Payloads.trans_implies_run
    timeOnce "trans_implies (second call)" Payloads.trans_implies_run
    Proof.LogLevel <- 0
    timeOnce "trans_implies (warm, LogLevel=0)" Payloads.trans_implies_run
    Proof.LogLevel <- 1
    time "sequal small eq" 10000 Payloads.sequal_small_eq
    time "sequal small neq" 10000 Payloads.sequal_small_neq
    time "sequal medium eq" 5000 Payloads.sequal_medium_eq
    time "sequal large eq" 200 Payloads.sequal_large_eq
    time "sequal large neq late" 200 Payloads.sequal_large_neq_late
    time "get_vars large" 200 Payloads.get_vars_large
    time "replace_expr large" 50 Payloads.replace_expr_large

/// SAT-reconstruction payloads (hermetic, no solver): run with `-- reconstruct` to get a
/// process profile dominated by reconstruction cost alone (Cnf.toCnf, conjElimAll /
/// Calc.chainImp, resolve folding, normalize). LogLevel 0 keeps console I/O out of the profile.
let runReconstruction () =
    Proof.LogLevel <- 0
    timeOnce "reconstruct chain 5 (cold)" (fun () -> Reconstruction.reconstruct_chain 5)
    timeOnce "reconstruct chain 8" (fun () -> Reconstruction.reconstruct_chain 8)
    timeOnce "reconstruct chain 12" (fun () -> Reconstruction.reconstruct_chain 12)
    timeOnce "conjElimAll 12 clauses" (fun () -> Reconstruction.conj_elim_all 12)
    Proof.LogLevel <- 1

/// DENSE reconstruction: run with `-- dense` to get a process profile dominated by pigeonhole
/// replay. Chains are the cheapest refutation shape there is (one resolution per atom, over
/// two-literal clauses), so the payloads above cannot show what a wide-clause, many-step
/// refutation costs — measured, a 20-atom pigeonhole costs what a 60-atom chain would.
///
/// `-- dense` runs 4→3 (~1.4 s) then 5→4 (~12 s); `-- dense43` runs only the fast one, for
/// iterating on a change without waiting. Both are hermetic: the LRAT traces are canned, so no
/// solver runs and the profile is entirely Sylvia-side replay.
///
/// Known distribution as of 2026-07-28, from in-process timers on 5→4 (Release):
///   rule application 0.4%, axiom recognition 15%, print_formula ~40% (at LogLevel 0, where none
///   of it is printed), remainder ~45% unattributed. Reference-keyed memoization of `expand` and
///   `AxEquiv` was tried and made no measurable difference despite 60% hit rates. The open
///   question this payload exists to answer is what the unattributed 45% is.
let runDense (fastOnly: bool) =
    // `LogLevel <- 0` silences LEMMA proofs, but a top-level proof still announces itself, and the
    // formulas here are hundreds of nodes wide. Decompiling and writing them would land in the
    // profile as console I/O and string building that a real caller at LogLevel 0 does pay, but
    // which has nothing to do with the replay this payload exists to measure. Swallow it.
    let quiet (f: unit -> 'a) () =
        let saved = Console.Out
        Console.SetOut IO.TextWriter.Null
        try f () finally Console.SetOut saved
    Proof.LogLevel <- 0
    // Each `dotnet run` is a fresh process, so a single 1.6 s measurement is a large slice of JIT.
    // Repeat and read the LATER rows: the first is cold, the rest are the steady state an A/B
    // comparison actually wants.
    for i in 1 .. 3 do
        timeOnce (sprintf "pigeonhole 4->3  (run %d)" i) (quiet ReconstructionDense.reconstruct_php_4_3)
    if not fastOnly then
        timeOnce "pigeonhole 5->4" (quiet ReconstructionDense.reconstruct_php_5_4)
    Proof.LogLevel <- 1

[<EntryPoint>]
let main argv =
    printfn "=== Sylvia.Expressions perf harness ==="
    match argv with
    | [| "reconstruct" |] -> runReconstruction ()
    | [| "micro" |] -> runMicro ()
    | [| "dense" |] -> runDense false
    | [| "dense43" |] -> runDense true
    | _ -> runMicro (); runReconstruction ()
    0
