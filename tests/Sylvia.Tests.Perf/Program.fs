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

[<EntryPoint>]
let main _ =
    printfn "=== Sylvia.Expressions perf harness ==="
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
    0
