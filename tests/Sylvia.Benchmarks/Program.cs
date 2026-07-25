using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using Sylvia.Tests.Perf;

namespace Sylvia.Benchmarks;

/// <summary>
/// Micro-benchmarks over the Sylvia.Expressions payloads defined in
/// tests/Sylvia.Tests.Perf/Payloads.fs (see docs/expressions-perf.md).
/// Run: dotnet run -c Release --project tests/Sylvia.Benchmarks -- --filter '*'
/// </summary>
[MemoryDiagnoser]
public class FsExprBenchmarks
{
    [Benchmark] public bool SequalSmallEqual() => Payloads.sequal_small_eq();
    [Benchmark] public bool SequalSmallNotEqual() => Payloads.sequal_small_neq();
    [Benchmark] public bool SequalMediumEqual() => Payloads.sequal_medium_eq();
    [Benchmark] public bool SequalLargeEqual() => Payloads.sequal_large_eq();
    [Benchmark] public bool SequalLargeNotEqualLate() => Payloads.sequal_large_neq_late();
    [Benchmark] public object GetVarsLarge() => Payloads.get_vars_large();
    [Benchmark] public object ReplaceExprLarge() => Payloads.replace_expr_large();
}

/// <summary>
/// Macro benchmark: a full prover theorem. Note the prover memoizes derived rules,
/// so steady-state iterations measure the post-warmup (memoized) path; the cold-start
/// cost is measured by the Stopwatch harness in Sylvia.Tests.Perf instead.
/// </summary>
[MemoryDiagnoser]
public class ProverBenchmarks
{
    [Benchmark] public object TransImplies() => Payloads.trans_implies_run();
}

public static class Program
{
    public static void Main(string[] args) =>
        BenchmarkSwitcher.FromAssembly(typeof(Program).Assembly).Run(args);
}
