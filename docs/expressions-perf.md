# Sylvia.Expressions performance plan (CPU + allocations)

Motivation: profiling `tests/Sylvia.Tests.Perf` (a single `trans_implies p q r` call, ~1.9s on the
big clause conjunction in the SAT-reconstruction work) shows the prover's time is dominated not by
proof logic but by three quotation-related costs, all rooted in `Sylvia.Expressions`:

| Profiler hotspot | Root cause in source |
|---|---|
| `FSharp.Quotations.PatternsModule.deserialize` / `Expr.Deserialize40` (top external cost) | Every evaluation of a quotation literal `<@ ... @>` **re-deserializes the pickled tree**. All `SpecificCall <@@ op @@>` templates written *inside* active-pattern bodies re-deserialize on **every match attempt**. |
| `Expr.ToString()` → `StructuredPrintfImpl.Display.layout_to_string` (inclusive cost of `FsExpr.sequal`) | `sequal` compares expressions **by rendering both sides to strings** (FsExpr.fs:363-366), and it's the guard on nearly every prover pattern (130 call sites; 26 in the prover's `Patterns.fs` alone, several per candidate expression per proof step). |
| Unquote `decompile` (`src`) | Used not just for display but in constructors and internal search: `ScalarVar` ctor, `IndexVar.Name`, `Term.Equals` (compares `Display` strings!), `Scalar.GetHashCode`, `find_expr` (decompiles **every subterm**), and eagerly-evaluated log formatting in the prover. |

None of this requires the fresh-start architecture change (interned terms / LCF `Thm`); it's
straight optimization of how the current code uses `Expr`.

---

## Phase 0 — Baseline + safety net

1. **Benchmarks** (fill in `tests/Sylvia.Benchmarks`, BenchmarkDotNet, referencing the F# projects):
   - `sequal` on small / medium / large prop expressions (equal, unequal-early, unequal-late).
   - The full `trans_implies p q r` proof (the perf-test scenario) as a macro benchmark.
   - `get_vars`, `replace_expr`, `expand` on representative trees.
   - A pattern-match sweep: run the prover's `(|Symm|_|)`-style patterns over a corpus.
   Record before-numbers (mean time + allocated bytes) for each.
2. **Correctness gates** (this is soundness-sensitive — `sequal` is the kernel's notion of
   syntactic identity):
   - Keep the old string-based comparison as `sequal_str`.
   - Add a conditional (`#if SEQUAL_CHECK` or mutable flag) dual-run assertion: new structural
     `sequal` must agree with `sequal_str`; run the full prover suite (90/90), the example
     scripts, and the ANF oracle sweep with the assertion enabled before switching.
   - Add unit tests for the FsExpr functions themselves (currently only parser tests exist):
     `sequal` edge cases, `get_vars`, `replace_*`, `traverse`, `is_inst_expr`.

## Phase 1 — Replace string-based `sequal` with structural equality (biggest win)

`FsExpr.sequal` (FsExpr.fs:363) is the single largest CPU sink (~180k inclusive samples; its
children `ToString`/`layout`/`showL` are the fsharp.core hotspots) **and** a major allocator
(3.6M + 2.9M + 1.4M bytes in the screenshots). Replace with a recursive structural walk:

- **Fast path first**: `obj.ReferenceEquals(l, r)` → true. The prover compares the same
  sub-objects repeatedly; combined with Phase 4's identity-preserving rebuilds this alone
  short-circuits a large fraction of calls.
- Recursive comparison via `ExprShape`:
  - `ShapeVar v1 / ShapeVar v2` → `vequal` (name + type — preserves current semantics where
    same-named vars are equal; **not** alpha-equivalence, matching the string behavior).
  - `ShapeLambda` → compare binder (name + type) and body.
  - `ShapeCombination(o1, args1) / ShapeCombination(o2, args2)` → `o1.Equals o2` (this compares
    the operation descriptor, incl. `MethodInfo` for calls) + pairwise recursion. This is the same
    discrimination `try_match` (FsExpr.fs:519) already relies on.
- **Semantics to preserve** (validate with the dual-run):
  - `ValueWithName`: the string form prints the *name*; truth constants T/F are
    `ValueWithName(_, bool, "True"/"False")` and are compared by name today. Structural version
    must compare ValueWithName by (name, type) — not the payload value.
  - `Value`: compare by (type, value). Note `%A` prints `1` for several numeric types; comparing
    by type+value is *stricter*, which is safer for the kernel — but confirm no test relies on the
    looser behavior.
  - The `"(" + s + ")"` special case (FsExpr.fs:365-366) exists to paper over parenthesized
    renderings; structural comparison has no such artifact and subsumes it — confirm via suite.
- `sequal2`/`sequal3` need no change. `replace_expr`, `try_match`, `is_inst_expr`,
  `apply_first_firing` all get faster for free since they call `sequal` per node.
- Optional (measure first): per-node memoized hash via `ConditionalWeakTable<Expr, int>` to make
  repeated unequal comparisons O(1); only if the plain structural version isn't enough.

## Phase 2 — Stop re-deserializing quotation templates

Every `SpecificCall <@@ op @@>` written inside a `function`/match body deserializes the template
each call. Sites (98 `SpecificCall` uses across `src/`, most with inline literals):

- `FsExpr.fs`: `(|And|_|)`, `(|Or|_|)` (lines 48-66) — these run on essentially every prover
  pattern probe.
- `Formula.fs`: `Equals`, `Not`, `NotEquals`, `Implies`, `Conseq`, `LessThan`, `Range`,
  `Sequence`, `Binary` (10 sites).
- `Symbolic.fs`: **47 sites** — `sprinte`/`latexe` try ~20 `SpecificCall` templates *per node
  visit*, each a fresh deserialize. This is why display/printing shows up so hot.
- `Term.fs` (2), `MathNetExpr.fs` (12), plus `Maxima.fs` (18) outside this project — same fix
  pattern applies later.

Fix, in order of preference:

1. **Hoist the template's MethodInfo, not the quotation**: at module init, extract the generic
   method definition once (e.g. `let andMi = match <@@ (&&) @@> with Lambdas(_, Call(_, mi, _)) ->
   mi`), then match `Call(None, mi, [l; r]) when mi.IsGenericMethod && mi.GetGenericMethodDefinition() = andMi`
   (or plain equality for non-generic ops). This removes both the deserialize *and*
   `SpecificCall`'s per-call template destructuring. Wrap it in a small helper so the 98 sites
   stay one-liners, e.g. a `SpecificCallTo` active pattern taking a cached `MethodInfo`.
2. Where the template is passed in as a parameter (`(|Binary|_|) (op:Expr<...>)`), the
   deserialization already happened at the caller — but callers often write `Binary <@ (&&) @>
   ...` inline; hoist those literals to module-level `let` bindings in the prover later.
- `(|Op|_|)` / `(|UnaryOp|_|)` / `(|BinaryOp|_|)` (FsExpr.fs:68-83): the `"op_" + n` concat
  allocates per probe — precompute full op names at the (module-level) pattern definition sites,
  or compare with `String.Equals(mi.Name, ...)` against constants.
- The `addOp`/`subOp`/... `Map`s (FsExpr.fs:153-319) deserialize ~80 quotations in the module
  cctor. One-time cost, but it's on every fsi startup: make them `lazy` per-map (they're only
  needed for arithmetic terms, not the prover) — low priority.

## Phase 3 — Confine the decompiler (Unquote `src`) to display time

- `find_expr` (FsExpr.fs:854) decompiles **every subterm** to compare against a string. Change the
  signature to take an `Expr` (or keep the string overload but compare with the Phase-1 structural
  walk after parsing once) — check callers first.
- `Term.fs` hot spots:
  - `Term.Equals` compares `a.Display = b.Display` → for `Prop` that's two `decompile` calls per
    equality test; `Scalar.GetHashCode` = `Display.GetHashCode()` → decompile per hash. Base
    equality/hash on the Expr (structural `sequal` + cached hash) and make `Display` a
    `lazy`-cached property instead of computed per call.
  - `ScalarVar` ctor: `defaultArg label (src expr)` decompiles on **every construction** even when
    a label is given — defer with `lazy`/`match label with Some l -> l | None -> src expr` only on
    access. Same for `IndexVar.Name`/`Display` and `Pred.Symbol`.
- Error paths (`failwithf ... (src e)`) are lazily evaluated already — fine as is.
- Prover follow-up (outside this project, but it's the visible `decompile` cost in the profile):
  `Proof.fs:202-213` formats and prints every axiom/lemma via `print_formula` at the default log
  level — the `sprintf` + decompile executes even when nobody reads the output. Gate the
  formatting behind the log-level check (make the formatting itself conditional/lazy), don't just
  gate the write.

## Phase 4 — Allocation reduction: identity-preserving rebuilds + linear `get_vars`

- `traverse` / `traverse'` / `replace_expr` / `replace_var_expr` / `subst_var_value` rebuild the
  **entire tree** even when nothing changes (`RebuildShapeCombination` on every node). Make the
  core traversal identity-preserving: after mapping children, if every child came back
  reference-equal, return the *original* node. This
  1. cuts most of the 7.7MB/4.6MB alloc rows,
  2. preserves reference identity so Phase 1's `ReferenceEquals` fast path keeps firing across
     rewrite steps (unchanged subtrees stay the same object through a whole proof).
- `get_vars` (FsExpr.fs:578): `prev @ [v]` inside recursion is O(n²) with list allocs, then
  `distinctBy` on top. Rewrite as a single pass with a mutable accumulator
  (`List<Var>`/`HashSet` keyed on name) preserving first-occurrence order; `get_varsl`/`get_varss`
  reuse it. `get_var_count`, `occurs`, `fail_if_not_has_var` benefit automatically; `occurs` can
  additionally early-exit (stop walking once found) instead of materializing all vars.
- `vequal'`/`has_var` are fine once callers stop being quadratic.

## Phase 5 — Verify + measure

- Prover suite 90/90, Sylvia.Tests.Expressions, example scripts (PropCalculus.fsx, SetTheory.fsx,
  PredCalculus.fsx), ANF oracle sweep — all green with the Phase-0 dual-run assertion on, then off.
- Re-run the BenchmarkDotNet suite and the `Sylvia.Tests.Perf` profile; compare against Phase-0
  baselines. Expected: `deserialize` and `layout_to_string` effectively disappear from the
  profile; `sequal` drops from ~180k inclusive to noise; `trans_implies` wall time drops by an
  order of magnitude class (the 8-atom 142s reconstruction should shrink substantially, though the
  O(|expr|)-per-step kernel cost remains the architectural ceiling).

## Baseline (2026-07-25, Release, string-based sequal)

`dotnet run -c Release --project tests/Sylvia.Tests.Perf` (Stopwatch/GC harness; corpus: small = `p∧q`,
medium = `(p∧q⇒r)=(¬p∨¬q∨r)`, large = 64-clause conjunction of `pᵢ∨qᵢ∨¬rᵢ`):

| Payload | Time | Alloc |
|---|---:|---:|
| trans_implies (first call) | 681 ms | 627 MB |
| trans_implies (second call) | 575 ms | 605 MB |
| sequal small eq | 19.8 us/op | 23.9 KB/op |
| sequal small neq | 54.3 us/op | 71.3 KB/op |
| sequal medium eq | 29.9 us/op | 100.8 KB/op |
| sequal large eq | 3552 us/op | 5.9 MB/op |
| sequal large neq late | 14055 us/op | 18.6 MB/op |
| get_vars large | 110.6 us/op | 312.6 KB/op |
| replace_expr large | 173114 us/op | 334 MB/op |

## Phase 1 results (2026-07-25, structural sequal)

Same harness after replacing `sequal` with the structural comparison:

| Payload | Before | After | Change |
|---|---:|---:|---:|
| trans_implies (first call) | 681 ms / 627 MB | 546 ms / 480 MB | 1.25x |
| trans_implies (second call) | 575 ms / 605 MB | 447 ms / 462 MB | 1.29x |
| sequal small eq | 19.8 us / 23.9 KB | 0.40 us / 384 B | 49x / 62x |
| sequal small neq | 54.3 us / 71.3 KB | 0.21 us / 320 B | 255x / 223x |
| sequal medium eq | 29.9 us / 100.8 KB | 1.84 us / 1.7 KB | 16x / 61x |
| sequal large eq | 3552 us / 5.9 MB | 72.7 us / 90.8 KB | 49x / 65x |
| sequal large neq late | 14055 us / 18.6 MB | 70.6 us / 90.6 KB | 199x / 205x |
| get_vars large | 110.6 us / 312.6 KB | 100.2 us / 312.6 KB | (untouched — Phase 4) |
| replace_expr large | 173114 us / 334 MB | 197.9 us / 307 KB | **875x / 1087x** |

The modest trans_implies gain confirms the remaining prover cost is Phase 2 (template
re-deserialization), Phase 3 (always-on log formatting + decompile), and Phase 4 (rebuild
allocations) — re-profile after Phase 2 to re-rank.

Validation: full prover suite 97/97, Sylvia.Tests.Expressions 22/23 (the 1 failure is a
pre-existing, unrelated MathNet `Infix.parse` test), and `examples/proofs/PropCalculus.fsx`
run clean — all with `SYLVIA_SEQUAL_CHECK=1`, which dual-runs every sequal against the old
string implementation and throws on any disagreement.

Semantic findings the dual-run check surfaced (now encoded in `sequal_structural`):

1. **Named values compare by (payload, name)** — the rendered form prints both, so
   `ValueWithName(true, "c")` ≠ `ValueWithName(false, "c")`. Type is not printed and
   payload equality across types is false anyway.
2. **Members and variables compare by name only** — the rendered form prints no types.
   Two different generic instantiations of `forall_expr`/`exists_expr` are the *same*
   expression to the kernel, and the ch.9 quantifier theorems rely on it (they failed
   under full-MethodInfo equality). Tightening identity to include types would be a
   semantic change to the trusted base, not an optimization — deliberately not done here.
3. **ExprShape tokens are unusable for equality** — the shape token embeds DebugRange
   (source-location) attributes, so token equality distinguishes identical trees quoted
   at different source lines. `sequal_structural` matches node kinds explicitly instead.
   (Note: `try_match`/`apply_first_schema` at FsExpr.fs use `po.Equals to_` on shape
   tokens and may therefore under-match across source locations — worth revisiting.)

## Phase 2 results (2026-07-25, hoisted quotation templates)

Implementation: `FsExpr.specific_call` (+ `CallPattern` type abbreviation) partially applies
`SpecificCall` so the template quotation is deserialized and destructured once at module
init; all inline `SpecificCall <@@ … @@>` sites in FsExpr/Formula/Symbolic/MathNetExpr were
converted to module-level cached matchers (a `: CallPattern` annotation is required — the
partial application otherwise trips the value restriction). The arithmetic patterns
(`Addition` etc.) now match precomputed `op_*` names instead of concatenating per probe.
On the prover side the same hoist was applied to the inline `<@(=)@>`-style templates in
`EquationalLogic.equational_logic_axioms` (up to 10 deserializations per axiom probe) and
the axiom functions in `BooleanAlgebra`/`Integers`/`RealNumbers` (in the generic
`boolean_algebra_axioms` the template binds once per theory construction).

| Payload | Baseline | Phase 1 | Phase 2 | Total change |
|---|---:|---:|---:|---:|
| trans_implies (first call) | 681 ms / 627 MB | 546 ms / 480 MB | 219 ms / 66 MB | **3.1x / 9.5x** |
| trans_implies (second call) | 575 ms / 605 MB | 447 ms / 462 MB | 68 ms / 64 MB | **8.4x / 9.4x** |
| prover test suite wall time | 64 s | 64 s | 43 s | 1.5x |
| sequal medium eq | 29.9 us | 1.84 us | 0.93 us | 32x |
| sequal large eq | 3552 us | 72.7 us | 47.5 us | 75x |
| replace_expr large | 173 ms | 0.198 ms | 0.145 ms | ~1190x |

Validation: prover suite 97/97 with `SYLVIA_SEQUAL_CHECK=1`; `PropCalculus.fsx` and
`SetTheory.fsx` (ALL PASS, exercises the BooleanAlgebra/metaset paths) both clean against
rebuilt Debug DLLs — note the example scripts' `Include.fsx` references **Debug** binaries,
so Debug must be rebuilt before fsi validation counts.

Still open for Phase 2 scope (lower value): `Term.fs` scalar_eqn/scalar_varmap (cold),
`Maxima.fs` (18 sites, CAS-only), `Tactics.fs` quotation literals (mostly success-path),
`EquationalLogic.fs:101/105` splice-built templates in quantifier guards (needs a small
restructure, only fires on quantified candidates), and the per-call destructuring inside
parameterized patterns (`Binary op` still runs SpecificCall's template destructure per
probe — fixable by switching the pattern parameters to cached `CallPattern` matchers, an
API change).

## Phase 3 results (2026-07-25, decompiler confinement + lazy log formatting)

Implementation:

- **Proof.fs**: `_prooflog`/`prooflog`/`alwayslog` now take a thunk — a log line that the
  current level suppresses is never formatted (each line formats via `print_formula`,
  which decompiles the whole expression through Unquote). The per-step message is a
  `Lazy<string>` (capturing the pre-step state so deferred evaluation can't observe the
  state mutation); `Proof.State` exposes the same `(Expr * string) list` content,
  materialized on access. The `theory.Axioms` re-probe done purely for the completion
  message also moved inside the thunk. **Printed output is byte-identical** — verified by
  diffing full PropCalculus.fsx and SetTheory.fsx logs before/after.
- **Term.fs**: `Prop.Display`, `Scalar.Display`, `ScalarRelation.Display`,
  `IndexVar.Name/Display`, `ScalarVar.Name/Label`, `Pred.Symbol` are now lazily cached
  per instance (they back `Term.Equals`/`GetHashCode`, which previously decompiled on
  every comparison/hash); the `ScalarVar` constructor no longer decompiles when a label
  is supplied (`defaultArg` evaluated the decompile unconditionally).
- `find_expr` (decompiles every subterm) turned out to have **no callers** — left as is.

| Payload | Baseline | Phase 2 | Phase 3 | Total |
|---|---:|---:|---:|---:|
| trans_implies (first call) | 681 ms / 627 MB | 219 ms / 66 MB | 217 ms / 55 MB | 3.1x / 11x |
| trans_implies (second call) | 575 ms / 605 MB | 68 ms / 64 MB | 55 ms / 53 MB | **10.5x / 11.4x** |
| trans_implies (warm, LogLevel=0) | — | — | **34.6 ms / 35 MB** | **16.6x / 17x** |
| prover test suite wall time | 64 s | 43 s | 40 s | 1.6x |

The LogLevel=0 row is the number that matters for programmatic use (Giant, SAT
reconstruction, benchmarks): suppressed lemma-machinery lines now cost nothing. The
remaining cold/default-level cost is dominated by console printing of the (intended)
proof trace plus the rules' replay work — Phase 4 (allocation reduction) and the
memoization TODOs pick those up.

Validation: prover suite 97/97 + Expressions 22/23 (pre-existing MathNet parser failure)
with `SYLVIA_SEQUAL_CHECK=1`; PropCalculus.fsx + SetTheory.fsx (ALL PASS) with
byte-identical logs.

## Sequencing and risk

| Phase | Impact | Risk | Notes |
|---|---|---|---|
| 1 (`sequal`) | Very high | Medium — kernel semantics | Dual-run assertion de-risks |
| 2 (templates) | High | Low — mechanical | Helper pattern keeps diffs small |
| 3 (`src`) | Medium-high in real sessions | Low | Mostly laziness/caching |
| 4 (allocs) | Medium, compounds Phase 1 | Low-medium | Identity-preserving rebuild must not change results (add tests) |

Phases 1+2 are where the profile says the time is; do them first, re-profile, then decide how far
to push 3-5.
