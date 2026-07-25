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

## Phase 4 results (2026-07-25, identity-preserving rebuilds + linear get_vars)

Implementation (all in FsExpr.fs):

- `traverse` / `traverse'` are identity-preserving: if the mapped children come back
  reference-equal, the ORIGINAL node is returned instead of an allocated copy (the
  `ShapeVar` case also no longer allocates a fresh `Expr.Var`). Everything built on
  them — `replace_expr`, `replace_var_expr`, `replace_var_var`, `expand`, `get_consts`,
  the theories' rewrite functions — inherits this, and unchanged subtrees now stay
  reference-equal across rewrite steps, feeding sequal's `ReferenceEquals` fast path.
- `replace_first_expr` / `apply_first_schema` / `apply_first_firing` return the original
  node on the no-match path instead of rebuilding the entire spine.
- `get_vars` is a single linear pass with a mutable accumulator + name `HashSet`
  (the old version threaded `prev @ [v]` lists — quadratic in time and allocation —
  then `distinctBy` on top). Order (first occurrence, binders included) and the
  instance-call-receiver quirk are preserved and now locked by tests.

| Payload | Baseline | Phase 3 | Phase 4 | Total |
|---|---:|---:|---:|---:|
| get_vars large | 110.6 us / 312.6 KB | 99 us / 312.6 KB | **23.5 us / 60 KB** | 4.7x / 5.2x |
| replace_expr large | 173114 us / 334 MB | 145 us / 307 KB | **107 us / 223 KB** | ~1600x / ~1500x |
| trans_implies (warm, LogLevel=0) | — | 34.6 ms / 35 MB | 35.2 ms / 35 MB | (unchanged) |

trans_implies is unmoved by Phase 4: the prover's rewrite steps fire near the root and
its rules build fresh spliced quotations per application, so identity preservation buys
little there — the remaining prover cost is rule replay + intended console output
(memoization territory, see the schema-instantiation notes), not traversal allocation.

Validation: prover suite 97/97, Expressions 25/26 (pre-existing MathNet parser failure),
identity-preservation + get_vars-quirk regression tests added, PropCalculus.fsx +
SetTheory.fsx (ALL PASS) with byte-identical logs vs Phase 3 — all under
`SYLVIA_SEQUAL_CHECK=1`.

## Final cumulative results (baseline → Phase 4)

| Payload | Before | After | Change |
|---|---:|---:|---:|
| trans_implies (cold, default log) | 681 ms / 627 MB | 222 ms / 56 MB | 3.1x / 11x |
| trans_implies (warm, default log) | 575 ms / 605 MB | 59 ms / 55 MB | 9.8x / 11x |
| trans_implies (warm, LogLevel=0) | — | 35 ms / 35 MB | 16x / 17x |
| prover test suite | 64 s | 40-54 s | ~1.3-1.6x |
| sequal (small/medium/large) | 20-14055 us | 0.2-47 us | 16-255x |
| get_vars large | 110.6 us | 23.5 us | 4.7x |
| replace_expr large | 173 ms / 334 MB | 0.107 ms / 223 KB | ~1600x |

## Real-world check: SAT reconstruction benchmark (2026-07-25)

`examples/sat/Reconstruct.fsx` (the CaDiCaL LRAT → kernel-checked `⊢ φ` pipeline from
[prover-sat-reconstruction.md](prover-sat-reconstruction.md), the workload whose slowness
motivated this whole effort), re-run against the optimized code with all Debug
dependencies rebuilt — ALL GREEN, goals matched structurally:

| Goal | Before (2026-07-13) | After | Speedup |
|---|---:|---:|---:|
| 3-atom chain | ~5 s | 1.6-1.9 s | ~3x |
| 5-atom chain | ~39 s | 2.1-3.0 s | 13-19x |
| 8-atom chain | 142 s | **4.8-7.2 s** | **~20-30x** |
| 12-atom chain | (never attempted) | **10.2 s** | — |

The growth is now mildly polynomial (~1.2x per added atom: 5→8 atoms 2.3x, 8→12 atoms
2.1x) with no wall in sight — extrapolating, ~16 atoms ≈ 20-25 s and ~20 atoms fits a
minute-scale budget, where the old code needed 142 s for 8. The `Calc.chainImp` /
O(|expression|)-per-kernel-step architectural cost remains the asymptotic ceiling, but
its constant factor is ~20x smaller, which moves the practical ceiling well past where
the fresh-start decision previously sat. (The 8- and 12-atom checks are now permanent
cases in Reconstruct.fsx.)

**Profiling the remaining cost**: `tests/Sylvia.Tests.Perf/Reconstruction.fs` is a
hermetic copy of the pipeline (no CaDiCaL — the chain refutation is synthesized
in-process, since the chain CNFs are unit-propagatable) so a profiler sees only
Sylvia-side reconstruction work. Run the perf exe with the `reconstruct` argument to
profile reconstruction alone (`micro` runs only the Phase 0 micro-payloads; no argument
runs both). Current shape (Release, LogLevel 0): chain-12 ≈ 9.9 s / **15.7 GB
allocated**, and `conjElimAll` over just 12 clauses — the isolated O(n)-`Calc.chainImp`
peel — is ≈ 5.4 s / 8.7 GB, confirming chainImp as the dominant remaining cost and the
first thing to look at in the profile.

## Phase 5 results (2026-07-25, profiler-driven: print_formula without the decompiler)

Profiling the reconstruction payload showed **76.6% of CPU in Unquote's `decompile`**,
reached via `Calc.chainImp` → `LogicalRules.Subst` → `Display.print_src`: rule NAMES
(`"Substitute %s ≡ %s …"`) are formatted eagerly on every rule construction — at every
log level — and `print_formula` routed every quantifier-free (i.e. every propositional)
formula through the decompiler.

Fix (user-directed, in `Display.print_formula`): drop the quantifier-free `print_src`
shortcut so the existing structural connective cases render propositional formulas
(bottoming out at the Var/Const/T/F cases with NO decompilation), render atom leaves
with the now-cheap `sprinte`, and keep `print_src` only as the last-resort fallback for
terms with no structural case (set-algebra operators etc. — `sprinte` has no patterns
for those). Note `sprinte` could not simply replace `print_src` wholesale: it has no
boolean-connective patterns and falls into the MathNet converter, which throws.

**Deliberate display-format change** (the one behavior change in this whole effort):
nested connectives now render with explicit structural parens instead of the
decompiler's F#-precedence-implicit form — `F = F = T` → `(F = F) = T`,
`a ∧ b ∨ a ∧ c` → `(a ∧ b) ∨ (a ∧ c)`, `¬ F` → `¬F`. One parser test updated for the
new form. Both example scripts pass (ALL PASS) with only this class of diff.

| Payload | Phase 4 | Phase 5 | vs baseline |
|---|---:|---:|---:|
| reconstruct chain 8 | 4.5-4.8 s / 7.3 GB | **2.0-2.4 s / 3.0 GB** | 142 s → **~65x** |
| reconstruct chain 12 | 9.9 s / 15.7 GB | **2.2-2.8 s / 6.1 GB** | — |
| conjElimAll 12 clauses | 5.4 s / 8.7 GB | 1.2-1.5 s / 3.3 GB | — |
| trans_implies (cold) | 217 ms / 55 MB | 183 ms / 27 MB | 681 ms → 3.7x |
| trans_implies (warm, default log) | 55-59 ms / 53 MB | **23.7 ms / 25 MB** | 575 ms → **24x** |
| trans_implies (warm, LogLevel=0) | 35 ms / 35 MB | **20.2 ms / 22 MB** | **28x** |

Validation: prover suite 97/97 (with `SYLVIA_SEQUAL_CHECK=1`), Expressions 25/26
(pre-existing), PropCalculus.fsx + SetTheory.fsx ALL PASS.

Remaining in the reconstruction profile after this: the rule-name `sprintf`s still run
eagerly per rule construction (now cheap but nonzero — making `Rule` names lazy is the
next step if profiling still shows them), and the rest is genuine kernel work
(`Taut`/`reduce`/axiom probing over the big conjunction) — the memoization /
architectural territory.

## Phase 6 results (2026-07-25, profiler-driven: lazy pattern/axiom descriptions)

The allocation profile showed `equational_logic_axioms` allocating
`AxiomDescription`/`PatternDescription` pairs on every successful axiom probe, with
`Descriptions.pattern_desc` **decompiling its example quotation eagerly** each time —
and `axiom_desc`/`set_axiom_desc_theory` force-rebuilding the pair. Only `.Name` is read
on the hot path (the `StartsWith "Definition"` check); the description text is almost
never read.

Fix (Descriptions.fs, no call-site changes): `PatternDescription` now carries
`Lazy<string>` — `pattern_desc` defers the decompile until `.Description` is actually
read, and `axiom_desc`/`set_axiom_desc_theory` pass the pattern description through
without forcing. Nothing outside the module destructures the DU cases, so the members'
API is unchanged. A printer swap (`print_formula` for `src`) was NOT possible here —
Descriptions compiles before Display and print_formula itself depends on the Patterns
module — but laziness sidesteps the ordering problem entirely.

| Payload | Phase 5 | Phase 6 |
|---|---:|---:|
| reconstruct chain 8 | 2.0-2.4 s / 3.0 GB | **1.5 s / 2.9 GB** |
| reconstruct chain 12 | 2.2-2.8 s / 6.1 GB | **2.0 s / 5.8 GB** |
| conjElimAll 12 clauses | 1.2-1.5 s / 3.3 GB | 1.05 s / 3.2 GB |

**Phase 6b — fully-lazy descriptions.** The leftovers above were then closed too:
`PatternDescription` is now lazy in BOTH fields (the name is only read when a completed
step is actually displayed), with fully-deferred variants `pattern_desc'`/`pattern_name'`
taking `Lazy` arguments. All 24 literal-example sites in `equational_logic_axioms`, the
2 in Patterns.fs, and the 5 `sprintf … (src op)` name sites (`Reflex`, `Def`,
`BinaryOpDef`*) were converted — so neither the example-quotation deserialization
(`Byte[402]`) nor the name decompiles run on a match that is never displayed. The
eager-signature `pattern_desc`/`pattern_name` remain for the cold call sites (math
theories etc.), unchanged.

| Payload | Phase 5 | Phase 6 | Phase 6b |
|---|---:|---:|---:|
| reconstruct chain 8 | 2.0-2.4 s | 1.5 s | **1.3 s** |
| reconstruct chain 12 | 2.2-2.8 s | 2.0 s | **1.98 s** |
| conjElimAll 12 clauses | 1.2-1.5 s | 1.05 s | 1.05 s |

The 8-atom reconstruction is now ~**109x** faster than the original 142 s. Validation
(both phases): prover suite 97/97 with `SYLVIA_SEQUAL_CHECK=1`, both example scripts
ALL PASS with **byte-identical** logs (only never-read text became lazy).

## Phase 7 results (2026-07-25, user-directed: Expr builders instead of spliced literals)

A spliced quotation literal (`<@@ (%%a:bool) || (%%b:bool) @@>`) re-deserializes its
pickled template on EVERY evaluation — and these literals were the result-construction
mechanism of the S-rule rewrite functions that `traverse` applies on every rule fire,
of `expand_as`/`expand_cast` (`<@ %%e:'t @>` — among the most-called helpers anywhere),
of the `Prop` operator algebra, and of the `Taut`/`Taut'` tactic statements.

Implementation:

- **FsExpr**: `mk_and`/`mk_or`/`mk_not`/`mk_eq_bool`/`mk_neq_bool` builders producing
  EXACTLY the tree shapes the literals produce — critically, `&&`/`||` compile to
  `IfThenElse(a, b, false)` / `IfThenElse(a, true, b)` inside quotations, and the
  builders replicate that, so sequal and all pattern matching are unaffected.
  `expand_as`/`expand_cast` now use `Expr.Cast` instead of a splice; same for
  `Symbolic.symbolic_var` and `Formula.pred_expr`.
- **Formula**: `mk_implies`/`mk_conseq` (the `===>`/`<===` operators live here).
- **EquationalLogic**: all ~45 rewrite-function literals transcribed to builders
  (`_right_assoc` … `_split_range_exists`, `_normalize_with` rebuilds, `_simp_laws`,
  the ANF rules, the QuantifierCollect guards), plus hoisted `forall_expr`/`exists_expr`
  call templates (annotated at `obj` — the same instantiation the inline literals
  defaulted to; only the MethodInfo name matters downstream).
- **Term.fs**: the 8 `Prop` operators (`!!`, `~-`, `*`, `+`, `==`, `!=`, `==>`, `<==`).
- **PropCalculus**: the per-application `Taut`/`Taut'` statement construction.

| Payload | Phase 6b | Phase 7 | vs baseline |
|---|---:|---:|---:|
| trans_implies (warm, default log) | 23.7 ms / 25 MB | **14.3 ms / 16 MB** | 575 ms → **40x** |
| trans_implies (warm, LogLevel=0) | 20.2 ms / 22 MB | **11.2 ms / 14 MB** | **51x** |
| trans_implies (cold) | 183 ms / 27 MB | 136 ms / 17 MB | 681 ms → 5x |
| reconstruct chain 8 | 1.3 s / 2.9 GB | 1.35 s / 2.6 GB | 142 s → **~105x** |
| reconstruct chain 12 | 1.98 s / 5.8 GB | **1.68 s / 5.3 GB** | — |
| conjElimAll 12 clauses | 1.05 s / 3.2 GB | 0.94 s / 3.0 GB | — |

Validation: prover suite 97/97 + Expressions 25/26 (pre-existing) with
`SYLVIA_SEQUAL_CHECK=1`, both example scripts ALL PASS with **byte-identical** logs —
every transcription provably produces the same trees. Left as-is (cold or generic):
`Term.(==)` (generic `'t` equality), `Pred`/`ScalarRelation` lambda splices, the
`pnot` helpers in tests/payloads, and the math-theory literals.

## Phase 8 results (2026-07-25, profiler-driven: per-instance caches for template
destructuring and reflected definitions)

With the literals hoisted, the profile still showed two per-probe reflection costs:
(1) `equational_logic_axioms`' parameterized patterns (`Assoc eq_op eq_op x`, …) re-ran
`SpecificCall`'s template destructuring (Lambdas match + `GetGenericMethodDefinition`)
inside `Binary`/`Unary` on every attempt; (2) `tryGetReflectedDefinitionInstantiated`
(11.6% self) — `expand` probing `MethodWithReflectedDefinition` (an uncached
deserialize/instantiate in FSharp.Core) for every Call node it visits.

Fix (FsExpr + Formula):

- `specific_call_cached` — `specific_call` memoized per template INSTANCE via
  `ConditionalWeakTable` (the hoisted module-level templates are perfect keys; note the
  factory needs an explicit `CreateValueCallback` delegate — a bare lambda uncurries
  because `CallPattern` is itself a function type). `Formula.(|Binary|_|)`/`Binary'`/
  `Unary` now use it, plus `getFuncInfo_cached` for the And/Or fallback guard.
- `try_reflected_definition` — `Expr.TryGetReflectedDefinition` memoized per method in a
  `ConcurrentDictionary` (a reflected definition is static per instantiated method, so
  the cache is exact); `expand` and `body` use cached drop-ins
  (`MethodWithReflectedDefinitionCached`/`PropertyGetterWithReflectedDefinitionCached`).

| Payload | Phase 7 | Phase 8 | vs baseline |
|---|---:|---:|---:|
| trans_implies (warm, default log) | 14.3 ms / 16 MB | **12.2 ms / 12 MB** | 575 ms → **47x** |
| trans_implies (warm, LogLevel=0) | 11.2 ms / 14 MB | **8.8 ms / 10 MB** | **65x** |
| reconstruct chain 8 | 1.35 s / 2.6 GB | 1.32 s / 2.1 GB | 142 s → **~108x** |
| reconstruct chain 12 | 1.68 s / 5.3 GB | **1.34 s / 4.4 GB** | — |
| conjElimAll 12 clauses | 0.94 s / 3.0 GB | **0.72 s / 2.5 GB** | — |

chain-12 now costs barely more than chain-8 — the atom-scaling curve is approaching
linear. Validation: prover suite 97/97 with `SYLVIA_SEQUAL_CHECK=1`, both examples ALL
PASS, logs **byte-identical**. Known remaining (deliberate): `_normalize_with` and the
ANF normalizers sort by `x.ToString()` (StructuredFormat keys) — changing the sort key
changes the canonical operand ORDER, i.e. observable proof output, so it was left alone;
`strengthen_and`-style rule replay is the memoization/architectural frontier.

## Sequencing and risk

| Phase | Impact | Risk | Notes |
|---|---|---|---|
| 1 (`sequal`) | Very high | Medium — kernel semantics | Dual-run assertion de-risks |
| 2 (templates) | High | Low — mechanical | Helper pattern keeps diffs small |
| 3 (`src`) | Medium-high in real sessions | Low | Mostly laziness/caching |
| 4 (allocs) | Medium, compounds Phase 1 | Low-medium | Identity-preserving rebuild must not change results (add tests) |

Phases 1+2 are where the profile says the time is; do them first, re-profile, then decide how far
to push 3-5.
