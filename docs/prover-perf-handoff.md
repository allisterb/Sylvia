# Prover performance — handoff (2026-07-30, extended 2026-08-04)

Entry point for the next session on prover/reconstruction speed. Read this, then
[`prover-sat-reconstruction.md`](prover-sat-reconstruction.md) §7 for the design detail.

§§1–7 as written on 2026-07-30 are committed through `6a6dbf4`. **§1b is a 2026-08-04 addition** and
supersedes two items of the original §5: the reprofile it asked for is done, and the clause-width
hypothesis it named is refuted. §3, §4 §5 and §6 were amended the same day; the numbers in §1 are
unchanged and still stand.

> **Since this handoff was written (same day, uncommitted at the time of writing).** Two things
> happened that a reader of §5 should know about, both found by *using* the now-faster pipeline rather
> than by optimizing it further:
>
> - **The set-theory metatheorem tactics were rerouted from `autoproof_anf` to `PropCalculus.decide`**,
>   lifting their 5-set-variable ceiling. That immediately exposed a real bug: **`Cnf.to_cnf` treated
>   the truth constants `T`/`F` as atoms**, so the SAT route reported *every* goal mentioning one as a
>   non-theorem. Fixed, with tests; see `prover-sat-reconstruction.md` §7 item 7. `dense43` measured
>   unchanged (warm 4→3: 297/341/312 ms before vs 329/309/309 ms after).
> - **`decide`'s routing threshold was re-measured** and stands at 3 — but two of its documented
>   reasons did not survive. The cited **stack overflow on 3-atom xor associativity is gone** (fixed by
>   `distribOr` pruning; that goal now reconstructs in 210 ms), and **`autoproof_anf` turned out not to
>   be complete** — it refused valid CNF⇒DNF goals at 2, 3 and 4 atoms, i.e. inside the range routed to
>   it. `decide` now falls back to the backend when its own prover refuses a goal the ANF oracle calls
>   a theorem.
> - **That completeness bug is fixed** (`prover-automation.md` §3.2b), and the diagnosis is a perf
>   lesson as much as a correctness one: it was never a confluence gap, it was the driver's **move
>   order**. `distrib_and_xor` — the only size-increasing rule — outranked the normalizers in a greedy
>   first-firing driver, so terms were fully expanded before cancellation could shrink them, and the
>   search burned its step budget. Distributing last fixes it and is FASTER: 4-atom chain 396 → 265
>   steps, prover test suite 1 m 43 s → 1 m 08 s. Exactly the mistake §7 item 4 fixed in `distribOr` by
>   pruning inside distribution rather than after. **Simplify before you expand** — twice now.
>
> Neither is a perf finding, but both change what the numbers below are numbers *of*.

---

## 1. Where it stands

Implication chains, Release, warm process (the range at a size is process warmth — a fresh process
measures at the top of it):

| atoms | 2026-07-28 handoff | now |
|---:|--:|--:|
| 20 | 5108 ms | **250 – 460 ms** |
| 24 | 7247 ms | **280 – 440 ms** |
| 32 | — | 464 ms |
| 40 | — | 691 ms |
| 50 | — | 1055 ms |
| 64 | — | 1676 ms |

Nested xor associativity, 4 variables: **141 s → 0.9 s**.

Dense refutations, with the per-LRAT-step figure that is the yardstick that actually travels between
goals:

| goal | atoms | clauses | LRAT adds | before | now | per step |
|---|--:|--:|--:|--:|--:|--:|
| chain 50 | 50 | 51 | 50 | 3075 ms | 1055 ms | 61 → **21 ms** |
| pigeonhole 4→3 | 12 | 22 | 15 | 1811 ms | 741 ms | 121 → **49 ms** |
| pigeonhole 5→4 | 20 | 45 | 48 | 11 514 ms | 3851 ms | 240 → **80 ms** |
| pigeonhole 6→5 | 30 | 81 | 156 | 102 231 ms | 33.6 s | 655 → **215 ms** |

**Per-step cost fell ~3× uniformly across shapes.** 5→4 and 6→5 did not run *at all* before the
`--plain` fix, so "3×" understates those two.

### Against the original target

§1 of the reconstruction doc set "~20–50 atoms in well under a second".

- **On chains: met up to ~40 atoms**, sitting on the bar at 50 (1.05 s).
- **On dense refutations: not met.** A 20-atom pigeonhole is 3.9 s, because it costs what a 60-atom
  chain costs. Practical ceiling there is ~20 atoms for single-digit seconds.
- **Atom count is the wrong unit** and should be retired. Cost tracks LRAT steps × clause-set size ×
  clause width. State future targets that way.

For external calibration: this is still one to two orders of magnitude off HOL4/Isabelle-class SAT
proof reconstruction, and that gap is per-step constant factors, not algorithmic.

---

## 1b. Reprofile (2026-08-04): the cost model

§5.1 asked for a reprofile before picking a target, and §5.4 named clause width as the suspect behind
dense refutations. **The reprofile is done, and clause width is refuted.** Run it with:

```bash
dotnet run --project tests/Sylvia.Tests.Perf/Sylvia.Tests.Perf.fsproj -c Release -- phases
```

Unlike `dense43`, that payload is NOT hermetic — it needs a solver, because a canned trace cannot
tell you how the pipeline divides. Solving is now ~0.4 ms of a multi-second payload, so which backend
runs is immaterial (see §6).

### Where the time goes

Release, warm, each phase timed separately:

| goal | to_cnf | clausify | solve | **refute** | dedup | AC bridge | close | total |
|---|--:|--:|--:|--:|--:|--:|--:|--:|
| chain 8 | 26.2 | 0.1 | 0.40 | **38.8** | 0.2 | 1.9 | 3.9 | 72 |
| chain 32 | 32.7 | 0.0 | 0.37 | **219.9** | 0.3 | 0.7 | 1.4 | 255 |
| pigeonhole 4→3 | 26.3 | 0.0 | 0.38 | **225.4** | 0.2 | 0.5 | 3.2 | 256 |
| pigeonhole 5→4 | 84.3 | 0.1 | 0.44 | **2308.1** | 0.7 | 1.1 | 5.8 | 2401 |

`refute` is 54% of a small goal and 96% of a large one, so it stays the target. Two side findings:

- **The AC-normalize warning is refuted.** Böhme & Weber report that a rewriting-based AC treatment
  is far too slow, and [`prover-z3-reconstruction.md`](prover-z3-reconstruction.md) §3 flagged that as
  applying to our `normalize` / `_chain_simp` clause path, unmeasured. It is **0–3% of total**,
  everywhere. Close that concern.
- **`Cnf.to_cnf` is now the number-two cost** — a flat 26–84 ms, which is **43% of chain 8**. It is
  the fixed tax that process spawn used to be (§6), and nobody has ever profiled it.

### What drives per-step cost

| goal | steps | links | links/step | max input width | max resolvent width | ms/step | **ms/link** |
|---|--:|--:|--:|--:|--:|--:|--:|
| chain 8 | 8 | 8 | 1.0 | 2 | 1 | 4.9 | 4.85 |
| chain 32 | 32 | 32 | 1.0 | 2 | 1 | 6.9 | 6.87 |
| pigeonhole 4→3 | 15 | 59 | 3.9 | 3 | 4 | 15.0 | 3.82 |
| pigeonhole 5→4 | 48 | 332 | 6.9 | 4 | 8 | 48.1 | 6.95 |

`ms/step` spans 10×. Input clause width spans 2→4 and resolvent width 1→8; **neither tracks it**.
**Links per step tracks it exactly**, and `ms/link` collapses into a 3.8–7.0 band across both shapes.
A "step" is not a unit of work — `SAT.rup_chain` unfolds each LRAT hint chain into binary
resolutions, and pigeonhole needs ~7 where a chain needs 1.

### The second variable, isolated

Control: the **same 3-link refutation in every row**, with `A` grown by hypotheses the proof can never
use (fresh variables, so the trace is byte-identical):

| pad | input literals | steps | links | refute ms | ms/link |
|--:|--:|--:|--:|--:|--:|
| 0 | 6 | 3 | 3 | 12.1 | 4.04 |
| 8 | 22 | 3 | 3 | 29.0 | 9.65 |
| 32 | 70 | 3 | 3 | 218.1 | 72.69 |

Same proof, 18× the cost. Per-link cost is a function of `|A|` — linear or slightly worse.

> **`refute` ≈ links × f(|A|).** Per-step cost follows LINKS, not clause width. Per-link cost follows
> `|A|`, because every link's obligation is `A ⇒ clause` and `A` is the whole clause conjunction.

That explains §1's dense-refutation numbers without appealing to width at all: pigeonhole 6→5 has
both more links per step and a larger `A` than any chain.

---

## 2. What changed, and what it cost

Eight commits, `1668035`..`6a6dbf4`. Three lifted a wall; five were profile-driven micro-work.

| # | Change | Effect |
|---|---|---|
| `b50b746` | Prune tautological clauses *inside* `Cnf.distribOr` | xor-assoc-4 clausification 229 s → 1.7 s |
| `3639e07` | `Tactics.Instantiate` + `Schema.p1/p2/p3` | 24-atom chain 4682 → 730 ms |
| `1668035` | `Cadical(?plain)` defaulting true; lazy `Theorem.Name` | pigeonhole 5→4 and 6→5 went from **failing** to working |
| `772c120` | `expand` destructures each node once | −7.7% warm, −11% allocations |
| `955ac2e` | `Term.(==)` builds via `mk_eq_bool` | −3% warm, −17% cold |
| `438cf08`/`44d9b98` | `logLevel > 0` guard on `[Axiom]`/`[Lemma]` headers; thunk API removed | −13% |
| `740ea4e` | `Theory.AxEquiv` cached on its **input** | −17% wall clock, −29% allocations |
| `49f4eb7` | `Display.print_formula` cached | −48% steady state, −68% allocations |
| `6a6dbf4` | `PropConst` constructor fix (it threw unconditionally) | correctness; zero prior callers |

---

## 3. What was measured and REFUTED

This is the most reusable part. Do not redo these.

- **Hash-consing / cheap term identity, "the deepest architectural limit".** Rule application — the
  actual term rewriting — is **0.4–0.8% of CPU**. Interning terms would have chased a ≤15% item. The
  claim was in the docs and in memory for months; it is withdrawn.
- **Expansion-stable marking of `traverse` results.** Skips 64% of node visits, −37.5% allocations,
  **zero** wall clock. Only 52 of 4,696,680 nodes ever change: traversal is cheap work done often,
  not expensive work.
- **A `traverse`-level cache.** Also unsound as usually proposed — `traverse` takes `f`, and nine
  different `f` values are passed (`rexpand vars`, `_dual`, `norm`, `subst(lhs,rhs)`, …), several of
  them freshly-allocated closures.
- **Caching `print_atom`.** Made it *worse*: 262 → 295 ms. Once `print_formula` is memoized, this is
  one match plus one concatenation, and a `ConditionalWeakTable` probe costs more.
- **Removing the logging thunks.** Output-neutral, measured zero — closures are ~0.1% of allocations.
  (Kept anyway, for clarity; it is what made the level-0 bug visible.)
- **`sprintf` → concatenation in the log headers.** Zero. F# caches literal format strings.
- **`print_formula` is not 40% of runtime.** An earlier claim of mine. That measurement included
  console I/O and eager `Theorem.Name`; both are gone.
- **Clause width does not drive per-step replay cost** (2026-08-04, §1b). It was §5.4's named
  suspect. Across chains and pigeonholes, input width spans 2→4 and resolvent width 1→8 while
  `ms/step` spans 10×; neither correlates. RUP **links per step** does, exactly.
- **The AC-`normalize` bridge is not slow** (2026-08-04, §1b). Böhme & Weber's warning that
  rewriting-based AC is far too slow was flagged as applying to our clause path and is now measured:
  **0–3% of total**, on every shape. Withdrawn.

### The two lessons behind those

**Cheap short-lived allocation is free here; large string building is not.** Five allocation-shaped
hypotheses measured zero. The one allocation win that mattered (`print_formula`, −68%) also cut time
48%, because those allocations were big strings. **Pick targets from the CPU view, not the allocation
view.**

**When a cache shows a high hit rate but no gain, check which side of the expensive call you keyed
on.** My first `AxEquiv` cache keyed on `expand a` — caching the 0.11% recognition after paying the
2.49% expansion. It showed 60% hits and nothing else, and I generalised that into "caching doesn't
help here" for four rounds. Both of the largest wins were hiding behind that error.

---

## 4. How to measure (this bit matters)

```bash
dotnet run --project tests/Sylvia.Tests.Perf/Sylvia.Tests.Perf.fsproj -c Release -- dense43
```

`-- dense` adds pigeonhole 5→4 (~4 s). Both are hermetic: the LRAT traces are canned, so no solver
runs and the profile is entirely Sylvia-side replay. A guard fails loudly if clausification output
drifts from what the traces were generated against.

```bash
dotnet run --project tests/Sylvia.Tests.Perf/Sylvia.Tests.Perf.fsproj -c Release -- phases
```

`-- phases` is the §1b payload: the phase split, the per-step structure table, and the `|A|` control.
It is the one measurement here that is NOT hermetic — a canned trace cannot tell you how the pipeline
divides — so it needs a solver, and it prints a skip message rather than failing if none is found.

- **Build Release.** Every `.fsx` under `examples/` references Debug DLLs, and Debug is ~1.5× slower.
- **Measure warm.** A fresh `dotnet run` spends over a second in JIT on a 600 ms payload. `runDense`
  repeats the payload; read runs 2+. Three of this session's early conclusions were noise read as
  results, and the `Term.(==)` fix was wrongly discarded because of it.
- **A/B properly**: `git stash` one side, rebuild, run the identical script. Ranges here overlap at
  ±5%, so single samples decide nothing.

---

## 5. What to try next, ranked

Reordered after the §1b reprofile. Items 1 and 4 of the previous list are DONE and REFUTED
respectively; the rest carry over.

1. **Get `A` out of the per-link obligation.** This is the deepest lever the cost model exposes.
   Every one of pigeonhole 5→4's 332 links proves `A ⇒ clause`, so each manipulates a term carrying
   all 100 input literals — and the control experiment shows that factor is the whole story. It is
   not inherent to the refutation: working with bare clauses under an assumption and discharging once
   would make per-link cost O(clause) instead of O(|A|). **This is a design job, not a drop-in** —
   `Deduce` / `Deduce'` in `Proof.fs` are substitution devices, not a hypothesis-discharge rule, so
   the deduction-theorem shape has to be built and its soundness argued.
2. **Shrink `A` to the clauses the refutation actually uses.** The cheap version of item 1, and it
   needs no new kernel capability: the LRAT antecedents already name the used clauses, so prove
   `A_used ⇒ F` and weaken to `A ⇒ F` once at the end. **Scope honestly** — on chains and pigeonhole
   every clause is used, so this buys nothing on the benchmark shapes. It is worth 18× on the §1b
   control, and the control is the shape of a real Sledgehammer-style call carrying a fact list.
   `SAT.Native` already exposes assumption cores (`sc_failed`), which gives the minimized hypothesis
   set for free; it is not yet surfaced through `ISatBackend`.
3. **Cut links.** `rup_chain` unfolds every LRAT hint chain into binary resolutions — 48 steps become
   332 links on pigeonhole 5→4. Fewer, larger kernel steps would cut the multiplier directly, but
   needs a resolution rule that consumes a whole propagation chain at once.
4. **Profile `Cnf.to_cnf`.** Flat 26–84 ms and never looked at; it is 43% of a small goal now that the
   solver is free. The first thing to establish is whether it is the clausification or the kernel
   equivalence proof.
5. **Named candidates not yet tested** (carried over): an allocation-free `specific_call` (was 5.4%
   self CPU), `traverse` without `ExprShape` (4.8%), and the source of `FSharpExpr.Deserialize40`
   (3.7%, not located — the `EquationalLogic` templates are already hoisted and `Term.(==)` was a red
   herring). All three are in the "asking quotations what they are" family.
6. **`state <- state @ [(_state, msg)]`** in the `Proof` step loop is an O(n) append per step, so
   O(n²) per proof. Harmless for the 1–4-step proofs the replay builds; would bite a long derivation.
7. **Schema coverage.** Böhme & Weber wrap 230+ schematic theorems, covering 76% of their `rewrite`
   obligations; we wrap six (five in `SatProof`, plus `trans_implies` in `Calc`). `Tactics.Schema` was
   the single biggest win to date, so more of it is tempting — but the cost model says per-link cost
   is dominated by `|A|`, not by how a link's lemma is obtained, so measure before investing.

---

## 6. Environment traps

- **`bin/cadical.exe` is an MSYS2 build** and needs `msys-2.0.dll` (in `C:\Git\usr\bin`) on PATH.
  Without it the process dies with `0xC0000135` and the wrapper reports "the solver exited without a
  verdict", which looks exactly like a solver bug. `dotnet test` from PowerShell fails three SAT
  tests this way; from the Bash/MSYS shell it passes. **Run the gate from Bash.**
- **The native backend avoids that entirely** (2026-08-04). `SAT.Native.CadicalNative` calls
  `bin/sylvia_cadical.dll` in process; it is statically linked and loads from a plain Windows process
  with nothing on PATH. It also removes the CLI's fixed **~18.5 ms per solve** of process spawn and
  DIMACS/LRAT file round-trip — measured constant across every goal shape, because CaDiCaL's actual
  solving at our sizes is sub-millisecond. Solve went 18.5 ms → 0.26 ms. That is 65% of a small
  goal's total and 4% of a large one's, which is why §1b now shows solving as a rounding error.
  Point it somewhere else with `SYLVIA_CADICAL_NATIVE`. See `examples/sat/NativeBench.fsx`.
- An IDE-hosted FSI session (`--fsi-server`) locks `Sylvia.Prover.dll` and blocks builds.
- In `.fsx` here, `set [...]` resolves to Sylvia's set-comprehension builder — use `Set.ofList`.
- Do not `open Sylvia.Tactics` in a script that opens `PropCalculus`; it shadows the specialized
  `Taut`/`Commute`.

## 7. The gate

Unchanged, and everything in this session passed all of it:

```bash
SYLVIA_SEQUAL_CHECK=1 dotnet test tests/Sylvia.Tests.Prover/Sylvia.Tests.Prover.fsproj
```
```bash
dotnet fsi examples/proofs/AdversarialSweep.fsx
```

plus the nine scripts under `examples/proofs`, `examples/sat`, `examples/atp`. Now **127/127**.

For anything touching proof structure or rendering, also diff the proof logs before and after and
**classify every difference** — capture "before" with `git stash` plus a rebuild. Every change this
session was either byte-identical or had every difference accounted for (the log-level guard removed
exactly `[Lemma]`/`[Axiom]` lines and nothing else; `Reconstruct.fsx`'s remaining diffs are its own
`(131ms)` timings).
