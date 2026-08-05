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
| chain 8 | 17.3 | 0.1 | 0.39 | **35.4** | 0.2 | 1.8 | 3.8 | 59 |
| chain 16 | 41.1 | 0.1 | 0.37 | **98.1** | 0.1 | 0.8 | 1.9 | 142 |
| chain 32 | 33.1 | 0.0 | 0.37 | **184.3** | 0.3 | 0.6 | 7.7 | 227 |
| pigeonhole 4→3 | 27.1 | 0.0 | 0.38 | **253.9** | 0.2 | 0.5 | 2.8 | 285 |
| pigeonhole 5→4 | 91.3 | 0.1 | 0.44 | **2536.5** | 0.8 | 1.3 | 6.2 | 2637 |

`refute` is 60% of a small goal and 96% of a large one, so it stays the target. Two side findings:

- **The AC-normalize warning is refuted.** Böhme & Weber report that a rewriting-based AC treatment
  is far too slow, and [`prover-z3-reconstruction.md`](prover-z3-reconstruction.md) §3 flagged that as
  applying to our `normalize` / `_chain_simp` clause path, unmeasured. It is **0–3% of total**,
  everywhere. Close that concern.
- **`Cnf.to_cnf` is the number-two cost** — a flat 17–91 ms, which is **29% of chain 8**. It is the
  fixed tax that process spawn used to be (§6), and nobody has ever profiled it.

### What drives per-step cost

| goal | steps | links | links/step | max input width | max resolvent width | ms/step | **ms/link** |
|---|--:|--:|--:|--:|--:|--:|--:|
| chain 8 | 8 | 8 | 1.0 | 2 | 1 | 4.9 | 4.85 |
| chain 32 | 32 | 32 | 1.0 | 2 | 1 | 6.9 | 6.87 |
| pigeonhole 4→3 | 15 | 59 | 3.9 | 3 | 4 | 15.0 | 3.82 |
| pigeonhole 5→4 | 48 | 332 | 6.9 | 4 | 8 | 48.1 | 6.95 |

The structural columns are exact counts; treat the two timing columns as indicative only, for the
reasons in the next section. `ms/step` spans 10×. Input clause width spans 2→4 and resolvent width
1→8; **neither tracks it**. **Links per step does**, and `ms/link` collapses into a 3.8–7.0 band
across both shapes. A "step" is not a unit of work — `SAT.rup_chain` unfolds each LRAT hint chain
into binary resolutions, and pigeonhole needs ~7 where a chain needs 1.

### The cost model, and its out-of-sample test

The first version of this section said `refute ≈ links × f(|A|)`. That was **under-specified**, and
validating it out-of-sample is what exposed the gap. Two corrections, both worth the space because
each was a wrong turn a reader could repeat:

1. **`refute` has a setup phase that is often the majority of it.** `conj_elim_all` derives `A ⇒ Cᵢ`
   for every input clause before any replay happens — `O(clauses)` chain-implications, each over a
   statement containing `A`. It is 17–60% of `refute` on the goals here, and **95%** on the padded
   control. The control's headline "18× for an unchanged proof" was almost entirely THIS, not the
   link loop, because the control holds links at 3 while growing the clause count.
2. **Measuring it needs fresh variable names.** `conj_elim_all` goes through `elimR = Memo.p2`, so
   timing it before `refute` warms `refute`'s own setup — the first decomposition produced a
   *negative* loop time that way. Every trial below uses structurally identical goals with distinct
   variable names, so each measurement is cold.

A quantitative law of the form `refute ≈ |A| × (K₁·clauses + K₂·links)` was fitted and **it did not
validate**. It predicted the held-out pigeonhole 6→5 to +1% on one run and −16% and −73% on two
others of the same computation, and its per-goal errors span −53% to +27%. Do not resurrect it
without reading the measurement trap below first. What survives is qualitative, and sharper than the
law would have been anyway.

### The measurement trap (read before profiling this pipeline)

Timings here are only reproducible in **a fresh process, warmed on a goal DIFFERENT from the payload**.
Two ways to get this wrong, both of which I did:

- **Payloads that share variable names contaminate each other.** `elimR` is a `Memo.p2` keyed
  structurally, so `chain 8`'s clauses are a subset of `chain 32`'s and warm its setup. Measured
  fresh, `chain 32`'s `conj_elim_all` is ~351 ms; measured after chains 8/16/24 in the same process,
  ~200 ms. The `-- phases` table above still has this — it is fine for the phase *split*, which is
  what it exists to show, and wrong for cross-goal comparison.
- **Warming on the payload itself with fresh names is worse.** Fresh names miss the memo and add
  entries that are never evicted, so the cache grows without bound and everything after slows down.
  That alone made pigeonhole 6→5 measure **104 s instead of 34 s.**

Done properly, repeat runs are tight: pigeonhole 5→4 measured 2471 / 2523 / 2530 ms across three
fresh processes. Chain 32 gave 371 / 378 / 512 — so expect the occasional 35% outlier and take three.

### What is robust

Measured one goal per fresh process (`-- model "<goal>"`):

| goal | clauses | \|A\| | links | setup | loop | refute | setup share |
|---|--:|--:|--:|--:|--:|--:|--:|
| chain 16 | 17 | 32 | 16 | 94 | 83 | 176 | 53% |
| chain 32 | 33 | 64 | 32 | 351 | 20 | 371 | **95%** |
| pigeonhole 4→3 | 22 | 48 | 59 | 202 | 516 | 718 | 28% |
| pigeonhole 5→4 | 45 | 100 | 332 | 566 | 1906 | 2471 | 23% |
| pigeonhole 6→5 | 81 | 180 | 1886 | 1335 | 32848 | 34183 | **4%** |

1. **`refute` dominates the pipeline** — 54% of a small goal, 96% of a large one. Every run agrees.
2. **Which HALF of `refute` dominates swings completely with shape.** `conj_elim_all` is ~95% of a
   chain-32 reconstruction and ~4% of pigeonhole 6→5. There is no single hot spot: sparse goals are
   setup-bound, dense goals are loop-bound. **This is the most actionable thing here**, and the fitted
   law would have obscured it.
3. **Per-step cost follows LINKS, not clause width.** A step is not a unit of work — `rup_chain`
   unfolds each LRAT step into 1 link on a chain and 12.1 on pigeonhole 6→5.
4. **Everything grows with `|A|`,** in direction if not by a reliable exponent — `A` is in the
   statement of every setup implication and every link's obligation.

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

1. **Get `A` out of the obligations.** The deepest lever, and after item 3 was closed into it, the
   ONLY one that addresses either half of `refute`. `A` is in the statement of every setup implication
   *and* every link's obligation, so it is the single quantity that matters whether a goal is
   setup-bound or loop-bound; and instantiation cost is priced by statement size (item 3). Working
   with bare clauses under an assumption and discharging once would replace `|A|` with clause width —
   on pigeonhole 6→5 that is 180 against a mean width near 2.
   **The ceiling is measured, not extrapolated** (`-- ceiling`). `resolveStep` is the entire A-free
   half of a link — the actual resolution inference, at clause scale — and its arguments come out of
   `rup_chain`, so it can be timed for every link of a real refutation without running the replay:

   | goal | links | loop ms | A-free ms | A-free share | loop ceiling |
   |---|--:|--:|--:|--:|--:|
   | chain 16 | 16 | 89.6 | 26.6 | 30% | 3× |
   | chain 32 | 32 | 96.8 | 6.0 | 6% | 16× |
   | pigeonhole 4→3 | 59 | 191.7 | 13.8 | 7% | 14× |
   | pigeonhole 5→4 | 332 | 2455.4 | 137.2 | **6%** | **18×** |

   **~94% of a link is carrying `A`**, so the loop alone is worth roughly 15–20× (a scratch run of
   the same thing gave 22–25×; treat it as an order of magnitude, not a figure). And setup does not
   get faster under this change — it *disappears*, since with the clauses available as assumptions
   there is nothing to eliminate. Both halves together put `refute` at **order 10–30×**.

   I had estimated ~400× from item 3's micro-benchmark. That was an order of magnitude optimistic —
   the second time extrapolation has misled here. Take the measured number.

   **The mechanism is smaller than it looked.** I previously wrote that `Deduce` / `Deduce'` are
   "substitution devices, not a hypothesis-discharge rule". **That was wrong.** `Rule.Deduce` IS the
   deduction theorem, and the kernel already checks it (`Proof.fs` ~294–307): given
   `t : (B₁ ∧ … ∧ Bₖ) ⇒ C`, it verifies every `Bᵢ` is a conjunct of the CURRENT goal's antecedent,
   then rewrites `C ↦ T` in the consequent. Assuming the antecedent is therefore already sound,
   already implemented, and already used (`PredCalculus` 9.7).

   What it could not do is accumulate. Resolution derives *new* clauses, and a derived clause is not
   a conjunct of `A`, so the guard rejected it at the second level. **That gap is now closed in the
   kernel** (2026-08-04) — the remaining work is rewiring `SatProof`, which has not been done:

   - **Proof state carries a set `Δ` of established facts**, per `Proof`, initially empty. Indexed by
     `skey` and `sequal`-verified on every hit, the `Memo` discipline — a key collision is a miss,
     never a wrongly-admitted premise. A list would be O(|Δ|) per lookup and Δ reaches ~1900 links.
   - **`Rule.Establish`**: given `t : (∧Bᵢ) ⇒ Z` with every `Bᵢ ∈ conjuncts(A) ∪ Δ`, adds `Z` to `Δ`
     and leaves the expression completely untouched. The KERNEL checks coverage, not the combinator.
   - **`Deduce`'s guard generalised** from `conjuncts(A)` to `conjuncts(A) ∪ Δ`.

   Four kernel tests pin it: a forward chain reaching a consequent `Deduce` alone cannot; the same
   proof minus the establishing step being refused (so `Establish` is load-bearing, not decorative);
   `Establish` refusing an uncovered premise; and `Establish` refused when the goal is not an
   implication. Gate 131 → **135**, `AdversarialSweep` ALL CLEAR, `VerifyRuleFixes` and the three
   proof examples unchanged.

   The refutation then becomes: no `conj_elim_all` at all, one `Establish` per link carrying
   `resolveStep`'s clause-scale theorem, and one closing `Deduce` of `(X ∧ Y) ⇒ F` that rewrites the
   consequent `F` to `T`.

   Soundness is induction on `Δ`, and it is short: the invariant is `A ⊨ Z` for every `Z ∈ Δ`. Base —
   conjuncts of `A` are entailed by `A`. Step — if every `Bᵢ` is entailed by `A` and `⊢ (∧Bᵢ) ⇒ Z`
   is a theorem, then `A ⊨ Z`. `Deduce`'s existing argument then goes through unchanged over the
   larger set.

   **Both risks I flagged earlier were dissolved, not managed.** The equational deduction theorem's
   side conditions on substitution and Leibniz are not in play, because `Establish` rewrites nothing.
   And "verify the body used only the declared assumptions" needed no axiom-firing record, because
   the check is local to each step.

   **What remains is the `SatProof` rewiring**, and it is not small. `refute` currently threads
   `A ⇒ ·` explicitly and returns `A, Theorem option`; under `Establish` it would instead emit a step
   list — one `Establish` per link carrying `resolveStep`'s clause-scale theorem, then a closing
   `Deduce` — to be run inside a single `theorem prop_calculus (A ==> F) [...]`. `conj_elim_all` and
   the `resolveUnder` plumbing (`conj`, `combine_implies`, `mp`, `chain_imp`) all go away. Do it
   behind the existing `-- model` / `-- ceiling` measurements so the predicted 10–30× is checked
   rather than assumed.
2. **Shrink `A` to the clauses the refutation actually uses.** The cheap version of item 1, needing
   no new kernel capability: the LRAT antecedents already name the used clauses, so prove
   `A_used ⇒ F` and weaken to `A ⇒ F` once at the end. It cuts `clauses` and `|A|` together, and the
   model is quadratic in that pair — the padded control goes from 283 ms to single digits.
   **Scope honestly**: on chains and pigeonhole every clause is used, so this buys nothing on the
   benchmark shapes. It pays on goals carrying irrelevant hypotheses, which is what a real
   Sledgehammer-style call with a fact list looks like. `SAT.Native` already exposes assumption cores
   (`sc_failed`) and would hand over the minimized set for free; not yet surfaced on `ISatBackend`.
3. ~~**Attack `conj_elim_all` on its own.**~~ **Tried and closed — it is not independently fixable,
   it IS item 1.** It looked like the best value for ordinary goals: measured fresh it is 95% of
   `refute` on chain 32 and 53% on chain 16 (against 4% on pigeonhole 6→5), and chains are what real
   callers hit while pigeonholes are what the benchmark hits. Two probes closed it:
   - **The repeated `rest j` term building is ~1% of it.** `rest j` is called ~2n times and each
     rebuilds an O(|A|) conjunction, so the function is O(n²) in term construction despite its
     docstring's "ONE O(n) pass" (which is true of `chain_imp` calls only). Measured: 0.21 ms of
     34.0 on chain 16, 1.79 of 285.6 on pigeonhole 5→4. Not the cost. Do not bother precomputing it.
   - **The cost is the schema instantiations, and each is LINEAR in the size of the statement it
     produces.** A `Tactics.Schema.p2` instantiation goes 18.9 µs → 678.2 µs as its result grows from
     1 to 128 conjuncts: ~15 µs fixed plus **~4.4 µs per conjunct**, flat from k=8 to k=128. So "the
     `chain_imp` calls" and "the `A`-sized statements they carry" were never two hypotheses; they are
     one. Reproduce with `-- conjelim`.

   The structural conclusion is the useful part: **`conj_elim_all` emits `n` theorems whose statements
   each contain `A`, so its output alone is Ω(n·|A|)** — it cannot be made faster without changing
   *what it produces*. Under item 1 it would not exist: with the clauses available as assumptions
   there is nothing to eliminate, and the whole phase disappears rather than getting cheaper.
4. **Cut links.** `rup_chain` unfolds every LRAT hint chain into binary resolutions — 48 steps become
   332 links on pigeonhole 5→4. Fewer, larger kernel steps would cut the multiplier directly, but
   needs a resolution rule that consumes a whole propagation chain at once.
5. **Profile `Cnf.to_cnf`.** Flat 26–84 ms and never looked at; it is 43% of a small goal now that the
   solver is free. The first thing to establish is whether it is the clausification or the kernel
   equivalence proof.
6. **Named candidates not yet tested** (carried over): an allocation-free `specific_call` (was 5.4%
   self CPU), `traverse` without `ExprShape` (4.8%), and the source of `FSharpExpr.Deserialize40`
   (3.7%, not located — the `EquationalLogic` templates are already hoisted and `Term.(==)` was a red
   herring). All three are in the "asking quotations what they are" family.
7. **`state <- state @ [(_state, msg)]`** in the `Proof` step loop is an O(n) append per step, so
   O(n²) per proof. Harmless for the 1–4-step proofs the replay builds; would bite a long derivation.
8. **Schema coverage.** Böhme & Weber wrap 230+ schematic theorems, covering 76% of their `rewrite`
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
