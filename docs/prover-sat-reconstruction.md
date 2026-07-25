# SAT-Backed Proof Reconstruction: CaDiCaL, `resolve`, and the LRAT-Replay Pipeline

*Design and implementation notes — 2026-07-13. Companion to
[`prover-automation.md`](prover-automation.md) and [`prover-e-atp.md`](prover-e-atp.md).*

**Status at a glance.** The pipeline **CaDiCaL → LRAT → kernel-checked `⊢ φ`** works
end-to-end for propositional goals with NO atom-count ceiling (verified through 12 atoms — Peirce,
implication chains, biconditionals). New trusted theorems `resolve` and `combine_implies`, a recursive CNF converter
`Cnf.toCnf`, a general `Memo` combinator, and the `Sylvia.Solver.CaDiCaL` project are all in the tree;
suite is **104/104**.
Both step 1 (the resolution replay) and step 2 (the CNF-equivalence link) now scale.

As of **2026-07-25** the replay is also *complete over the trace*: every LRAT step is replayed, not
just the binary ones, and merge resolvents are discharged (§4.7–4.9). That is what took the pipeline
from "implication chains" to arbitrary refutations — pigeonhole, distributivity, `≡`-chains, the
full 8-clause refutation over 3 variables. The remaining limitations are **speed**, and
clausification blowup in formula *size* (not atom count) — see §7.

Runnable demos:

```
dotnet fsi examples/sat/CaDiCaL.fsx       # decide validity + the resolve rule in action
dotnet fsi examples/sat/Reconstruct.fsx   # full pipeline: LRAT → ⊢ φ, kernel-checked
```

Both require the bundled CaDiCaL at `bin/cadical.exe` (MSYS2/mingw64 build, v3.0.0).

---

## 1. The goal: a *scalable, trace-emitting* propositional decider

Sylvia's prover certifies a theorem by producing an explicit, kernel-checked sequence of
rewrite steps. For the **propositional** fragment we already had two ways to close a goal:

- `PropCalculus.valid` / `equiv` — a *decision tool* (ANF / Zhegalkin normal form). Fast, but it
  only answers *yes/no*; it is **out of the trusted base** and emits no proof.
- `PropCalculus.autoproof_anf` — a *complete, trace-emitting* decider that DOES emit a checkable
  proof. But it is **exponential in the number of distinct atoms** (~21 s at 4 atoms, non-terminating
  at 6) and is guarded at `autoproof_max_atoms = 5`.

That ~5-atom ceiling caps *all* native reconstruction — most importantly the ∀-instantiation step in
the [E Sledgehammer loop](prover-e-atp.md) — and it is the wall this work removes. The research
question is **not** *deciding* (we can already decide); it is **emitting a kernel-checkable trace
efficiently**. See the memory anchor `prover-scalable-prop-prover` for the framing.

The plan chosen (of several — CDCL+replay, guided ANF, native resolution, tableau): **run a
state-of-the-art SAT solver, take its resolution refutation as a DRAT/LRAT certificate, and *replay*
that certificate as native Sylvia kernel steps.**

## 2. Why CaDiCaL, and the validity↔UNSAT duality

The starting point was reading [`reference/papers/dpllt.pdf`](../reference/papers/dpllt.pdf)
(Ganzinger, Hagen, Nieuwenhuis, Oliveras, Tinelli — *DPLL(T): Fast Decision Procedures*). That is an
**SMT** paper; its theory-solver half is off-target for pure propositional logic, but two ideas carried
over:

1. §3 is a clean description of a modern **CDCL** engine (2-watched literals, VSIDS, restarts, 1UIP
   clause learning).
2. Its `Explanation` operation / implication graph is the seed of **proof emission** — the modern,
   standardized form of which is the **DRAT/LRAT** proof format.

**The duality that makes this work.** Sylvia proves *validity* (`⊢ φ`); a SAT solver decides
*satisfiability*. They are dual:

> φ is valid  ⇔  ¬φ is unsatisfiable.

So to prove φ we feed `CNF(¬φ)` to the solver; it returns **UNSAT**, and its UNSAT proof is a
**refutation of ¬φ**. Each step of an LRAT proof names the exact antecedent clauses that entail the
new clause by unit propagation (RUP), so replaying it needs **no search** — the hints *are* the plan.
The terminal empty clause is the ⊥ of a proof by contradiction, which closes back to `⊢ φ`.

**CaDiCaL specifically** (over Z3's DRAT): it emits **LRAT directly** (`--lrat --no-binary`, no
`drat-trim` hop), its proof output is reference-quality, and the user built it natively via MSYS2.
Invocation: `cadical -q --lrat --no-binary in.cnf out.lrat`; exit **20 = UNSAT**, **10 = SAT**. Keep it
on **RUP-only** proofs (no heavy inprocessing) — RAT steps are only satisfiability-preserving and have
no forward reading.

## 3. The pipeline

```
   goal φ
     │  cnfOfNegatedGoal                          (F#, Sylvia.Solver.CaDiCaL)
     ▼
   CNF(¬φ)  ──dimacsOf──▶ DIMACS ──cadical──▶ UNSAT + LRAT proof
     │                                              │  parseLrat
     │                                              ▼
     │                                        LratStep list
     │   ┌──────────────────────────  STEP 1  ──────────────────────────┐
     │   │  fold each binary step through PropCalculus.resolve,          │
     │   │  AC-matched to canonical clauses, threaded through the input   │
     │   │  conjunction A with combine_implies + Calc.chainImp            │
     │   └───────────────────────────────┬───────────────────────────────┘
     ▼                                    ▼
   A = ∧ Cᵢ                    R : (∧ Cᵢ) ⇒ F       (kernel-checked refutation)
     │   ┌──────────────────────────  STEP 2  ──────────────────────────┐
     │   │  ¬φ = A  (CNF-equivalence);  rewrite R;  Contradiction        │
     │   └───────────────────────────────┬───────────────────────────────┘
     ▼                                    ▼
                                        ⊢ φ    (kernel-checked Theorem)
```

Following the E-integration precedent (solver code depends only on `Sylvia.Expressions`; the
kernel-level reconstruction lives in `.fsx` scripts that open both the solver and the prover), the
**solver never enters the trusted base**. The kernel replay is what certifies.

## 4. What was added to the project

### 4.1 `Sylvia.Solver.CaDiCaL` (new project)

`src/lang/solvers/Sylvia.Solver.CaDiCaL/` — `namespace Sylvia; module SAT`. Filed alongside
`Sylvia.Solver.Z3` because a SAT solver *is* a solver (E, by contrast, is an ATP under `atp/`).
Depends only on `Sylvia.Expressions`. Public surface:

| Value | Type | Role |
|-------|------|------|
| `cnfOfNegatedGoal` | `Prop -> CnfProblem` | Clausify **¬goal** (direct NNF+distribute). Atoms stay 1‑1 with Sylvia `Prop`s — the key property the replay depends on. |
| `dimacsOf` | `CnfProblem -> string` | Render DIMACS CNF text. |
| `Cadical(?exePath,?timeoutMs)` | class | `.Solve(cnf)` / `.Prove(goal)` — runs CaDiCaL with a wrapper-enforced timeout (like `EProver`), parses the `v`-line countermodel on SAT. |
| `parseLrat` | `string -> LratStep list` | `Add(id, literals, hints) \| Delete(afterId, ids)`. |
| `reconstructionPlan` | `CnfProblem -> LratStep list -> ResolutionStep list` | The integer proof lifted to Sylvia `Prop` obligations (`clause ⇐ antecedents`, empty clause = `F`). |
| `rupChain` | `(int -> Clause option) -> Clause -> int list -> Result<RupChain,string>` | Unfold ONE LRAT step's hints into an explicit chain of binary resolutions (see §4.7). |
| `litProp` / `clauseProp` | | Build a `Prop` from a DIMACS literal / clause. |

Design note: clausification is **direct NNF+distribute** (worst-case exponential in formula size, but
keeps atoms in 1‑1 correspondence with `Prop`s). Tseitin is the scalable upgrade but complicates the
replay (auxiliary variables need definitional-clause discharge) — deferred.

### 4.2 `PropCalculus.resolve` — binary resolution (new trusted theorem)

The workhorse. Added after `trans_implies` in
[`PropCalculus.fs`](../src/lang/core/Sylvia.Prover/Theories/PropCalculus.fs):

```fsharp
resolve (p:Prop) (q:Prop) (x:Prop) : Theorem      //  ((p ∨ x) ∧ (¬x ∨ q)) ⇒ (p ∨ q)
```

This is propositional (ground) resolution — the core of Robinson's rule minus unification (which SAT
does not need). **The proof is a re-orientation of transitivity, not FOIL/ANF**: reading the two
clauses as implications `(¬p ⇒ x)` and `(x ⇒ q)` gives `(¬p ⇒ q)` ≡ `(p ∨ q)` via `trans_implies`
(Gries 3.82a):

```fsharp
double_negation p |> Commute |> at [left_branch; left_branch; left_branch]   // p ↦ ¬¬p
ident_implies_not_or (-p) x |> Commute |> at [left_branch; left_branch]      // (¬¬p ∨ x) ↦ (¬p ⇒ x)
ident_implies_not_or x q |> Commute |> at [left_branch; right_branch]        // (¬x ∨ q) ↦ (x ⇒ q)
double_negation p |> Commute |> at [right_branch; left_branch]               // p ↦ ¬¬p
ident_implies_not_or (-p) q |> Commute |> at [right_branch]                  // (¬¬p ∨ q) ↦ (¬p ⇒ q)
trans_implies (-p) x q |> Taut |> apply                                      // transitivity closes it
```

**Why this route matters:** every step rewrites a *whole clause* with `p`, `q`, `x` opaque, so
instantiating at wide/compound clauses replays in **polynomial** time (measured: atoms 0.4 s, 8+8
clauses 1.6 s, 20+20 clauses 4.7 s — no ANF blow-up). The obvious FOIL-and-`simp` proof was tried
and **rejected**: it depends on `simp`'s exact output shape, which changes when clauses are compound,
making replay fragile.

`resolve` is a *derived theorem* (like `trans_implies`), so it is **sound by construction** — no new
admitted rewrite, no oracle sweep required. It was cross-checked against the truth-table and ANF
`valid` oracles anyway.

### 4.3 `PropCalculus.combine_implies` — implication ∧-introduction (new trusted theorem)

```fsharp
combine_implies (p:Prop) (q:Prop) (r:Prop) : Theorem    //  ((p ⇒ q) ∧ (p ⇒ r)) ⇒ (p ⇒ (q ∧ r))
```

The ⇒-half of `⇒` distributing over `∧`. Needed to thread `resolve` steps into a single
`(∧ inputs) ⇒ …`. Proved the same robust way as `resolve` (material form `p ⇒ q = ¬p ∨ q`, then
`distrib_or_and`), so it too replays cheaply at compound clauses.

### 4.4 `Cnf.toCnf` — recursive CNF conversion with a kernel proof (new tactic)

`src/lang/core/Sylvia.Prover/Theories/Cnf.fs` — `module Cnf`. This is the scalable step‑2. It solves
the problem that `autoproof_anf` (used for the `¬φ = A` equivalence before) is exponential in atom
count:

```fsharp
Cnf.toCnf (p:Prop) : Prop * Theorem      //  (cnf, proof : p == cnf),  cnf in clean CNF
```

A **structural recursive descent** on the `Prop` tree — eliminate `⇒`/`=`, push negations to leaves
(De Morgan `distrib_not_or`/`distrib_not_and`, `¬¬`-elim), distribute `∨` over `∧` — composing the
sub-proofs by **congruence** (`Ident subproof |> at [pos]` through a `congAnd`/`congOr`/`congNot`
plumbing layer) and equational transitivity. Its cost is bounded by the **size of the CNF**, not by an
atom-count exponential, so it has **no atom ceiling** (verified through 8 atoms, and on biconditionals).

Two design notes that made it work: (1) the recursive congruence approach sidesteps the `autoapply`
schema-matcher, which could not reliably handle nested negations (an earlier fixpoint attempt stalled
there); (2) it uses the **existing** De Morgan theorems `distrib_not_or`/`distrib_not_and` (Gries
3.47), which treat their operands opaquely — the built-in `double_neg` rule *cancels* `¬¬` and so does
not preserve the exact `¬x ∧ ¬y` shape the recursion needs. In the reconstruction, `Cnf.toCnf` is both
the clausifier (clauses are read off its CNF) and the equivalence proof; `normalize` bridges its CNF to
the reconstruction's right-associated conjunction `A`.

### 4.5 `Memo` — a general memoization combinator (new kernel utility)

End of [`Proof.fs`](../src/lang/core/Sylvia.Prover/Proof.fs). Motivation: profiling `resolve`
showed 60 % of its 385 ms is **re-deriving `trans_implies`** — because a parametric derived rule is an
F# *function* that replays its entire proof tree on every call. In a SAT replay the same clauses recur
as premises across many steps, so caching those pure derivations is a big win.

```fsharp
module Memo =
    let p1 (f: Prop -> 'r) : Prop -> 'r         // memoize a 1-Prop-arg builder
    let p2 (f: Prop -> Prop -> 'r) : …           // 2 args
    let p3 (f: Prop -> Prop -> Prop -> 'r) : …   // 3 args
```

Thread-safe (`ConcurrentDictionary`), keyed by the **injective** `%A` AST dump of the expanded `Expr`
(so distinct arguments never collide — no wrong theorem is ever returned). Sound because the builders
are pure. `resolve` and its hot dependencies (`trans_implies`, `ident_implies_not_or`,
`double_negation`) are wrapped via private memoized aliases; the public `resolve` stays a `[<Theorem>]`
method forwarding to the cache (reflection/attributes intact).

Results: a repeated resolution goes **400 ms → 0.08 ms** (~4 800×); a 256-resolution replay, warm, is
**9 ms total** (0.04 ms/step). Caveat: memoization only kills *repeated* derivations; a first-time
novel-argument call still pays full price — see §7.

### 4.6 The reconstruction loop (in `examples/sat/Reconstruct.fsx`)

The kernel-level replay (which needs both the solver and the prover) lives in the example, per the E
precedent. It is a thin plumbing layer over trusted lemmas — **no new kernel primitive**:

- `resolveStep cnf h1 h2` — one binary LRAT step → `cp(apos) ∧ cp(aneg) ⇒ cp(resolvent)`, where each
  clause is AC-matched to `resolve`'s `(C∨x)` / `(¬x∨D)` shape by `acEq = ident (l==r) [simp]`.
- `conjElim`, `elimR` — `(∧ inputs) ⇒ Cᵢ` (conjunction elimination).
- `conj`, `mp` — conjoin two theorems / modus ponens, reusing the idiom from `Calc.chainImp`.
- `refute` — fold the steps into **R : `(∧ inputs) ⇒ F`** (STEP 1).
- `reconstruct` — STEP 2: `¬φ = A` via `Cnf.toCnf` (clauses read off its CNF; `normalize` bridges to
  `A`), rewrite R, then `PropCalculus.Contradiction` → **`⊢ φ`**.

Reconnaissance finding that shaped this: on long implication chains CaDiCaL emits **almost entirely
binary** resolution steps (2 hints) — so folding is ~one `resolve` per step. That is a property of
*chains*, not of refutations in general; see §4.7.

### 4.7 `SAT.rupChain` — every LRAT step as a binary-resolution chain

The original replay handled only 2-hint steps and skipped the rest. Measuring what CaDiCaL actually
emits (probe over eight goals) showed that assumption is specific to implication chains:

| Goal | binary steps | of which MERGE | non-binary steps |
|---|---:|---:|---:|
| 3-atom implication chain | 3 | 0 | 0 |
| `(p∨q)∧(¬p∨q) ⇒ q` | 1 | 0 | 1 (3 hints) |
| all 8 clauses over 3 vars | 7 | **6** | 1 (1 hint) |
| `∨` distributes over `∧` | 2 | 0 | 1 (3 hints) |
| pigeonhole 3→2 | 5 | 0 | 2 (3 and 5 hints) |
| `(p≡q)∧(q≡r)∧(r≡s) ⇒ (p≡s)` | 3 | 0 | 2 (3 and 4 hints) |

Non-binary steps appear in **every** non-chain refutation, and the empty clause itself is often
derived by one — so the old replay did not merely miss a rare case, it could not close these goals
at all. `rupChain` removes the special case entirely:

> LRAT hints are the antecedents of a *unit-propagation* refutation. Assign every literal of the
> step's clause to false and walk the hints in order: each is unit under the running assignment and
> propagates its one remaining literal, until the last is falsified — the conflict. That is a
> resolution derivation in disguise. Starting from the conflicting clause and resolving BACKWARDS
> against each propagating antecedent, on the literal it propagated, eliminates exactly the assigned
> literals and lands on a clause that subsumes the declared one.

A 2-hint step comes out as a one-link chain, so the binary case is *subsumed* rather than kept
alongside. A 1-hint step (CaDiCaL restating a clause) comes out as a link-free chain. `Derived` may
be a strict subset of the declared clause, which the replay closes by ∨-weakening (§4.9). Negative
hints (RAT steps), unknown antecedents and non-unit hints are rejected with a message rather than
mis-replayed — the chain is checked by the kernel afterwards either way, but failing early says why.

### 4.8 `_chain_simp` — making `simp` confluent on clauses

`resolve` produces the resolvent `C ∨ D`; the replay must then AC-match it to the clause the solver
declared. When the two resolved clauses share a **non-pivot** literal — a *merge* resolution, 6 of 7
binary steps in the all-8-clauses refutation — `C ∨ D` has a duplicate the declared clause does not.

`simp` could not discharge that. Its laws (`_simp_laws`) match a single node, so they see
`p ∨ p` but not `p ∨ (q ∨ p)`; the same blind spot hides a complementary pair that association has
separated (`(p ∨ q) ∨ ¬q`). The consequence is sharper than "a missing simplification": `simp` was
**not confluent on clauses** — two disjunctions over the same literal set could reduce to different
normal forms (one collapsing to `T`, the other not), so an equality between them failed to close.

`EquationalLogic._chain_simp` applies idempotence, complement and the identity/annihilator constants
to the **flattened** operand list of a `∨`/`∧` chain rather than to one node, and is folded into
`_simp`'s bottom-up pass. Operand order is preserved and a chain with nothing to remove is returned
untouched, so it never disturbs a shape `simp` would otherwise have kept. Every case is an instance
of a law already in the trusted base, modulo associativity/commutativity; it is covered by the
admissible-rule equivalence sweep alongside the others.

### 4.9 Clause weakening, input dedup, and one CNF

Three smaller repairs in the replay (`examples/sat/Reconstruct.fsx`), all needed before the dense
goals close:

- **`clauseImp`** — `src ⇒ dst` whenever src's literals are a subset of dst's: ∨-weaken by the
  missing literals (Gries 3.76a), then AC-match. This is what absorbs the gap between what a chain
  derives and what the step declares, and it also covers the case where CaDiCaL lists the resolvent's
  literals in a different order than the replay computes them (observed).
- **`dedupCnf`** — `Cnf.toCnf`'s distribution readily emits clauses with repeated literals (Peirce's
  law yields a `p ∨ p`). Carrying those into the input conjunction `A` mis-targets the `idemp_or`
  rewrite inside `absorb_or`, which `strengthen_and` — and hence `conjElimAll` — is built on, and the
  reconstruction fails on a lemma that has nothing to do with the refutation. Each clause is rewritten
  to its deduped form by **congruence at an exact position** (no searching, so nothing can
  mis-target); `¬φ == A` is then the dedup proof followed by pure-AC reassociation.
- **One CNF, not two.** `reconstruct` used to clausify twice — `Cnf.toCnf` for the equivalence proof
  and `cnfOfNegatedGoal` (inside `Cadical.Prove`) for the solver — and interpret the LRAT clause ids
  against the first while the solver numbered them by the second. The two agree on implication chains
  and diverge elsewhere (`cnfOfNegatedGoal` drops tautological clauses, `Cnf.toCnf` keeps them). The
  replay now solves the exact clause list it reads off `Cnf.toCnf`.

`Cnf.toCnf` also gained a case for `≢`/xor (via Gries 3.10, `def_not_eq`). It previously fell through
to `VAtom`, abstracting the whole subformula away — which is sound but leaves a valid xor goal
unprovable, reported as "¬φ is satisfiable".

## 5. Tests and demos

- **Suite 104/104** (`tests/Sylvia.Tests.Prover/`): in `KernelProofTests.fs` — `resolve` (atoms,
  compound-clause robustness, tautology-vs-oracle incl. the empty-clause `resolve F F p`), `Memo`
  (cache-hit same-instance, distinct-arg no-collision), `combine_implies`, `Cnf.toCnf` (checked
  equivalence to clean CNF at up to 6 atoms, both xor polarities), `_chain_simp` in the
  admissible-rule equivalence sweep, and simp-confluence on AC-equal clauses. In `SatChainTests.fs` —
  `rupChain` over verbatim CaDiCaL LRAT traces (1-, 2- and 3-hint steps, merge resolvents), with every
  link checked to be a genuine binary resolution and every chain checked to subsume the declared
  clause, plus the rejection cases. These are pure integer logic: no solver executable is involved.
- [`examples/sat/CaDiCaL.fsx`](../examples/sat/CaDiCaL.fsx) — decides validity of 6 goals (incl. an
  **8-atom tautology**, past the old ceiling), dumps DIMACS + LRAT + the reconstruction plan, and shows
  a real wide resolution `((a∨b∨g)∧(¬g∨(c∨d))) ⇒ (a∨b∨(c∨d))` as a checked theorem.
- [`examples/sat/Reconstruct.fsx`](../examples/sat/Reconstruct.fsx) — **ALL GREEN, 13 goals**. The
  chains: `⊢ p∨¬p`, `⊢ ((p⇒q)⇒p)⇒p`, `⊢ (p⇒q)∧(q⇒r)⇒(p⇒r)`, and the 5-, 8- and 12-atom scaling
  benchmarks. The dense refutations (§4.7–4.9), none of which the replay could close before: the
  minimal merge `(p∨q)∧(¬p∨q) ⇒ q`, the all-8-clauses-over-3-variables refutation, a 4-clause
  resolution chain, `∨` distributing over `∧`, xor commutativity, pigeonhole 3→2, and the
  `≡`-transitivity chain. Each result is checked structurally against the goal.

## 6. Current state

Both steps scale — the full pipeline produces a kernel-checked `⊢ φ` with **no atom ceiling** (measured
end-to-end through 8 atoms: 2→5 s, 5→39 s, 8→142 s).

> **Update 2026-07-25:** after the Sylvia.Expressions optimization pass
> ([`expressions-perf.md`](expressions-perf.md) — structural `sequal`, hoisted quotation
> templates, lazy log formatting, identity-preserving rebuilds), the same end-to-end
> reconstructions measure **3-atom 1.6 s, 5-atom 2.1 s, 8-atom 4.8 s, 12-atom 10.2 s**
> (~20-30x on the 8-atom case; 12 atoms was never even attempted before). Growth is now
> ~1.2x per added atom with no wall in sight. The per-step O(|expression|) kernel cost
> below remains the architectural ceiling, but its constant factor is now ~20x smaller.

- **Step 1 (resolution replay)** — the refutation `R : (∧ inputs) ⇒ F` is produced and verified sound
  (`valid`) for every test goal.
- **Step 2 (CNF-equivalence)** — `¬φ = A` is produced by `Cnf.toCnf`, a recursive CNF proof that is
  size-bounded, not atom-exponential. The old `autoproof_anf` ≤5-atom ceiling is gone.

> **Update 2026-07-25 (replay completeness).** The replay no longer skips anything. Every LRAT step
> is unfolded into binary resolutions by `SAT.rupChain` (§4.7); merge resolvents are discharged by a
> `simp` that is now confluent on clauses (§4.8); and the input side is deduped, weakened and
> clausified once (§4.9). Before this, the pipeline closed implication chains and nothing denser —
> the all-8-clauses, distributivity, pigeonhole and `≡`-chain goals all failed, most of them because
> the empty clause was derived by a non-binary step. Timings are unchanged (8-atom chain 1.1 s).

The remaining limitation is **speed**, and it is **architectural**. The bottleneck is
`Calc.chainImp` at ~1.9 s per call — it pushes the large input-clause conjunction `A` through several
kernel steps (`Taut`/`reduce`/completeness-check), each **O(|expression|)** — and it is called O(n)
times. So the cost is the equational kernel's per-step cost on a large object, not leaf re-derivation.

Memoization was tried (2026-07-13) and **did not help a single reconstruction**: within one proof the
lemma args (clauses) are mostly distinct → cache-miss-heavy, and the `Memo` `%A` key is itself
O(|expr|). It was reverted. Memoization still helps *cross-invocation* reuse (the DSL-level decision),
but the single-proof-assembly cost needs a cheaper proof step / cheaper term identity — a kernel-layer
concern that motivates a fresh-start redesign.

## 7. Remaining work (honest boundaries)

1. **Speed of the reconstruction assembly (architectural).** The bottleneck is the O(|expr|)-per-step
   kernel cost on the large conjunction `A` (via `chainImp`), not leaf re-derivation — memoization does
   not fix it (tried, reverted). An O(m) `conjElimAll` (share the peel-chain) is in place but is a wash
   at these sizes. The real levers are kernel-layer: interned/hash-consed terms (cheap identity + keys)
   and cheaper proof steps / a proof object with `instantiate`. See the architectural-limits memory.
2. ~~**Merge-clause AC-dedup.**~~ **DONE (2026-07-25)** — `_chain_simp` (§4.8).
3. ~~**Non-binary RUP steps.**~~ **DONE (2026-07-25)** — `SAT.rupChain` (§4.7). They are not rare.
4. **Clausification blowup (formula size).** `Cnf.toCnf` and `cnfOfNegatedGoal` both distribute
   directly, which is exponential in the formula's ∨/∧ nesting — independent of the atom count that
   used to be the ceiling. Nested `≢` is where it bites first: xor associativity over 3 variables
   produces **441 clauses / 2940 literals**, enough to overflow the replay's stack, while xor
   commutativity (36 clauses) reconstructs fine. Tseitin/Plaisted-Greenbaum is the fix; the cost is
   teaching the replay to discharge the definitional clauses for the auxiliary variables.
5. **`absorb_or`'s positional rewrites are shape-sensitive.** Its `idemp_or` step searches for a
   `p ∨ p`, so it can pick the wrong occurrence when the other operand contains one. §4.9's input
   dedup keeps such clauses out of `A`, but the fragility is in the theorem, not the caller, and the
   same pattern recurs across the Gries derivations. A precise-position variant would fix the class.
6. **Schema-instantiation gap** (`prover-schema-instantiation-gap` memory). Because derived rules are
   F# functions that *replay*, a fresh-argument instantiation costs a full derivation, not a
   substitution. Memoization fixes repeats; the systemic fix is LCF-style *prove-once-at-metavars +
   uniform substitution* (`Thm.instantiate`), a new trusted primitive.

## 8. File index

| Path | What |
|------|------|
| `src/lang/solvers/Sylvia.Solver.CaDiCaL/CaDiCaL.fs` | Clausifier, runner, LRAT parser, `rupChain`, reconstruction plan |
| `src/lang/core/Sylvia.Prover/Theories/PropCalculus.fs` | `resolve`, `combine_implies` (+ their memoized aliases) |
| `src/lang/core/Sylvia.Prover/Theories/Cnf.fs` | `Cnf.toCnf` — recursive CNF conversion with kernel proof |
| `src/lang/core/Sylvia.Prover/EquationalLogic.fs` | `_chain_simp` — whole-chain ∨/∧ normalization inside `_simp` |
| `src/lang/core/Sylvia.Prover/Proof.fs` | `Memo` combinator |
| `examples/sat/CaDiCaL.fsx` | Validity decision + `resolve` demo |
| `examples/sat/Reconstruct.fsx` | Full LRAT → `⊢ φ` reconstruction |
| `tests/Sylvia.Tests.Prover/KernelProofTests.fs` | `resolve` / `Memo` / `combine_implies` / `Cnf` / simp-confluence tests |
| `tests/Sylvia.Tests.Prover/SatChainTests.fs` | `rupChain` over real LRAT traces |
| `bin/cadical.exe` | CaDiCaL 3.0.0 (MSYS2 build) |
| `reference/papers/dpllt.pdf` | The DPLL(T) paper that motivated the approach |

## 9. References

- Ganzinger, Hagen, Nieuwenhuis, Oliveras, Tinelli — *DPLL(T): Fast Decision Procedures* (CAV 2004).
- Wetzler, Heule, Hunt — *DRAT-trim* and the LRAT format (proof logging / verified checking).
- J. A. Robinson — *A Machine-Oriented Logic Based on the Resolution Principle* (JACM 1965) — `resolve`
  is the propositional (ground, unification-free) case.
- Companion memory anchors: `prover-scalable-prop-prover`, `prover-schema-instantiation-gap`,
  `prover-e-atp-integration`.
