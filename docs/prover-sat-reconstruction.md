# SAT-Backed Proof Reconstruction: CaDiCaL, `resolve`, and the LRAT-Replay Pipeline

*Design and implementation notes — 2026-07-13. Companion to
[`prover-automation.md`](prover-automation.md) and [`prover-e-atp.md`](prover-e-atp.md).*

**Status at a glance.** The pipeline **CaDiCaL → LRAT → kernel-checked `⊢ φ`** works
end-to-end for propositional goals with NO atom-count ceiling (verified through 12 atoms — Peirce,
implication chains, biconditionals). New trusted theorems `resolve` and `combine_implies`, a recursive CNF converter
`Cnf.toCnf`, a general `Memo` combinator, and the `Sylvia.Solver.CaDiCaL` project are all in the tree;
suite is **113/113**. The pipeline is a library — **`Sylvia.Prover.SAT` / `SatProof.prove`** (§4.6) —
not a script.
Both step 1 (the resolution replay) and step 2 (the CNF-equivalence link) now scale.

As of **2026-07-25** the replay is also *complete over the trace*: every LRAT step is replayed, not
just the binary ones, and merge resolvents are discharged (§4.7–4.9). That is what took the pipeline
from "implication chains" to arbitrary refutations — pigeonhole, distributivity, `≡`-chains, the
full 8-clause refutation over 3 variables. As of **2026-07-28** `Cnf.toCnf` also prunes tautological
clauses, which matched its clause counts to the solver-side clausifier's on every goal measured and
let nested xor reconstruct; the clausification-blowup item in §7 is retracted with it. The remaining
limitation is **speed**.

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

### 4.6 The reconstruction loop — `Sylvia.Prover.SAT` (`SatProof`)

The kernel-level replay needs both the solver and the prover, so it lives in its **own assembly**,
`src/lang/core/Sylvia.Prover.SAT/`, referencing `Sylvia.Prover` and `Sylvia.Solver.CaDiCaL`. That is
what lets `Sylvia.Prover` stay solver-free (the `Sylvia.ATP.E` discipline): the solver never enters
the trusted base, and nothing in the kernel depends on it. It is a thin plumbing layer over trusted
lemmas — **no new kernel primitive**:

| Value | Role |
|-------|------|
| `SatProof.prove : Prop -> Theorem` | Decide and replay; raises on a non-theorem or an unusable solver. Resolves `cadical` from `SYLVIA_CADICAL` / PATH. |
| `SatProof.proveWith : Cadical -> Prop -> Theorem` | Same, with an explicit solver (path, timeout). `proveWithLog` keeps the kernel trace. |
| `SatProof.tryProve` / `tryProveWith` | `Result<Theorem,string>`. The message distinguishes **"NOT a theorem"** from "solver not found / timed out" — a caller choosing whether to fall back needs to know which. |
| `SatProof.Sat` / `SatWith` | The proof as a `Rule`, so a SAT-discharged subgoal can sit inside a hand-written proof: `SatProof.SatWith sat sub \|> apply_left`. |
| `SatProof.install` / `installWith` / `uninstall` | Register this backend as `PropCalculus.decide`'s decider — see §4.10. |
| `SatProof.clausesOf` / `dedupCnf` / `refute` / `conjElimAll` | The stages, exposed for testing and for callers that want the refutation rather than the theorem. |

Proof logging is silenced for the duration of a `prove` and restored afterwards (the `Calc` precedent)
— a reconstruction emits thousands of kernel steps, which is noise at any call site.

The stages themselves:

- `resolveStep cnf apos aneg pv out` — one binary resolution → `cp(apos) ∧ cp(aneg) ⇒ cp(out)`, where
  each clause is AC-matched to `resolve`'s `(C∨x)` / `(¬x∨D)` shape by `acEq = ident (l==r) [simp]`.
- `conjElimAll`, `elimR` — `(∧ inputs) ⇒ Cᵢ` for every input clause, in one O(n) pass sharing the
  peel-chain (per-clause elimination is O(n²) in the expensive `Calc.chainImp`).
- `conj`, `mp` — conjoin two theorems / modus ponens, reusing the idiom from `Calc.chainImp`.
- `clauseImp` — subset weakening (§4.9).
- `refute` — replay every LRAT step (§4.7) into **R : `(∧ inputs) ⇒ F`** (STEP 1).
- `proveWith` — STEP 2: `¬φ = A` via `Cnf.toCnf` + `dedupCnf` + `normalize`, rewrite R, then
  `PropCalculus.Contradiction` → **`⊢ φ`**.

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
  law yields a `p ∨ p`). Each clause is rewritten to its deduped form by **congruence at an exact
  position**; `¬φ == A` is then the dedup proof followed by pure-AC reassociation. This was
  originally a workaround: a `p ∨ p` inside `A` mis-targeted the `idemp_or` step inside `absorb_or`,
  which `strengthen_and` — and hence `conjElimAll` — is built on, so the reconstruction failed on a
  lemma unrelated to the refutation. With §7.5's addressing fix that dependency is gone (verified:
  all 13 goals close with dedup disabled), and it is kept purely as an **optimization** — every
  kernel step costs O(|A|), so smaller clauses are cheaper: `∨` over `∧` 12.1 s → 7.7 s, xor
  commutativity 26.8 s → 16.6 s, 12-atom chain 2.6 s → 1.9 s.
- **One CNF, not two.** `reconstruct` used to clausify twice — `Cnf.toCnf` for the equivalence proof
  and `cnfOfNegatedGoal` (inside `Cadical.Prove`) for the solver — and interpret the LRAT clause ids
  against the first while the solver numbered them by the second. The two agree on implication chains
  and diverge elsewhere (`cnfOfNegatedGoal` drops tautological clauses, `Cnf.toCnf` keeps them). The
  replay now solves the exact clause list it reads off `Cnf.toCnf`.

`Cnf.toCnf` also gained a case for `≢`/xor (via Gries 3.10, `def_not_eq`). It previously fell through
to `VAtom`, abstracting the whole subformula away — which is sound but leaves a valid xor goal
unprovable, reported as "¬φ is satisfiable".

### 4.10 `PropCalculus.decide` — lifting the atom ceiling

`autoproof_max_atoms = 5` is a fail-fast guard on the *exponential* provers. It is **not raised** —
raising it would not make `autoproof_anf` scale, only let it run longer before hanging. Instead
`PropCalculus.decide : Prop -> Theorem` **routes by atom count**, on its own knob:

- **at or below `decide_max_anf_atoms` (3)** → the in-kernel `autoproof_anf`;
- **above it** → the installed decider (no atom ceiling); with none installed it still falls back to
  the in-kernel prover, which works up to `autoproof_max_atoms`, and only then does the guard fire.

The two thresholds are deliberately separate. `autoproof_max_atoms` (5) is a **guard** — where the
exponential prover stops working at all (12 s at 5 atoms, fails at 6). `decide_max_anf_atoms` (3) is
a **preference** — where the backend simply becomes better, which happens earlier. Collapsing them
would make a solver-free caller *fail* on 4- and 5-atom goals it can currently prove in 1–12 s.

Routing rather than always preferring the backend is not a hedge — **neither prover dominates**, and
an unconditional dispatch is a regression. The two blow up on different axes: `autoproof_anf` is
exponential in atom count (exactly what the guard bounds) but untroubled by deep ∨/∧ nesting, while
the SAT route has no atom ceiling but pays clausification on precisely that nesting. Measured on
small goals, with the backend installed:

| goal | in-kernel | SAT route |
|---|---:|---:|
| `∨` over `∧` distributivity (3 atoms) | **1 ms** | 8328 ms |
| xor associativity (3 atoms) | **0 ms** | **stack overflow** |
| `p ∨ ¬p` (2 atoms) | 25 ms | 224 ms |

The overflow is the decisive one: a `StackOverflowException` cannot be caught, so there is no
try-the-backend-then-fall-back option — the routing has to prevent it. With the atom check in place,
installing a backend **extends** `decide` rather than altering it: goals under the limit prove exactly
as they did before. A regression test pins this.

### 4.11 Where the routing threshold came from, and why reuse dominates

The two routes produce *very different proof objects* of the same statement, and the difference is
larger downstream than it is at construction. Measured on implication chains (the shape ANF handles
worst) and on `∨`-over-`∧` distributivity (the shape the SAT route handles worst):

| goal | atoms | ANF time | SAT time | ANF top-level steps | SAT top-level steps | ANF peak expr | SAT peak expr |
|---|--:|--:|--:|--:|--:|--:|--:|
| Peirce | 2 | **11 ms** | 220 ms | 28 | 2 | 80 | — |
| chain 3 | 3 | **95 ms** | 489 ms | 100 | 2 | 361 | 339 |
| chain 4 | 4 | 1130 ms | **340 ms** | 396 | 2 | 1513 | 465 |
| chain 5 | 5 | 12389 ms | **432 ms** | 1496 | 2 | 5833 | 591 |
| distributivity | 3 | **0 ms** | 7753 ms | 8 | 2 | 40 | — |

ANF emits **few enormous steps** — its peak intermediate expression grows ~4× per atom, because it
materialises the exponentially-large polynomial as one term. The SAT replay emits **many tiny steps**
— peak grows ~1.3×, since it works clause by clause. Its derivation *tree* is larger (3897 vs 396
nodes at chain-4) yet far cheaper, because kernel step cost is O(|expr|).

**Reuse used to be the bigger effect — it has since been fixed.** Every step-transforming tactic in
`Tactics.fs` built its new proof by SPLICING the input's own step list:

```fsharp
let p = Proof(stmt, theory, <one new step> :: proof.Steps, true)
```

and a `Proof`'s constructor *executes* its steps. So each use of a theorem re-ran that theorem's whole
derivation. All fourteen did it (`Truth`, `Taut`, `Taut'`, `Dual`, `Dual'`, `Commute`, `CommuteL`,
`CommuteR`, `RightAssoc`, `LeftAssoc`, and the four `*Recurse*`), and the composite tactics
(`MutualImplication`, `Contradiction`, `Cases`) inherited it through the `taut` they are passed.

The fix mirrors what `Subst`/`Ident` had always done — carry the completed proof as a rule's
*justification* instead of replaying it. `Tactics.theoremIsTrue` is a `Derive` rule holding a
completed proof that rewrites that proof's statement to `T`. Each tactic is now exactly two steps: its
own rewrite, which returns the state to the input's statement, then that justified rewrite to `T`,
which is an axiom. Nothing is re-derived; `A` is already proved, so `A ≡ T` follows directly.

| on a 396-step theorem, ×50 | before | after |
|---|--:|--:|
| `Taut th` (no application) | 7534 ms | **9 ms** |
| using the theorem in another proof | 7510 ms | **12 ms** |
| the same for a 2-step theorem | 11 ms | 12 ms |

Reuse cost is now **independent of how the theorem was proved** — the ~680× gap between an ANF-built
and a SAT-built theorem of the same statement is gone.

Verification, since this touches every proof in the codebase: suite 115/115, all nine example scripts
green, `AdversarialSweep.fsx` ALL CLEAR, and the proof logs of `PropCalculus.fsx`, `PredCalculus.fsx`
and `SetTheory.fsx` compared line by line. **Every top-level derivation is byte-identical** (108, 142
and 1107 lines respectively). The only differences are inside `[Lemma]` blocks, and they are all
*removals* of the redundant replay, plus the header punctuation the constructor switches at
`steps.Length <= 2` (`= T:` → `= T.`) now that those lemmas are genuinely short.

The threshold of 3 was set before this fix, when reuse still favoured the backend heavily. It happens
to remain right on construction cost alone (see the table above), but the reuse argument for it no
longer applies.

The kernel cannot reference a solver — `Sylvia.Prover` must stay solver-free, and the dependency
runs the other way — so the backend registers *itself*, through `PropCalculus.prop_decider`, via
`SatProof.install()`. This is the mutable oracle hook considered and rejected in §4.6's design
discussion; what changed is that there is now a caller for it. It is deliberately narrow:

- a **registration slot**, not general dispatch: `decide` is its only consumer;
- **explicit**, not a module initializer — a caller that has not asked for the SAT route keeps the
  previous solver-free behaviour, and `uninstall()` restores it;
- **it does not widen the trusted base.** The registered function comes from outside the assembly,
  so `decide` verifies (by `sequal`) that what came back is a theorem of the goal it asked about. An
  incorrect installer can cause a failure but cannot inject a theorem of something else — there is a
  test that registers a decider returning a valid theorem of the *wrong* proposition and asserts it
  is rejected.

```fsharp
SatProof.installWith sat
let th = PropCalculus.decide eightAtomGoal      // ⊢ … , ~0.6 s, no ceiling
```

## 5. Tests and demos

- **Suite 113/113** (`tests/Sylvia.Tests.Prover/`): in `KernelProofTests.fs` — `resolve` (atoms,
  compound-clause robustness, tautology-vs-oracle incl. the empty-clause `resolve F F p`), `Memo`
  (cache-hit same-instance, distinct-arg no-collision), `combine_implies`, `Cnf.toCnf` (checked
  equivalence to clean CNF at up to 6 atoms, both xor polarities), `_chain_simp` in the
  admissible-rule equivalence sweep, and simp-confluence on AC-equal clauses. In `SatChainTests.fs` —
  `rupChain` over verbatim CaDiCaL LRAT traces (1-, 2- and 3-hint steps, merge resolvents), with every
  link checked to be a genuine binary resolution and every chain checked to subsume the declared
  clause, plus the rejection cases. These are pure integer logic: no solver executable is involved.
  Also in `SatChainTests.fs` — `SatProof`'s clause plumbing (`clausesOf`, `dedupCnf`) as pure tests,
  and end-to-end `prove` checks asserting the result is a theorem **of the goal** and that a
  non-theorem is rejected distinguishably from a missing solver. `bin/cadical.exe` is not tracked by
  git, so those last tests skip when it is absent — but they say so in the test output rather than
  passing quietly.
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

> **Update 2026-07-25 (the ceiling is gone).** `PropCalculus.decide` with the backend installed
> proves goals past `autoproof_max_atoms` — the 8-atom chain in ~0.6 s, where the guard used to fail
> fast. The guard itself is unchanged and still protects the exponential fallback; see §4.10.

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

1. **Speed of the reconstruction assembly.** Leaf re-derivation is fixed (§7.6,
   `Tactics.Instantiate`). What remains was long recorded here as an **O(|A|)-per-step kernel cost**
   calling for interned/hash-consed terms. **Profiling in 2026-07 does not support that**, and the
   claim is withdrawn — it would have sent the next person to build term interning for a cost that
   is at most ~15%.

   Measured on `tests/Sylvia.Tests.Perf -- dense43` (pigeonhole 4→3, hermetic, canned LRAT), with
   in-process timers and then with allocation and CPU profilers:

   | | share |
   |---|--:|
   | rule application (the actual rewriting) | **0.4 – 0.8%** |
   | axiom recognition (`Theory.AxEquiv`) | ~15% |
   | `FsExpr.expand` subtree | **27.8% CPU** |
   | `CallPattern` + `SpecificCall` + `ExprShape.loop`, self CPU | **19.1%** |

   The cost is not rewriting terms; it is **asking quotations what they are**. `FSharpOption`,
   tuples, lists and `FSharpChoice` from active-pattern probes were ~6.3M of 8.0M allocations. One
   experiment that did NOT work, recorded so it is not repeated: reference-keyed memoization of
   `expand` and `AxEquiv` (60%+ hit rates, zero time effect — caching the entry point never hits the
   expensive call, which is a fresh `A ⇒ Cᵢ` at every step).

   Two changes that did work, together **-10.6% warm and -17% cold**, with byte-identical proof logs
   across every example script:

   - **`expand` destructures once.** It matched five separate `Call(...)` rules, and F# re-invokes
     an active pattern per match rule rather than sharing the destructuring, so every call node was
     probed and allocated five times over. One `Call(body, mi, args)` and a dispatch on `mi.Name`:
     -7.7% warm, -11% allocations.
   - **`Term.(==)` builds through `mk_eq_bool`** instead of a spliced quotation literal, which
     re-deserializes its pickled template on every evaluation. Worth stating why this was nearly
     missed: `Prop` declares its own `==` over `#Prop` that already used `mk_eq_bool`, which makes
     the `Term<'t>` one look dead — but **F# overload resolution picks the inherited `Term<'t>`
     member for two `Prop`s**. Counted on pigeonhole 4→3: `Term<'t>.(==)` fires 2175 times and
     `Prop.(==)` fires zero. The `#Prop` overload is the dead one. -3% warm, and the cold win is
     much larger because the deserialization machinery is never JITted.

   Remaining candidates in the same vein: an allocation-free `specific_call` (5.4% self CPU),
   `traverse` without `ExprShape` (4.8%), and the other spliced quotation literals in `Term.fs`
   (`IndexVar` arithmetic, the quantifier builders, `Pred` combinators) — none of which are on a
   propositional path, but the same trap applies wherever they are hot. **Count the call before
   assuming an overload is dead.**

   Measure warm, not cold: a fresh `dotnet run` spends over a second in JIT on a 1.6 s payload,
   which is why `runDense` repeats the payload and why single-shot numbers in this file's history
   should be treated with suspicion.

   **Atom count is the wrong yardstick, and it flattered every benchmark in this document.** Cost
   tracks LRAT STEPS × CLAUSE-SET SIZE (and clause WIDTH), and an implication chain is the cheapest
   refutation shape there is: exactly one resolution per atom, over 2-literal clauses. Measured,
   Release, warm:

   | goal | atoms | clauses | LRAT adds | total | per step |
   |---|--:|--:|--:|--:|--:|
   | chain 50 | 50 | 51 | 50 | 3075 ms | 61 ms |
   | pigeonhole 4→3 | 12 | 22 | 15 | 1811 ms | 121 ms |
   | pigeonhole 5→4 | 20 | 45 | 48 | 11 514 ms | 240 ms |
   | pigeonhole 6→5 | 30 | 81 | 156 | 102 231 ms | 655 ms |

   A 20-atom pigeonhole costs what a 60-atom chain would. So the §1 target of "20–50 atoms in well
   under a second" is met on chains at the low end and is not the right way to state the goal: a
   target in LRAT steps and clause-set size would actually track whether this is improving. The
   honest present ceiling for DENSE refutations is around 20 atoms for single-digit seconds. 4→3 and
   5→4 are now in `Reconstruct.fsx` so this class cannot silently regress; 6→5 deliberately is not —
   it is the ceiling, not a test.

   Solving is never the bottleneck: CaDiCaL refutes pigeonhole 6→5 in 27 ms and we then spend 102 s
   replaying it.

1a. ~~**RUP-only replay is complete enough.**~~ **NO — found and mitigated (2026-07-28).** This was
   never written down as an assumption, which is precisely how it survived. `SAT.rupChain` replays
   RUP steps and rejects RAT ones (a negative hint is satisfiability-preserving, not entailed, so it
   has no forward reading as resolution). Every goal in the suite was a chain or tiny, and chains
   never produce RAT — so the first genuinely dense instance pointed at the pipeline **failed
   outright**: pigeonhole 5→4 died at step 46 with "the hints never reach a conflict", 12 of its 82
   steps unreplayable.

   The cause is upstream. CaDiCaL's default pre/inprocessing **introduces fresh variables** — on a
   20-variable input its LRAT referenced variables 21–29 — and justifies the clauses defining them
   with RAT steps. Running the identical instance with `--plain` gives 48 steps, no fresh variables
   and no RAT at all. `Cadical` therefore now takes `?plain` and **defaults it to true**, so a caller
   who intends to reconstruct gets a replayable trace without having to know any of this; pass
   `plain = false` for verdict-only use, where preprocessing makes the solver stronger. Pinned by a
   test. No measurable cost on anything else (24-atom chain 730 → 737 ms).

   Supporting RAT properly is a real option if a future instance needs it, but nothing measured does:
   with `--plain`, every dense goal tried reconstructs. Note this makes the earlier "no atom ceiling"
   claim precise rather than false — it was always about `autoproof_anf`'s exponential in atoms, and
   it was never a claim that any propositional theorem reconstructs.
2. ~~**Merge-clause AC-dedup.**~~ **DONE (2026-07-25)** — `_chain_simp` (§4.8).
3. ~~**Non-binary RUP steps.**~~ **DONE (2026-07-25)** — `SAT.rupChain` (§4.7). They are not rare.
4. ~~**Clausification blowup (formula size)** — Tseitin is the top blocker.~~ **RETRACTED, and fixed
   (2026-07-28).** This item claimed the recursive descent was exponential in ∨/∧ nesting on the
   strength of one measurement: xor associativity over 3 variables produced **441 clauses / 2940
   literals**, enough to overflow the replay's stack, where the solver-side `cnfOfNegatedGoal`
   produced **8**. Two successive explanations for that gap were wrong, and the audit is worth
   recording because the wrong ones were plausible.

   The first guess was a *bad xor expansion* — `Cnf.toCnf` routes `x ≢ y` through `¬(x = y)` and
   mutual implication, so a direct `(x ∨ y) ∧ (¬x ∨ ¬y)` theorem should have collapsed it. But
   `cnfOfNegatedGoal` does not use that form either; it expands xor to the DNF
   `(x ∧ ¬y) ∨ (¬x ∧ y)`, essentially what `Cnf.toCnf` already reaches. Counting instead of
   theorising settled it:

   ```
   Cnf.toCnf raw clauses        : 441
     of those, TAUTOLOGICAL     : 433
     non-tautological           : 8
   cnfOfNegatedGoal clauses     : 8
   ```

   Both clausifiers produce **the same essential CNF**. The entire gap was that `cnfOfNegatedGoal`
   drops tautological clauses (in `normClause`) and `Cnf.toCnf` kept every one. Not an expansion
   problem, and not size blowup in the Tseitin sense at all.

   `Cnf.toCnf` now prunes them, clause by clause, by congruence — each obligation is `clause == T`
   from a complementary pair, discharged by `simp`, then the `T` conjunct collapses via `ident_and`
   (Gries 3.39). Two things that a global `simp` over the conjunction gets wrong, and which cost a
   round each: it simplifies ACROSS clauses, so the CNF of `¬(p ∨ ¬p)` — which is `¬p ∧ p`, with no
   tautological clause — collapses to `F`, leaving nothing to clausify; and even where that is not
   fatal, the two sides do not reliably converge (it fails on `∨`-over-`∧` distributivity).

   `Cnf.toCnf` now matches the solver-side clausifier's clause count on **every** goal measured:

   | goal | `Cnf.toCnf` before | after | `cnfOfNegatedGoal` |
   |---|--:|--:|--:|
   | excluded middle / Peirce / chains 3, 8 | 2 / 3 / 4 / 9 | unchanged | 2 / 3 / 4 / 9 |
   | `∨` over `∧` distributivity | 24 | **12** | 12 |
   | biconditional chain | 8 | 8 | 8 |
   | pigeonhole 3→2 | 9 | 9 | 9 |
   | 3-var all-8-clause | 8 | 8 | 8 |
   | xor commutativity | 36 | **4** | 4 |
   | **xor associativity** | **441** | **8** | 8 |

   Xor associativity now **reconstructs end to end in 6.1 s**, and is in `Reconstruct.fsx`'s goal
   list. It was the only goal that had ever overflowed the replay.

   **Pruning moved inside `distribOr` (2026-07-28).** The version above pruned once, at the end, so
   conversion still *built* all 441 clauses before discarding 433 of them. Distribution is
   multiplicative, so a tautology left in an intermediate is multiplied against every clause of
   every enclosing `∨` — the descent was still doing exponential work even though its output was
   not. Each clause is now tested the moment `distribOr` builds it and replaced by `T` if it holds a
   complementary pair, with `T` absorbed through the enclosing `∨` (Gries 3.29) and collapsed out of
   the enclosing `∧` (3.39). The final CNF is identical either way; the intermediates are now the
   size of the answer.

   | goal | clausification before | after |
   |---|--:|--:|
   | implication chains 4 – 24 | 28 – 55 ms | unchanged |
   | xor associativity, 3 vars | 4459 ms | **1265 ms** |
   | xor associativity, 4 vars | **229 360 ms** | **1722 ms** |

   (Release build, warm. The 4-variable case end to end: 141 s → 5.3 s.) Clausification is no longer
   a super-linear term anywhere measured. Tseitin still has no measurement supporting it; it remains
   the right answer for genuine size blowup, and nothing measured here is an instance of one.

   One consequence to keep in mind: pruning can now reduce the whole conversion to `T`, which is not
   a clause set. That happens exactly when `¬φ` is valid — i.e. `φ` is unsatisfiable, not a theorem —
   and `toCnf` handles it by converting again with pruning off, so the caller still has clauses to
   hand the solver and still gets a `SAT` verdict rather than an internal error.
5. ~~**`absorb_or`'s positional rewrites are shape-sensitive.**~~ **DONE (2026-07-25)** — and it was
   a class, not one theorem. A reflection-driven sweep instantiating every all-`Prop`-parameter
   schema in `PropCalculus` at arguments *containing* the terms its own steps search for (`p ∨ p`,
   `p ∧ p`, `¬¬p`, `p = p`, …) found schemas failing that collapse to **seven root derivations**:
   `absorb_or`, `absorb_and`, `ident_and_implies`, `ident_or_conseq`, `ident_and_eq`,
   `ident_eq_and_or_not`, `shunt'` and `distrib_implies_eq_implies`. The rest inherit — including
   **`trans_implies`**, which fails at `q = p ∧ p` and which `Calc.chainImp` instantiates at
   whatever the caller is composing, so this was live risk in the reconstruction itself and not
   only in `conjElimAll`. The sweep is now [`examples/proofs/AdversarialSweep.fsx`](../examples/proofs/AdversarialSweep.fsx)
   (3370 instantiations, ~9 min, ALL CLEAR); re-run it after adding or editing any derivation. Its
   fast subset is pinned as a unit test.

   The cause is uniform: a *substitution* rule (`Derive`) rewrites the leftmost-outermost match
   inside the subterm it is addressed to, so a loose address (`at_left`) picks the wrong occurrence
   as soon as an argument contains a competing one. Admissible rules (`Admit`) fire only at the
   addressed node and were never affected. All five derivations now address every substitution step
   with an exact `at [ ... ]` path, annotated with the intermediate state. Two of them previously
   carried comments *documenting* the reliance on first-match order — that reliance is now gone.

   `replace_eq` still fails at compound arguments and is the one case that is not a bug: it is
   Leibniz substitution of one variable for another (`subst_and` matches `(Var = Var) ∧ E`), so the
   variable precondition is real. It is now documented on the schema and pinned by a test.
6. ~~**Schema-instantiation gap**~~ **DONE (2026-07-28) — `Tactics.Instantiate`.** Derived rules are
   F# functions that *replay*, so instantiating a lemma at fresh arguments cost a full derivation
   rather than a substitution. Memoization only ever fixed repeats, and a reconstruction's arguments
   are distinct at every step, so it never hit. `Tactics.Instantiate` takes `⊢ S` and a substitution
   on propositional variables and returns `⊢ Sσ` in one kernel step; `Tactics.Schema.p1/p2/p3` wrap
   a schema so it is derived once at metavariables and served by substitution thereafter.

   It is **not a new trusted primitive**, on either axis. Logically it is admissible: uniform
   substitution of propositional variables adds no theorems to a system whose axioms are schemes,
   and Sylvia's `Admit` rules are exactly such schemes — the prover already relies on this every
   time a derived rule is replayed at new arguments. Mechanically it adds no kernel case either: the
   instantiated statement is closed by a `Derive` step holding the parent's completed proof, which
   is the same device `Taut` has used since the tactic-splicing fix (§7.5). The combinator is the
   sole guardian and refuses unless the parent proof is complete, every domain element is a plain
   `bool` `Var` (so `T`/`F`, being named constants, are unreachable) with no duplicates, and the
   parent statement is **binder-free**. That last restriction is what keeps the admissibility
   argument honest: quantified derivations discharge `¬occurs_free` side conditions that
   substitution can invalidate, and a substituted term can be captured. Extending to the quantified
   case needs its own argument. Eight tests in `KernelProofTests.fs` pin the behaviour, most of them
   on what it refuses.

   Measured on implication chains, Release, warm — `Calc.chainImp` routed through the instantiated
   `trans_implies`, and `SatProof` through instantiated `resolve` / `combine_implies` /
   `strengthen_and` / `weaken_or` / `reflex_implies`:

   | atoms | handoff baseline (Debug) | Release | + `distribOr` pruning | + `Instantiate` |
   |---:|--:|--:|--:|--:|
   | 4  |  844 ms |  557 ms |  564 ms | **156 ms** |
   | 8  | 1465 ms | 1053 ms |  993 ms | **394 ms** |
   | 12 | 2039 ms | 1245 ms | 1273 ms | **270 ms** |
   | 16 | 3357 ms | 2060 ms | 2049 ms | **362 ms** |
   | 20 | 5108 ms | 3231 ms | 3297 ms | **570 ms** |
   | 24 | 7247 ms | 4899 ms | 4682 ms | **730 ms** |

   and it keeps going: 32 atoms 1.3 s, 40 atoms 2.0 s, 50 atoms 3.0 s, 64 atoms 4.9 s. The §1 target
   of "20–50 atoms in well under a second" is now met at the low end of that range and missed by
   about 3× at the top.

   Why it was the right lever, from the profile. At 24 atoms the reconstruction built 8626 `Proof`
   objects, 92% of them lemmas. Timed at fresh arguments so memoization could not mask anything,
   the per-call costs were `resolve` 157 ms / 109 nested proofs, `trans_implies` 107 ms / 45,
   `combine_implies` 62 ms / 58, `strengthen_and` 13 ms / 9 — every one a Gries schema being
   re-derived over an entire clause conjunction. Instantiating `trans_implies` alone took the 24-atom
   case from 4682 ms to 1375 ms; the rest followed.

   Instantiation is also **stricter** than replay, which is worth stating because it sounds like the
   opposite. A `Derive` step rewrites the leftmost-outermost match inside the subterm it addresses,
   so replaying a schema at compound arguments can target the wrong occurrence — the failure class
   §7.5 exists to catch, and which named `trans_implies` reached through `chainImp` as live risk in
   this very pipeline. A schema instantiated by substitution only ever ran its derivation at
   metavariables, where there is no competing subterm to mis-target.

   Currently wired in at `Calc.chainImp` and inside `SatProof`, deliberately not pushed into
   `PropCalculus` itself, so no existing proof changes shape. The only proof-log difference across
   the whole example suite is that `trans_implies`'s derivation now prints once at metavariables
   instead of at each caller's arguments; every conclusion is byte-identical.

## 8. File index

| Path | What |
|------|------|
| `src/lang/solvers/Sylvia.Solver.CaDiCaL/CaDiCaL.fs` | Clausifier, runner, LRAT parser, `rupChain`, reconstruction plan |
| `src/lang/core/Sylvia.Prover/Theories/PropCalculus.fs` | `resolve`, `combine_implies` (+ their memoized aliases) |
| `src/lang/core/Sylvia.Prover/Theories/Cnf.fs` | `Cnf.toCnf` — recursive CNF conversion with kernel proof |
| `src/lang/core/Sylvia.Prover/EquationalLogic.fs` | `_chain_simp` — whole-chain ∨/∧ normalization inside `_simp` |
| `src/lang/core/Sylvia.Prover/Proof.fs` | `Memo` combinator |
| `src/lang/core/Sylvia.Prover/Tactics.fs` | `Instantiate` / `Schema.p1-p3` — schema instantiation (§7.6) |
| `src/lang/core/Sylvia.Prover.SAT/SatProof.fs` | **The reconstruction library** — `prove`/`proveWith`/`tryProve`/`Sat`, and the stages |
| `examples/sat/CaDiCaL.fsx` | Validity decision + `resolve` demo |
| `examples/sat/Reconstruct.fsx` | Demo + end-to-end gate for `SatProof` |
| `examples/proofs/AdversarialSweep.fsx` | Schema-instantiation sweep (§7.5) |
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
