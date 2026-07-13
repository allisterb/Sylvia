# SAT-Backed Proof Reconstruction: CaDiCaL, `resolve`, and the LRAT-Replay Pipeline

*Design and implementation notes — 2026-07-13. Companion to
[`prover-automation.md`](prover-automation.md) and [`prover-e-atp.md`](prover-e-atp.md).*

**Status at a glance.** The pipeline **CaDiCaL → LRAT → kernel-checked `⊢ φ`** works
end-to-end for propositional goals with NO atom-count ceiling (verified through 8 atoms — Peirce,
implication chains, biconditionals). New trusted theorems `resolve` and `combine_implies`, a recursive CNF converter
`Cnf.toCnf`, a general `Memo` combinator, and the `Sylvia.Solver.CaDiCaL` project are all in the tree;
suite is **97/97**.
Both step 1 (the resolution replay) and step 2 (the CNF-equivalence link) now scale; the remaining
limitation is **speed**, not the atom ceiling.

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

Reconnaissance finding that shaped this: CaDiCaL emits **almost entirely binary** resolution steps (2
hints) even for long implication chains — so folding is ~one `resolve` per step; 3+‑hint RUP steps are
rare.

## 5. Tests and demos

- **Suite 97/97** (`tests/Sylvia.Tests.Prover/KernelProofTests.fs`): `resolve` (atoms,
  compound-clause robustness, tautology-vs-oracle incl. the empty-clause `resolve F F p`), `Memo`
  (cache-hit same-instance, distinct-arg no-collision), `combine_implies`, and `Cnf.toCnf` (checked
  equivalence to clean CNF at up to 6 atoms).
- [`examples/sat/CaDiCaL.fsx`](../examples/sat/CaDiCaL.fsx) — decides validity of 6 goals (incl. an
  **8-atom tautology**, past the old ceiling), dumps DIMACS + LRAT + the reconstruction plan, and shows
  a real wide resolution `((a∨b∨g)∧(¬g∨(c∨d))) ⇒ (a∨b∨(c∨d))` as a checked theorem.
- [`examples/sat/Reconstruct.fsx`](../examples/sat/Reconstruct.fsx) — **ALL GREEN**: closes
  `⊢ p∨¬p`, `⊢ ((p⇒q)⇒p)⇒p`, `⊢ (p⇒q)∧(q⇒r)⇒(p⇒r)`, and a **5-atom chain** (past the old ceiling),
  each matching the goal structurally.

## 6. Current state

Both steps scale — the full pipeline produces a kernel-checked `⊢ φ` with **no atom ceiling** (measured
end-to-end through 8 atoms: 2→5 s, 5→39 s, 8→142 s).

- **Step 1 (resolution replay)** — the refutation `R : (∧ inputs) ⇒ F` is produced and verified sound
  (`valid`) for every test goal.
- **Step 2 (CNF-equivalence)** — `¬φ = A` is produced by `Cnf.toCnf`, a recursive CNF proof that is
  size-bounded, not atom-exponential. The old `autoproof_anf` ≤5-atom ceiling is gone.

The remaining limitation is **speed**: the bottleneck is the kernel proof assembly (many
`resolve`/`conjElim`/`chainImp` derivations), which grows super-linearly with clause count. It is
polynomial, not the old atom-exponential — but 142 s at 8 atoms shows the constant needs work
(memoization of the reconstruction plumbing, and the schema-instantiation fix below, are the levers).

## 7. Remaining work (honest boundaries)

1. **Speed of the reconstruction assembly** — see above. `resolve` and its deps are already memoized;
   the reconstruction plumbing (`conjElim`, `resolveStep`, `chainImp`) is not, and re-derives lemmas at
   fresh clauses. The systemic fix is the schema-instantiation gap (item 4).
2. **Merge-clause AC-dedup.** `acEq` uses `simp`, which handles reorder + F-drop + *adjacent* dedup but
   not *non-adjacent* duplicate literals (`a∨(a∨b) ≠ a∨b` under `simp`/`normalize`). This arises when
   two resolving clauses share a non-pivot literal (denser CNFs). Needs a real clause AC-normalizer.
3. **Non-binary RUP steps** (3+ hints, rare) are not folded yet — need the RUP → binary-resolution-chain
   extraction.
4. **Schema-instantiation gap** (`prover-schema-instantiation-gap` memory). Because derived rules are
   F# functions that *replay*, a fresh-argument instantiation costs a full derivation, not a
   substitution. Memoization fixes repeats; the systemic fix is LCF-style *prove-once-at-metavars +
   uniform substitution* (`Thm.instantiate`), a new trusted primitive.

## 8. File index

| Path | What |
|------|------|
| `src/lang/solvers/Sylvia.Solver.CaDiCaL/CaDiCaL.fs` | Clausifier, runner, LRAT parser, reconstruction plan |
| `src/lang/core/Sylvia.Prover/Theories/PropCalculus.fs` | `resolve`, `combine_implies` (+ their memoized aliases) |
| `src/lang/core/Sylvia.Prover/Theories/Cnf.fs` | `Cnf.toCnf` — recursive CNF conversion with kernel proof |
| `src/lang/core/Sylvia.Prover/Proof.fs` | `Memo` combinator |
| `examples/sat/CaDiCaL.fsx` | Validity decision + `resolve` demo |
| `examples/sat/Reconstruct.fsx` | Full LRAT → `⊢ φ` reconstruction |
| `tests/Sylvia.Tests.Prover/KernelProofTests.fs` | `resolve` / `Memo` / `combine_implies` tests |
| `bin/cadical.exe` | CaDiCaL 3.0.0 (MSYS2 build) |
| `reference/papers/dpllt.pdf` | The DPLL(T) paper that motivated the approach |

## 9. References

- Ganzinger, Hagen, Nieuwenhuis, Oliveras, Tinelli — *DPLL(T): Fast Decision Procedures* (CAV 2004).
- Wetzler, Heule, Hunt — *DRAT-trim* and the LRAT format (proof logging / verified checking).
- J. A. Robinson — *A Machine-Oriented Logic Based on the Resolution Principle* (JACM 1965) — `resolve`
  is the propositional (ground, unification-free) case.
- Companion memory anchors: `prover-scalable-prop-prover`, `prover-schema-instantiation-gap`,
  `prover-e-atp-integration`.
