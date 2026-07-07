# A Theory of Sets in Sylvia

Design and implementation notes for the Sylvia prover's theory of sets, following Gries &
Schneider, *A Logical Approach to Discrete Math*, **Chapter 11**. Companion to
[`prover-predicate-calculus.md`](prover-predicate-calculus.md) and
[`prover-automation.md`](prover-automation.md).

Runnable foundation check: `dotnet fsi examples/proofs/SetTheory.fsx`.

## 1. What Chapter 11 actually builds

Gries builds set theory in **two layers**, and the distinction drives the whole design.

### 1a. Foundational layer (§11.1–11.2) — sets over predicate calculus

Set theory is defined as an **extension of predicate calculus**. Everything is defined by
**membership**, and membership reduces to quantifiers:

| Ref     | Name              | Statement |
|---------|-------------------|-----------|
| (11.3)  | **Membership**    | `F ∈ {x | R : E} = (∃x | R : F = E)`  (provided ¬occurs 'x' 'F') |
| (11.4)  | **Extensionality**| `S = T = (∀x |: x∈S = x∈T)` |
| (11.5)  | —                 | `S = {x | x∈S : x}` |
| (11.7)  | Sets vs predicates| `x ∈ {x | R} = R` |
| (11.2)  | Enumeration       | `{e0,…,en−1} = {x | x=e0 ∨ … ∨ x=en−1 : x}` |

Each operator is then an axiom that **reduces membership to a connective**:

| Ref     | Operator      | Defining axiom |
|---------|---------------|----------------|
| (11.12) | Size `#`      | `#S = (Σx | x∈S : 1)` |
| (11.13) | Subset `⊆`    | `S ⊆ T = (∀x | x∈S : x∈T)` |
| (11.17) | Complement `~`| `v ∈ ~S = v∈U ∧ v∉S` |
| (11.20) | Union `∪`     | `v ∈ S∪T = v∈S ∨ v∈T` |
| (11.21) | Intersection `∩` | `v ∈ S∩T = v∈S ∧ v∈T` |
| (11.22) | Difference `−`| `v ∈ S−T = v∈S ∧ v∉T` |
| (11.23) | Power set `𝒫` | `v ∈ 𝒫S = v⊆S` |

Sample proofs (e.g. 11.5) run through **Trading (9.19)** and the **One-point rule (8.14)** — pure
predicate calculus. So this layer is only expressible if the set theory sits *on top of* predicate
calculus.

### 1b. Algebraic layer (§11.3) — sets as a Boolean algebra

Definition (11.24) gives a syntactic translation between set expressions `Es` and boolean
expressions `Ep`:

```
∅ ↔ false      U ↔ true      ~ ↔ ¬      ∪ ↔ ∨      ∩ ↔ ∧
```

and **Metatheorem (11.25)** states: `Es = Fs` is valid iff `Ep = Fp` is valid; `Es ⊆ Fs` iff
`Ep ⇒ Fp`; `Es = U` iff `Ep` is valid. This is what hands you (11.26)–(11.42) — symmetry,
associativity, idempotency, identity, zero, excluded middle, contradiction, De Morgan,
distributivity — for free, i.e. `(set, ∪, ∩, ~, ∅, U)` is a Boolean algebra mirroring
`(bool, ∨, ∧, ¬, false, true)`.

## 2. How Sylvia models it

```
                         Proof engine
                  ┌────────────────────────────┐
   ambient logic  │  Proof.Logic = Theory.S     │   predicate calculus (∀/∃, Trading,
   (always on)    │                             │   One-point, prop axioms + rules)
                  └────────────────────────────┘
                               ▲  consulted for EVERY proof, under any theory
                               │
   theory         SetTheory : SetAlgebra : BooleanAlgebra<Set<'t>> : Theory
                    │            │              │
                    │            │              └─ §11.3 algebra: ∪=join, ∩=meet,
                    │            │                 ∅=zero, U=one, ~=comp  (Metatheorem 11.25)
                    │            └─ n-ary union/intersection quantifiers, indexed rules
                    └─ (slot for) Membership 11.3, Extensionality 11.4, operator defs 11.12-11.23
```

### The ambient-logic insight

The proof engine already consults **both** the theory's axioms/rules **and** the ambient logic's
(`Proof.Logic`, permanently `Theory.S`): see `Proof.fs` — the completion checks
`theory |- state || logic |- state` and the step-rule validation unions `logic.Rules` with
`theory.Rules`. `Proof.Logic` is never reassigned, so **predicate calculus is available underneath
any theory for free**. `SetTheory` therefore does *not* need to re-inherit S; it already "extends
predicate calculus" simply by being used as a proof theory. This is verified by check (D) in
`examples/proofs/SetTheory.fsx` (a prop tautology `P ⇒ P` closes under `set_theory` via the
"Logical Axiom of Implication").

The remaining job is the *set* side: the Boolean-algebra operators (inherited) plus the
set-specific axioms (injected).

### Axiom / rule composition (the plumbing)

`Theory` takes a single `Axioms` function (`Expr -> AxiomDescription option`) with no built-in
merging. Previously `BooleanAlgebra` **declared** `?axioms`/`?rules` but hard-coded
`inherit Theory(boolean_algebra_axioms …, [fixed rules])`, silently **discarding** anything a
subclass passed — so `SetTheory` could not add membership/extensionality even if it tried.

Fixed by:

- **`combine_axioms extra base`** (`Theories/BooleanAlgebra.fs`): try the subclass's `extra`
  recognizer first, then fall back to the Boolean-algebra `base`. Injected axioms compose *over*
  the algebra rather than replacing it.
- The `BooleanAlgebra` constructor now threads `?axioms` (via `combine_axioms`), appends `?rules`
  **after** the seven built-ins (preserving the fixed indices `SetAlgebra` relies on), and forwards
  `?formula_printer`.
- `SetAlgebra` and `SetTheory` forward their optional `?axioms`/`?rules` with `?axioms = …` instead
  of collapsing them to `fun _ -> None`.

Verified by checks (A)/(B) in the foundation script: an injected marker axiom is recognized through
a `SetAlgebra(axioms = …)`, while the built-in algebra axioms still fire.

## 3. Complement-law correctness fix

While wiring the base, three related bugs in the inherited `BooleanAlgebra` complement law were
found and fixed (they would have silently produced **unsound** set reasoning):

- The `Inverse` axiom lines were inverted: they recognized `S ∪ ~S = ∅` and `S ∩ ~S = U`. Corrected
  to `S ∪ ~S = U` (11.32, excluded middle) and `S ∩ ~S = ∅` (11.39, contradiction).
- `_comp` (the admissible complement rule) had two identical `join` branches (the second dead) and
  produced the wrong constants; it now rewrites `a ∪ ~a → one` and `a ∩ ~a → zero`, splicing the
  constants (`%one`/`%zero`) instead of quoting the local bindings.
- `_left_assoc`'s fallthrough recursed with `(_left_assoc meet meet)` instead of `(join meet)`.

Check (C) confirms the corrected polarity is recognized and the inverted forms are rejected.

## 4. Status and plan

- **Step 1 — composition plumbing.** ✅ Done. `combine_axioms` + constructor threading;
  complement-law fix. Foundation script green (12/12).
- **Step 2 — the base theory.** ✅ Done. `SetTheory` sits over predicate calculus (ambient logic S)
  *and* the Boolean set algebra; both bases verified reachable. No re-inheritance of S needed.
- **Step 3 — foundational axioms.** ✅ Done. Added a symbolic comprehension builder `set_comp bound
  range body : Set<'t>` (mirroring `forall_expr`; the runtime `set`/`finite_set` constructors in
  `Set.fs` are 4-arg *value* constructors, not the 3-arg symbolic form the patterns need). Set-theorem
  arguments are symbolic **`SetVar`s**, so `ElementOf` now returns the *raw* set operand (it previously
  required a literal `Set`, so `x ∈ S` for a variable never matched); **Membership (11.3)** decomposes
  the comprehension itself via `SetComp`, and **Extensionality (11.4)** was rewritten for `SetVar`s
  (`S = T = (∀x|: x∈S = x∈T)`). Both are live, recognized axioms. Also fixed **One-Point (8.14)** in the
  predicate-calculus kernel (see below) and proved (11.5) and (11.7) as cross-layer smoke tests
  (`examples/proofs/SetTheory.fsx`, checks E–G).
- **Step 4 — operator axioms + the metatheorem.** ⬜ Planned. Add the membership-reduction axioms
  for Union/Intersection/Complement/Difference/**Subset**/Power set/Size (11.12–11.23), and — if
  worthwhile — represent **Metatheorem (11.25)** as a derivation tactic (a set identity `Es = Fs`
  discharged by translating to `Ep = Fp` and proving that with the propositional automation, then
  translating back). Note `⊆` corresponds to `⇒` (11.25b), so subset proofs reuse implication.

## 3a. One-Point (Gries 8.14) kernel fix

The (11.5)/(11.7) proofs reduce membership `∈` to an `∃`, which is then collapsed by One-Point. Two
bugs in `(|OnePoint|_|)` (`Patterns.fs`) blocked this (both latent — one-point was evidently never
exercised on predicate-application bodies):

- It substituted `P[x:=E]` with `subst_var_value`, whose `Application(a,x)` case replaces only the
  *function* position, leaving the argument — so `subst_var_value x e (R x) = R x`, not `R e`. Now uses
  `replace_var_expr` (general substitution). `subst_var_value` is unchanged (it is a beta-reduction
  helper used elsewhere).
- It only accepted the range `x = E` (dummy on the left), but membership yields `e = x` (dummy on the
  right) and there is no term-level equality-symmetry rule. It now accepts `E = x` as well; equality
  is symmetric, so this is a sound generalization of 8.14.

Verified: full prover suite 85/85 (no regression), and both set-theory smoke tests close.

## 5. Files

- `src/math/Sylvia.AbstractAlgebra/Theories/BooleanAlgebra.fs` — Boolean-algebra proof theory;
  `combine_axioms`, constructor threading, complement-law fix.
- `src/math/Sylvia.AbstractAlgebra/Theories/SetAlgebra.fs` — `∪/∩/~/∅/U` instantiation of
  `BooleanAlgebra<Set<'t>>` (§11.3).
- `src/math/Sylvia.AbstractAlgebra/Theories/SetTheory.fs` — membership/extensionality recognizer
  patterns, the two-foundation `SetTheory` type.
- `src/math/Sylvia.AbstractAlgebra/Definitions/Set.fs` — the `Set<'t>` data type and runtime
  operators / comprehension constructors.
- `examples/proofs/SetTheory.fsx` — runnable foundation verification (also a regression guard).
