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
- **Step 4 — operator axioms + the metatheorem.** ◐ Core done. Added the membership-reduction axioms
  **Union (11.20)** `v∈S∪T = v∈S ∨ v∈T`, **Intersection (11.21)** `v∈S∩T = v∈S ∧ v∈T`,
  **Complement (11.18)** `v∈~S = ¬(v∈S)`, **Subset (11.13)** `S⊆T = (∀x|x∈S:x∈T)` — all live,
  recognized, keyed on the SetTerm operator methods (`|+|`/`|*|`/`-`/`|<|`). Proved **11.28**
  `S ∪ S = S` via the membership route (extensionality → Union axiom → ∨-idempotency) and **De Morgan
  (11.42a)** `~(S∪T) = ~S∩~T`. Resolved the two coherence issues (§4a): union/intersection are keyed
  on the `|+|`/`|*|` operators in *both* the algebra and the membership axioms, so one `S |+| T`
  expression matches both routes, and `S ⊆ T` is now a proposition. See `examples/proofs/SetTheory.fsx`
  checks H–J.
- **Metatheorem (11.25a/b/c) tactics.** ✅ Done. `metaset` (a) mechanizes the membership-route proof
  for *any* set identity over `{∪, ∩, ~, ∅, U, variables}` (each named law 11.26–11.42 in one call,
  now including the ∅/U identity/zero/excluded-middle/contradiction laws via the `EmptyMember`/
  `UniverseMember` axioms); `metasubset` (b) proves `Es ⊆ Fs` via `Ep ⇒ Fp` (reflexivity 11.58, the
  ∩/∪ bound laws, …); (c) `Es = U` is just `metaset Es U`. See §4c and `examples/proofs/SetTheory.fsx`
  sections K–M. Still open: Difference/Power set/Size (11.22/11.23/11.12 — the last needs a Σ quantifier).

## 4a. Two coherence issues — resolved

- **Union/intersection representation.** ✅ Fixed by unifying on the `|+|`/`|*|` operators. Both
  `SetAlgebra` (join/meet) and the Union/Intersection membership axioms now key on
  `op_BarPlusBar`/`op_BarMultiplyBar`, so a single `S |+| T` expression written in the natural operator
  notation is recognized by *both* the algebra laws and the membership axioms. The key insight: a
  *type-annotated bare operator quotation* `<@ (|+|) : Set<'t> -> Set<'t> -> Set<'t> @>` is a direct
  method reference that `SpecificCall` accepts (and it resolves generically over the element type). Only
  an explicit *lambda* `<@ fun a b -> a |+| b @>` fails — that was the earlier red herring. The axiom
  patterns check `mi.Name = "op_BarPlusBar"` directly rather than via `Binary <@ (|+|) @>`, because
  `Binary`'s type guard would pin each axiom to a single element type. Complement already aligned
  (`-`/`Set.(~-)` both `op_UnaryNegation`). *(The earlier `sunion`/`sinter` combinators are gone.)*
- **Subset typing.** ✅ Fixed. The five `SetTerm.(|<|)` overloads (`Definitions/Set.fs`) now return
  `Scalar<bool>` instead of `SetTerm`, so `S ⊆ T` is a proposition. (`ssubset` is also provided.)

## 4b. De Morgan (11.42a)

`~(S ∪ T) = ~S ∩ ~T` is proved via the membership route (`examples/proofs/SetTheory.fsx` section J):
extensionality; the complement/union/intersection axioms reduce each membership; propositional
De Morgan (`distrib_not_or`, `¬(p∨q) = ¬p ∧ ¬q`) equates the sides; then reflexivity and
`(∀v|:true) = true`. It exercises all three Boolean operators together in one proof.

## 4c. Metatheorem 11.25(a) — the `metaset` tactic

Metatheorem (11.25a): a set identity `Es = Fs` is valid **iff** its propositional translation
`Ep = Fp` is valid, where Definition (11.24) maps `∅↦false, U↦true, ~↦¬, ∪↦∨, ∩↦∧`, and each set
variable `S` becomes its membership proposition `v∈S`. Rather than add this as a new *trusted*
primitive (which would import an out-of-kernel translation + validity oracle into the trusted base),
we **mechanize** the hand proof used for 11.28 / De Morgan, so every result is an ordinary
kernel-checked `Theorem` built only from the already-recognized axioms. The tactic
(`examples/proofs/SetTheory.fsx` section K) has three parts:

1. **`translate : SetTerm → Prop`** — Definition 11.24, structurally, keeping `v∈S` atoms for
   variables (`∪↦+`, `∩↦*`, `~↦!!`).
2. **`unfold : SetTerm → Rule`** — a rewrite `(v∈s) = translate s`, built by recursion that mirrors
   the operator axioms: at each node apply the Union/Intersection/Complement membership axiom
   (`id_ax`), then recurse into any *compound* operand (a bare variable is already an atom, so its
   step is skipped — avoids a no-op rewrite).
3. **`metaset lhs rhs : Theorem`** — apply **Extensionality** to get `(∀v|: v∈Es = v∈Fs)`; rewrite
   each side with its `unfold` lemma to reach the body `Ep = Fp`; prove `Ep = Fp` with the
   **complete** ANF prover `autoproof_anf` and fold it in with `Taut'` (replaces the body with
   `true`); close with `ident_forall_true'`.

Because `autoproof_anf` is complete for — and *only* for — propositional tautologies, `metaset`
proves exactly the valid identities over `{∪, ∩, ~, variables}` and **rejects** invalid ones
(`autoproof_anf` throws): section K checks both `S∪T = S∩T` and `~(S∪T) = ~S∪~T` are rejected.
Named laws proved by a single `metaset` call: **11.26/11.36** symmetry, **11.27** associativity,
**11.28** idempotency, **11.40/11.41** distributivity, **11.42a/b** De Morgan, absorption,
**11.19** double complement. This is the object-level payoff of §11.3 — the algebra laws "for free".

### Metatheorem 11.25(b) — subset via implication (`metasubset`)

Gries (11.56) states that `Es ⊆ Fs` iff the characteristic predicate of `Es` *implies* that of `Fs`
— i.e. Metatheorem 11.25(b): `Es ⊆ Fs` valid iff `Ep ⇒ Fp` valid. `metasubset` (section L) mechanizes
it. The goal `Es ⊆ Fs` is a bare proposition (not an equality), so we reduce it to `true`:

1. **Subset (11.13)** rewrites `Es ⊆ Fs` to `(∀v | v∈Es : v∈Fs)`.
2. **Trading (9.2)** `trade_forall_implies` rewrites that to `(∀v |: v∈Es ⇒ v∈Fs)`. The trade uses the
   *simple* membership predicates `(·∈Es)` / `(·∈Fs)` (`memPred`, a one-line `Pred`), so no recursion
   is needed here — the compound structure is untouched until the next step.
3. The section-K **`unfold`** lemmas rewrite the antecedent `v∈Es → Ep` (`at [select_body; left_branch]`)
   and consequent `v∈Fs → Fp` (`at [select_body; right_branch]`).
4. The body `Ep ⇒ Fp` is a tautology; **`autoproof_anf`** proves it and **`Taut`** (not `Taut'` — the
   body is an implication, a bare proposition, not an equality) replaces it with `true`.
5. `ident_forall_true'` closes.

Same completeness/soundness guarantee: valid subset relations prove, non-subsets are rejected
(`autoproof_anf` throws on a non-tautological implication). Section L proves **11.58** reflexivity,
the ∩ lower-bound (`S∩T ⊆ S`, `S∩T ⊆ T`), the ∪ upper-bound (`S ⊆ S∪T`, `T ⊆ S∪T`), `S∩T ⊆ S∪T`,
and rejects `S ⊆ S∩T` / `S∪T ⊆ S`.

### ∅ / U membership atoms → the identity/zero/complement laws + Metatheorem 11.25(c)

Two constant-membership axioms were added to `SetTheory.fs`: **`EmptyMember`** `v∈∅ = false`
(the empty set has no members; Gries ∅ = `{x|false}`) and **`UniverseMember`** `v∈U = true` (every
value is in the universe). They match the *structured* forms `NewUnionCase Empty` and
`PropertyGet U` — so a `∅`/`U` SetTerm must be built inside a quotation (`SetTerm<int>(<@ Set.Empty @>)`);
writing `Set.Empty` outside one evaluates it to an opaque value that no axiom matches. With these,
`translate` gains `∅ ↦ false`, `U ↦ true`, and `unfold` gains the terminal cases `v∈∅ → false`,
`v∈U → true`, so `metaset` now covers every Gries law mentioning `∅` or `U`: **11.29/11.30** identity
of ∪ and zero, **11.34/11.35** identity of ∩ and zero, **11.32** excluded middle `S∪~S = U`, **11.39**
contradiction `S∩~S = ∅` (section M). **Metatheorem 11.25(c)** (`Es = U` valid iff `Ep` valid) needs
*no* separate tactic — it is just `metaset Es U`, whose body reduces to `Ep = true`. Example is now
**55/55**. Remaining ch.11: Difference (11.22), Power set (11.23), Size (11.12 — needs a Σ quantifier).

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
