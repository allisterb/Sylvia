# A Theory of Sets in Sylvia

Design and implementation notes for the Sylvia prover's theory of sets, following Gries &
Schneider, *A Logical Approach to Discrete Math*, **Chapter 11**. Companion to
[`prover-predicate-calculus.md`](prover-predicate-calculus.md) and
[`prover-automation.md`](prover-automation.md).

Runnable foundation check: `dotnet fsi examples/proofs/SetTheory.fsx` (**133/133**).

**Status.** Chapter 11 is covered apart from Size (11.12): the foundational layer (Membership 11.3,
Extensionality 11.4), every operator definition (Subset 11.13, Complement 11.18, Union 11.20,
Intersection 11.21, Difference 11.22, Power set 11.23, and `∅`/`U` membership), the Boolean-algebra
layer, and Metatheorem 11.25(a)/(b)/(c) mechanized as the `meta_set_ident` / `meta_subset` tactics. Size needs
a Σ quantifier — see §4d.

All of it now lives in the **library** (`Theories/SetTheory.fs`), not in the example script — see §6.

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
- **Metatheorem (11.25a/b/c) tactics.** ✅ Done. `meta_set_ident` (a) mechanizes the membership-route proof
  for *any* set identity over `{∪, ∩, ~, ∅, U, variables}` (each named law 11.26–11.42 in one call,
  now including the ∅/U identity/zero/excluded-middle/contradiction laws via the `EmptyMember`/
  `UniverseMember` axioms); `meta_subset` (b) proves `Es ⊆ Fs` via `Ep ⇒ Fp` (reflexivity 11.58, the
  ∩/∪ bound laws, …); (c) `Es = U` is just `meta_set_ident Es U`. See §4c and `examples/proofs/SetTheory.fsx`
  sections K–M.
- **Difference (11.22) and Power set (11.23).** ✅ Done — sections O and P. **Size (11.12) remains the
  one gap in the chapter**, and it is a real one: `#S = (Σx | x∈S : 1)` needs a Σ quantifier the pure
  fragment does not have. See §4d.
- **Step 5 — promotion into the theory.** ✅ Done (2026-08-08). The tactics and the named laws moved
  out of the example script into `Theories/SetTheory.fs`, generic over the element type; the script
  keeps the foundation checks, the hand proofs, and the harness. See §6.

## 4a. Two coherence issues — resolved

- **Union/intersection representation.** ✅ Fixed by unifying on the `|+|`/`|*|` operators. Both
  `SetAlgebra` (join/meet) and the Union/Intersection membership axioms now key on
  `op_BarPlusBar`/`op_BarMultiplyBar`, so a single union expression written in the natural operator
  notation (today `S + T` — see §6) is recognized by *both* the algebra laws and the membership
  axioms. The key insight: a
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

## 4c. Metatheorem 11.25(a) — the `meta_set_ident` tactic

Metatheorem (11.25a): a set identity `Es = Fs` is valid **iff** its propositional translation
`Ep = Fp` is valid, where Definition (11.24) maps `∅↦false, U↦true, ~↦¬, ∪↦∨, ∩↦∧`, and each set
variable `S` becomes its membership proposition `v∈S`. Rather than add this as a new *trusted*
primitive (which would import an out-of-kernel translation + validity oracle into the trusted base),
we **mechanize** the hand proof used for 11.28 / De Morgan, so every result is an ordinary
kernel-checked `Theorem` built only from the already-recognized axioms. The tactic
(`Theories/SetTheory.fs`; exercised in `examples/proofs/SetTheory.fsx` section K) has three parts:

1. **`translate : SetTerm → Prop`** — Definition 11.24, structurally, keeping `v∈S` atoms for
   variables (`∪↦+`, `∩↦*`, `~↦!!`).
2. **`unfold : SetTerm → Rule`** — a rewrite `(v∈s) = translate s`, built by recursion that mirrors
   the operator axioms: at each node apply the Union/Intersection/Complement membership axiom
   (`ax_ident`), then recurse into any *compound* operand (a bare variable is already an atom, so its
   step is skipped — avoids a no-op rewrite).
3. **`meta_set_ident lhs rhs : Theorem`** — apply **Extensionality** to get `(∀v|: v∈Es = v∈Fs)`; rewrite
   each side with its `unfold` lemma to reach the body `Ep = Fp`; prove `Ep = Fp` with
   `PropCalculus.decide` and fold it in with `Taut'` (replaces the body with `true`); close with
   `ident_forall_true'`.

`meta_set_ident` proves exactly the valid identities over `{∪, ∩, ~, variables}` and **rejects** invalid
ones (the discharge throws): section K checks both `S∪T = S∩T` and `~(S∪T) = ~S∪~T` are rejected.
Named laws proved by a single `meta_set_ident` call: **11.26/11.36** symmetry, **11.27** associativity,
**11.28** idempotency, **11.40/11.41** distributivity, **11.42a/b** De Morgan, absorption,
**11.19** double complement. This is the object-level payoff of §11.3 — the algebra laws "for free".

> **The body used to be discharged by `autoproof_anf` directly, which capped set identities at five
> set variables (2026-07-30).** Definition 11.24 gives one propositional atom per distinct set
> variable, so *which* propositional prover closes the body is what bounds how many variables an
> identity may mention — and `autoproof_anf` is exponential in atom count, guarded at
> `autoproof_max_atoms = 5`. Both tactics now go through **`PropCalculus.decide`**, which routes small
> bodies to the same in-kernel prover and larger ones to the SAT-refutation backend when one is
> installed (`SatProof.install()`).
>
> The reroute is non-regressive and needs no solver: with none installed `decide` falls back to
> `autoproof_anf`, and every section-K/L/M check proves exactly as it did — all of them mention at most
> 3 set variables, so they route to the in-kernel prover either way. What it buys is section **N** of
> `examples/proofs/SetTheory.fsx`: 6-variable De Morgan, a 6-variable ∪/∩ shuffle, 6-variable
> distributivity, a 6-variable subset obligation, and the same past the constants — with the
> soundness checks repeated at that size, and one check that uninstalls the backend to show the old
> ceiling is what was actually lifted.
>
> Rerouting is also what exposed a real bug in the SAT pipeline: `Cnf.to_cnf` treated `T` and `F` as
> atoms, so every goal mentioning a truth constant was reported a non-theorem. The ∅/U laws translate
> to bodies containing exactly those constants. See `docs/prover-sat-reconstruction.md` §7 item 7.

### Metatheorem 11.25(b) — subset via implication (`meta_subset`)

Gries (11.56) states that `Es ⊆ Fs` iff the characteristic predicate of `Es` *implies* that of `Fs`
— i.e. Metatheorem 11.25(b): `Es ⊆ Fs` valid iff `Ep ⇒ Fp` valid. `meta_subset` (section L) mechanizes
it. The goal `Es ⊆ Fs` is a bare proposition (not an equality), so we reduce it to `true`:

1. **Subset (11.13)** rewrites `Es ⊆ Fs` to `(∀v | v∈Es : v∈Fs)`.
2. **Trading (9.2)** `trade_forall_implies` rewrites that to `(∀v |: v∈Es ⇒ v∈Fs)`. The trade uses the
   *simple* membership predicates `(·∈Es)` / `(·∈Fs)` (`memPred`, a one-line `Pred`), so no recursion
   is needed here — the compound structure is untouched until the next step.
3. The section-K **`unfold`** lemmas rewrite the antecedent `v∈Es → Ep` (`at [select_body; left_branch]`)
   and consequent `v∈Fs → Fp` (`at [select_body; right_branch]`).
4. The body `Ep ⇒ Fp` is a tautology; **`decide`** proves it and **`Taut`** (not `Taut'` — the
   body is an implication, a bare proposition, not an equality) replaces it with `true`.
5. `ident_forall_true'` closes.

Same completeness/soundness guarantee: valid subset relations prove, non-subsets are rejected
(the discharge throws on a non-tautological implication). Section L proves **11.58** reflexivity,
the ∩ lower-bound (`S∩T ⊆ S`, `S∩T ⊆ T`), the ∪ upper-bound (`S ⊆ S∪T`, `T ⊆ S∪T`), `S∩T ⊆ S∪T`,
and rejects `S ⊆ S∩T` / `S∪T ⊆ S`.

### ∅ / U membership atoms → the identity/zero/complement laws + Metatheorem 11.25(c)

Two constant-membership axioms were added to `SetTheory.fs`: **`EmptyMember`** `v∈∅ = false`
(the empty set has no members; Gries ∅ = `{x|false}`) and **`UniverseMember`** `v∈U = true` (every
value is in the universe). They match the *structured* forms `NewUnionCase Empty` and
`PropertyGet U` — so a `∅`/`U` SetTerm must be built inside a quotation (`SetTerm<int>(<@ Set.Empty @>)`);
writing `Set.Empty` outside one evaluates it to an opaque value that no axiom matches. With these,
`translate` gains `∅ ↦ false`, `U ↦ true`, and `unfold` gains the terminal cases `v∈∅ → false`,
`v∈U → true`, so `meta_set_ident` now covers every Gries law mentioning `∅` or `U`: **11.29/11.30** identity
of ∪ and zero, **11.34/11.35** identity of ∩ and zero, **11.32** excluded middle `S∪~S = U`, **11.39**
contradiction `S∩~S = ∅` (section M). **Metatheorem 11.25(c)** (`Es = U` valid iff `Ep` valid) needs
*no* separate tactic — it is just `meta_set_ident Es U`, whose body reduces to `Ep = true`.

## 4d. Difference (11.22) and Power set (11.23)

These are the last two operators of the chapter, and they land on opposite sides of the metatheorem —
which is the interesting part.

**Difference `S − T` (11.22, `v ∈ S−T = v∈S ∧ v∉T`) extends `meta_set_ident`.** New: `SetOps.difference` and
the `SetTerm.(|-|)` overloads in `Definitions/Set.fs` (`Set<'t>` already had `|-|`; `SetTerm<'t>`, the
*symbolic* type theories are written against, did not), the `DifferenceMember` axiom, and an `SDiff`
case in the example's classifier / `translate` (`− ↦ ∧¬`) / `unfold`. In `unfold` the right operand
sits under the `¬` that 11.22 introduces, so its sub-rewrite needs the extra `apply_unary` descent the
complement case also makes.

Note carefully that **`−` is NOT in Definition 11.24's grammar** (`{set variables, ∅, U, ~, ∪, ∩}`),
so this is a conservative *extension* of the mechanized metatheorem, not an instance of it. It is
sound because `−` is definable from operators that are in the grammar — `S − T = S ∩ ~T` — so the
translated body stays inside the fragment 11.25 talks about. Section O opens by proving that defining
identity **through the extended translation**, which is what distinguishes "the extension agrees with
11.22" from "the extension compiles": eleven further laws follow, including Gries' own remark that
`~S = U − S` (p.203), the two De Morgan forms over `−`, and the 11.25(b) bounds `S−T ⊆ S`, `S−T ⊆ ~T`.

**Power set `𝒫S` (11.23, `T ∈ 𝒫S = T ⊆ S`) does not.** Membership in `𝒫S` does not reduce to a
propositional combination of memberships *of the same element*: the right-hand side is a subset
proposition, itself a `∀` over a different element. The type climbs too — `𝒫S : set(set(t))` — so the
member is itself a set. Mechanically it is an instance `PropertyGet` rather than an operator, and it
needed one new overload, `SetTerm<'t> |?| SetTerm<Set<'t>>`, because F# will not upcast `SetTerm<'t>`
to `Term<Set<'t>>` while resolving `|?|`.

So the power set sits one layer up, and the way to use it is a composition of the two metatheorem
tactics: let 11.23 take the goal **down** to a subset obligation, then discharge that with 11.25(b).
That is `powerset_member` in section P, and it proves `∅ ∈ 𝒫S`, `S ∈ 𝒫S`, `S∩T ∈ 𝒫S`, `S−T ∈ 𝒫S`
while refusing `S∪T ∈ 𝒫S`.

Example is now **133/133** — the 89 checks described above, plus section Q, which proves every named
law through the library exports listed in §6 (including two at element types other than `int`).

**Size (11.12) is the one thing left in ch.11**, and it is genuinely out of reach rather than merely
unstarted: `#S = (Σx | x∈S : 1)` needs a Σ quantifier, i.e. a quantified fold over a numeric codomain.
Sylvia's quantifier machinery is `∀`/`∃` over `Prop` bodies; a Σ needs arithmetic in the body and a
different one-point/split-off-term story — the same reason Gries 8.22/8.23 are out of scope for the
predicate calculus (see `prover-predicate-calculus.md` §6).

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
  patterns, the two-foundation `SetTheory` type, **the metatheorem tactics and the named laws** (§6).
- `src/math/Sylvia.AbstractAlgebra/Definitions/Set.fs` — the `Set<'t>` data type and runtime
  operators / comprehension constructors.
- `examples/proofs/SetTheory.fsx` — runnable foundation verification (also a regression guard).

## 6. The library API

Until 2026-08-08 the metatheorem tactics existed only inside `examples/proofs/SetTheory.fsx`, pinned
to `int`, so nothing outside that script could cite a set-theoretic law. They are now part of the
theory, generic over the element type, in the same shape `PredCalculus` uses for the ch.8/9 theorems:
each named law is a **function of its set arguments returning a `Theorem`**, so a proof elsewhere
writes `SetTheory.de_morgan_union S T` instead of restating the identity and re-proving it.

### Notation: the symbolic operators are arithmetic

On `SetTerm<'t>` — the *symbolic* type theories are written against — union, intersection and
difference are `+`, `*` and `-`, mirroring `Prop`'s `+` = ∨ and `*` = ∧. That is Definition (11.24)
made syntactic: a set expression and the propositional body it translates to are written the same
way. Unary `-` remains complement (F# keeps `~-` and binary `-` distinct), and `⊆` remains `|<|`,
since a subset is a proposition rather than an algebraic operation.

This is a **surface** rename. `SetTerm`'s operators still *build* `Set<'t>`'s `op_BarPlusBar` /
`op_BarMultiplyBar` / `op_BarMinusBar`, so every axiom pattern, expression tree and proof is
unaffected by which spelling the caller used. Renaming the underlying `Set<'t>` methods would **not**
be equivalent, and the reasons are worth recording:

- `Set<'t>` already spends `(*)` on the Cartesian product (`Set<'a> * Set<'b> -> Set<'a*'b>`).
- The membership axioms match the method **name**, deliberately element-type-agnostically.
  `op_BarPlusBar` is unique to sets in this tree; `op_Addition` is not.
- `Formula.(|Add|_|)` and `FsExpr.(|Addition|_|)` / `(|Multiplication|_|)` are name-only patterns.
- `Z3.fs` maps `op_Addition` to `MkAdd` — a set union reaching the arithmetic translator would
  silently become integer addition.

Consequently, inside a **quotation** the operands are `Set<'t>` *values*, not `SetTerm`s, so those
sites keep `|+|` / `|*|`. There are two: `SetAlgebra`'s join/meet method references, which must name
`Set`'s method by construction, and one identity check in the example script.

What the change buys is precedence. `|+|` and `|*|` both begin with `|`, so F# gives them equal
precedence and left-associates: `s |+| t |*| u` parsed as `(s ∪ t) ∩ u`, the opposite of the
conventional reading, and ∩-binds-tighter was simply not expressible. `s + t * u` is `s ∪ (t ∩ u)`.

### Rendering

Two independent renderers show set expressions, and both now use Gries' notation:

- **`SetOps.sprintset`** (`SetTerm.Display`/`ToString`) — `∪ ∩ − ~ ∅ 𝕌`, parenthesizing compound
  operands, since the printed symbols carry no precedence of their own.
- **`Display.print_formula`** (proof logs and step descriptions) — via **two** theory-agnostic
  mechanisms, neither of which puts any mention of sets in `Sylvia.Prover`. A proof step now reads
  `(S − T) ⊆ ~T`.

**1. Structural, from `[<Symbol>]`.** `Display` gained `SymbolicUnary`/`SymbolicBinary` cases: a
`Call` whose method declares a display symbol renders as that symbol rather than being decompiled.
The set operators carry the attribute — `∪ ∩ − ∈ ⊆` on `Set<'t>`'s methods, and `~` on `(~-)`.
Operands are bracketed unless *tight* (a leaf, or a prefix application, which binds tighter than any
infix operator), so the output is `(S − T) ⊆ ~T` and `~(S ∪ T)`, never `~S ∪ T`.

The unary case alone is **not** enough, and this is the part worth remembering: a term with no
structural case is decompiled *whole* by `print_src`, so a complement nested inside a set expression
is never reached and reverts to `-`. Rendering the infix operators structurally is what lets the
recursion get down to it. This is also why `~` cannot be done as a text substitution at all: it
decompiles to a bare `-`, and no substring rewrite can distinguish that from subtraction.

**2. Textual, from `Symbols.BulitIn`.** `print_src` applies the table after its own private map, for
terms that reach the fallback. `∅` and `𝕌` are rendered this way (a nullary union case and a property
get, neither of which is a `Call`), and the operator spellings are registered too as a safety net for
set *values* built outside `SetTerm`. Two constraints: the keys are what the **decompiler** emits
(` |+| `, not the surface `+`), and the surrounding **spaces are load-bearing** — without them
` ||+|| ` (LinearAlgebra's matrix/vector operator) contains ` |+| ` and would be mangled.
`builtin_substitutions` also sorts longest-key-first so that holds for keys nobody anticipated.

Registration lives in a `static let` on `SetTerm` that the instance constructor reads, **not** in a
module `do` binding. F# runs a file's `do` bindings only on first access to a *value* in that file,
and the type is `beforefieldinit` — measured, a module `do` (in either `Set.fs` or `SetAlgebra.fs`)
and a bare `static do` all left the table unregistered through `setvar`, `s + t`, and a full set
proof. `Display` also invalidates its render cache when the table's size changes, so anything
rendered before a theory registered its notation is not left cached and wrong.

**Cost.** Rendering set formulas got **~5× faster**, because the structural path skips the Unquote
decompilation it used to pay: a 40-term set formula renders cold in ~1.0 ms against 5.24 ms before.
The textual fallback that remains is 0.072 µs of substitution against a 25.5 µs decompile (0.29%);
a hypothetical 128-entry table is ~2%. The propositional path is untouched — it matches on the
boolean connectives well before these cases — and pays only one `Symbols.BulitIn.Count` read per
top-level render.

> Whole-formula A/B is useless on this machine: the *propositional* payload, which none of this can
> affect, was measured at 0.63 / 0.33 / 0.31 / 0.18 ms across four runs. Anything under ~3× on these
> payloads is noise; the set result is 5× and the substitution cost was measured in isolation.

| Export | Meaning |
|---|---|
| `set_theory<'t>` | the theory instance, now **cached per element type** (see below) |
| `empty_set<'t>` / `universe<'t>` | `∅` / `U` as structured `SetTerm`s (they must be built inside a quotation, or no axiom matches) |
| `translate v s` | Definition 11.24, structurally |
| `unfold v s` | the rewrite rule `(v ∈ s) = translate v s` |
| `meta_set_ident l r` | Metatheorem 11.25(a) — and (c) is `meta_set_ident Es universe` |
| `meta_subset l r` | Metatheorem 11.25(b) |
| `powerset_member t s` | 11.23 composed down onto 11.25(b) |
| named laws | 11.19, 11.26–11.30, 11.32, 11.34–11.36, 11.39–11.42, absorption, the ∩/∪ bounds, 11.58, the difference laws, the power-set memberships |

`set_theory<'t>` used to be a plain generic `let` value, which F# re-evaluates on **every access** — so
every `ax_ident` inside a tactic built a fresh `Theory` and threw away its per-instance axiom cache
(`Proof.fs`). It is now a static member of a private generic class, i.e. one instance per element
type, initialized once.

### An F# trap worth knowing: silent loss of genericity

Two of these functions compiled cleanly while being **not generic at all** — `'t` had been quietly
solved as `obj`, which only shows up when a caller tries to use them at a concrete element type:

- `<@ fun (z:'t) -> z |?| %s.Expr @>` (the membership predicate). With a rigid `'t` the compiler
  cannot rule out that `'t` is itself a `Term<_>`, so `|?|` is ambiguous between the `('t, Set<'t>)`
  and `(Term<'t>, Set<'t>)` overloads.
- `t |?| s.Powerset`. `SetTerm<'t>` **is** a `Term<Set<'t>>`, so both the `(Term<'u>, SetTerm<'u>)`
  and the `(SetTerm<'u>, SetTerm<Set<'u>>)` overloads apply.

Both are fixed by building the call explicitly (or upcasting one operand to pick an overload); both
build the same expression either way, so nothing about the proofs changes. Two things make this easy
to miss: warning **FS0064 is reported at the CALLER**, not at the definition that lost its type
variable, and a degenerate function is perfectly usable from other degenerate code — the example
script never noticed because everything in it was `int`. When generalizing a theory over its element
type, check the compiled signatures (`m.IsGenericMethodDefinition`) rather than trusting a clean
build.
