# Predicate Calculus in Sylvia's Prover (Gries & Schneider ch. 8–9)

*Design note — 2026-07-06. Status: quantifier kernel verified and hardened; ~32 of the ch. 9
theorems ported and machine-checked; a runnable example and a CI test suite are in place. A few
conditional / metatheorem-Witness theorems remain deferred (see §6).*

The prover (`Sylvia.Prover`) formalizes Gries & Schneider's calculational logic **E/S**. This note
covers the **predicate** layer — general quantification (ch. 8) specialized to `∀` (quantify over
`∧`) and `∃` (quantify over `∨`) in ch. 9. See [`prover-automation.md`](prover-automation.md) for
the propositional automation and the ANF decision procedure it builds on.

Runnable example: [`docs/examples/proofs/PredCalculus.fsx`](examples/proofs/PredCalculus.fsx)
(`dotnet fsi docs/examples/proofs/PredCalculus.fsx`). Tests:
[`QuantifierKernelTests.fs`](../tests/Sylvia.Tests.Prover/QuantifierKernelTests.fs) (trusted base)
and [`PredCalculusTests.fs`](../tests/Sylvia.Tests.Prover/PredCalculusTests.fs) (theorems).

## 1. Representing quantifiers

A quantifier is `forall_expr bound range body` / `exists_expr bound range body`
([`Formula.fs`](../src/lang/core/Sylvia.Expressions/Formula.fs)); `bound` is a (tuple of)
variable(s), `range` and `body` are propositions. The surface combinators
([`Term.fs`](../src/lang/core/Sylvia.Expressions/Term.fs) `Pred` module) build these from a bound
variable and **predicates** applied to it:

- `forall (x, N, P)` / `exists (x, N, P)` → `(∀x | N : P)` / `(∃x | N : P)`
- `forall' (x, P)` / `exists' (x, P)` → the empty (`true`) range form `(∀x |: P)`

Here `N`, `P` are `Pred<'t>` and `N.[x]` is the proposition "N holds of `x`", so **every argument
depends on the dummy**. Several Gries laws (distributivity 9.5 / 9.21 / 9.22, trading-out 9.6)
instead need an operand that is **x-free** — the `¬occurs(x, ·)` side condition. For those the
[`PredCalculus`](../src/lang/core/Sylvia.Prover/Theories/PredCalculus.fs) module provides
**Prop-body builders** whose range/body are arbitrary propositions in `x`:

```fsharp
let qall (x:TermVar<'t>) (R:Prop) (B:Prop) : Prop  // (∀ x | R : B)
let qex  (x:TermVar<'t>) (R:Prop) (B:Prop) : Prop  // (∃ x | R : B)
```

so an x-free proposition `pp` can be written directly as an operand, and `truepred`
(`Pred(func = <@ fun _ -> %T.Expr @>)`, whose application reduces to the *named* `True`) stands in
where a `true` range/body is needed by a Pred-based combinator.

## 2. The trusted base and how it is verified

The quantifier **axioms** are recognized by `EquationalLogic.equational_logic_axioms`
(`EmptyRange` 8.13, `OnePoint` 8.14, `QuantifierCollect` 8.15, `RangeSplit` 8.16/8.18, `Interchange`
8.19, `Nesting` 8.20, `Renaming` 8.21, `Trading` 9.2, `ForAllDistribOr` 9.5, `GeneralizedDeMorgan`
9.17, `UniversalInstantiation` 9.13). Seven **admissible rewrite rules** (`empty_range`,
`trade_body`, `collect_forall_and`, `collect_exists_or`, `distrib_or_forall`, `split_range_forall`,
`split_range_exists`) are registered in `Theory.S`.

The propositional trusted base is checked by an ANF oracle, but that oracle cannot see through
`∀`/`∃`. The quantifier base is instead verified by **finite-domain grounding**: interpret
`(∀x|R:P)` as `∧` and `(∃x|R:P)` as `∨` over a domain of size 1–3, replacing the dummy by fresh
domain constants and leaving predicate applications as opaque atoms; two schematic quantifier
formulas are equal over every interpretation iff their groundings are propositionally equivalent
for each domain size (decided by the existing ANF oracle). This lives in `QuantifierKernelTests.fs`
and covers all seven rewrites plus the axioms, plus the `¬occurs` guards and instantiation.

### Two soundness fixes this surfaced

- **`occurs_free`** ([`Patterns.fs`](../src/lang/core/Sylvia.Prover/Patterns.fs)) returned `false`
  at every variable *leaf* and searched only a quantifier's body (ignoring its range), so
  `occurs_free x (P x)` was `false`. Every `¬occurs` side condition was silently bypassed — e.g.
  `distrib_or_forall` fired on an x-dependent disjunct (grounding-confirmed unsound). Rewritten to
  the standard binder-aware definition.
- **`is_inst_expr`** ([`FsExpr.fs`](../src/lang/core/Sylvia.Expressions/FsExpr.fs)), the matcher
  behind Universal Instantiation, extracted the substituted token with `takeWhile (c <> ' ')`,
  which did not stop at `)` — so instantiating with a body like `¬(P E)` failed. Rewritten to a
  **structural** check: read a candidate `E` off `r` where `l` is the bound variable, then verify
  by substituting that `E` everywhere and comparing.

(Also fixed: `_collect_forall_and` called `<@ forall @>` — the tupled combinator — instead of
`<@ forall_expr @>`, so the rule threw; and the `Nesting` pattern was mislabeled "Interchange
Variables".)

## 3. Instantiation

`inst x P e` builds the theorem `(∀x |: P) ⇒ P[x:=e]` (Gries 9.13), closed directly by the
Universal Instantiation axiom; `inst' x P = inst x P x`. `exists_intro x P e` is the dual
`P[x:=e] ⇒ (∃x |: P)` (9.28), the contrapositive of instantiating `¬P`.

## 4. Ported theorems

All are constructed-and-checked in `PredCalculusTests.fs` and catalogued in the example script.

| Gries | Name | Statement |
|------|------|-----------|
| 9.2  | `trade_forall_implies` | `(∀x|N:P) = (∀x|: N⇒P)` |
| 9.4a | `trade_forall_and_implies` | `(∀x|Q∧N:P) = (∀x|Q: N⇒P)` |
| 9.5  | `distrib_or_forall'` | `pp ∨ (∀x|N:Q) = (∀x|N: pp∨Q)` |
| 9.6  | `trade_forall_or_not` | `(∀x|N:pp) = pp ∨ (∀x|:¬N)` |
| 8.15 | `collect_forall_and'` / `distrib_forall_and'` | `(∀x|N:P) ∧ (∀x|N:Q) = (∀x|N: P∧Q)` |
| 9.8  | `ident_forall_true` / `ident_forall_true'` | `(∀x|N:true) = true` |
| 9.9  | `distrib_forall_body` | `(∀x|N:P=Q) ⇒ ((∀x|N:P) = (∀x|N:Q))` |
| 9.10 | `strengthen_forall_range_or` | `(∀x|N1∨N2:P) ⇒ (∀x|N1:P)` |
| 9.11 | `strengthen_forall_body_and` | `(∀x|N:P∧Q) ⇒ (∀x|N:P)` |
| 9.12 | `mono_forall_body` | `(∀x|N:Q⇒P) ⇒ ((∀x|N:Q) ⇒ (∀x|N:P))` |
| 9.13 | `inst` / `inst'` / `forall_implies` | `(∀x|:P) ⇒ P[x:=E]` |
| 9.16 | `forall_conseq` | `pp ⇒ (∀x|:pp)` (⇐ metatheorem) |
| 9.17 | `ident_exists_not_forall` | `(∃x|N:P) = ¬(∀x|N:¬P)` |
| 9.18a–c | `ident_not_exists_forall`, `…_not`, `ident_exists_not_forall_not` | De Morgan family |
| 9.19 | `trade_exists_and` | `(∃x|N:P) = (∃x|: N∧P)` |
| 9.20 | `trade_exists_and_and` | `(∃x|Q∧N:P) = (∃x|Q: N∧P)` |
| 8.15 | `collect_exists_or'` / `distrib_exists_or'` / `split_range_exists'` | ∃ over ∨, range split |
| 9.21 | `distrib_and_exists_and` | `pp ∧ (∃x|N:Q) = (∃x|N: pp∧Q)` |
| 9.22 | `distrib_and_exists` | `(∃x|N:pp) = pp ∧ (∃x|:N)` |
| 9.24 | `ident_exists_false` | `(∃x|N:false) = false` |
| 9.25 | `weaken_exists_range` | `(∃x|N:P) ⇒ (∃x|Q∨N:P)` |
| 9.26 | `weaken_exists_body` | `(∃x|N:P) ⇒ (∃x|N: P∨Q)` |
| 9.27 | `mono_exists` | `(∀x|N:Q⇒P) ⇒ ((∃x|N:Q) ⇒ (∃x|N:P))` |
| 9.28 | `exists_intro` | `P[x:=E] ⇒ (∃x|:P)` |
| 9.29 | `exists_forall_interchange` | `(∃x|:∀y|:P) ⇒ (∀y|:∃x|:P)` |

## 5. Writing quantifier proofs

Proof steps are addressed rule applications. There are two spellings; the deeper quantifier proofs
use the second.

**Explicit combinators.** `apply` / `apply_left` / `apply_right` / `apply_body` / `apply_range` take
a **rule** and produce a step; `left_branch` / `right_branch` / `apply_unary` / `select_body` /
`select_range` **wrap** an existing step. Chained with `|>` they build the step *innermost-first*, so
`rule |> apply_body |> apply_unary |> left_branch` executes *outermost*-first: descend into the left
operand, then the `¬`, then the quantifier body, and apply `rule` there. (Reading the address means
scanning the chain right-to-left.)

**`at` paths (preferred for deep steps).** `rule |> at [ left_branch; apply_unary; select_body ]`
applies `rule` at the position reached by that **outside-in** navigation — the same step as above, but
read top-down with the rule first. The path uses the pure navigators (`left_branch` / `right_branch`
/ `apply_unary` / `select_body` / `select_range`); the rule application is fused in at the deepest
point (so the terminal `apply_body`/`apply_left`/… of a `|>` chain becomes the navigator
`select_body`/`left_branch`/…). `at []` is `apply`.

**Bare rule = whole expression.** A rule used directly as a step (no addressing) means "apply to the
whole expression" — `def_implies` is `def_implies |> apply` — via an implicit `Rule -> RuleApplication`
conversion (hence the file-level `#nowarn "3391"`). This only fires for a bare rule *value*, not
through a `|>` pipe, so a rule produced by `|> Taut` / `|> Commute` still needs an explicit `|> apply`.

The generalized-De-Morgan rewrites use the structural `double_neg` admissible rule, which works on
`qall`/`qex` mixed bodies (unlike the Pred-only De Morgan theorems).

## 6. Deferred

These are conditional / metatheorem-Witness results whose previous proofs relied on the
(now-fixed) under-reporting `occurs_free`; a correct port needs Prop-body variants of the ∃/∀
distributivity theorems and the range-nonempty assumption discharged with `Deduce`, following
Gries' own proofs: `distrib_forall_and_cond` (9.7), `trade_exists_or` (9.23), and the metatheorem
Witness `ident_exists_implies` / `forall_conseq_trade_body` (9.30). The general change-of-dummy
(8.22) and split-off-term (8.23) need arithmetic and are out of scope for the pure fragment.
