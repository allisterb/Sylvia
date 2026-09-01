# Mathematical Induction in Sylvia

Design and implementation notes for Gries & Schneider, *A Logical Approach to Discrete Math*,
**Chapter 12** (§12.1 and the part of §12.2 that follows from it). Companion to
[`prover-predicate-calculus.md`](prover-predicate-calculus.md) and
[`prover-set-theory.md`](prover-set-theory.md).

Runnable check: `dotnet fsi examples/proofs/Induction.fsx` (**32/32**).

**Status.** Induction over the integers is mechanized as **one axiom** — Gries (12.8), induction over
`{n₀, n₀+1, …}` — plus a combinator that discharges it, and a lemma (`hypothesis_at`) that packages
the only order reasoning a proof by induction needs. A worked example proves
`(∀n | 0 ≤ n : f.n = 2·n)` for the inductively defined `f.0 = 0`, `f(n+1) = f.n + 2`, and its
inductive step genuinely uses the inductive hypothesis.

---

## 1. What ch.12 asks for, and what is here

Gries states induction three ways in §12.1:

| Ref | Form |
|---|---|
| (12.3) | `(∀n:ℕ \|: (∀i \| 0 ≤ i < n : P.i) ⇒ P.n) ⇒ (∀n:ℕ \|: P.n)` — the axiom, no separate base case |
| (12.5) | `P.0 ∧ (∀n:ℕ \|: (∀i \| 0 ≤ i ≤ n : P.i) ⇒ P(n+1)) ⇒ (∀n:ℕ \|: P.n)` — "the form we generally use" |
| (12.8) | the same over `{n₀, n₀+1, …}` |

**Sylvia takes (12.8) as the axiom.** (12.5) is its `n₀ := 0` instance, so it costs nothing extra and
is not a second axiom — the example checks the axiom is recognized at base point `0`, at `1`, and at
a symbolic `k`.

(12.3) is **not** stated. Gries derives (12.5) from it with Split off term (8.23) over an integer
range, Empty range (8.13) and Change of dummy (8.22); 8.23 over integer ranges does not exist in this
prover, so mechanizing that derivation is its own piece of work. Since (12.5)/(12.8) is the form
every proof uses, taking it as the axiom is the honest trade — and it is a form Gries himself
numbers, not an invention here.

Also not here, and each for a stated reason: §12.2's Fibonacci and factorial examples (they need
`Π`, and `Σ`/`Π` need Split off term), §12.3 Peano arithmetic, §12.4 well-founded induction, §12.5
structural induction, §12.6 loop correctness. See §6.

## 2. The principle needs no arithmetic

This is the design point that makes the chapter tractable. In

```
P.n₀ ∧ (∀n | n₀ ≤ n : (∀i | n₀ ≤ i ∧ i ≤ n : P.i) ⇒ P(n+1))  ⇒  (∀n | n₀ ≤ n : P.n)
```

the `≤`s are **matched structurally and never reasoned about**. Nothing in the axiom, the recognizer
or the combinator knows that `≤` is transitive, or that `n < n+1`. Arithmetic is needed only in the
base and inductive cases, and those are the caller's proofs.

The one exception is `hypothesis_at` (§4), which needs exactly one order fact — reflexivity of `≤` —
and that is now an axiom of `Integers`.

## 3. The axiom, and what pins `P` down

`P` is **not** a syntactic argument: an axiom recognizer sees a statement, not the intent behind it.
So the four applications of `P` (at `n₀`, at `i`, at `n+1`, at `n`) are tied to one predicate by
RECONSTRUCTING three of them from the fourth — the conclusion's body `P.n` — by substitution:

```fsharp
sequal bse (replace_var_expr n2 n0c body)                                   // base = P.n[n := n₀]
sequal ih  (replace_var_expr n2 (Expr.Var i1) body)                         // hyp  = P.n[n := i]
sequal stp (replace_var_expr n2 (call_add (Expr.Var n1) (Expr.Value 1)) body)  // step = P.n[n := n+1]
```

That is the device the One-Point rule (8.14) uses. `replace_var_expr` is a naive traversal, so it is
sound only under side conditions, and all of them are checked:

| Condition | Why |
|---|---|
| the two outer dummies are the same variable, and differ from `i` | otherwise the step is about a different quantification than the conclusion |
| the three occurrences of `n₀` agree and mention neither dummy | a base point mentioning `n` would be CAPTURED by the range that binds it |
| `i` does not occur in `P.n` **at all** (free or bound) | substituting `i` in would capture |
| `n` is not re-bound inside `P.n` | the substitution would reach under a binder |

`occurs_at_all` is deliberately stronger than `occurs_free`: `occurs_free` would pass a `P` that
merely BINDS `i` somewhere inside, and substituting into that is exactly the capture being excluded.
Section (B) of the example turns each row into a rejection, plus wrong base case, wrong step
consequent (`P(n+2)`) and a strict hypothesis range (`i < n`).

## 4. `hypothesis_at` — where the order reasoning is isolated

The inductive hypothesis is `(∀i | n₀ ≤ i ∧ i ≤ n : P.i)` — all of `P.n₀ … P.n`. What an inductive
step normally wants is one conjunct of it, `P.n`. Getting there is the same three moves every time,
so they are done once:

```fsharp
hypothesis_at th n₀ n i P  :  ⊢ (n₀ ≤ n ∧ (∀i | n₀ ≤ i ∧ i ≤ n : P.i)) ⇒ P.n
```

Trading (9.2) turns the hypothesis into a true-range `∀`; Instantiation (9.13) applies it at `i := n`
and leaves the range obligation `n₀ ≤ n ∧ n ≤ n`; `n ≤ n` is discharged by **reflexivity of `≤`**
and `n₀ ≤ n` by the antecedent. The proof is six rewrites and ends on Instantiation, which is an
axiom of the ambient logic.

`n₀ ≤ n` has to be a premise: it can come from nowhere else, and it is available in a step proof only
if the step's `∀` has been traded so that its range is an antecedent. That is why the worked example
proves the traded body `n₀ ≤ n ⇒ (IH ⇒ P(n+1))` as its own theorem and then generalizes.

**Weak vs strong induction.** Gries folds them into one form deliberately (§12.1): a proof that uses
only `P.n` is a weak induction and needs no different principle. `hypothesis_at` is that conjunct,
which is why weak induction needs nothing further here; a strong induction instantiates the
hypothesis at some other `i` instead, with the same three moves and a different range obligation.

## 5. The combinator adds no trusted step

```fsharp
by_induction th n₀ n i P base step : Theorem
```

instantiates the axiom at `P`, conjoins the two supplied theorems, and detaches by modus ponens. The
two pieces of plumbing that needed are general enough to live in `PropCalculus`:

| Export | Meaning |
|---|---|
| `mp imp ant` | from `⊢ A ⇒ B` and `⊢ A`, conclude `⊢ B` |
| `conj l r` | from `⊢ A` and `⊢ B`, conclude `⊢ A ∧ B` |

Neither is a kernel primitive. `mp` is three rewrites — `B → (T ⇒ B)` by left identity of `⇒` (3.73)
used in reverse, `T → A` by the completed proof of `A` read backwards, `(A ⇒ B) → T` by the completed
proof of the implication — and `T` is an axiom. `conj` rewrites each side to `T` by its own proof and
reduces `T ∧ T`. Both carry the supplied proofs as the justification of their rewriting rules, so
neither derivation is replayed (the cost `Tactics.Taut` was built to avoid).

`by_induction` refuses unless each theorem proves EXACTLY its obligation, and the example checks it
refuses a base case at the wrong point, the two obligations swapped, and a step for a different
predicate.

## 6. The worked example, and what it exercises

`examples/proofs/Induction.fsx` section (D). An inductively defined function in the sense of §12.2:

```
f.0 = 0        f(n+1) = f.n + 2                 ⊢ (∀n | 0 ≤ n : f.n = 2·n)
```

The two defining equations are the CALLER's axioms, passed to `Induction(f_axioms)`. That is what the
optional-axioms constructor is for: the base case, the inductive case and the induction axiom
instance must all be proved in one theory, or they cannot be detached from each other.

```
base case:  f.0 = 2·0            definition of f, then constant folding
step body:  0 ≤ n ⇒ (IH ⇒ f(n+1) = 2·(n+1))
              shunt to (0 ≤ n ∧ IH) ⇒ …    so the antecedent is available
              f(n+1) → f.n + 2              definition of f
              f.n → 2·n                     Deduce' on hypothesis_at — THE inductive step
              2·(n+1) → 2·n + 2·1 → 2·n + 2 distributivity, then folding
              (2·n + 2 = 2·n + 2) → T
step:       trade the ∀, replace its body by the theorem just proved, (∀n |: T) = T
```

The example also checks the negative: **the same step proof with the `Deduce'` line removed does not
close**. Without that check "proved by induction" would not distinguish a genuine use of the
hypothesis from a `P` that was provable outright.

An example whose statement genuinely CANNOT be proved without induction needs either a recursive
definition (as here) or `Σ`/`Π` with Split off term. Gries' own (12.7) `(Σi | 1 ≤ i ≤ n : 2i−1) = n²`
is the second kind, and it needs three things this theory does not have yet:

1. **Split off term (8.23)** over an integer range, for `Σ` — `(Σi | 1 ≤ i ≤ n+1 : E) = (Σi | 1 ≤ i ≤ n : E) + E(n+1)`.
2. An **empty-range fact for a numeric range** — `1 ≤ i ≤ 0` is empty, which is arithmetic, not the
   syntactic `false` that `Sums`' (8.13) matches.
3. More of the **order on ℤ** than reflexivity.

That is the natural next increment, and it is arithmetic work rather than induction work.

## 7. A bug this found: `Integers.reduce` never reduced anything

`_reduce_constants` folded constants like this, at every numeric type:

```fsharp
| Multiply(Int32 l, Int32 r) -> <@@ l * r @@>
```

A quotation over the pattern-bound `l`/`r` captures them as `ValueWithName(v, "l")` **operands** and
rebuilds the CALL. Measured: `2 * 0` went to `l * r` — the values 2 and 0, unevaluated and now
misleadingly named. The rule was inert, and had been at every type. It is now
`Expr.Value(l * r)`, and `2 * 0` folds to `0`.

Nothing depended on the old behaviour (suite 135/135 and every example unchanged), which is itself
the point: an admitted rule that silently does nothing is invisible until something needs it.

## 8. Files

- `src/math/Sylvia.AbstractAlgebra/Theories/Induction.fs` — the axiom, the obligations,
  `hypothesis_at`, `by_induction`, and the `Induction` theory (integers + (12.8), extensible with a
  caller's axioms).
- `src/math/Sylvia.AbstractAlgebra/Theories/Integers.fs` — reflexivity of `≤`; the `reduce` fix.
- `src/lang/core/Sylvia.Prover/Theories/PropCalculus.fs` — `mp` and `conj`.
- `src/lang/core/Sylvia.Expressions/Formula.fs` — the `LessEq` pattern (named that because
  `LessThanOrEqual` is a fixed-point CONSTRAINT type in `Sylvia.Arithmetic`, auto-opened into the
  same namespace).
- `src/lang/core/Sylvia.Expressions/Term.fs` — `IntTerm`'s `*` and `-` (it already had `+`).
- `src/lang/core/Sylvia.Prover/Display.fs` — structural `<` and `≤`, so a range prints as Gries
  writes it.
- `examples/proofs/Induction.fsx` — the runnable check.

## 9. The API

| Export | Meaning |
|---|---|
| `Induction.induction` | the theory: integer arithmetic plus (12.8) |
| `Induction(axioms)` | the same, extended with a caller's axioms (an inductive definition, say) |
| `induction_axiom n₀ n i P` | the (12.8) instance for `P` |
| `base_case` / `inductive_case` / `conclusion` / `hypothesis` | the four pieces of it, separately |
| `hypothesis_at th n₀ n i P` | `(n₀ ≤ n ∧ IH) ⇒ P.n` |
| `by_induction th n₀ n i P base step` | (12.8) discharged |
| `by_induction_nat th n i P base step` | the `n₀ := 0` case, i.e. (12.5) |
| `leq` / `succ` / `tm` / `pred_of` | building blocks for stating a `P` and its obligations |
| `PropCalculus.mp` / `PropCalculus.conj` | modus ponens and ∧-introduction, as combinators |

## 10. Gate

```bash
dotnet build src/Sylvia.sln                                        # 0 errors
SYLVIA_SEQUAL_CHECK=1 dotnet test tests/Sylvia.Tests.Prover/...    # 135/135
dotnet fsi examples/proofs/Induction.fsx                           # 32/32 ALL PASS
dotnet fsi examples/proofs/SetTheory.fsx                           # 167/167 ALL PASS
dotnet fsi examples/proofs/AdversarialSweep.fsx                    # ALL CLEAR
dotnet fsi examples/atp/Sledgehammer.fsx                           # ALL PASS
dotnet fsi examples/proofs/VerifyRuleFixes.fsx                     # ALL CHECKS PASSED
```

`PropCalculus.fsx`, `PredCalculus.fsx` and `IntegerAlgebra.fsx` all close.

Perf A/B'd against the pre-session tree the way §4 of the perf handoff asks (`git stash -u` +
rebuild, min of 3): `PredCalculus.fsx` 2044 → 2021 ms, `SetTheory.fsx` 4043 → 4150 ms — inside the
run-to-run spread on this machine (the baseline's own three runs of the set example spanned
4043–4452 ms). Nothing here is on a hot path: the two new `Display` cases sit in the tail of a match
that propositional formulas never reach, `mp`/`conj` are new functions, and the `Integers.reduce` fix
changes a rule that no propositional or predicate payload runs.
