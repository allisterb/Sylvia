# Mathematical Induction in Sylvia

Design and implementation notes for Gries & Schneider, *A Logical Approach to Discrete Math*,
**Chapter 12** (§12.1, and §12.2's inductively defined functions and strong induction — §20).
Companion to [`prover-predicate-calculus.md`](prover-predicate-calculus.md) and
[`prover-set-theory.md`](prover-set-theory.md).

Runnable checks: `dotnet fsi examples/proofs/Induction.fsx` (**134/134**),
`dotnet fsi examples/proofs/InductiveDefinitions.fsx` (**49/49**, §20) and
`dotnet fsi examples/proofs/OrderedDomain.fsx` (**47/47**, §21: Gries ch.15's ordered domains).

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
range, Empty range (8.13) and Change of dummy (8.22), applied to a **∀** quantification. Split off
term now exists for Σ (`split_off_term`, §12) but not for ∀, so mechanizing that derivation is still
its own piece of work. Since (12.5)/(12.8) is the form every proof uses, taking it as the axiom is
the honest trade — and it is a form Gries himself numbers, not an invention here.

`Π` now exists too (§19), with its own Split off term, and `(Πi | 1 ≤ i ≤ n : i) = n!` is proved by
induction. Not here: §12.3 Peano arithmetic, §12.4 well-founded induction, §12.5 structural
induction, §12.6 loop correctness. See §6. (Updated 2026-10-18: this paragraph predated §12 and
listed Split off term itself as missing.)

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
is the second kind, and it needed three things this theory did not have when this section was written
— all now done, and **(12.7) is proved by induction (§15)**:

1. ~~**Split off term (8.23)** over an integer range, for `Σ`~~ — **done, §12.**
2. ~~An **empty-range fact for a numeric range** — `1 ≤ i ≤ 0` is empty, which is arithmetic, not the
   syntactic `false` that `Sums`' (8.13) matches~~ — **done, §14** (`empty_range` / `empty_sum`).
3. ~~More of the **order on ℤ** than reflexivity~~ — **done, §12/§14/§15** (Discreteness,
   successor-not-≤, Transitivity, Translation; `shift_bound`).

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
- `src/lang/core/Sylvia.Expressions/Term.fs` — `IntVar` (§11), and `IntTerm`'s `*` and `-` (it already had `+`).
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
| `leq` / `succ` / `tm` / `pred_of` | building blocks for stating a `P` and its obligations (`tm`/`succ` are only needed for a dummy WITHOUT arithmetic; with an `IntVar` write `n` and `n + 1`) |
| `IntVar name` | an integer variable that is also an integer term — the dummy to use (§11) |
| `PropCalculus.mp` / `PropCalculus.conj` | modus ponens and ∧-introduction, as combinators |

## 10. Gate

```bash
dotnet build src/Sylvia.sln                                        # 0 errors
SYLVIA_SEQUAL_CHECK=1 dotnet test tests/Sylvia.Tests.Prover/...    # 135/135
dotnet fsi examples/proofs/Induction.fsx                           # 86/86 ALL PASS
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

## 11. `IntVar` — the dummy to write proofs over

The API takes its dummies as `#ISymbolicVar<int>`, so ANY integer variable works — an `ElemVar<int>`
included, and the example checks that. But an `ElemVar` has no arithmetic, so every use had to be
wrapped: `two * (tm n)`, `f (tm n) + two`, `succ n`. `IntVar` (`Term.fs`) is the `SetVar` design at
`int`: it inherits `IntTerm`'s STRUCTURAL arithmetic, implements `ISymbolicVar<int>` so it can be a
quantifier dummy, and `IFreshVar<IntVar>` so a fresh witness is still an `IntVar`. The worked example
now reads `pred_of n ((f n) == (2 * n))` and `P.[n + 1]`.

`n + 1` builds exactly the tree `succ n` does, which the example checks, so the two are
interchangeable and the axiom's `P(n+1)` reconstruction sees no difference.

**Why not implicit conversions instead.** Measured on SDK 10.0.401, an `Expr<int> → IntTerm`
`op_Implicit` fires ONLY at a let-bound parameter typed exactly `IntTerm`. It is rejected at a
parameter typed as the base `Term<int>`, at a flexible `#Term<int>`, at a method parameter, and at an
operator — and the API's parameters and the proofs' operators are all of those. That is the same wall
`ISymbolicVar` hit for sets: making the variable itself a term is what works.

**`IntVar`, not `ScalarVar<int>` (`intvar`), in proofs.** `ScalarVar`'s operators send every result
through the CAS (`simplifye`) — right for computation, wrong for a proof, where an axiom is about to
be matched against the exact tree, and the CAS throws outright on a term it does not know.

## 12. Split off term (8.23) for Σ, and what it rests on

```
split_off_term th lo n i E  :  ⊢ lo ≤ n+1 ⇒ (Σi | lo ≤ i ∧ i ≤ n+1 : E) = (Σi | lo ≤ i ∧ i ≤ n : E) + E[i := n+1]
```

A **theorem**, as in Gries — nothing about Σ or ranges is axiomatized for it. His proof (p.152) is
Range split plus the One-point rule plus two facts about integer ranges that he defers to ch.15, and
it is carried out here step for step:

```
Σ(lo≤i≤n+1)  =  Σ(R ∨ Q)                    range_extend, under the proviso      R = lo≤i≤n,  Q = i=n+1
             =  Σ(R ∨ Q) + 0                 unit of +, introduced
             =  Σ(R ∨ Q) + Σ(false)          Empty range (8.13), introduced
             =  Σ(R ∨ Q) + Σ(R ∧ Q)          range_disjoint, introduced
             =  Σ(R) + Σ(i = n+1)            Range split (8.18)
             =  Σ(R) + E[i := n+1]           One-point (8.14)
```

It goes through Σ's UNCONDITIONAL range split (8.18), not the disjoint one (8.16) Gries cites: the
disjointness is used instead to show the second split term is `Σ(false) = 0`. Three of the steps
introduce something (an integer identity used right-to-left); each is a local lemma proved by
rewriting the other side of its own goal, the device `size_disjoint_union` uses (set-theory doc §10).

Stated in the `≤`-both-sides form the induction axiom uses (`n₀ ≤ i ∧ i ≤ n`), not Gries'
`0 ≤ i < n+1`, so a split range is exactly an inductive hypothesis's range. The **proviso
`lo ≤ n+1` is genuine**: at `lo = n+2` the left range is empty and the right is `{n+1}`. Gries hides it
in `n:ℕ`.

### Three new trusted pieces, each the smallest that does the job

| Piece | Where | Statement |
|---|---|---|
| **Substitution (3.84a)** | the logic (`EquationalLogic`) | `(e = f ∧ E[z := e]) = (e = f ∧ E[z := f])`, any type — generalized in §13 |
| **Discreteness** | `Integers`, `int` only | `(x ≤ y+1) = (x ≤ y ∨ x = y+1)` |
| **Successor is not ≤** | `Integers`, `int` only | `(x+1 ≤ x) = false` |

**(3.84a) was missing from the kernel.** Its Leibniz (3.83) matches only `Var = Var`, so there was
no way to put an EXPRESSION for a variable under the conjunct that equates them — and every argument
about an integer range needs exactly that (`i = n+1 ∧ i ≤ n` to `i = n+1 ∧ n+1 ≤ n`). The right-hand
side is reconstructed by `replace_var_expr` and compared, so the only obligation is that the naive
substitution be capture-free: `z` is not re-bound inside `E`, and no variable free in `e` is bound
inside `E`. Both guards are exercised as rejections. The binder check, `rebinds`, now lives in
`Patterns` and is shared with the induction axiom, which had a private copy.

**Discreteness is what makes 8.23 an INTEGER theorem**: over the reals there are values strictly
between `y` and `y+1`, and the identity is false. Both order axioms check `x.Type = typeof<int>`, and
the example checks discreteness is not recognized over `float`.

Gries proves facts like these from the arithmetic axioms of ch.15. When those exist here, both order
axioms should become theorems; until then they are stated, with the same care as the induction
axiom, and they are the only arithmetic Split off term rests on.

### The induction theory now carries Σ

`Induction(…)` composes `Sums.sum_axioms` alongside `Integers.integer_axioms` and (12.8): Split off
term needs Σ's axioms and the integer order in ONE theory, and so does any induction over a sum.

### What (12.7) still needs

`(∀n | 0 ≤ n : (Σi | 1 ≤ i ≤ n : 2i−1) = n²)` now has its key step. Three smaller pieces remain:

1. ~~**The base case's empty range**~~ **— done, §14.** `(1 ≤ i ∧ i ≤ 0) = false`. Arithmetic, not the syntactic
   `false` (8.13) matches; it needs something like transitivity of `≤` plus `(1 ≤ 0) = false`.
2. **The proviso from the inductive hypothesis's range** — the step has `0 ≤ n` and Split off term
   wants `1 ≤ n+1`: translation, `(x ≤ y) = (x+1 ≤ y+1)`, then constant folding.
3. **The algebra** `n·n + (2(n+1) − 1) = (n+1)·(n+1)` with `Integers`' rewrite rules, which only
   rewrite in fixed directions — fiddly rather than hard.

Section (G) of `Induction.fsx` checks everything above: (3.84a) firing and refusing
(wrong substitution, capture of `e`'s variables, substitution under a binder of `z`), both order
axioms firing and refusing (wrong shape, `y+2`, reals), the two range lemmas at a symbolic lower bound,
and Split off term at `lo = 1` with `E = 2i−1` and at a symbolic lower bound and body.

**Gate.** Suite 135/135, `Induction.fsx` 50/50, `SetTheory.fsx` 167/167, AdversarialSweep ALL CLEAR,
Sledgehammer ALL PASS, VerifyRuleFixes passed, Prop/Pred/IntegerAlgebra close. (3.84a) sits in
`equational_logic_axioms`, which every proof step probes, so it was A/B'd (stash + rebuild, min of
3): PropCalculus 1755 → 1704 ms, PredCalculus 2328 → 2219 ms, SetTheory 5024 → 4827 ms; the suite ran
in 34–35 s either side. No regression — most expressions fail its first structural match.

## 13. Substitution (3.84a) in full, and what it unblocked in ch.3

§12 added (3.84a) in the narrow form the integer ranges needed — `(z = e ∧ E) = (z = e ∧ E[z := e])`,
a VARIABLE on the left of the equation. It is now Gries' full form:

```
(e = f ∧ E[z := e])  =  (e = f ∧ E[z := f])       e, f any expressions, of any type
```

No template is given to an axiom recognizer, so it is recovered: the two conjuncts must agree
everywhere except at positions where one holds `e` and the other `f` (`exchanges_some`). Two things
follow from that, both needed:

- **Some occurrences, not all.** `replace_eq` (3.51) at `r := (p ∨ p)` is
  `(p = q) ∧ ((p ∨ p) = p)  =  (p = q) ∧ ((p ∨ p) = q)` — only the bare `p` changes. A
  substitute-everywhere rule produces `(q ∨ q) = q` and fails; that was the shape of most of the
  AdversarialSweep's standing exceptions.
- **Compound `e`.** The old form needed `z` to be a variable, so `replace_eq` with a compound `p`
  could not even be stated as an instance.

The capture obligation is unchanged in kind: no variable of `e` or `f` may be bound anywhere inside
either conjunct.

### `replace_eq` (3.51) holds at any arguments

It is now the axiom instance itself (`ax_ident`). Its old doc comment said the schema "is not valid
to instantiate at arbitrary `Prop`s"; that was wrong as logic — (3.51) holds for any propositions —
and was only a limit of the proof (`subst_and` matches `(Var = Var) ∧ E` and substitutes everywhere).
The kernel test that PINNED the failure now asserts success at five compound instantiations, and a
new test pins three refusals. The AdversarialSweep's one standing exception — 43 failing
instantiations — is gone: it reports a plain **ALL CLEAR**, with the exception list removed, so a
regression would now fail loudly.

### Shannon (3.89), and Replace by true/false

Parked since the ch.3 work "pending metavariable support in the kernel" (the note at the end of
`PropCalculus.fs`, now replaced). It needed (3.84a), not metavariables: the rules are F# functions of
a template `E` and its hole `z`, and compute `E[z := …]` (`subst_prop`), the way every schema in the
module already works.

| Export | Statement |
|---|---|
| `subst_prop z v E` | `E[z := v]` |
| `ident_eq_false p` | `(p = F) = ¬p` — Gries (3.15), rearranged |
| `replace_true_and z p E` | `p ∧ E[z := p] = p ∧ E[z := true]` — Gries (3.87) |
| `replace_false_and z p E` | `¬p ∧ E[z := p] = ¬p ∧ E[z := false]` — the `¬p` counterpart; NOT Gries (3.88), which is `p ∨ …` |
| `shannon z p E` | `E[z := p] = (p ∧ E[z := true]) ∨ (¬p ∧ E[z := false])` — Gries (3.89) |

Over booleans `p ≡ (p = true)` and `¬p ≡ (p = false)`, so each "replace" is two rewrites to put the
equation in front and then IS (3.84a) at `e := p`, `f := true/false` — at exactly the template's
holes, so occurrences of `p` already in `E` are left alone. Shannon is excluded middle, distributivity
(3.46) and the two replace rules. `p` may be compound.

A template whose binder would capture `p`'s variables is REFUSED rather than proved. The kernel test
uses one where capture would prove a falsehood: `E = (∀x |: z ≡ x = 0)`, `p := (x = 0)` — naively
the left side is `(∀x |: (x = 0) ≡ (x = 0)) = true` and the right is false over ℤ.

Gries' (3.85)/(3.86) (the `⇒` forms) and (3.88) (the `∨` form) are now short derivations from these
and were not needed here, so they are not stated.

### Checks

- **Semantic fuzz** (3000 random templates, exchanges and corruptions, checked by truth table): every
  one of the 1978 genuine instances is recognized, and every one of the 2027 accepted statements is a
  tautology. Propositional only — capture is covered by the kernel and `Induction.fsx` tests instead.
- **AdversarialSweep** now covers template schemas too (a `PropVar` hole gets a fixed `z`, and each
  instantiation also runs with `z` conjoined into the template): 3556 instantiations, ALL CLEAR.
- Suite **138/138** (three new tests), `Induction.fsx` 50/50, SetTheory 167/167, Sledgehammer,
  VerifyRuleFixes, Prop/Pred/IntegerAlgebra.
- **Perf**, A/B'd against `HEAD` (stash + rebuild, min of 3): PropCalculus 1736 → 1684 ms, PredCalculus
  2238 → 2157 ms, SetTheory 4890 → 4869 ms. Suite timing was re-done back to back because it moved
  during the session — `HEAD` itself went from 34 s to 42 s with no code change — and back to back the
  difference is the ~1.3 s of the new tests.

## 14. The base case of (12.7)

```
⊢ (Σi | 1 ≤ i ∧ i ≤ 0 : 2i−1) = 0·0          P.0 of (12.7), with P exactly as Gries states it
```

The range `1 ≤ i ≤ 0` is empty by ARITHMETIC, which Σ's Empty range (8.13) cannot see — it matches the
syntactic `false`. Rather than an axiom about empty ranges, two general order facts, so that EVERY
literal empty range is a derivation:

| Axiom | Where | Statement |
|---|---|---|
| **Transitivity of ≤** | `Integers`, `int` | `(x ≤ y ∧ y ≤ z) ⇒ x ≤ z` |
| **Comparison of literals** | `Integers` | `(a ≤ b) = true` / `= false` for integer literals, by evaluation — only the correct value |

The literal comparison is the same trust as `reduce` folding `2·0` to `0`. It is an axiom rather than a
new case of `reduce` so that proofs already running `reduce` do not change shape.

| Export | Statement |
|---|---|
| `empty_range th lo hi i` | `(lo ≤ i ∧ i ≤ hi) = false`, for literals `lo > hi` |
| `empty_sum th lo hi i E` | `(Σi | lo ≤ i ∧ i ≤ hi : E) = 0`, the same |

`empty_range` is propositional around one fact: `A = false` is `¬A` (3.15, `ident_eq_false`) is
`A ⇒ false` (3.74), the `false` is `lo ≤ hi` evaluated, and `(lo ≤ i ∧ i ≤ hi) ⇒ lo ≤ hi` IS
Transitivity. It REFUSES — at the evaluation step, which is the right place — a non-empty range
(`0..1`) and a symbolic bound (`1..n`). The base case is then two steps: `empty_sum` rewrites the Σ to
`0`, and `reduce` folds `0·0`.

Section (H) of `Induction.fsx` checks both axioms firing and refusing (no shared middle term; `⇔` in
place of `⇒`; the wrong truth value; a non-literal), `empty_range` at `1..0` and `5..−3`, its two
refusals, `empty_sum`, and the base case.

Neither axiom is in the logic `S`, so neither is on the per-step hot path, and neither bears on
`Tactics.Instantiate`'s soundness: it substitutes BOOLEAN variables, and neither axiom mentions one.

**Gate.** Suite 138/138, `Induction.fsx` 63/63, SetTheory 167/167, AdversarialSweep ALL CLEAR,
Sledgehammer, VerifyRuleFixes, Prop/Pred/IntegerAlgebra.

**(12.7) now needed only its inductive step** — done, §15. It needed two things:

- **The proviso.** Split off term wants `1 ≤ n+1`; the step's antecedent supplies `0 ≤ n`.
  Transitivity does not bridge them — nothing here relates `0 ≤ n` to a `1` on the left. The clean
  single axiom is translation, `(x ≤ y) = (x + 1 ≤ y + 1)`, after which `reduce` folds `0 + 1`.
- **The algebra** `n·n + (2(n+1) − 1) = (n+1)·(n+1)`, with `Integers`' one-direction rewrite rules
  (§12, item 3).

## 15. The inductive step of (12.7) — and (12.7) proved

```
⊢ (∀n | 0 ≤ n : (Σi | 1 ≤ i ∧ i ≤ n : 2i−1) = n·n)
```

Gries' (12.7), by `by_induction` from the §14 base case and this step. The step body is FIVE proof
steps, and they are Gries' calculation:

```
(0 ≤ n ∧ IH) ⇒ (Σi | 1 ≤ i ≤ n+1 : 2i−1) = (n+1)·(n+1)
  Establish 1 ≤ n+1                              shift_bound, from the antecedent's 0 ≤ n
  Σ(1..n+1)  →  Σ(1..n) + (2(n+1)−1)             Split off term (8.23), under that proviso
  Σ(1..n)    →  n·n                              hypothesis_at — the inductive hypothesis
  n·n + (2(n+1)−1) = (n+1)·(n+1)  →  T            the algebra, a separate theorem
```

`Establish` is what makes the two order facts coexist: Split off term wants `1 ≤ n+1`, `hypothesis_at`
wants `0 ≤ n`, and the step uses them in an order that needs both. `Establish` records the first as
usable without rewriting the antecedent, so the second is still there. The example also checks that the
step does NOT close with the inductive-hypothesis line removed.

The hypothesis dummy is `j`, not `i`: (12.8)'s side condition is that the hypothesis dummy not occur in
`P.n` AT ALL, and `P.n` binds `i` inside its Σ.

### New pieces

| Piece | Where | |
|---|---|---|
| **Translation of ≤** | `Integers` axiom, `int` | `(x ≤ y) = (x+1 ≤ y+1)` |
| `assoc_add_sub` | `Integers` rule | `(a + b) − c → a + (b − c)` — nothing else reassociates across `−` |
| `shift_bound th lo n` | `Induction` | `lo ≤ n ⇒ lo′ ≤ n+1`, `lo′` = `lo + 1` folded |

The algebra `n·n + (2(n+1) − 1) = (n+1)·(n+1)` takes both sides to `n·n + (2n + 1)` in sixteen
addressed rewrites with `Integers`' rules. Its one awkward move is `n + n → 2·n`: there is no rule for
it, so each `n` becomes `n·1` (the Identity axiom used right-to-left, as a local lemma — nothing flips
an integer identity), then `collect_mul_add` and folding. That is a sign of how thin `Integers`'
rewrite toolkit is, not of difficulty; a polynomial normalizer would make this one step — and now
does: the example proves it with `ring_ident` (§16).

### A soundness bug this found: right-associativity of `·` was `a·(b + c)`

```fsharp
| Multiply(Multiply(a1, a2), a3) -> call_mul a1 (call_add a2 a3)     // (a·b)·c → a·(b + c)
```

in BOTH `_right_assoc_mul` and `_right_assoc_add`. Measured before the fix: the kernel proved
`(a·b)·c = a·(b + c)` through either rule, and `(2·3)·4 = 2·(3+4)` — **24 = 14**. Every proof in a theory
carrying these rules (`Integers`, `RealNumbers`) could reach a false statement. Fixed: `_right_assoc_mul`
multiplies, and `_right_assoc_add` — which had no business touching products, unlike its
`_left_assoc_add` sibling — keeps only its `+` case. No proof called either rule directly, so nothing
else changed shape. (`left_assoc_mul`'s description also said "Addition"; corrected.)

The bug survived because an admitted rule is TRUSTED and nothing checked these semantically. So the
example now does: **every `Integers` rewrite rule is fuzzed for value preservation** — random integer
expressions, rewritten, evaluated at random points, before and after. All fourteen pass (600 expressions
each in the example; 4000 each when first run). It is the check that would have caught this, and it is
cheap.

**Gate.** Suite 138/138, `Induction.fsx` 75/75, SetTheory 167/167, AdversarialSweep ALL CLEAR,
Sledgehammer, VerifyRuleFixes, Prop/Pred/IntegerAlgebra. (`examples/math/Statistics.fsx`, the one
other script opening `RealNumbers`, fails on a missing external data file before any proof runs; it does
not use these rules.) None of the new axioms is in the logic `S`, and none mentions a boolean variable, so
neither the hot path nor `Tactics.Instantiate`'s soundness is affected.

## 16. Ring normalization — Gries' "(Arithmetic)" step as a tactic

```fsharp
ring_ident th L R : Theorem      // ⊢ L = R, for an identity of commutative-ring arithmetic over ℤ
ring th L R       : Rule         // the same, as a rewrite L → R inside a larger proof
ring_normal_form th e : Expr     // the canonical polynomial, no proof
```

Gries' ch.15 proofs justify steps with "(Arithmetic)" constantly; here each was hand-addressed rewrites
(the (12.7) algebra took sixteen). `ring_ident` takes both sides to ONE canonical polynomial and
closes on syntactic equality. (12.7)'s algebra is now this one call.

**It produces a proof; it is not trusted.** Every rewrite it makes is an EXISTING rule of `Integers`
applied at an explicit address — or the Identity axiom right-to-left, through a local lemma — and the
kernel re-checks every step of the theorem it returns. The tactic only SEARCHES: it simulates each step
with the very function the kernel will run, to know where the next one goes. So a bug in it makes it
FAIL; it cannot make it prove a falsehood. (This is the "compose existing rules" design rather than a
trusted normalizer — the choice agreed before building it.)

**Canonical form.** A right-nested sum of monomials sorted by their atoms, constants last; each
monomial its atoms sorted and left-nested times a literal coefficient, `((x·y)·z)·c`, with `·1`
written. An ATOM is anything not `+`, `−`, `·`, unary `−` or an integer literal — a variable, `f n`, a
Σ — and the normalizer never looks inside one, so `f(n)·2 + f(n) = 3·f(n)` proves with `f(n)` opaque.

**Phases:** eliminate `−` (binary and unary); distribute `·` over `+` on either side; flatten the sum;
per monomial — flatten, bubble-sort the factors (each adjacent swap is `left_assoc_mul`, `commute_mul`
on the left child, `right_assoc_mul`), fold the literals into one trailing coefficient, add `·1` if
there is none, zero the monomial on a `0` coefficient, left-nest the atoms; bubble-sort the monomials;
combine like terms (`collect_mul_add` then folding the coefficient); drop zeros.

### Three primitive rules — since retired (§17)

`sub_to_add_neg` (`a − b → a + b·(−1)`), `neg_to_mul` (`−a → a·(−1)`), `zero_mul` (`x·0 → 0`) were added
as primitives, value-checked, pending the integral-domain theory. §17 replaced all three with theorems. `IntTerm` also
gained unary minus — which, as with `SetVar`, does not apply to an `IntVar` directly (`~-` is
`^a -> ^a`): write `-(n :> IntTerm)`.

### Two bugs the tests caught before they shipped

Both made the tactic FAIL on valid identities — the failure mode that matters, since soundness is the
kernel's:

- **`reduce` is one bottom-up pass that does not re-fold a parent**: `1·(1·1)` becomes `1·1`, not `1`.
  Folding a monomial's literals now repeats until one is left. (Found on the cube `(x+1)³`.)
- **`0 + 0`** sent zero-removal into a `commute_add` that changes nothing; two literals are now folded.

### Completeness, measured

Soundness is the kernel's; what needed testing is that the tactic does not MISS identities:

- **Equal pairs:** a random expression scrambled by random value-preserving rule applications — the
  tactic proved **300 / 300** equal (100 in the example's gate).
- **Independent random pairs:** the tactic proved exactly those equal by evaluation — **300 / 300**
  agreement (100 in the example).
- Named identities: square of a sum, difference of squares, the cube (121 steps), cancellation to 0,
  unary minus, opaque atoms; and three refusals, each reported with both normal forms.

The whole example runs in ~4 s with its ~230 kernel-checked `ring_ident` proofs.

### What it does not do

No division, no order (`≤` is not arithmetic here), and no reasoning INSIDE atoms — two atoms that are
equal only by some other theorem are different atoms to it. It is commutative-ring arithmetic over ℤ,
which is exactly Gries' "(Arithmetic)" in ch.15's integral-domain sections.

**Gate.** Suite 138/138, `Induction.fsx` 86/86, SetTheory 167/167, AdversarialSweep ALL CLEAR,
Sledgehammer, VerifyRuleFixes, Prop/Pred/IntegerAlgebra.

## 17. Integral domains (Gries ch.15 §15.1) — and the normalizer's primitives retired

```fsharp
type DomainOps<'t> = { Name; Add; Mul; Zero; One; Neg; Sub }       // the carrier's operations
type IntegralDomain<'t>(ops, ?axioms, ?rules)                       // a Theory
type Integers(...) = inherit IntegralDomain<int>(int_ops, integer_axioms, [...])
```

Ch.12 only needed enough of ℤ for induction; ch.15 develops it properly, and the agreed order was:
normalization tactic (§16) → integral domain → ordered domain → well-ordering. This is step two.

**One theory, any carrier.** `IntegralDomain.fs` states Gries' ring axioms ONCE, over `DomainOps<'t>`:
associativity, commutativity, identity (both sides, `+ 0` and `· 1`), distributivity (both sides),
additive inverse (15.13), subtraction (15.14) and cancellation of `·` (15.7, `c ≠ 0` or `¬(c = 0)`
accepted). `Integers` is now `IntegralDomain<int>`; its own axioms shrank to the order facts (and the
legacy ∃ form). Theorems proved over `IntegralDomain<'t>` apply in `Integers` and `Induction` because
those theories derive from it — the kernel's `IsAssignableFrom` check.

**(15.8)–(15.29) are theorems**, each proved from those axioms, mostly following Gries' own proofs —
cancellation (15.8, by mutual implication), zero (15.9, verbatim Gries: cancel `a·a`), unique zero/one
(15.10), zero divisors (15.11, via `autoproof_anf` glue), unique inverse (15.12), the negation laws
(15.15)–(15.23) and the subtraction laws (15.24)–(15.29). `sub_def` exposes (15.14) as a rewrite. With no
symmetry of `=` at non-bool types, the workhorses are `rev` (an axiom right-to-left), `flip` (a derived
identity right-to-left) and `by_cancel` (prove `x = y` by cancelling `c` from `c + x = c + y`).

**The payoff: the ring normalizer has no trusted rules for minus or zero.** §16's three primitives are
gone from `Integers`. The normalizer now takes `a − b → a + (−b)` from (15.14), `−x → (−1)·x` from (15.20)
followed by `reduce` (which now folds `−(1)` to `−1`), and `x·0 → 0` from (15.9). What remains trusted are
the associativity / commutativity / distributivity / identity / constant-folding rules (14 now, all
value-checked).

**Cost — and term schemas.** Those are THEOREMS, so a call at a new argument replays a derivation:
(15.20) goes through (15.8)'s mutual implication, and the first version (a per-argument cache) cost
80–290 ms for any goal containing minus. The fix is the one the SAT reconstruction got — derive each
lemma ONCE at a metavariable and instantiate it — which needed `Tactics.Instantiate` widened from
propositional variables to TERM variables:

```fsharp
Tactics.InstantiateTerms (t: Theorem) (σ: (Expr * Expr) list) : Theorem   // ⊢ S(?a) to ⊢ S(E)
Tactics.Schema.t1 / t2 / t3 name (f: Expr<'a> -> … -> Rule)             // derive once, instantiate per call
```

**Its soundness argument is semantic, so its domain is restricted.** The bool rule argues
syntactically (each axiom instance stays an instance under σ). That fails for terms: `reduce` folds
`2 + 3` but not `?a + 3`, and a recognizer may test literal-ness, so a substituted derivation is not
literally a derivation. What holds is Gries' Substitution rule read semantically: every recognizer and
`Admit` rule is value-sound (the rules are fuzzed for it), so a theorem holds for EVERY value of a
variable nothing else constrains. An ordinary variable may be constrained: a theory can name it in an
axiom (the recursively defined `f` of (D), a hypothesis like `c = 3`), and substituting for it would be
unsound. So `InstantiateTerms` accepts only `?`-metavariables, minted by the `Schema` wrappers AFTER the
theory exists, plus the bool rule's guards (complete parent, no variable twice, types match,
binder-free statement). The bool `Instantiate` had the same hazard in principle — it accepted any
propositional variable — and now carries the same metavariable-only guard; every production caller
already used `Schema.pN` metavariables, so no proof changed.

**A false positive this found in the bool rule too.** The binder guard walked `ExprShape` children, and
F# attaches trait-call WITNESSES — closed lambdas implementing `+`, `·`, unary `−` — to inline operator
calls. Every statement with integer arithmetic looked like it contained a binder. The guard now reads a
`Call`'s real operands only (`has_binder` in `Tactics.fs`, shared by both rules).

Per goal, after the normalizer's two schemas are derived once per theory:

| goal | per-argument cache, first use | schema |
|---|---|---|
| `(x+y)(x−y) = x² − y²` | 177 ms | 6 ms |
| `−(x − y) = y − x` | 290 ms | 2 ms |
| `(x−y)² = x² − 2xy + y²` | 106 ms | 4 ms |

**Logging.** Library sub-proofs in `IntegralDomain.fs`, `Induction.fs`, `ring_ident` and the
`PropCalculus.mp`/`conj` combinators are now `lemma`s rather than `theorem`s. The lemma flag only affects
logging (and a more detailed failure message): a non-lemma proof prints at EVERY log level, so these
printed in full even at level 0. `Induction.fsx` output went from 1.9 MB to 0.8 MB and 11.3 s to 7.9 s.
Note the levels run 0 (quietest) to 3; level 2 ADDS every lemma proof. What still prints at level 0 is
Gries' named `[<Theorem>]`s (`reflex_implies` and the like), which are top-level by design.
**Checks.** `Induction.fsx` (K): all of (15.8)–(15.29), mostly at a compound argument to catch
mis-targeting; (15.9) used inside an `Integers` proof; two refusals; and two theorems at a SECOND carrier
(`IntegralDomain<int64>`) to show the theory is generic. The (J) completeness fuzz now exercises the
theorem-based normalizer (its scrambler keeps a local `a − b → a + b·(−1)`).

**Gate.** Suite 145/145 (6 new `InstantiateTermsTests`, 1 new metavariable-only `Instantiate` test), `Induction.fsx` 112/112, SetTheory ALL PASS, AdversarialSweep ALL CLEAR (3556
instantiations), IntegerAlgebra.

## 18. Integer literals in induction proofs

Bounds, predicate arguments and integer equations take plain `int`s: `by_induction th 0 n i P bse stp`,
`leq 0 n`, `P.[0]`, `f n == 0`, `split_off_term ind 1 n i E`. `let zero = IntTerm 0` is gone from
`Induction.fsx`. Each `int` form builds EXACTLY the tree its `IntTerm` form builds (checked in (A)), so
nothing the kernel matches changes.

**Not implicit conversions** — measured in a mock of the `Term` hierarchy, they fire only at a
parameter typed exactly `IntTerm` and never in operators, indexers or `Term<int>` parameters. Instead:

- `IntTerm.(==)` overloads with `int` on either side.
- `IntPred`, a `Pred<int>` subclass with an `Item(int)` indexer, returned by `intpred` and `pred_of`.
  **Not** an `Item('t)` overload on `Pred<'t>`: that was tried and broke the solution build — with
  two indexers, `P.[x]` whose `'t` is still being inferred is ambiguous (`'t` could be `x`'s own
  type), and code relies on the indexer to fix `'t`. `IntPred` also re-declares unary minus: F#'s `~-`
  is `^a -> ^a`, so the inherited one (returning `Pred<int>`) made `-P` fail to compile. The other
  `Pred` operators still return a plain `Pred<int>`, so `(P * Q).[0]` needs `IntTerm 0`.
- `int_term` (`Term.fs`, module `IntArgs`): an `int` or any `Term<int>` to a `Term<int>`, by static
  overload resolution. Let-bound functions cannot be overloaded, so every `Induction` function taking
  a bound (`leq`, `base_case` … `induction_axiom`, `hypothesis_at`, `subst_body`, the range/Σ lemmas,
  `shift_bound`, `split_off_term`, `by_induction`) is `inline` and converts its bounds first.
  `ring_ident` and other non-inline functions still take `Term<int>`; `IntTerm 2 * IntTerm 3` (a
  product of literal TERMS) is unaffected.

## 19. Π — the product quantifier, and n! by induction

```fsharp
Sums.pi / Sums.qprod x R E : IntTerm            // (Πx | R : E), a Formula.product at ★ := int_mul
Sums.prod_axioms                                 // §8.2's ★ requirements for · + Π's three axioms
split_off_factor th lo n i E : Theorem           // lo ≤ n+1 ⇒ Π(lo..n+1 : E) = Π(lo..n : E) · E[i := n+1]
empty_prod th lo hi i E : Theorem                // (Πi | lo ≤ i ≤ hi : E) = 1, for literals lo > hi
split_off / empty_quant (q: RangeQuantifier) …   // both, for any range quantifier: sigma_q, pi_q
```

**Π is Σ with `·` for `+` and 1 for 0.** It is a `Formula.product` term — the generic multiplicative
quantification ⋂ already uses — so `Quantifier`, `ProductTerm`, `rebuild_quantifier` (hence
`select_range`/`select_body`) and Display all knew it, and the GENERIC quantifier axioms — One-Point
(8.14), Nesting (8.20), Dummy renaming (8.21) — apply to it unchanged. The named operator `int_mul`
keys Π's own axioms, as `int_add` does Σ's, so neither matches the other's terms.

**Π's three axioms** (`Sums.prod_axioms`, combined into `Induction` with `sum_axioms`): Empty range
(8.13) `(Πx | false : E) = 1`; Range split (8.18) in its unconditional form; Distributivity (8.15).
Plus §8.2's requirements of `·` (symmetry, associativity, unit 1). Each is checked in (L) with a
refusal beside it — notably that the Σ-SHAPED split with `+` over Π terms is not accepted (it is false).

**Split off term is one derivation for both.** `split_off_term` was rewritten as `split_off` over a
`RangeQuantifier` — the builder, the operator on terms, and its unit — because the derivation never
used anything else about Σ: `x ★ u` intro, `u = ★(false)`, range disjointness, Range split, One-Point.
That is Gries' own point in stating (8.23) for any ★. `split_off_term` (Σ) and `split_off_factor` (Π)
are its two instances; `empty_sum`/`empty_prod` likewise share `empty_quant`. The Σ results in (G)–(I)
are unchanged.

**The worked example**: `n!` by its inductive definition (§12.2), `0! = 1` and `(n+1)! = (n+1)·n!`, as
two theory axioms in the shape of (D)'s `f`, and then

```
(∀n | 0 ≤ n : (Πi | 1 ≤ i ≤ n : i) = n!)
```

by `by_induction` — the (12.7) proof's shape with Π: the base case is `empty_prod` and `0! = 1`; the
step is `shift_bound` (the proviso), `split_off_factor`, the inductive hypothesis, `(n+1)! = (n+1)·n!`,
and `ring_ident` for `n!·(n+1) = (n+1)·n!`. The same step with the hypothesis removed does not close.

**Two things found on the way.**
- Display had structural cases for `+` and `−` but not `·`, so any product containing a Π was
  decompiled whole and printed as `FormulaModule.product Sums.int_mul "Π" …`. `·` is now structural
  too (`print_arith_operand`, like `+`).
- `Theory.AxEquiv` consults only the theory's OWN axioms; One-Point is an axiom of the logic, so it is
  recognized in a proof (`ax_ident`) but not by `AxEquiv`. Added `Theory.IsAxiom` — this theory's
  axioms OR the logic's, exactly the kernel's completeness test (a zero-step proof closes iff it is
  true); `autoproof` and `PropCalculus`' search now use it, (L) checks with it, and four tests in
  `IsAxiomTests` pin the difference. `AxEquiv` is unchanged (the kernel labels `Axiom of …` vs
  `Logical Axiom of …` from the two separate checks) and its doc comment now states its scope.

**Gate.** Solution builds; suite 149/149; `Induction.fsx` 130/130 ((L) is 17 new); SetTheory 175/175,
PredCalculus 24/24, E 5/5, Sledgehammer 7/7, Reconstruct 29/29, AdversarialSweep ALL CLEAR.
## 20. §12.2 — inductive definitions, and strong induction

`examples/proofs/InductiveDefinitions.fsx` (45/45). Four inductions over three inductively defined
functions, one of them a **strong** induction:

| | Statement | What the step uses |
|---|---|---|
| (B) | `(b·c)ⁿ = bⁿ·cⁿ` | the conditional definition of `bⁿ`, three times, under the step's `0 ≤ n` |
| (C) | `(b−1)·(Σi \| 1 ≤ i ≤ n : bⁱ) = bⁿ⁺¹ − b` | Split off term, distribution, the IH, `le_step` for `0 ≤ n+1` |
| (D) | `(Σi \| 1 ≤ i ≤ n : Fᵢ) = Fₙ₊₂ − 1` | Split off term, the IH, the Fibonacci recurrence |
| (F) | `F(m+n+1) = F(m+1)·F(n+1) + F(m)·F(n)` | the IH at **n and n−1**, by cases on `n = 0` |

**An unconditional recurrence must run backwards, or it is inconsistent.** An inductive definition
is a set of axiom SCHEMES, matched at every integer. (L)'s `n!` was stated as `(e+1)! = (e+1)·e!` with
no condition. At `e := −1` that says `0! = 0·(−1)! = 0`, next to `0! = 1`. Section (A) proves both
theorems in that theory. Nothing else in the gate had used the bad instance. A scheme `F(e+k) = …`
is consistent on all of ℤ when it can be solved for its lowest term: `f.e = f(e+1) − 2` and
`F.e = F(e+2) − F(e+1)` can be. `n!` and `bⁿ` would have to divide by `e+1` and by `b`. Those two
are now stated as Gries states them, under `0 ≤ e`. Their inductive equation is an implication, used
with `Deduce'` under the step's `0 ≤ n`. (L) in Induction.fsx is fixed the same way, and (A) checks
that the bad instance now needs `0 ≤ −1`, which is false. The `Induction` theory's doc comment states
the rule.

**Strong induction needs no new principle.** (12.8)'s hypothesis is already all of `P.n₀ … P.n`
(§4). What a step using `P(n−1)` needs is (a) the range obligation `n₀ ≤ n−1 ∧ n−1 ≤ n`, and (b) a
case split for `n = n₀`, where `n−1` is out of range. These lemmas are all THEOREMS of `Integers`'
order axioms plus the ring normalizer, added to `Induction.fs`:

| Export | Statement |
|---|---|
| `le_succ x` | `x ≤ x + 1` (Discreteness + Reflexivity) |
| `le_step lo n` | `lo ≤ n ⇒ lo ≤ n + 1` (Transitivity) |
| `le_of_succ_le lo n` | `lo′ ≤ n ⇒ lo ≤ n` (`lo′` = `lo + 1`, literals folded by `fold_succ`) |
| `pred_translate lo n` | `(lo ≤ n − 1) = (lo′ ≤ n)` (Translation) |
| `le_pred lo n`, `pred_le n` | `lo′ ≤ n ⇒ lo ≤ n − 1`, `n − 1 ≤ n` |
| `lower_split lo n` | `(lo ≤ n) = (lo′ ≤ n ∨ lo = n)`: the case split |
| `eq_sym a b` | `(a = b) ⇒ (b = a)` at any carrier, from (3.84a) + reflexivity |
| `hypothesis_at_point … k` | `((n₀ ≤ k ∧ k ≤ n) ∧ IH) ⇒ P.k`, the IH anywhere in its range |
| `hypothesis_below …` | `(n₀′ ≤ n ∧ IH) ⇒ P(n − 1)` |
| `step_by_cases … above at_base` | from `(IH ∧ n₀′ ≤ n) ⇒ P(n+1)` and `(IH ∧ n₀ = n) ⇒ P(n+1)`, the step body |
| `generalize_step … body` | the step body to the inductive case: the three lines every example repeated |

`eq_sym` matters beyond induction. With `mp` it **flips a proved integer identity**, which the memory
notes had recorded as impossible ("nothing flips an integer identity", since S's symmetry is
symmetry of ≡). (E) flips `⊢ (n+1)·2 = 2n+2` this way. The mirror lemmas in `split_off` predate it
and still work, so they are left as they are. In (F)'s `n = 0` case, `Deduce' (eq_sym 0 n)` rewrites
each `n` to `0` under the hypothesis `0 = n`. That stands in for the implication form of
Substitution (3.84b), which S still lacks.

The negatives: the `n ≥ 1` case does not close without `P(n−1)`, `hypothesis_below` is refused from
`0 ≤ n` alone, and `step_by_cases` refuses its two cases swapped.

**Not done, and why.** Exponent addition `b^(m+n) = bᵐ·bⁿ` needs `0 ≤ m ∧ 0 ≤ n ⇒ 0 ≤ m + n` to
unfold `b^((m+n)+1)` under the conditional definition. Translation by 1 cannot give that. It is
exactly what `OrderedDomain` (closure of the positives under `+`, §15.2) provides, so it is the
first thing to try once that exists.

**Also.** `reflex_implies` and `strengthen_and` are `lemma`s now, not `theorem`s. Every closing
`Taut (reflex_implies …)` printed a proof at log level 0. `Integers.reduce` folds one level per
pass (`(1 + 0) − 1` takes two).

**Gate.** Solution builds; suite 149/149; `InductiveDefinitions.fsx` 45/45 (new); `Induction.fsx`
131/131; SetTheory 175/175, PredCalculus 24/24, E 5/5, Sledgehammer 7/7, Reconstruct 29/29,
AdversarialSweep ALL CLEAR; PropCalculus, IntegerAlgebra, VerifyRuleFixes clean.

## 21. Ordered domains (Gries ch.15 §15.1, pp. 307–309), and `Integers`' order axioms demoted

`Theories/OrderedDomain.fs`; check: `dotnet fsi examples/proofs/OrderedDomain.fsx` (47/47).

**The theory.** `OrderedDomain<'t>` inherits `IntegralDomain<'t>` and adds an uninterpreted
predicate `pos` (`OrderedDomain.pos`, never evaluated) with Gries' axioms. Like the integral domain
it is GENERIC: the order operators are an `OrderOps<'t>` record (`Pos`, `Lt`, `Gt`, `Le`, `Ge`), and
the example runs the theorems at `int64` as well as `int`.

| Ref | Axiom |
|---|---|
| (15.30) | Addition: `pos.a ∧ pos.b ⇒ pos(a + b)` |
| (15.31) | Multiplication: `pos.a ∧ pos.b ⇒ pos(a·b)` |
| (15.32) | `¬pos.0` (also accepted as `pos.0 = false`) |
| (15.33) | `b ≠ 0 ⇒ (pos.b = ¬pos(−b))` |
| (15.36)–(15.39) | Less `a < b = pos(b − a)`, Greater, At most `a ≤ b = a < b ∨ a = b`, At least |

**The theorems.** (15.34) `b ≠ 0 ⇒ pos(b·b)` follows Gries' proof by cases on `pos.b`. Its
corollaries are `1 ≠ 0 ⇒ pos.1` and `1 ≠ 0 ⇒ (pos(−1) = false)`. Then come (15.40) `pos.b = 0 < b`
and (15.41a) `a < b ∧ b < c ⇒ a < c`, which is Gries' own proof: Less, Addition, and Exercise
15.22's `(b−a) + (c−b) = c − a` (`sub_chain`). (15.41d) `a ≤ b ∧ b ≤ c ⇒ a ≤ c` takes four cases
via (3.78): one is (15.41a) and three are Substitutions. The rest are (15.42) Monotonicity for `<`
and `≤`, (15.46) Reflexivity, `1 ≠ 0 ⇒ ((a + 1 ≤ a) = false)`, `0 ≤ a ∧ 0 ≤ b ⇒ 0 ≤ a + b`, and
`eq_sym`, which moved here from `Induction` (that version now delegates to this one). Each has a
refusal or a compound-argument check beside it.

**The `1 ≠ 0` premise is real.** The one-element ring satisfies every ordered-domain axiom, and in
it `a + 1 ≤ a` is true. So the generic successor theorem carries `1 ≠ 0` as a premise. `Integers`
discharges it with a new evaluation axiom, `(a = b) = false` for distinct int literals (the same
trust as literal `≤`).

**`Integers` is an `OrderedDomain<int>`, and four axioms became theorems:**

| Was an axiom of `Integers` | Now |
|---|---|
| Reflexivity `(x ≤ x) = true` | `Integers.le_refl` ← (15.46) |
| Transitivity `(x ≤ y ∧ y ≤ z) ⇒ x ≤ z` | `Integers.le_trans` ← (15.41d) |
| Translation `(x ≤ y) = (x+1 ≤ y+1)` | `Integers.le_translate` ← (15.42) + Cancellation |
| Successor `(x+1 ≤ x) = false` | `Integers.succ_not_le` ← the above + `one_ne_zero` |
| Discreteness `(x ≤ y+1) = (x ≤ y ∨ x = y+1)` | **still an axiom**: ℚ is an ordered domain; needs (15.49)/(15.50) |
| Literal `≤` evaluation | still an axiom (computation), now joined by literal `=` |

Every proof that used the four now uses the theorems, and they all still close: (12.7), n!, the
§12.2 inductions, and the strong Fibonacci step. That means seven sites in `Induction.fs`, three of
which previously *ended* on "and that is Transitivity". The `Induction.fsx` checks that asserted
these were axioms now assert the opposite and prove the theorem. `Integers.le_*` are derived once
per theory at schema metavariables and instantiated by `InstantiateTerms`, in a `ConditionalWeakTable`
keyed on the theory as the ring normalizer's lemmas are. New `Tactics.Schema.th1/th2/th3` wrap
THEOREM-valued schemas, since `t1`–`t3` wrap identities only.

**The payoff, `0 ≤ m ⇒ b^(m+n) = bᵐ·bⁿ`** (`InductiveDefinitions.fsx` (G)), was blocked in §20. Its
step unfolds `b^((m+n)+1)` and needs `0 ≤ m + n`, which `Integers.nonneg_add` now supplies. The step
detaches the hypothesis `0 ≤ m ⇒ …` with `modus_ponens` under `Establish`, and the check that it
fails without `0 ≤ m + n` passes.

**Performance.** (15.34)'s case analyses are three-atom tautologies. Proving them by the ANF search
cost ~1.5 s each, and every new `Induction(...)` theory paid ~7 s for `succ_not_le` because
`split_off` needs it. They are now `Schema.p3` boolean schemas, proved once per process in the
logic, and re-homed with one `Taut` step. First use per theory: 7.2 s → 0.25 s. `Induction.fsx`
runs in 9.7 s (21.5 s before the fix), `InductiveDefinitions.fsx` in 6.1 s, `OrderedDomain.fsx` in
5.6 s. `weaken_or` and `modus_ponens` are now `lemma`s, for the same log-noise reason as §20.

**Display.** `pos` prints as `OrderedDomain.pos x`. That's cosmetic, and it is left as is.

**Next.** Well-ordering (15.49) and (15.50) "no element between 0 and 1", to demote Discreteness,
the last order axiom. Gries proves (15.50) by contradiction from well-ordering of ℕ. Then §15.2
(min/max) and §15.3 (abs), which are now direct.

**Gate.** Solution builds; suite 149/149; OrderedDomain 47/47 (new); InductiveDefinitions 49/49;
Induction 134/134; SetTheory 175/175; PredCalculus 24/24; E 5/5; Sledgehammer 7/7; Reconstruct 29/29;
AdversarialSweep ALL CLEAR; PropCalculus, IntegerAlgebra, VerifyRuleFixes clean.
