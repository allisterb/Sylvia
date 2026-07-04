# Automating Sylvia's Equational Logic Prover: `simp`, `decide`, `auto`

*Design note — 2026-07-02. Status: `simp`, `auto` (bounded search), `decide` (complete
propositional ANF decision, exposed as a validity-checking tool — NOT a trusted rule), the
position/matching primitives, and automation-as-composable-steps (`autoident`/`autodeduce`/
`Auto`) are all DONE. All three layers of the plan are built.*

## 1. Motivation

Sylvia's prover (`Sylvia.Prover`) is faithful to Gries & Schneider's calculational
logic **E/S**: a proof is an explicit sequence of *addressed* rule applications
(`apply`, `apply_left`, `left_branch |> …`) that rewrite a formula until it reduces
to an axiom (terminally, `SEqual`: the two sides become syntactically identical).

Hands-on formalization of the Gries Ch. 3 theorems surfaced a consistent friction:
a large fraction of steps are **"obvious"** — associativity/commutativity
bookkeeping, constant folding, identity/absorption laws, small lemma applications —
and the effort goes into *addressing* (where to apply a rule) and *counting branch
combinators*, not the logical content. Gries himself performs these steps "without
mention." The [`normalize`](../src/lang/core/Sylvia.Prover/EquationalLogic.fs) rule
(AC-canonicalization) removed one class of this; automation targets the rest.

This matters most for **Giant** (the neurosymbolic system driving the prover with an
LLM). Today Giant must emit an *exact addressed step sequence* — brittle, and the
usual failure mode. An automation tactic converts the contract from *"produce the
whole derivation"* into *"propose a (sub)goal; the tactic discharges the routine
part and returns a checkable trace."* That is a qualitative de-risking of Giant.

## 2. The key insight: our fragment is decidable

Isabelle's `auto`/`simp` are heuristic because HOL is undecidable. **Propositional
equational logic is decidable and has a canonical normal form** — the Boolean ring /
algebraic normal form (Zhegalkin polynomials over `⊕` and `∧`), which is exactly what
E is built on (`≡` is `¬⊕`; `∧`, `true` complete the ring). Two propositions are
E-equivalent **iff their ANFs are identical**.

Consequences:

- For the **propositional** fragment we can *decide*, not merely search. `normalize`
  is already the AC-canonicalization special case of this idea; extending the same
  machinery to a full Boolean-ring normal form yields a **complete** decision
  procedure that emits a verifiable canonicalization trace.
- Every rewrite the automation applies is an already-verified,
  equivalence-preserving rule, so **any trace it finds is valid by construction**.
  For propositional goals we can *additionally* validate with the truth-table oracle
  in [`KernelProofTests`](../tests/Sylvia.Tests.Prover/KernelProofTests.fs).
- The oracle *decides validity* but does **not** yield a calculational path. The
  value of the automation is producing the *checkable proof trace* (the Giant point),
  so the oracle is used for validation/pruning, never as a substitute for the proof.

Caveat: propositional validity is coNP-complete; ANF can blow up exponentially. The
decision layer must be bounded. Textbook-sized goals are unaffected.

## 3. Three layers, lightest first

### 3.1 `simp` — deterministic simplifier (this prototype)

A fixpoint of **size-reducing / canonicalizing** rules applied at every subterm
position until nothing changes:

- `normalize` (AC canonical form),
- `reduce_constants` (fold closed constants),
- a schematic **`simp_laws`** rule: identities (`p ∧ T = p`, `p ∨ F = p`),
  annihilators (`p ∧ F = F`, `p ∨ T = T`), complement (`p ∧ ¬p = F`,
  `p ∨ ¬p = T`), idempotence (`p ∧ p = p`, `p ∨ p = p`), double negation
  (`¬¬p = p`), constant/reflexive equivalence (`T = p ⤳ p`, `F = p ⤳ ¬p`,
  `p = p ⤳ T`).

Properties: **terminating** (every law is size-non-increasing; `normalize` is
idempotent) with a hard iteration cap as a safety net; **sound** (each law is
equivalence-preserving, verified by the oracle); **deterministic** (no search).

`simp` alone closes any (sub)goal that collapses to `T` or to a canonical identity —
the bulk of the "obvious" cases — and is the 80/20 of the whole effort. Usage is
just another proof step: `apply simp`. If the simplified state is `T` (or `x = x`),
the existing completion check closes the proof.

**Position visiting is implicit** for `simp`: a recursive bottom-up traversal
(`traverse`) visits every node, applies the local laws, then re-normalizes. No
explicit position enumeration is required until the search layer (§3.3).

### 3.2 `decide` — complete propositional decision procedure — DONE

Every propositional formula has a unique **algebraic normal form** (ANF / Zhegalkin):
a XOR of AND-monomials over the Boolean ring (`⊕`, `∧`, `true`). Two formulas are
E-equivalent **iff their ANFs are equal**, so ANF *decides* the propositional fragment.

`EquationalLogic.Anf` computes it directly: a monomial is a `Set<string>` of atom keys
(`∧` is idempotent → set union), an ANF is a `Set<monomial>` (`⊕` is self-inverse → a
repeated monomial cancels → symmetric difference). `poly` recurses over the connectives
(`¬a = a⊕1`, `a∨b = a⊕b⊕ab`, `a⇒b = 1⊕a⊕ab`, `a≡b = a⊕b⊕1`, …); any bool subterm that
isn't a known connective (a variable, a comparison like `x<y`, …) is an **opaque atom**
keyed by its text — sound, because a formula valid for all atom assignments is valid
regardless of what the atoms mean. `is_tautology e = (poly e = one)`.

**Deliberately a checking *tool*, NOT a proof rule / not in the trusted base.** Exposed
as `is_tautology`/`equivalent` (on `Expr`) and `PropCalculus.valid`/`equiv` (on `Prop`):
*does a proof of this goal exist?* It is **complete** for the propositional fragment —
it recognizes even the `(p⇒q)∧(q⇒p) = (p≡q)` that `auto`'s bounded search missed — but it
answers yes/no; it never closes a proof.

This is a design choice, and the right one (it matches §2 and §5): a whole decision
algorithm is a large trust unit that produces *no derivation*, so admitting it as a rule
that discharges goals would let any propositional theorem be "proved" by oracle appeal,
bypassing the calculational proof that is the entire point (and Giant's deliverable). As a
*check* it has none of that downside: the user/agent calls it to confirm a proof exists
(so as not to waste effort on a non-theorem, or to know when `auto`/a hand proof *should*
succeed) while the actual proof remains a real derivation. It is cross-checked against the
independent truth-table oracle (they agree on every input). Worst case is exponential
(monomial blowup), fine for textbook goals.

An earlier iteration admitted a whole-algorithm `decide` *rule* (tautology → `T`); it was
pulled from the trusted base for exactly this reason. `valid`/`equiv` remain as the *check*.

### 3.2b `autoproof_anf` — complete trace-emitting decider — DONE

Separately from the check, there is now a **complete, trace-emitting** decider that produces
a real derivation: `PropCalculus.autoproof_anf : Prop -> Proof`. It drives the goal to
Boolean-ring (ANF) normal form using four **admitted, oracle-verified, local rewrites**:

- `elim_to_xor` (`¬/∨/⇒/⇐/≡ → ⊕/∧`), `distrib_and_xor` (`∧` over `⊕`),
- `and_normalize` (flatten a `∧` monomial, `F`-annihilate, drop `T`, **dedup atoms**, sort),
- `xor_normalize` (flatten a `⊕` chain, cancel `x⊕x`, drop `F`, sort).

A **deterministic** driver (`Auto.normalize_trace`) applies these greedily to a fixpoint —
*not* best-first search, because the rules grow the term before it collapses, so a size
heuristic would avoid them. A tautology collapses to `T`; `autoproof_anf` returns the
replayable proof, or throws on a non-theorem. It is **complete and sound**: it closes a goal
iff `valid` says it is a theorem (cross-checked in `KernelProofTests`), with proofs ≤ ~28
steps, and it closes the `(p⇒q)∧(q⇒p) = (p≡q)` that `auto` missed.

This is admitted rules, not a black-box oracle: each is a *local rewrite* producing one
visible step of a normalization — the same character as `distrib`/`golden_rule`/`normalize`/
`simp` — and all four are in the truth-table oracle sweep, so their equivalence-preservation
is guarded on every test run. The decision *emerges from the trace*, so nothing is proved by
oracle appeal.

Two hard-won lessons are baked in here:
- The first attempt was **incomplete** and reverted; the bug was a **confluence** gap
  (missing `∧`-monomial dedup — `simp`/`normalize` sort but don't collapse repeated atoms in
  a nested `∧`), *not* an unsoundness. "Complete by construction" was wrong; only running the
  oracle caught it. Completeness bugs are safe (the decider fails/throws, never fabricates);
  soundness is what the oracle guards.
- The elimination identities are *derivable* in principle, but proving them by search costs
  47–108 s **each** (unusable at load), and explicit hand-proofs are very finicky `⊕`-algebra.
  Admitting the four verified rewrites was the pragmatic, sound choice. Hand-deriving the
  `⊕`-introduction identities later (keeping the two AC-normalizers admitted) remains open.

### 3.3 `auto` — bounded heuristic search — DONE

A bounded **best-first** rewrite search. `Auto.search` (generic, theory-agnostic) at
each state tries every structural *move*, simplifies the result with `simp`, keeps the
smallest terms first, dedups states by textual form, and caps at a `budget`. It only
composes existing `RuleApplication.ApplyRule` transforms, so **any path it finds is a
real, replayable, checkable proof** — the search can't fabricate an invalid step.

`PropCalculus.autoproof` wires it up: moves are `applyfirst` of {`golden_rule`,
`def_implies`, `mutual_implication`, `distrib`, `collect`, `double_neg`}, `simp`
between steps, budget 800. Completion is the ordinary axiom check.

- Heuristic: smallest term first (toward `T` / `x = x`). Because the frontier retains
  larger nodes, proofs needing temporary growth (e.g. `golden_rule` expanding `∧`) are
  still found within budget.
- Bounded + visited-set pruned because the rules are bidirectional (distrib/collect,
  assoc directions) and would otherwise loop.

**Eval: `auto` closes 19/20 of a mixed Gries set vs `simp` 12/20** — everything `simp`
does plus the implication theorems (def-of-`⇒`, contrapositive, weaken, strengthen,
modus ponens) and the distribution goals. The one miss (`(p⇒q)∧(q⇒p) = (p≡q)`) is the
expected incompleteness. `auto` is explicitly **incomplete** — the Isabelle contract:
it handles the routine, the user finishes the rest.

Still open: enumerate *all* positions (not just first-firing) to branch the search;
fold the Gries theorem bank into the moves via `try_match`; predicate/quantifier reach.

### 3.4 Automation as composable proof steps — DONE

The real leverage for **Giant** is not "prove a whole goal" but "let the human/LLM
write the skeleton and have automation discharge the routine obligations." Three
combinators lift an auto-prover `f : Prop -> Proof` into things usable *inside* a
larger proof:

- **`autoident` / `autodeduce`** — `e |> f |> Theorem |> Ident/Deduce` : auto-prove a
  named sub-identity and drop it in as a rewrite/deduction **step** (the Isabelle
  `by auto` model). `Theorem(...)` throws unless the search actually closed, so a rule
  can only be minted from a genuinely proven fact — no new trust.
- **`Auto`** (the `RuleApplication.Auto of (Expr->Rule)` case) — applies an auto-prover
  to the **current proof state** to discharge the remaining obligation. Use
  `PropCalculus.Auto` directly as a step: after a few manual moves, `Auto` finishes.
  Dispatch by shape: an identity `A = B` is folded with `Ident` (rewrite `A→B`, close by
  reflexivity); any other proven statement (an implication, a bare tautology) is
  replaced wholesale by `T` via `Taut`. (Note: `Deduce` is *not* used to discharge a
  whole goal — it substitutes a consequent in a surrounding context, and is guarded to
  right-of-implication positions.)

Soundness is unchanged: `autoproof` replays the found steps through the ordinary Proof
engine, and every discharge goes through the existing verified `Ident`/`Taut` path.

## 4. Enabling infrastructure: position enumeration & matching

The one shared primitive the search layer needs is *"try a rule at a subterm
position,"* i.e. enumerate positions and rebuild the whole term after a local
rewrite. `simp` gets this implicitly from recursive traversal; `auto` needs it
*explicitly* to branch the search. Building it also directly discharges two backlog
items: declarative **subterm targeting** and **auto-instantiation**.

**Shipped (2026-07-02):**
- `FsExpr.apply_first_firing (f:Expr->Expr) expr` — apply a rule at the first
  (leftmost-outermost) position where it fires; surfaced as the `applyfirst`
  combinator. Removes hand-threaded `apply_left |> left_branch` navigation.
- `FsExpr.try_match : Expr -> Expr -> Map<string,Expr> option` — first-order matcher;
  metavariables are Vars named `?…`, multi-occurrence bindings must agree (sequal).
  With `instantiate_schema` and `apply_first_schema`, this powers the `autoapply`
  combinator: a derived rule invoked on metavar args (`meta "a"`) carries its L→R
  schema in its proof, so unifying L against the target infers **both** the
  arguments and the position (no spelling out subterms, no addressing).

`auto` (§3.3) is built on these — its moves are `applyfirst` of the structural rules.
`try_match` is the reusable unification primitive to fold the oriented Gries theorem
bank into `auto`'s moves next. What remains for full search is enumerating *all*
positions (not just the first) to branch over — a small extension of the same traversal.

## 5. Soundness & verification strategy

- Automation composes only **already-verified** admitted rules; found traces are
  valid by construction. New schematic pieces (e.g. `simp_laws`) are added to the
  **truth-table oracle sweep** (each input must fire *and* preserve equivalence),
  exactly as the kernel rules are.
- Keep automation **theory-local** (admitted rules inside S, or tactics over S) — do
  **not** bake E/S operators into the shared kernel primitives (`sequal`). This is
  the same boundary decision recorded for AC-matching: a different logic simply omits
  these rules. (Rejected alternative: an AC-aware/`simp`-aware global `sequal` —
  high blast radius, non-general.)
- Restrict the propositional layers to **bool**; predicate logic is `auto`'s
  (later, harder) territory and needs a finite-domain model checker to validate.

## 6. The Gries corpus as an evaluation harness

The solution-manual theorems/proofs are **both** a rewrite-rule source *and* a
benchmark. Trust and tune the automation empirically, without overclaiming, by
measuring:

- **coverage** — what fraction of the Gries theorems each of `simp`/`decide`/`auto`
  closes from scratch;
- **compression** — how much each shortens the existing hand proofs.

This is the same eval-driven discipline as the kernel truth-table sweep, scaled to a
benchmark, and it dovetails with the "goal + applicable-rules view" backlog item.

## 7. Risks / non-goals

- **Termination**: `simp` uses only oriented size-reducing rules + idempotent
  `normalize`; `auto` must be bounded with visited-set pruning. Never an unbounded
  fixpoint over bidirectional rules.
- **Completeness**: only `decide` (propositional) is complete. `simp` and `auto` are
  best-effort; sell them as "handles the routine, you finish the rest."
- **Decide ≠ nice proof**: canonicalization traces are checkable but not
  pedagogical. Different goal from producing a Gries-style derivation.
- **Scope**: propositional first. Predicate/quantifier automation is later.

## 8. Phased plan

1. **`simp`** — DONE. `simp_laws` (verified) + the `simp` fixpoint; closes 17/19 of the
   Gries simplification bank.
2. **Position-enumeration primitive + auto-instantiation** — DONE. `applyfirst`
   (`apply_first_firing`) and the first-order matcher `try_match` powering `autoapply`.
3. **`auto`** — DONE (prototype). `Auto.search`: bounded best-first over `applyfirst`
   moves + `simp`, returning a replayable step list. Closes 19/20 of a mixed Gries set
   (vs simp 12/20). Incomplete by design (first-firing positions, term-size heuristic).
4. **Automation as composable steps** — DONE (§3.4). `autoident`/`autodeduce` (auto-prove
   a sub-identity, use it as a step) and the `RuleApplication.Auto` case /
   `PropCalculus.Auto` (discharge the current obligation). The discharge dispatches
   `Equals → Ident`, everything else → `Taut` (not `Deduce`).
5. **`decide`** — DONE, in two forms: (a) a **checking tool** (`Anf` + `valid`/`equiv`), out
   of the trusted base, answers "does a proof exist?"; (b) a **complete trace-emitting
   decider** `autoproof_anf` (§3.2b) via four admitted, oracle-verified ANF rewrites +
   `normalize_trace`. Both complete; cross-checked against the truth-table oracle; `autoproof_anf`
   closes `auto`'s one miss with a ≤~28-step real proof.
6. **Later**: hand-derive the `⊕`-introduction identities to shrink the trusted base by two
   (keep the AC-normalizers admitted); enumerate *all* positions to branch `auto`'s search;
   fold the Gries theorem bank into `auto`'s moves via `try_match`; predicate/quantifier.
