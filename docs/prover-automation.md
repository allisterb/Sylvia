# Automating Sylvia's Equational Logic Prover: `simp`, `decide`, `auto`

*Design note — 2026-07-02. Status: `simp` prototype in progress; `decide`/`auto` proposed.*

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

### 3.2 `decide` — complete propositional decision procedure

Extend `simp`'s canonicalization to a full **Boolean-ring normal form** (ANF):
push everything to `⊕`/`∧`/`true`, distribute, cancel (`x ⊕ x = 0`, `x ∧ x = x`),
sort. Two formulas are E-equivalent iff their ANFs match, so `decide` *solves* any
valid propositional (sub)goal and returns the canonicalization trace. Bounded to
guard the exponential worst case. This is the "auto that actually solves" for the
propositional fragment, and is **complete** there — unlike heuristic `auto`.

Note: a `decide` trace is verifiable but may be long/ugly, *not* a slick Gries
derivation. Perfect for discharging subgoals; not a substitute when a *pedagogical*
proof is the goal.

### 3.3 `auto` — bounded heuristic search

For goals `simp`/`decide` don't reach — chiefly **predicate/quantifier** logic,
which has no cheap canonical form and is outside the propositional oracle. A bounded
best-first rewrite search over the admitted rules **plus the Gries theorem bank** as
an (oriented) rewrite set:

- `simp` as the between-step normalizer,
- `normalize` output as the **visited-state key** (dedup modulo AC),
- a step/depth/time budget (the rules are bidirectional — distrib/collect, assoc
  directions — so unbounded rewriting loops; search must be bounded with visited-set
  pruning),
- a heuristic cost (e.g. term size toward `T`, or distance to a lemma's LHS).

`auto` is explicitly **incomplete** — the Isabelle contract: it handles the routine,
the user finishes the rest.

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

`try_match` is the reusable unification primitive the `auto` search layer will reuse
for rewriting with the oriented Gries theorem bank. What remains for full search is
enumerating *all* positions (not just the first) to branch over — a small extension
of the same traversal.

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

1. **`simp`** (this prototype): `simp_laws` (verified) + the `simp` fixpoint; measure
   against the Gries bank. *← current step.*
2. **`decide`**: Boolean-ring/ANF normal form; complete propositional decision.
3. **Position-enumeration primitive** + declarative subterm targeting / auto-instantiation.
4. **`auto`**: bounded heuristic search using the theorem bank; predicate-logic reach.
