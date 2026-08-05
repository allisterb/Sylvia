# Z3 Proof Reconstruction — feasibility measurement and handoff

*2026-08-04. Companion to [`prover-sat-reconstruction.md`](prover-sat-reconstruction.md) (the CaDiCaL
route we already have) and [`prover-e-atp.md`](prover-e-atp.md).*

**Status: measured, not built.** This session added a test suite for the Z3 integration and ran a
feasibility experiment. No reconstruction code exists yet. The question was *should we build it*, and
the answer came out **yes, for non-propositional goals** — but only after two false starts caused by
misconfiguration, which is the main thing to read before continuing.

Re-run the measurement with:

```bash
dotnet fsi examples/smt/Z3ProofShape.fsx
```

---

## 1. Read this first: the options fail silently

Z3 4.12 exposes two different proof vocabularies, and which one you get depends on options that are
**easy to set in a way that does nothing**:

| setting | where it must go | vocabulary |
|---|---|---|
| `proof=true` | `Context` **constructor** | natural deduction: `mp`, `asserted`, `lemma`, `th-lemma`, `quant-inst`, `not-or-elim`, `monotonicity`, `transitivity`, `rewrite` |
| `sat.euf=true` | **`Global.SetParameter`** | clausal log with hints: `rup`, `euf`, `farkas`, `tseitin`, `inst`, `smt` |

Only a short list of parameters is legal in the `Context` constructor (`proof`, `model`, `timeout`,
`auto_config`, `unsat_core`, …). **Anything module-qualified — `sat.euf`, `tactic.default_tactic` —
is a global.** Passing one to the constructor prints `WARNING: unknown parameter` on stdout and
carries on with it unset. The run looks configured. It is not.

I drew and reported a conclusion from a misconfigured run **twice** in one session:

1. First without `proof=true`, concluding theory reasoning was an opaque `smt` blob.
2. Then with `sat.euf` passed to the constructor, where it was silently dropped — so the column
   labelled "sat.euf" was really "proof generation off".

Both conclusions were wrong, and the corrected picture is considerably better than either. **Check for
the warning before believing any output.**

## 2. What Z3's proofs actually look like at our scale

Microsoft.Z3 4.12.2. `ND proof` counts proof RULES in `Solver.Proof` (nodes tagged `Z3_OP_PR_*`), not
the conclusion terms they carry.

| goal | ND proof (`proof=true`) | on-clause (`proof=true`) | clause log (`sat.euf=true`) |
|---|--:|---|---|
| propositional chain 3 | 13 | 6 · mp:4 not-or-elim:2 | 6 · **rup:6** |
| set body De Morgan | 13 | 0 | 13 · **rup:7 tseitin:6** |
| set body **6-var** De Morgan | **13** | 0 | 25 · tseitin:14 rup:11 |
| EUF congruence | **4** | 2 · asserted:2 | 4 · **rup:3 euf:1** |
| EUF + case split | 10 | 6 · asserted:4 smt:1 lemma:1 | 8 · **rup:6 euf:2** |
| linear arithmetic | 20 | 3 · mp:3 | 6 · **rup:4 farkas:1** smt:1 |
| arith case split | 31 | 9 · mp:7 th-lemma:2 | 9 · **rup:7 farkas:1** smt:1 |
| quantified, 2 instances | 24 | 8 · mp:3 quant-inst:3 | 6 · **rup:4 inst:2** |
| pigeonhole 4→3 | 176 | 99 | 42 · **rup:42** |

### What that says

**Scale is a non-issue.** Four to thirty-one inferences for everything except pigeonhole, and *flat*
in problem size — the set-theory De Morgan body is 13 inferences at both 2 and 6 variables; EUF
congruence is 4. Compare Böhme & Weber, who were replaying proofs of millions of inferences. At our
measured ~27 ms per kernel step these are seconds, not hours.

**The `sat.euf` clause log is the artifact we want**, because its hints are the *checkable* kind:

- **`rup`** — reverse unit propagation. **We already replay this**: `SAT.rup_chain` unfolds an LRAT
  RUP step into binary resolutions through `PropCalculus.resolve`, and `SatProof.refute` folds them.
  It is also the *majority* of every goal measured, and 100% of pigeonhole.
- **`euf`** — congruence. Maps onto Leibniz substitution, the kernel's native move.
- **`farkas`** — the linear-arithmetic certificate Böhme & Weber asked Z3's authors for in 2010 and
  did not have. It exists now.
- **`tseitin`** — definitional clauses; each a small propositional tautology, dischargeable by
  `decide`.
- **`inst`** — quantifier instantiation, with the instance term supplied. `PredCalculus.inst` is the
  kernel rule; finding the term is the hard part and Z3 hands it over.
- **`smt`** — the opaque residue. Appears **once** on each of the two arithmetic goals and nowhere
  else.

**Keep CaDiCaL for pure propositional.** Pigeonhole 4→3 is 42 Z3 hints (or 176 ND inferences) against
15 LRAT adds on the route we already have. Z3 earns its place on goals with *terms* in them, not on
the propositional fragment.

**`Solver.Proof` is a different, complementary view.** It includes preprocessing; the on-clause stream
only sees search. Small goals are frequently decided entirely in preprocessing — the De Morgan bodies
produce **zero** on-clause callbacks under `proof=true` while having a 13-inference proof object. If
you only instrument the callback API you will conclude there is nothing to reconstruct when in fact
the whole proof is sitting in `Solver.Proof`.

## 3. Where this leaves the two papers

- [`boehme10fast.pdf`](../reference/papers/boehme10fast.pdf) — Böhme & Weber, *Fast LCF-Style Proof
  Reconstruction for Z3*. Now directly applicable as an implementation recipe rather than an analogy,
  because the `proof=true` vocabulary is the same one their Table 1 maps to reconstruction techniques.
  Their central lever — **schematic theorems instantiated by substitution rather than re-derived** —
  is `Tactics.Instantiate` / `Tactics.Schema`, which we built independently for the same reason. They
  found 230+ schematic theorems covered 76% of `rewrite` obligations; we wrap five. Their warning that
  a rewriting-based AC treatment is "far too slow" applies to our `_chain_simp` / `normalize` clause
  path and is unmeasured here.
- [`2601.14495v1.pdf`](../reference/papers/2601.14495v1.pdf) — Clune, Barbosa & Avigad, *Hint-Based
  SMT Proof Reconstruction*. A **different mechanism** despite the shared word "hints": it harvests
  *derived facts* and discards the proof, guiding the ITP's own automation rather than replaying
  anything. Attractive because certificates stay tiny (8 hints filtered to 3 in their example) and the
  artifact keeps no solver dependency. Its analogue of their `grind` — something to discharge a theory
  lemma — is the piece we lack for arithmetic.

The two compose along the CDCL(T) seam: replay the Boolean skeleton (cheap, we own it), treat theory
lemmas as hint-subgoals (small, independent).

## 4. Recommended next step

**Reconstruct one goal end to end**, smallest first: EUF congruence, whose clause log is `rup:3 euf:1`.
That exercises every layer in miniature and is the honest test of feasibility:

1. Translate Z3 clause literals back to Sylvia `Prop`s. **This is the main missing piece** — `Z3.fs`
   has `create_expr` (Sylvia → Z3) and reads back *models* (ints, rationals, bools), but there is no
   Z3-AST → `Prop` direction.
2. Feed the `rup` hints through the existing `SAT.rup_chain` / `SatProof.refute` machinery. Its clause
   representation is currently DIMACS integers keyed by `CnfProblem.AtomOfVar`; it needs generalizing
   so atoms can be keyed by Z3 terms.
3. Discharge the single `euf` hint by congruence — a Leibniz step.

If that closes, EUF is real and the same skeleton extends to `tseitin` and `inst`. If it does not, we
have learned it cheaply and on the smallest possible instance.

## 5. What was added this session

- **`tests/Sylvia.Tests.Solver.Z3`** — 2 → 26 tests, covering translation (int/real/bool/comparisons/
  sorts), satisfiability, model read-back, string-constraint parsing, solver state and the optimizer.
  Assertions check that models *satisfy* the constraints rather than pinning particular witnesses, so
  the suite survives solver search-order and version changes — validated by the 4.11.2 → 4.12.2 bump,
  which it passed unchanged.
- **`examples/smt/Z3ProofShape.fsx`** — the measurement harness that produced §2.

### Two findings from writing the tests

- **`check_sat` constraints are per-call assumptions, not accumulated assertions.** `NumAssertions`
  stays 0 across calls, so two contradictory checks in a row both succeed. The module also exposes
  `push`/`pop`/`reset`, which implies the opposite. Now pinned explicitly.
- **`parse_bool_expr` silently accepts trailing garbage** — `"x +"` → `Ok x`, `"x > 1 garbage"` →
  `Ok (x > 1)`. `TermParsers.parseProp` anchors with `.>> eof`; `parseBoolExpr`, `parseIntExpr` and
  `parseRealExpr` do not, so they parse a prefix and discard the rest. Silent wrong answers, on the
  path the Giant SMT plugin uses for LLM-authored constraint strings. Pinned as `KNOWN DEFECT` rather
  than fixed: the parser is shared and adding the anchor turns currently-accepted input into errors
  for every caller. One line per function when you want it.

## 6. Loose ends

- **`examples/proofs/Include.fsx` still references `Microsoft.Z3, 4.11.2`** while the project is on
  4.12.2. Harmless today (no proof script uses Z3) but inconsistent. Changing it means re-running the
  full gate, so it was left alone.
- **Proof sizes are version-sensitive.** The 4.11.2 → 4.12.2 bump took the propositional chain from 34
  ND inferences to 13, and EUF congruence from 9 to 4. This is a concrete instance of the instability
  the hint-based paper cites as its motivation for not depending on solver proofs at all. Re-run the
  harness after any bump.
- Böhme & Weber's O(n log n) treatment of polyadic conjunctions/disjunctions versus our AC-rewriting
  clause path is unmeasured.
