namespace Sylvia
// A bare rule as a proof step implicitly means "apply to the whole expression" (Rule -> RuleApplication).
#nowarn "3391"

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

open FsExpr
open Formula
open Patterns
open Descriptions

open PropCalculus
open PredCalculus
open Sums

/// Mathematical induction over the integers — Gries & Schneider ch.12, §12.1.
///
/// ONE axiom, (12.8), induction over `{n₀, n₀+1, …}`:
///
/// ```
/// P.n₀ ∧ (∀n | n₀ ≤ n : (∀i | n₀ ≤ i ∧ i ≤ n : P.i) ⇒ P(n+1))  ⇒  (∀n | n₀ ≤ n : P.n)
/// ```
///
/// Gries' (12.5), induction over ℕ, is its `n₀ := 0` instance, so it costs nothing extra here.
/// (12.3) — the same principle with hypothesis `(∀i | 0 ≤ i < n : P.i)` and no separate base case —
/// is NOT stated: Gries derives (12.5) from it using Split off term (8.23) over an integer range for
/// a ∀ quantification — this prover has Split off term for Σ and Π, not yet for ∀ — and (12.5) is the
/// form proofs actually use. Strong induction needs no other principle (see `step_by_cases`).
///
/// The PRINCIPLE needs no arithmetic — `n₀ ≤ n` is matched structurally and never reasoned about.
/// The base and inductive cases are where arithmetic lives, and those are the caller's proofs, which
/// is why this theory layers the axiom over `Integers` instead of standing alone.
module Induction =

    let private desc = Some << axiom_name "Induction"

    (* Building blocks *)

    /// `l ≤ r` over the integers, as a proposition. Each side is an `int` or any `Term<int>`
    /// (`int_term`), so `leq 0 n` and `leq n (n + 1)` both work. The same holds for every bound
    /// (`n0`, `lo`, `hi`, `n`) taken by the functions below, which are `inline` for that reason.
    let inline leq l r : Prop =
        let l, r = int_term l, int_term r
        <@ %l.Expr <= %r.Expr @> |> expand_as<bool> |> Prop

    /// A dummy as an integer term.
    let tm (x: #ISymbolicVar<int>) : IntTerm = IntTerm(x.Expr)

    /// `x + 1`, structurally (`IntTerm`, not `Scalar` — see the note there on the CAS).
    let succ (x: #ISymbolicVar<int>) : IntTerm = tm x + IntTerm 1

    /// Abstract a dummy back out of a proposition written in it, giving the `Pred` the
    /// predicate-calculus theorems take. Binds the `Var` OBJECT occurring in the body rather than a
    /// structurally equal copy, so `Q.[x]` beta-reduces back to exactly the proposition given.
    let pred_of (x: #ISymbolicVar<int>) (p: Prop) : IntPred =
        let body = expand p.Expr
        let xv = x.Var
        let v = match get_vars body |> List.tryFind (fun w -> vequal w xv) with Some w -> w | None -> xv
        IntPred(func = Expr.Cast<int -> bool>(expand (Expr.Lambda(v, body))))

    /// The base case `P.n₀`.
    let inline base_case n0 (P: Pred<int>) : Prop = P.[int_term n0]

    /// The conclusion `(∀n | n₀ ≤ n : P.n)`.
    let inline conclusion n0 (n: #ISymbolicVar<int>) (P: Pred<int>) : Prop =
        qall n (leq n0 (tm n)) P.[tm n]

    /// The INDUCTIVE HYPOTHESIS at `n`: `(∀i | n₀ ≤ i ∧ i ≤ n : P.i)` — all of `P.n₀ … P.n`, not
    /// just `P.n`. Gries folds weak and strong induction into this one form deliberately (§12.1): a
    /// proof that happens to use only `P.n` is a weak induction and needs no different principle.
    let inline hypothesis n0 (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) : Prop =
        qall i ((leq n0 (tm i)) * (leq (tm i) (tm n))) P.[tm i]

    /// The inductive case `(∀n | n₀ ≤ n : (∀i | n₀ ≤ i ∧ i ≤ n : P.i) ⇒ P(n+1))`.
    let inline inductive_case n0 (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) : Prop =
        qall n (leq n0 (tm n)) ((hypothesis n0 n i P) ==> P.[succ n])

    /// The (12.8) instance for `P` — `base_case ∧ inductive_case ⇒ conclusion`.
    let inline induction_axiom n0 (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) : Prop =
        ((base_case n0 P) * (inductive_case n0 n i P)) ==> (conclusion n0 n P)

    (* Axiom *)

    // `rebinds` (Patterns): the reconstruction of `P.n₀` / `P.i` / `P(n+1)` from `P.n` below is a
    // naive `replace_var_expr`, sound only when `n` is not re-bound inside `P.n`.

    /// True iff `v` occurs in `e` AT ALL — free or bound. Stronger than `occurs_free`, and it is
    /// what makes substituting `v` INTO `e` capture-free.
    let private occurs_at_all (v: Var) (e: Expr) = get_vars e |> List.exists (vequal v)

    /// **(12.8) Mathematical Induction over {n₀, n₀+1, …}**
    ///
    /// ```
    /// P.n₀ ∧ (∀n | n₀ ≤ n : (∀i | n₀ ≤ i ∧ i ≤ n : P.i) ⇒ P(n+1))  ⇒  (∀n | n₀ ≤ n : P.n)
    /// ```
    ///
    /// `P` is not a syntactic argument — an axiom recognizer sees only the statement — so the four
    /// applications of `P` are tied together by RECONSTRUCTING three of them from the conclusion's
    /// body `P.n`: the base case must be `P.n` with `n := n₀`, the hypothesis body must be it with
    /// `n := i`, and the step's consequent must be it with `n := n+1`. That is the device the
    /// One-Point rule (8.14) uses, and it is what pins all four occurrences to ONE predicate.
    ///
    /// Side conditions, all checked and all exercised as rejections in
    /// `examples/proofs/Induction.fsx`: the two outer dummies are the same variable and differ from
    /// `i`; the three occurrences of `n₀` agree and mention neither dummy (or a range would bind it);
    /// `i` does not occur in `P.n` at all (or substituting it would capture); and `n` is not re-bound
    /// inside `P.n` (or the substitution would reach under a binder).
    let (|NatInduction|_|) =
        function
        | Implies(And(bse,
                      ForAll(_, [n1], LessEq(n0a, Var n1a),
                             Implies(ForAll(_, [i1], And(LessEq(n0b, Var i1a), LessEq(Var i1b, Var n1b)), ih),
                                     stp))),
                  ForAll(_, [n2], LessEq(n0c, Var n2a), body))
            when vequal n1 n1a && vequal n1 n1b && vequal n1 n2 && vequal n2 n2a
                 && vequal i1 i1a && vequal i1 i1b && not (vequal i1 n1)
                 && sequal n0a n0b && sequal n0b n0c
                 && not (occurs_at_all n1 n0a) && not (occurs_at_all i1 n0a)
                 && not (occurs_at_all i1 body) && not (rebinds n2 body)
                 && sequal bse (replace_var_expr n2 n0c body)
                 && sequal ih  (replace_var_expr n2 (Expr.Var i1) body)
                 && sequal stp (replace_var_expr n2 (call_add (Expr.Var n1) (Expr.Value 1)) body)
            -> desc "Mathematical Induction"
        | _ -> None

    let induction_axioms =
        function
        | NatInduction x -> Some x
        | _ -> None

    (* Theory *)

    /// Integer arithmetic — an integral domain at `int` (`Integers` inherits `IntegralDomain<int>`) —
    /// Σ (`Sums`), and (12.8). It INHERITS `Integers`, so every integral-domain theorem proved in an
    /// `IntegralDomain<int>` is usable here. A caller with its own axioms — the two equations
    /// of an inductively defined function, say (§12.2) — passes them in, so that everything a proof
    /// by induction touches lives in ONE theory: the base case, the inductive case, and the axiom
    /// instance the two are detached from must all be proved in the same one.
    ///
    /// The caller's axioms must be CONSISTENT, and an unconditional recurrence scheme is consistent
    /// only if it can be run backwards to every integer: `f(e+1) = f.e + 2` and Fibonacci can, but
    /// `(e+1)! = (e+1)·e!` at `e := −1` gives `0! = 0`. State such an equation under `0 ≤ e`, as Gries
    /// does (n ≥ 0), and use it with `Deduce'` under the step's `0 ≤ n` — see
    /// `examples/proofs/InductiveDefinitions.fsx`. Σ is here because the
    /// classic inductions are about sums, and Split off term (below) needs Σ's axioms and the
    /// integer order in the same theory.
    type Induction(?axioms: Axioms, ?rules: Rules) =
        inherit Integers.Integers(BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None))
                                    (BooleanAlgebra.combine_axioms induction_axioms (BooleanAlgebra.combine_axioms Sums.sum_axioms Sums.prod_axioms)),
                                  defaultArg rules [])

    /// The theory: integer arithmetic, Σ, and (12.8), with nothing extra.
    let induction = Induction()

    (* Using the inductive hypothesis *)

    /// `(n₀ ≤ n ∧ (∀i | n₀ ≤ i ∧ i ≤ n : P.i)) ⇒ P.n` — the inductive hypothesis AT ITS OWN UPPER
    /// BOUND, which is the conjunct of it a weak induction actually uses.
    ///
    /// Every proof of an inductive case needs this and nothing else about `≤`, so the order
    /// reasoning is isolated here: Trading (9.2) turns the hypothesis into a true-range ∀,
    /// Instantiation (9.13) applies it at `i := n`, and the range obligation that leaves,
    /// `n₀ ≤ n ∧ n ≤ n`, is discharged by the ANTECEDENT together with reflexivity of `≤`
    /// (`Integers`). `n₀ ≤ n` can come from nowhere else, which is why it is a premise here rather
    /// than something the caller can skip.
    let inline hypothesis_at (th: Theory) n0 (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) : Theorem =
        let range = pred_of i ((leq n0 (tm i)) * (leq (tm i) (tm n)))
        let q = pred_of i ((range.[tm i]) ==> P.[tm i])          // the traded hypothesis body
        let a = leq n0 (tm n)
        let f = forall'(tm i, q)
        let pn = P.[tm n]
        lemma th ((a * (hypothesis n0 n i P)) ==> pn) [
            trade_forall_implies i range P      |> at [left_branch; right_branch]   // IH → (∀i |: R ⇒ P)
            commute_and a f                     |> at_left                          // (A ∧ F) → (F ∧ A)
            shunt' f a pn                                                           // (F ∧ A) ⇒ P → F ⇒ (A ⇒ P)
            ident_and a |> Commute              |> at [right_branch; left_branch]   // A → A ∧ T
            Integers.le_refl th (tm n) |> Commute
                                                |> at [right_branch; left_branch; right_branch]  // T → n ≤ n  (15.46)
            // and the state IS Instantiation (9.13) at i := n, which closes it.
        ]

    (* ------------------------------------------------------------------------------------------
       Split off term (Gries 8.23), for Σ over an integer range.

       Gries proves 8.23 for any ★ from Range split, the One-point rule, and two facts about integer
       ranges that he defers to ch.15. It is a THEOREM here too: the integer facts are Discreteness
       and Successor-is-not-≤ (`Integers`), the substitution is (3.84a) (the logic), and the Σ steps
       are `Sums`' unconditional Range split (8.18), its Empty range (8.13), and the generic One-Point
       rule (8.14). Nothing about Σ or ranges is axiomatized for the purpose.

       Stated in the `≤`-on-both-sides form the induction axiom uses (`n₀ ≤ i ∧ i ≤ n`) rather than
       Gries' `0 ≤ i < n+1`, so a split range is exactly an inductive hypothesis's range.
       ------------------------------------------------------------------------------------------ *)

    /// `E[i := v]`, for an integer body written in the dummy `i`.
    let inline subst_body (i: #ISymbolicVar<int>) v E : IntTerm =
        let v, E = int_term v, int_term E
        IntTerm(replace_var_expr i.Var (expand v.Expr) (expand E.Expr) |> expand_as<int>)

    /// `lo ≤ n+1 ⇒ ((lo ≤ i ∧ i ≤ n+1) = ((lo ≤ i ∧ i ≤ n) ∨ i = n+1))` — extending a range by one.
    ///
    /// Discreteness splits `i ≤ n+1`; ∧ distributes over the split; Substitution (3.84a) turns
    /// `lo ≤ i ∧ i = n+1` into `lo ≤ n+1 ∧ i = n+1`; and the antecedent discharges `lo ≤ n+1`. The
    /// proviso is genuine: with `lo = n+2` the left range is empty and the right is `{n+1}`.
    let inline range_extend (th: Theory) lo n (i: #ISymbolicVar<int>) : Theorem =
        let lo, n = int_term lo, int_term n
        let n1 = IntTerm(n.Expr) + 1
        let a = leq lo n1
        let loi, ile, ieq = leq lo (tm i), leq (tm i) n, (tm i :> Term<int>) == (n1 :> Term<int>)
        lemma th (a ==> ((loi * (leq (tm i) n1)) == ((loi * ile) + ieq))) [
            ax_ident th ((leq (tm i) n1) == (ile + ieq)) |> at [right_branch; left_branch; right_branch]  // Discreteness
            distrib_and_or loi ile ieq                   |> at [right_branch; left_branch]
            commute_and loi ieq                          |> at [right_branch; left_branch; right_branch]
            ax_ident th ((ieq * loi) == (ieq * a))       |> at [right_branch; left_branch; right_branch]  // (3.84a)
            Deduce (reflex_implies a)                    |> apply_right                                   // lo ≤ n+1 → T
            ident_and ieq                                |> at [right_branch; left_branch; right_branch]
            def_true ((loi * ile) + ieq) |> Commute      |> apply_right
        ]

    /// `((lo ≤ i ∧ i ≤ n) ∧ i = n+1) = false` — the two parts of an extended range are DISJOINT.
    /// Substitution (3.84a) puts `n+1` for `i`, and `n+1 ≤ n` is false.
    let inline range_disjoint (th: Theory) lo n (i: #ISymbolicVar<int>) : Theorem =
        let lo, n = int_term lo, int_term n
        let n1 = IntTerm(n.Expr) + 1
        let loi, ile, ieq = leq lo (tm i), leq (tm i) n, (tm i :> Term<int>) == (n1 :> Term<int>)
        lemma th (((loi * ile) * ieq) == F) [
            commute_and (loi * ile) ieq                                  |> at_left
            ax_ident th ((ieq * (loi * ile)) == (ieq * ((leq lo n1) * (leq n1 n)))) |> at_left  // (3.84a)
            Integers.succ_not_le th n                                    |> at [left_branch; right_branch; right_branch]
            zero_and (leq lo n1)                                         |> at [left_branch; right_branch]
            zero_and ieq                                                 |> at_left
        ]

    /// `(lo ≤ i ∧ i ≤ hi) = false`, for integer LITERALS `lo > hi` — a numeric empty range.
    ///
    /// What a base case needs: `(Σi | 1 ≤ i ∧ i ≤ 0 : E)` is empty by ARITHMETIC, and `Sums`' Empty
    /// range (8.13) matches only the syntactic `false`. The derivation is propositional around one
    /// fact: `A = false` is `¬A` (3.15) is `A ⇒ false` (3.74), the `false` is `lo ≤ hi` evaluated, and
    /// `(lo ≤ i ∧ i ≤ hi) ⇒ lo ≤ hi` IS Transitivity. Refuses (the evaluation step does not close)
    /// when `lo`, `hi` are not literals with `lo > hi`.
    let inline empty_range (th: Theory) lo hi (i: #ISymbolicVar<int>) : Theorem =
        let lo, hi = int_term lo, int_term hi
        let A = (leq lo (tm i)) * (leq (tm i) hi)
        lemma th (A == F) [
            ident_eq_false A                                                    // (A = F) → ¬A
            ident_implies_false_not A |> Commute |> apply                       // ¬A → (A ⇒ F)
            ax_ident th ((leq lo hi) == F) |> Commute |> at_right               // F → (lo ≤ hi)
            Taut (Integers.le_trans th lo (tm i) hi) |> apply                   // Transitivity (15.41d)
        ]

    /// A range quantification over the integers that Split off term and Empty range are stated for:
    /// Σ or Π, as its builder, its operator on terms, and the operator's unit. Gries states (8.23)
    /// for any ★ with a unit, and the derivation below uses nothing else about ★.
    type RangeQuantifier =
        { Name: string
          Build: ISymbolicVar<int> -> Prop -> Term<int> -> IntTerm
          Op: IntTerm -> IntTerm -> IntTerm
          Unit: int }

    let sigma_q = { Name = "Σ"; Build = (fun i R E -> qsum i R E);  Op = (fun a b -> a + b); Unit = 0 }
    let pi_q    = { Name = "Π"; Build = (fun i R E -> qprod i R E); Op = (fun a b -> a * b); Unit = 1 }

    /// `(★i | lo ≤ i ∧ i ≤ hi : E) = u`, for integer literals `lo > hi` and ★'s unit `u` — the numeric
    /// range is empty (`empty_range`), and ★ over the empty range is its unit (8.13).
    let empty_quant (q: RangeQuantifier) (th: Theory) (lo: Term<int>) (hi: Term<int>) (i: ISymbolicVar<int>) (E: Term<int>) : Theorem =
        lemma th ((q.Build i ((leq lo (tm i)) * (leq (tm i) hi)) E) == IntTerm q.Unit) [
            Ident (empty_range th lo hi i) |> at [left_branch; select_range]
            // and (★i | false : E) = u is Empty range (8.13).
        ]

    /// `(Σi | lo ≤ i ∧ i ≤ hi : E) = 0`, for integer literals `lo > hi`.
    let inline empty_sum (th: Theory) lo hi (i: #ISymbolicVar<int>) E : Theorem =
        empty_quant sigma_q th (int_term lo) (int_term hi) i (int_term E)

    /// `(Πi | lo ≤ i ∧ i ≤ hi : E) = 1`, for integer literals `lo > hi`.
    let inline empty_prod (th: Theory) lo hi (i: #ISymbolicVar<int>) E : Theorem =
        empty_quant pi_q th (int_term lo) (int_term hi) i (int_term E)
    /// `lo ≤ n ⇒ lo′ ≤ n + 1`, where `lo′` is `lo + 1` with literals folded (`0 ≤ n ⇒ 1 ≤ n + 1`).
    ///
    /// The bridge from an inductive case's antecedent to Split off term's proviso: the step has
    /// `n₀ ≤ n`, and splitting the last term off `(Σi | n₀+1 ≤ i ≤ n+1 : E)` wants `n₀+1 ≤ n+1`.
    /// Translation of ≤ (`Integers`) then constant folding. Meant for `Establish`, which records the
    /// consequent as usable while leaving `n₀ ≤ n` available for `hypothesis_at`.
    let inline shift_bound (th: Theory) lo n : Theorem =
        let lo, n = int_term lo, int_term n
        let lo1 = IntTerm(Integers._reduce_constants (call_add lo.Expr (Expr.Value 1)) |> expand_as<int>)
        let n1 = IntTerm(n.Expr) + 1
        lemma th ((leq lo n) ==> (leq lo1 n1)) [
            Integers.le_translate th lo n |> at_left                                  // Monotonicity (15.42)
            Integers.reduce |> at [left_branch; left_branch]                          // lo + 1 folded
            Taut (reflex_implies (leq lo1 n1)) |> apply
        ]

    /// **Split off term (Gries 8.23)**, for any range quantifier ★ with unit `u` (Σ, Π):
    ///
    /// ```
    /// lo ≤ n+1  ⇒  (★i | lo ≤ i ∧ i ≤ n+1 : E) = (★i | lo ≤ i ∧ i ≤ n : E) ★ E[i := n+1]
    /// ```
    ///
    /// The derivation is Gries' own, carried through ★'s UNCONDITIONAL range split (8.18) rather than
    /// the disjoint one (8.16): the disjointness `range_disjoint` proves is used to turn the second
    /// split term into `(★i | false : E) = u`, which is the unit of ★.
    ///
    /// ```
    /// ★(lo≤i≤n+1)  =  ★(R ∨ Q)                    range_extend, under the proviso
    ///              =  ★(R ∨ Q) ★ ★(R ∧ Q)         ★ u, and u = ★(false) = ★(R ∧ Q) by range_disjoint
    ///              =  ★(R) ★ ★(i = n+1)           Range split (8.18)
    ///              =  ★(R) ★ E[i := n+1]          One-point (8.14)
    /// ```
    ///
    /// Three of those steps rewrite an INTEGER identity right-to-left, which nothing in the logic can
    /// do directly (its symmetry is symmetry of ≡); each is a local lemma proved by rewriting the
    /// other side of its own goal — see the note on orientation in `docs/prover-set-theory.md` §10.
    let split_off (q: RangeQuantifier) (th: Theory) (lo: Term<int>) (n: Term<int>) (i: ISymbolicVar<int>) (E: Term<int>) : Theorem =
        let n1 = IntTerm(n.Expr) + 1
        let a = leq lo n1
        let R = (leq lo (tm i)) * (leq (tm i) n)
        let Q = (tm i :> Term<int>) == (n1 :> Term<int>)
        let lhs = q.Build i ((leq lo (tm i)) * (leq (tm i) n1)) E
        let rhs = q.Op (q.Build i R E) (subst_body i n1 E)
        let sRQ, sF = q.Build i (R + Q) E, q.Build i F E
        let u = IntTerm q.Unit
        let unit_intro = ident th (sRQ == q.Op sRQ u) [ ax_ident th (q.Op sRQ u == sRQ) |> at_right ]   // x → x ★ u
        let unit_quant = ident th (u == sF) [ ax_ident th (sF == u) |> at_right ]                       // u → ★(false)
        lemma th (a ==> (lhs == rhs)) [
            Deduce' (range_extend th lo n i)                 |> apply_right
            unit_intro                                       |> at [right_branch; left_branch]
            unit_quant                                       |> at [right_branch; left_branch; right_branch]
            Ident (range_disjoint th lo n i) |> Commute      |> at [right_branch; left_branch; right_branch; select_range]
            ax_ident th ((q.Op sRQ (q.Build i (R * Q) E)) == (q.Op (q.Build i R E) (q.Build i Q E))) |> at [right_branch; left_branch]  // (8.18)
            ax_ident th ((q.Build i Q E) == (subst_body i n1 E)) |> at [right_branch; left_branch; right_branch]                        // (8.14)
            ax_ident th (T == (rhs == rhs)) |> Commute       |> apply_right
        ]

    /// **Split off term (8.23) for Σ**: `lo ≤ n+1 ⇒ (Σi | lo ≤ i ≤ n+1 : E) = (Σi | lo ≤ i ≤ n : E) + E[i := n+1]`.
    let inline split_off_term (th: Theory) lo n (i: #ISymbolicVar<int>) E : Theorem =
        split_off sigma_q th (int_term lo) (int_term n) i (int_term E)

    /// **Split off term (8.23) for Π**: `lo ≤ n+1 ⇒ (Πi | lo ≤ i ≤ n+1 : E) = (Πi | lo ≤ i ≤ n : E) · E[i := n+1]`.
    let inline split_off_factor (th: Theory) lo n (i: #ISymbolicVar<int>) E : Theorem =
        split_off pi_q th (int_term lo) (int_term n) i (int_term E)
    (* The principle, as a combinator *)

    /// **Proof by induction over `{n₀, n₀+1, …}`** — Gries (12.8).
    ///
    /// From a proof of the base case `P.n₀` and a proof of the inductive case
    /// `(∀n | n₀ ≤ n : (∀i | n₀ ≤ i ≤ n : P.i) ⇒ P(n+1))`, conclude `(∀n | n₀ ≤ n : P.n)`.
    ///
    /// NOT a new trusted step: it instantiates the axiom above at `P`, conjoins the two supplied
    /// theorems and detaches by modus ponens (`PropCalculus.conj` / `PropCalculus.mp`). The axiom is
    /// what carries the induction; this does the plumbing, and refuses unless each theorem proves
    /// exactly the obligation it is supposed to. `th` must recognize (12.8) — i.e. be an `Induction`.
    let inline by_induction (th: Theory) n0 (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>)
                     (bse: Theorem) (stp: Theorem) : Theorem =
        let check (what: string) (t: Theorem) (obligation: Prop) =
            if not t.Proof.Complete then failwithf "by_induction: the %s proof is not complete." what
            if not (sequal (expand t.Stmt) (expand obligation.Expr)) then
                failwithf "by_induction: the %s proof is of\n  %s\nbut the obligation is\n  %s"
                    what (th.PrintFormula (expand t.Stmt)) (th.PrintFormula (expand obligation.Expr))
        check "base case" bse (base_case n0 P)
        check "inductive case" stp (inductive_case n0 n i P)
        mp (axiom th (induction_axiom n0 n i P)) (conj bse stp)

    /// **Proof by induction over ℕ** — Gries (12.5), the `n₀ := 0` instance of (12.8).
    let by_induction_nat (th: Theory) (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>)
                         (bse: Theorem) (stp: Theorem) : Theorem =
        by_induction th 0 n i P bse stp

    /// The inductive case from its BODY: from `⊢ n₀ ≤ n ⇒ (IH ⇒ P(n+1))`, for an arbitrary `n`,
    /// conclude `⊢ (∀n | n₀ ≤ n : IH ⇒ P(n+1))`. Trading (9.2) puts the range in the body, the body is
    /// replaced by its own theorem, and `(∀n |: true) = true` closes it — the three steps every
    /// inductive case ends with.
    let inline generalize_step (th: Theory) n0 (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) (body: Theorem) : Theorem =
        let a = leq n0 (tm n)
        lemma th (inductive_case n0 n i P) [
            trade_forall_implies n (pred_of n a) (pred_of n ((hypothesis n0 n i P) ==> P.[succ n])) |> apply
            Taut body |> at [select_body]
            ident_forall_true' n
        ]

    (* ------------------------------------------------------------------------------------------
       Strong induction (Gries §12.1–12.2): an inductive step that uses the hypothesis at some point
       OTHER than its upper bound — `P(n−1)` as well as `P.n`, as a Fibonacci step does.

       The principle is unchanged — (12.8)'s hypothesis is already all of `P.n₀ … P.n` — so what is
       needed is order reasoning to discharge the range obligation `n₀ ≤ k ∧ k ≤ n` at the chosen `k`,
       and a case split for the steps where `k` would fall below `n₀`. Everything below is a THEOREM
       of the ordered domain `Integers` is (Reflexivity, Transitivity, Monotonicity — `OrderedDomain`),
       its one remaining order axiom (Discreteness), and the ring normalizer; nothing is axiomatized
       for the purpose.
       ------------------------------------------------------------------------------------------ *)

    /// `lo + 1` with integer literals folded: `0 + 1` is `1`, a symbolic `lo + 1` is left as it is.
    let fold_succ (lo: Term<int>) : IntTerm =
        IntTerm(Integers._reduce_constants (call_add lo.Expr (Expr.Value 1)) |> expand_as<int>)

    /// `(a = b) ⇒ (b = a)` — symmetry of `=`, at any carrier, as an IMPLICATION. The logic's symmetry
    /// is symmetry of ≡, so nothing flips `a = b` at a non-bool type directly; this derives it from
    /// Substitution (3.84a) and reflexivity: `a = b ∧ a = a` is `a = b ∧ b = a`. With `Deduce'` it
    /// rewrites `b` to `a` under a hypothesis `a = b`, and with `mp` it flips a proved identity.
    let eq_sym (th: Theory) (a: Term<'t>) (b: Term<'t>) : Theorem = OrderedDomain.eq_sym th a.Expr b.Expr

    /// `x ≤ x + 1`. Discreteness splits it into `x ≤ x ∨ x = x + 1`, whose left disjunct is reflexivity.
    let inline le_succ (th: Theory) x : Theorem =
        let x = int_term x
        let x1 = IntTerm(x.Expr) + 1
        let eq = x == (x1 :> Term<int>)
        lemma th (leq x x1) [
            ax_ident th ((leq x x1) == ((leq x x) + eq)) |> apply     // Discreteness
            Integers.le_refl th x                        |> at_left   // Reflexivity (15.46)
            commute_or T eq                              |> apply
            zero_or eq                                   |> apply
        ]

    /// `lo ≤ n ⇒ lo ≤ n + 1` — Transitivity through `n ≤ n + 1`.
    let inline le_step (th: Theory) lo n : Theorem =
        let lo, n = int_term lo, int_term n
        let a = leq lo n
        lemma th (a ==> (leq lo (IntTerm(n.Expr) + 1))) [
            ident_and a |> Commute              |> at_left                       // A → A ∧ T
            Taut (le_succ th n) |> Commute      |> at [left_branch; right_branch]  // T → n ≤ n + 1
            Taut (Integers.le_trans th lo n (IntTerm(n.Expr) + 1)) |> apply        // Transitivity (15.41d)
        ]

    /// `lo′ ≤ n ⇒ lo ≤ n`, where `lo′` is `lo + 1` folded — Transitivity through `lo ≤ lo + 1`.
    let inline le_of_succ_le (th: Theory) lo n : Theorem =
        let lo, n = int_term lo, int_term n
        let a = leq (fold_succ lo) n
        lemma th (a ==> (leq lo n)) [
            ident_and a |> Commute              |> at_left                       // A → A ∧ T
            commute_and a T                     |> at_left                       // → T ∧ A
            Taut (le_succ th lo) |> Commute     |> at [left_branch; left_branch] // T → lo ≤ lo + 1
            Integers.reduce                     |> at [left_branch; left_branch; right_branch]  // lo + 1 folded
            Taut (Integers.le_trans th lo (fold_succ lo) n) |> apply              // Transitivity (15.41d)
        ]

    /// `(lo ≤ n − 1) = (lo′ ≤ n)`, `lo′` being `lo + 1` folded — Translation of ≤, and `(n−1)+1 = n`.
    let inline pred_translate (th: Theory) lo n : Theorem =
        let lo, n = int_term lo, int_term n
        let nm1 = IntTerm(n.Expr) - 1
        lemma th ((leq lo nm1) == (leq (fold_succ lo) n)) [
            Integers.le_translate th lo nm1                                       |> at_left   // Monotonicity (15.42)
            Integers.reduce                                     |> at [left_branch; left_branch]   // lo + 1 folded
            Ident (Integers.ring_ident th (nm1 + 1) n)         |> at [left_branch; right_branch]  // (n−1)+1 → n
        ]

    /// `lo′ ≤ n ⇒ lo ≤ n − 1`.
    let inline le_pred (th: Theory) lo n : Theorem =
        let lo, n = int_term lo, int_term n
        let a = leq (fold_succ lo) n
        lemma th (a ==> (leq lo (IntTerm(n.Expr) - 1))) [
            Ident (pred_translate th lo n)      |> at_right
            Taut (reflex_implies a)             |> apply
        ]

    /// `n − 1 ≤ n` — `le_succ` at `n − 1`, after writing `n` as `(n − 1) + 1`.
    let inline pred_le (th: Theory) n : Theorem =
        let n = int_term n
        let nm1 = IntTerm(n.Expr) - 1
        lemma th (leq nm1 n) [
            Ident (Integers.ring_ident th n (nm1 + 1)) |> at_right
            Taut (le_succ th nm1)                      |> apply
        ]

    /// `(lo ≤ n) = (lo′ ≤ n ∨ lo = n)`, `lo′` being `lo + 1` folded — a range's lower end is either
    /// exceeded or hit. Discreteness at `n = (n − 1) + 1`, then `pred_translate`. This is the case
    /// split a strong-induction step makes when its smaller hypothesis would fall below `n₀`.
    let inline lower_split (th: Theory) lo n : Theorem =
        let lo, n = int_term lo, int_term n
        let nm1 = IntTerm(n.Expr) - 1
        lemma th ((leq lo n) == ((leq (fold_succ lo) n) + (lo == n))) [
            Ident (Integers.ring_ident th n (nm1 + 1))                       |> at [left_branch; right_branch]   // n → (n−1)+1
            ax_ident th ((leq lo (nm1 + 1)) == ((leq lo nm1) + (lo == ((nm1 + 1) :> Term<int>)))) |> at_left      // Discreteness
            Ident (pred_translate th lo n)                                   |> at [left_branch; left_branch]    // lo ≤ n−1 → lo′ ≤ n
            Ident (Integers.ring_ident th (nm1 + 1) n)                       |> at [left_branch; right_branch; right_branch]  // (n−1)+1 → n
        ]

    /// `((n₀ ≤ k ∧ k ≤ n) ∧ (∀i | n₀ ≤ i ∧ i ≤ n : P.i)) ⇒ P.k` — the inductive hypothesis at ANY
    /// point `k` of its range. `hypothesis_at` is the case `k := n`. The range obligation is the
    /// antecedent's first conjunct, for the caller to establish (`le_pred`, `pred_le`, …).
    let inline hypothesis_at_point (th: Theory) n0 (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) k : Theorem =
        let k = int_term k
        let range = pred_of i ((leq n0 (tm i)) * (leq (tm i) (tm n)))
        let a = range.[k]
        let f = forall'(tm i, pred_of i ((range.[tm i]) ==> P.[tm i]))
        let pk = P.[k]
        lemma th ((a * (hypothesis n0 n i P)) ==> pk) [
            trade_forall_implies i range P      |> at [left_branch; right_branch]   // IH → (∀i |: R ⇒ P)
            commute_and a f                     |> at_left
            shunt' f a pk                                                           // F ⇒ (R.k ⇒ P.k)
            // and that is Instantiation (9.13) at i := k.
        ]

    /// `(n₀′ ≤ n ∧ (∀i | n₀ ≤ i ∧ i ≤ n : P.i)) ⇒ P(n − 1)`, `n₀′` being `n₀ + 1` folded — the
    /// hypothesis one below its upper bound, which is in range exactly when `n` is above the base.
    let inline hypothesis_below (th: Theory) n0 (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) : Theorem =
        let n0 = int_term n0
        let nm1 = tm n - 1
        let a = leq (fold_succ n0) (tm n)
        lemma th ((a * (hypothesis n0 n i P)) ==> P.[nm1]) [
            Establish (le_pred th n0 (tm n))                                              // n₀ ≤ n − 1
            Establish (lemma th (a ==> (leq nm1 (tm n))) [ Taut (pred_le th (tm n)) |> apply_right ])  // n − 1 ≤ n
            Deduce (hypothesis_at_point th n0 n i P nm1)  |> apply_right
        ]

    /// The body of an inductive step BY CASES on whether `n` is the base point. From
    ///
    /// ```
    /// above:    ⊢ (IH ∧ n₀′ ≤ n) ⇒ P(n+1)          (n₀′ = n₀ + 1 folded)
    /// at_base:  ⊢ (IH ∧ n₀ = n) ⇒ P(n+1)
    /// ```
    ///
    /// conclude `⊢ n₀ ≤ n ⇒ (IH ⇒ P(n+1))`, the body `generalize_step` takes. `lower_split` turns
    /// `n₀ ≤ n` into the two cases and Case analysis (3.78) separates them. A strong induction whose
    /// step reaches back two places (Fibonacci) proves `P(n₀+1)` outright in `at_base`, and uses
    /// `hypothesis_below` in `above`.
    let inline step_by_cases (th: Theory) n0 (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>)
                             (above: Theorem) (at_base: Theorem) : Theorem =
        let n0 = int_term n0
        let a = leq n0 (tm n)
        let ih = hypothesis n0 n i P
        let pn1 = P.[succ n]
        let up, eq = leq (fold_succ n0) (tm n), (n0 == (tm n :> Term<int>))
        lemma th (a ==> (ih ==> pn1)) [
            (shunt' a ih pn1 |> Commute)                       |> apply          // (n₀ ≤ n ∧ IH) ⇒ P(n+1)
            Ident (lower_split th n0 (tm n))                   |> at [left_branch; left_branch]
            commute_and (up + eq) ih                           |> at_left
            distrib_and_or ih up eq                            |> at_left        // (IH ∧ up) ∨ (IH ∧ eq)
            case_analysis_1 (ih * up) (ih * eq) pn1 |> Commute |> apply          // (3.78)
            Taut above                                         |> at_left
            Taut at_base                                       |> at_right
            ident_and T                                        |> apply
        ]

    (* Module information members *)

    type private IModuleTypeLocator = interface end

    let Type = match typeof<IModuleTypeLocator>.DeclaringType with | NonNull m -> m | _ -> failwith "Failed to locate module type."
