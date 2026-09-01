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
/// is NOT stated: Gries derives (12.5) from it using Split off term (8.23) over an integer range,
/// which this prover does not have, and (12.5) is the form proofs actually use.
///
/// The PRINCIPLE needs no arithmetic — `n₀ ≤ n` is matched structurally and never reasoned about.
/// The base and inductive cases are where arithmetic lives, and those are the caller's proofs, which
/// is why this theory layers the axiom over `Integers` instead of standing alone.
module Induction =

    let private desc = Some << axiom_name "Induction"

    (* Building blocks *)

    /// `l ≤ r` over the integers, as a proposition.
    let leq (l: Term<int>) (r: Term<int>) : Prop = <@ %l.Expr <= %r.Expr @> |> expand_as<bool> |> Prop

    /// A dummy as an integer term.
    let tm (x: #ISymbolicVar<int>) : IntTerm = IntTerm(x.Expr)

    /// `x + 1`, structurally (`IntTerm`, not `Scalar` — see the note there on the CAS).
    let succ (x: #ISymbolicVar<int>) : IntTerm = tm x + IntTerm 1

    /// Abstract a dummy back out of a proposition written in it, giving the `Pred` the
    /// predicate-calculus theorems take. Binds the `Var` OBJECT occurring in the body rather than a
    /// structurally equal copy, so `Q.[x]` beta-reduces back to exactly the proposition given.
    let pred_of (x: #ISymbolicVar<int>) (p: Prop) : Pred<int> =
        let body = expand p.Expr
        let xv = x.Var
        let v = match get_vars body |> List.tryFind (fun w -> vequal w xv) with Some w -> w | None -> xv
        Pred<int>(func = Expr.Cast<int -> bool>(expand (Expr.Lambda(v, body))))

    /// The base case `P.n₀`.
    let base_case (n0: Term<int>) (P: Pred<int>) : Prop = P.[n0]

    /// The conclusion `(∀n | n₀ ≤ n : P.n)`.
    let conclusion (n0: Term<int>) (n: #ISymbolicVar<int>) (P: Pred<int>) : Prop =
        qall n (leq n0 (tm n)) P.[tm n]

    /// The INDUCTIVE HYPOTHESIS at `n`: `(∀i | n₀ ≤ i ∧ i ≤ n : P.i)` — all of `P.n₀ … P.n`, not
    /// just `P.n`. Gries folds weak and strong induction into this one form deliberately (§12.1): a
    /// proof that happens to use only `P.n` is a weak induction and needs no different principle.
    let hypothesis (n0: Term<int>) (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) : Prop =
        qall i ((leq n0 (tm i)) * (leq (tm i) (tm n))) P.[tm i]

    /// The inductive case `(∀n | n₀ ≤ n : (∀i | n₀ ≤ i ∧ i ≤ n : P.i) ⇒ P(n+1))`.
    let inductive_case (n0: Term<int>) (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) : Prop =
        qall n (leq n0 (tm n)) ((hypothesis n0 n i P) ==> P.[succ n])

    /// The (12.8) instance for `P` — `base_case ∧ inductive_case ⇒ conclusion`.
    let induction_axiom (n0: Term<int>) (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) : Prop =
        ((base_case n0 P) * (inductive_case n0 n i P)) ==> (conclusion n0 n P)

    (* Axiom *)

    /// True iff `v` is BOUND anywhere inside `e`, by a quantifier or a lambda. A naive substitution
    /// for `v` — which is what `replace_var_expr` does, and what the checks below use to reconstruct
    /// `P.n₀` / `P.i` / `P(n+1)` from `P.n` — is only sound when this is false.
    let rec private rebinds (v: Var) (e: Expr) =
        match e with
        | Quantifier(_, bound, range, body) -> (bound |> List.exists (vequal v)) || rebinds v range || rebinds v body
        | ShapeLambda(b, body) -> vequal v b || rebinds v body
        | ShapeCombination(_, es) -> es |> List.exists (rebinds v)
        | ShapeVar _ -> false

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

    /// Integer arithmetic plus (12.8). A caller with its own axioms — the two equations of an
    /// inductively defined function, say (§12.2) — passes them in, so that everything a proof by
    /// induction touches lives in ONE theory: the base case, the inductive case, and the axiom
    /// instance the two are detached from must all be proved in the same one.
    type Induction(?axioms: Axioms, ?rules: Rules) =
        inherit Theory(BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None))
                                                     (BooleanAlgebra.combine_axioms induction_axioms Integers.integer_axioms),
                       (defaultArg rules []) @ Integers.integers.Rules)

    /// The theory: integer arithmetic plus (12.8), with nothing extra.
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
    let hypothesis_at (th: Theory) (n0: Term<int>) (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>) : Theorem =
        let range = pred_of i ((leq n0 (tm i)) * (leq (tm i) (tm n)))
        let q = pred_of i ((range.[tm i]) ==> P.[tm i])          // the traded hypothesis body
        let a = leq n0 (tm n)
        let f = forall'(tm i, q)
        let pn = P.[tm n]
        theorem th ((a * (hypothesis n0 n i P)) ==> pn) [
            trade_forall_implies i range P      |> at [left_branch; right_branch]   // IH → (∀i |: R ⇒ P)
            commute_and a f                     |> at_left                          // (A ∧ F) → (F ∧ A)
            shunt' f a pn                                                           // (F ∧ A) ⇒ P → F ⇒ (A ⇒ P)
            ident_and a |> Commute              |> at [right_branch; left_branch]   // A → A ∧ T
            ax_ident th ((leq (tm n) (tm n)) == T) |> Commute
                                                |> at [right_branch; left_branch; right_branch]  // T → n ≤ n
            // and the state IS Instantiation (9.13) at i := n, which closes it.
        ]

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
    let by_induction (th: Theory) (n0: Term<int>) (n: #ISymbolicVar<int>) (i: #ISymbolicVar<int>) (P: Pred<int>)
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
        by_induction th (IntTerm 0) n i P bse stp

    (* Module information members *)

    type private IModuleTypeLocator = interface end

    let Type = match typeof<IModuleTypeLocator>.DeclaringType with | NonNull m -> m | _ -> failwith "Failed to locate module type."
