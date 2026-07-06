namespace Sylvia
// A bare rule as a proof step implicitly means "apply to the whole expression" (Rule -> RuleApplication);
// acknowledge that implicit conversion (see the new-style proofs below, e.g. `def_implies` / `at […]`).
#nowarn "3391"

open FSharp.Quotations
open PropCalculus

/// Predicate calculus using the axioms and rules of S.
module PredCalculus =
    let pred_calculus = Theory.S

    (* Expression functions for admissible rules *)
    
    let _empty_range = EquationalLogic._empty_range

    let _trade_body = EquationalLogic._trade_body

    let _distrib_or_forall = EquationalLogic._distrib_or_forall
    
    let fail_if_occurs_free x q = 
        do if Patterns.occurs_free (x |> get_vars) q then failwithf "One of the variables in %s occurs free in the quantifier %s." (src x) (src q)

    (* Admissible rules *)
    
    let empty_range = Theory.S.Rules.[21] 

    let trade_body = Theory.S.Rules.[22]

    let collect_forall_and = Theory.S.Rules.[23]

    let collect_exists_or = Theory.S.Rules.[24]

    let distrib_or_forall = Theory.S.Rules.[25]

    let split_range_forall = Theory.S.Rules.[26]

    let split_range_exists = Theory.S.Rules.[27]

    (* Prop-body quantifier builders. The Pred-based `forall`/`exists` combinators apply a
       predicate to the dummy, so every argument is forced to depend on x. Several Gries laws
       (distributivity 9.5/9.21/9.22, trading-out 9.6) instead need a sub-formula that is
       x-FREE (the ¬occurs side condition). These builders take the range and body as ready
       propositions in x, so an x-free proposition can be written directly. *)

    /// (∀ x | R : B) with range R and body B given as propositions in x.
    let qall (x:TermVar<'t>) (R:Prop) (B:Prop) : Prop = Prop <@ Formula.forall_expr %x.Expr %R.Expr %B.Expr @>

    /// (∃ x | R : B) with range R and body B given as propositions in x.
    let qex (x:TermVar<'t>) (R:Prop) (B:Prop) : Prop = Prop <@ Formula.exists_expr %x.Expr %R.Expr %B.Expr @>

    /// A predicate constantly equal to the truth constant T. Its application to any term
    /// reduces to T (the named True), so it stands in for a `true` range/body where the
    /// Pred-based combinators need a predicate.
    let truepred<'t when 't:equality> : Pred<'t> = Pred(func = <@ fun (_z:'t) -> %T.Expr @>)

    (* Derived rules *)

    /// (∀x|N:P) = (∀x|: N⇒P)   (Gries 9.2, Trading)
    let trade_forall_implies (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) = id_ax pred_calculus (forall (x, N, P) == forall' (x, N ==> P))

    /// (∀x|Q∧N:P) = (∀x|Q: N⇒P)   (Gries 9.4a)
    let trade_forall_and_implies (x:TermVar<'t>) (Q:Pred<'t>) (N:Pred<'t>) (P:Pred<'t>) = ident pred_calculus (forall(x, Q * N, P) == (forall (x, Q, N ==> P))) [
        trade_forall_implies x (Q * N) P |> at_left
        shunt |> at [left_branch; select_body]
        trade_forall_implies x Q  (N==>P) |> Commute |> at_left
    ]

    /// (∀x|N:P) = P ∨ (∀x|:¬N)   (Gries 9.6). P is an x-free proposition (¬occurs(x,P)).
    let trade_forall_or_not (x:TermVar<'t>) (N:Pred<'t>) (P:Prop) = ident pred_calculus (qall x N[x] P == (P + forall'(x, -N))) [
        distrib_or_forall |> at_right
        commute_or P (-(N[x])) |> at_right
        ident_implies_not_or N[x] P |> Commute |> at_right
    ]

    /// P ∨ (∀x|N:Q) = (∀x|N: P∨Q)   (Gries 9.5). P is an x-free proposition (¬occurs(x,P)).
    let distrib_or_forall' (x:TermVar<'t>) (N:Pred<'t>) (P:Prop) (Q:Pred<'t>) = id_ax pred_calculus ((P + forall(x, N, Q)) == qall x N[x] (P + Q[x]))

    /// (∀x|N1∨N2:P) = ((∀x|N1:P) ∧ (∀x|N2:P))   (Gries 8.18, range split)
    let split_range_forall' (x:TermVar<'t>) (N1:Pred<'t>) (N2:Pred<'t>) (P:Pred<'t>) = id_ax pred_calculus (forall (x, (N1 + N2), P) == ((forall(x, N1, P) * (forall (x, N2, P)))))

    (* Universal instantiation (Gries 9.13) *)

    /// Universal Instantiation (Gries 9.13): (∀x |: P) ⇒ P[x:=E]. Closed directly by the
    /// Universal Instantiation axiom of S; `e` is the instantiating term E.
    let inst (x:TermVar<'t>) (P:Pred<'t>) (e:Term<'t>) : Theorem = theorem pred_calculus ((forall'(x, P)) ==> (P[e])) []

    /// Universal Instantiation specialized to the dummy itself (Gries 9.13 with E := x):
    /// (∀x |: P) ⇒ P.
    let inst' (x:TermVar<'t>) (P:Pred<'t>) : Theorem = inst x P x

    (* Universal quantification: distributivity over ∧ and identities (Gries 9.6–9.9) *)

    /// ((∀x|N:P) ∧ (∀x|N:Q)) = (∀x|N: P∧Q)   (Gries 8.15, distributivity of ∀ over ∧)
    let collect_forall_and' (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) (Q:Pred<'t>) =
        id_ax pred_calculus (((forall(x,N,P)) * (forall(x,N,Q))) == forall(x, N, P * Q))

    /// (∀x|N: P∧Q) = ((∀x|N:P) ∧ (∀x|N:Q))
    let distrib_forall_and' (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) (Q:Pred<'t>) = collect_forall_and' x N P Q |> Commute

    /// (∀x|N:true) = true   (Gries 9.8)
    let ident_forall_true (x:TermVar<'t>) (N:Pred<'t>) = ident pred_calculus (qall x N[x] T == T) [
        trade_forall_or_not x N T |> at_left
        commute_or T (forall'(x, -N)) |> at_left
        zero_or (forall'(x, -N)) |> at_left
    ]

    /// (∀x|:true) = true   (Gries 9.8 at the true range)
    let ident_forall_true' (x:TermVar<'t>) = ident_forall_true x truepred

    /// P ⇒ (∀x|:P)   (Gries 9.16, ⇐ direction of the metatheorem). P is an x-free proposition:
    /// under the antecedent P the body becomes true, and (∀x|:true) = true.
    let forall_conseq (x:TermVar<'t>) (P:Prop) = theorem pred_calculus (P ==> qall x T P) [
        Deduce (axiom prop_calculus (P ==> P)) |> at_right
        ident_forall_true' x |> at_right
    ]

    (* Existential quantification via Generalized De Morgan (Gries 9.17–9.18) *)

    /// (∃x|N:P) = ¬(∀x|N:¬P)   (Gries 9.17, Generalized De Morgan)
    let ident_exists_not_forall (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) =
        id_ax pred_calculus (exists(x, N, P) == (-(forall(x, N, -P))))

    /// ¬(∃x|N:¬P) = (∀x|N:P)   (Gries 9.18a)
    let ident_not_exists_forall (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) =
        ident pred_calculus ((-(exists(x, N, -P))) == forall(x, N, P)) [
            ident_exists_not_forall x N (-P) |> at_left
            double_negation P[x] |> at_left
            double_negation (forall(x, N, P)) |> at_left
        ]

    /// ¬(∃x|N:P) = (∀x|N:¬P)   (Gries 9.18b)
    let ident_not_exists_forall_not (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) =
        ident pred_calculus ((-(exists(x, N, P))) == forall(x, N, -P)) [
            ident_exists_not_forall x N P |> at_left
            double_negation (forall(x, N, -P)) |> at_left
        ]

    /// (∃x|N:¬P) = ¬(∀x|N:P)   (Gries 9.18c)
    let ident_exists_not_forall_not (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) =
        ident pred_calculus (exists(x, N, -P) == (-(forall(x, N, P)))) [
            ident_not_exists_forall x N P |> Commute |> at_right
            double_negation (exists(x, N, -P)) |> at_right
        ]

    (* Existential quantification: distributivity over ∨ and range split (Gries 8.15/8.18) *)

    /// ((∃x|N:P) ∨ (∃x|N:Q)) = (∃x|N: P∨Q)   (Gries 8.15, distributivity of ∃ over ∨)
    let collect_exists_or' (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) (Q:Pred<'t>) =
        id_ax pred_calculus (((exists(x,N,P)) + (exists(x,N,Q))) == exists(x, N, P + Q))

    /// (∃x|N: P∨Q) = ((∃x|N:P) ∨ (∃x|N:Q))
    let distrib_exists_or' (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) (Q:Pred<'t>) = collect_exists_or' x N P Q |> Commute

    /// (∃x|N1∨N2:P) = ((∃x|N1:P) ∨ (∃x|N2:P))   (Gries 8.18, range split)
    let split_range_exists' (x:TermVar<'t>) (N1:Pred<'t>) (N2:Pred<'t>) (P:Pred<'t>) =
        id_ax pred_calculus (exists(x, (N1 + N2), P) == ((exists(x,N1,P)) + (exists(x,N2,P))))

    (* Existential quantification: trading (Gries 9.19–9.20) *)

    /// (∃x|N:P) = (∃x|: N∧P)   (Gries 9.19, Trading)
    let trade_exists_and (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) =
        ident pred_calculus (exists(x, N, P) == exists'(x, N * P)) [
            // rewrite both ∃ as ¬∀¬ (double_neg = generalized De Morgan), then show the two
            // ∀ bodies equal:  N ⇒ ¬P  =  ¬(N ∧ P)  (both are ¬N ∨ ¬P).
            double_neg |> at_left
            double_neg |> at_right
            trade_body |> at [left_branch; apply_unary]
            ident_implies_not_or N[x] (-(P[x])) |> at [left_branch; apply_unary; select_body]
            distrib_not_and N[x] P[x] |> at [right_branch; apply_unary; select_body]
        ]

    /// (∃x|Q∧N:P) = (∃x|Q: N∧P)   (Gries 9.20)
    let trade_exists_and_and (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) (Q:Pred<'t>) =
        ident pred_calculus (exists(x, Q * N, P) == (exists(x, Q, N * P))) [
            trade_exists_and x (Q * N) P |> at_left
            right_assoc_and Q[x] N[x] P[x] |> at [left_branch; select_body]
            trade_exists_and x Q (N * P) |> Commute |> at_left
        ]

    (* Universal quantification: body distributivity, weakening and monotonicity (Gries 9.9–9.12) *)

    /// (∀x|N: P=Q) ⇒ ((∀x|N:P) = (∀x|N:Q))   (Gries 9.9)
    let distrib_forall_body (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) (Q:Pred<'t>) =
        theorem pred_calculus (forall(x, N, P == Q) ==> (forall(x,N,P) == forall(x,N,Q))) [
            distrib_implies_eq_and (forall(x,N,P == Q)) (forall(x,N,P)) (forall(x,N,Q))
            collect_forall_and |> at_left
            collect_forall_and |> at_right
            commute_and (P[x] == Q[x]) P[x] |> at [left_branch; select_body]
            commute_and (P[x] == Q[x]) Q[x] |> at [right_branch; select_body]
            commute_eq P[x] Q[x] |> at [left_branch; select_body]
            ident_and_eq P[x] Q[x] |> at [left_branch; select_body]
            ident_and_eq Q[x] P[x] |> at [right_branch; select_body]
            commute_and Q[x] P[x] |> at [right_branch; select_body]
        ]

    /// (∀x|N1∨N2:P) ⇒ (∀x|N1:P)   (Gries 9.10, range strengthening)
    let strengthen_forall_range_or (x:TermVar<'t>) (N1:Pred<'t>) (N2:Pred<'t>) (P:Pred<'t>) =
        theorem pred_calculus (forall(x, N1 + N2, P) ==> forall(x, N1, P)) [
            split_range_forall |> at_left
            strengthen_and (forall(x,N1,P)) (forall(x,N2,P)) |> Taut |> apply
        ]

    /// (∀x|N: P∧Q) ⇒ (∀x|N:P)   (Gries 9.11, body strengthening)
    let strengthen_forall_body_and (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) (Q:Pred<'t>) =
        theorem pred_calculus (forall(x, N, P * Q) ==> forall(x, N, P)) [
            distrib_forall_and' x N P Q |> at_left
            strengthen_and (forall(x,N,P)) (forall(x,N,Q)) |> Taut |> apply
        ]

    /// (∀x|N: Q⇒P) ⇒ ((∀x|N:Q) ⇒ (∀x|N:P))   (Gries 9.12, monotonicity of ∀)
    let mono_forall_body (x:TermVar<'t>) (N:Pred<'t>) (Q:Pred<'t>) (P:Pred<'t>) =
        theorem pred_calculus (forall(x, N, Q ==> P) ==> (forall(x,N,Q) ==> forall(x,N,P))) [
            rshunt
            collect_forall_and |> at_left
            commute_and (Q[x] ==> P[x]) Q[x] |> at [left_branch; select_body]
            ident_and_implies Q[x] P[x] |> at [left_branch; select_body]
            commute_and Q[x] P[x] |> at [left_branch; select_body]
            strengthen_forall_body_and x N P Q |> Taut |> apply
        ]

    (* Universal instantiation consequences, and existential weakening (Gries 9.13, 9.25–9.26) *)

    /// (∀x|N:P) ⇒ (N⇒P)   (Gries 9.13 via Trading: instantiate the traded body at x)
    let forall_implies (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) =
        theorem pred_calculus (forall(x, N, P) ==> (N[x] ==> P[x])) [
            trade_body |> at_left   // (∀x|:N⇒P) ⇒ (N⇒P) is Universal Instantiation
        ]

    /// P[x:=E] ⇒ (∃x|:P)   (Gries 9.28, ∃-introduction). The contrapositive of instantiation of
    /// ¬P: (∀x|:¬P) ⇒ ¬P[E], recast through De Morgan (∃x|:P = ¬∀x|:¬P).
    let exists_intro (x:TermVar<'t>) (P:Pred<'t>) (e:Term<'t>) =
        theorem pred_calculus (P[e] ==> exists'(x, P)) [
            double_neg |> at_right
            def_implies_contr P[e] (-(forall'(x, -P)))
            double_negation (forall'(x, -P)) |> at_left
            inst x (-P) e |> Taut |> apply
        ]

    /// (∃x|N:P) ⇒ (∃x|Q∨N:P)   (Gries 9.25, range weakening)
    let weaken_exists_range (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) (Q:Pred<'t>) =
        theorem pred_calculus (exists(x, N, P) ==> exists(x, Q + N, P)) [
            split_range_exists |> at_right
            commute |> at_right
            weaken_or (exists(x,N,P)) (exists(x,Q,P)) |> Taut |> apply
        ]

    /// (∃x|N:P) ⇒ (∃x|N: P∨Q)   (Gries 9.26, body weakening)
    let weaken_exists_body (x:TermVar<'t>) (N:Pred<'t>) (P:Pred<'t>) (Q:Pred<'t>) =
        theorem pred_calculus (exists(x, N, P) ==> exists(x, N, P + Q)) [
            distrib_exists_or' x N P Q |> at_right
            weaken_or (exists(x,N,P)) (exists(x,N,Q)) |> Taut |> apply
        ]

    (* Existential quantification: distributivity of ∧ over ∃ (Gries 9.21) *)

    /// P ∧ (∃x|N:Q) = (∃x|N: P∧Q)   (Gries 9.21). P is an x-free proposition (¬occurs(x,P)).
    /// Both ∃ are rewritten as ¬∀¬ (De Morgan), the x-free ¬P is pulled through the ∀ by
    /// distributivity 9.5, and De Morgan folds the result back.
    let distrib_and_exists_and (x:TermVar<'t>) (N:Pred<'t>) (P:Prop) (Q:Pred<'t>) =
        ident pred_calculus ((P * exists(x,N,Q)) == qex x N[x] (P * Q[x])) [
            double_neg |> at [left_branch; right_branch]
            double_neg |> at_right
            distrib_not_and P Q[x] |> at [right_branch; apply_unary; select_body]
            distrib_or_forall' x N (-P) (-Q) |> Commute |> at [right_branch; apply_unary]
            distrib_not_or (-P) (forall(x,N,-Q)) |> at_right
            double_negation P |> at [right_branch; left_branch]
        ]

    /// (∃x|N:P) = (P ∧ (∃x|:N))   (Gries 9.22). P is an x-free proposition (¬occurs(x,P)).
    let distrib_and_exists (x:TermVar<'t>) (N:Pred<'t>) (P:Prop) =
        ident pred_calculus (qex x N[x] P == (P * exists'(x, N))) [
            double_neg |> at_left
            trade_forall_or_not x N (-P) |> at [left_branch; apply_unary]
            distrib_not_or (-P) (forall'(x, -N)) |> at_left
            double_negation P |> at [left_branch; left_branch]
            double_neg |> at [right_branch; right_branch]
        ]

    /// (∃x|N:false) = false   (Gries 9.24). Via De Morgan: ∃N F = ¬∀N¬F = ¬∀N true = ¬true = false.
    let ident_exists_false (x:TermVar<'t>) (N:Pred<'t>) =
        ident pred_calculus (qex x N[x] F == F) [
            double_neg |> at_left
            not_false |> at [left_branch; apply_unary; select_body]
            ident_forall_true x N |> at [left_branch; apply_unary]
            not_false |> Commute |> at [left_branch; apply_unary]
            double_negation F |> at_left
        ]

    /// (∀x|N: Q⇒P) ⇒ ((∃x|N:Q) ⇒ (∃x|N:P))   (Gries 9.27, monotonicity of ∃).
    /// The ∃'s become ¬∀¬ (De Morgan); the consequent contrapositive turns ∃Q⇒∃P into
    /// ∀¬P⇒∀¬Q, and Q⇒P is the contrapositive of ¬P⇒¬Q, so ∀-monotonicity 9.12 closes it.
    let mono_exists (x:TermVar<'t>) (N:Pred<'t>) (Q:Pred<'t>) (P:Pred<'t>) =
        theorem pred_calculus (forall(x, N, Q ==> P) ==> (exists(x,N,Q) ==> exists(x,N,P))) [
            ident_exists_not_forall x N Q |> at [right_branch; left_branch]
            ident_exists_not_forall x N P |> at [right_branch; right_branch]
            def_implies_contr (-(forall(x,N,-Q))) (-(forall(x,N,-P))) |> at_right
            double_negation (forall(x,N,-P)) |> at [right_branch; left_branch]
            double_negation (forall(x,N,-Q)) |> at [right_branch; right_branch]
            def_implies_contr Q[x] P[x] |> at [left_branch; select_body]
            mono_forall_body x N (-P) (-Q) |> Taut |> apply
        ]

    (* Existential quantification: an ∃ antecedent, and conditional trading of ∨ (Gries 9.23, ex.9.27) *)

    /// (∃x|R:P) ⇒ Q  =  (∀x|R: P⇒Q)   (Gries ch.9 ex.9.27). Q is an x-free proposition. An ∃ in an
    /// antecedent becomes a ∀: ¬(∃x|R:P) is (∀x|R:¬P) (De Morgan), the x-free Q distributes through
    /// the ∀ (9.5), and ¬P∨Q folds back to P⇒Q.
    let ident_exists_implies (x:TermVar<'t>) (R:Pred<'t>) (P:Pred<'t>) (Q:Prop) =
        ident pred_calculus (((exists(x,R,P)) ==> Q) == qall x R[x] (P[x] ==> Q)) [
            ident_implies_not_or (exists(x,R,P)) Q |> at_left                  // → ¬(∃x|R:P) ∨ Q
            ident_not_exists_forall_not x R P |> at [left_branch; left_branch] // ¬(∃x|R:P) → (∀x|R:¬P)
            commute_or (forall(x,R,-P)) Q |> at_left
            distrib_or_forall' x R Q (-P) |> at_left                          // Q ∨ (∀x|R:¬P) → (∀x|R: Q∨¬P)
            commute_or Q (-(P[x])) |> at [left_branch; select_body]
            ident_implies_not_or P[x] Q |> Commute |> at [left_branch; select_body]  // ¬P∨Q → P⇒Q
        ]

    /// (∃x|:R) ⇒ ((∃x|R: P∨Q) = (P ∨ (∃x|R:Q)))   (Gries 9.23). P is an x-free proposition. A
    /// CONDITIONAL law (the ∨ can only be traded out when the range is nonempty): split the body
    /// (8.15), pull the x-free P out of the P-disjunct (9.22), then the assumption (∃x|:R) discharges
    /// it to true. The range-nonempty assumption is supplied by `Deduce`.
    let trade_exists_or (x:TermVar<'t>) (R:Pred<'t>) (P:Prop) (Q:Pred<'t>) =
        // ((∃x|R:P) ∨ (∃x|R:Q)) = (∃x|R: P∨Q) with the left body x-free (structural QuantifierCollect axiom).
        let collect_mixed = id_ax pred_calculus (((qex x R[x] P) + exists(x,R,Q)) == qex x R[x] (P + Q[x]))
        theorem pred_calculus (exists'(x, R) ==> ((qex x R[x] (P + Q[x])) == (P + exists(x,R,Q)))) [
            collect_mixed |> Commute |> at [right_branch; left_branch]                             // ∃x R (P∨Q) → (∃x R P) ∨ (∃x R Q)
            distrib_and_exists x R P |> at [right_branch; left_branch; left_branch]                // ∃x R P → P ∧ (∃x|:R)
            axiom pred_calculus (exists'(x,R) ==> exists'(x,R)) |> Deduce |> at [right_branch; left_branch; left_branch; right_branch]  // assume (∃x|:R)
            ident_and P |> at [right_branch; left_branch; left_branch]                             // P ∧ true → P
            def_true (P + exists(x,R,Q)) |> Commute |> at_right                                    // consequent Y=Y → T
            implies_true (exists'(x, R)) |> Taut |> apply                                          // (∃x|:R) ⇒ T → true
        ]

    (* Existential/universal interchange (Gries 9.29) *)

    /// (∃x|:(∀y|:P)) ⇒ (∀y|:(∃x|:P))   (Gries 9.29, interchange of quantifications). P is a
    /// proposition in both dummies x and y. One direction only (the converse is invalid). Recast
    /// ⇒ via def of ⇒, pull ∃x∀yP into ∀y (9.5), collect the two ∃x (8.15), and absorb
    /// (∀y|:P) ∨ P into P by instantiation ((∀y|:P) ⇒ P).
    let exists_forall_interchange (x:TermVar<'t>) (y:TermVar<'t>) (P:Prop) =
        let absorb = lemma pred_calculus (((qall y T P) + P) == P) [
            def_implies' (qall y T P) P |> Commute |> apply   // ((∀y|:P)∨P = P) ⇐ (∀y|:P)⇒P (Universal Instantiation)
        ]
        theorem pred_calculus (qex x T (qall y T P) ==> qall y T (qex x T P)) [
            def_implies
            distrib_or_forall |> at_left
            collect_exists_or |> at [left_branch; select_body]
            Ident absorb |> at [left_branch; select_body; select_body]
        ]

    (* Module information members *)

    type private IModuleTypeLocator = interface end
    
    let Type = match typeof<IModuleTypeLocator>.DeclaringType with | NonNull m -> m | _ -> failwith "Failed to locate module type."

    (* Deferred. `trade_exists_or` (9.23) and `ident_exists_implies` (ex.9.27) are now done above,
       following Gries' own exercise derivations (Prop-body distributivity + the range-nonempty
       assumption discharged by Deduce). Still deferred:
         - distrib_forall_and_cond (Gries 9.7): conditional distributivity of ∧ over ∀.
         - forall_conseq_trade_body (metatheorem Witness, 9.30). *)
