namespace Sylvia

/// Propositional calculus using the axioms and rules of S.
module PropCalculus =

    let prop_calculus = Theory.S

    (* Expression functions for admissible rules *)
    
    let _reduce_constants = EquationalLogic._reduce_constants

    let _left_assoc = EquationalLogic._left_assoc

    let _right_assoc = EquationalLogic._right_assoc

    let _commute = EquationalLogic._commute

    let _distrib = EquationalLogic._distrib

    let _collect = EquationalLogic._collect

    let _idemp = EquationalLogic._idemp

    let _excluded_middle = EquationalLogic._excluded_middle

    let _golden_rule = EquationalLogic._golden_rule

    let _shunt = EquationalLogic._shunt
  
    let _mutual_implication = EquationalLogic._mutual_implication

    let _subst_and = EquationalLogic._subst_and

    let _subst_implies = EquationalLogic._subst_implies

    let _subst_and_implies = EquationalLogic._subst_and_implies

    let _subst_true = EquationalLogic._subst_true

    let _subst_false = EquationalLogic._subst_false

    let _subst_or_and = EquationalLogic._subst_or_and

    let _distrib_implies = EquationalLogic._distrib_implies

    let _double_neg = EquationalLogic._double_neg

    let _normalize = EquationalLogic._normalize

    let _normalize_assoc = EquationalLogic._normalize_assoc

    let _simp = EquationalLogic._simp

    (* Admissible rules *)

    /// Reduce logical constants in expression. 
    [<AdmissibleRule "Reduce logical constants in expression.">]
    let reduce = Theory.S.Rules.[0]

    /// Logical expression is left associative.
    [<AdmissibleRule "Logical expression is left associative.">]
    let left_assoc = Theory.S.Rules.[1]

    /// Logical expression is right associative.
    [<AdmissibleRule "Logical expression is right associative.">]
    let right_assoc = Theory.S.Rules.[2]
  
    /// Logical expression is commutative.
    [<AdmissibleRule "Logical expression is commutative.">]
    let commute = Theory.S.Rules.[3]

    /// Distribute logical terms in expression.
    [<AdmissibleRule "Distribute logical terms in expression.">]
    let distrib = Theory.S.Rules.[4]

    /// Collect distributed logical terms in expression.
    [<AdmissibleRule "Collect distributed logical terms in expression.">]
    let collect = Theory.S.Rules.[5]

    /// Logical operators are idempotent.
    [<AdmissibleRule "Logical operators are idempotent.">]
    let idemp = Theory.S.Rules.[6]

    /// Logical expression satisfies law of excluded middle.
    [<AdmissibleRule "Logical expression satisfies law of excluded middle.">]
    let excluded_middle = Theory.S.Rules.[7]

    /// Logical expression satisfies golden rule.
    [<AdmissibleRule "Logical expression satisfies golden rule.">]
    let golden_rule = Theory.S.Rules.[8]

    let def_implies = Theory.S.Rules.[9]

    let shunt = Theory.S.Rules.[10]

    let rshunt = Theory.S.Rules.[11]

    let mutual_implication = Theory.S.Rules.[12]

    let subst_and = Theory.S.Rules.[13]

    let subst_implies = Theory.S.Rules.[14]

    let subst_and_implies = Theory.S.Rules.[15]

    let subst_true = Theory.S.Rules.[16]

    let subst_false = Theory.S.Rules.[17]

    let subst_or_and = Theory.S.Rules.[18]

    let distrib_implies = Theory.S.Rules.[19]

    let double_neg = Theory.S.Rules.[20]

    /// Normalize associative-commutative logical operators (≡, ∨, ∧) in expression
    /// into a canonical flattened, sorted form, collapsing runs of
    /// associativity/commutativity bookkeeping steps.
    [<AdmissibleRule "Normalize associative-commutative logical operators in expression.">]
    let normalize = Theory.S.Rules.[28]

    /// Normalize associativity of logical operators (≡, ∨, ∧) in expression,
    /// flattening and right-associating chains while PRESERVING operand order
    /// (unlike normalize, which also sorts). Reshapes association without commuting.
    [<AdmissibleRule "Normalize associativity of logical operators in expression.">]
    let normalize_assoc = Theory.S.Rules.[29]

    /// Simplify expression to a fixpoint using the propositional simplification laws
    /// (identity, annihilator, complement, idempotence, double negation, constant
    /// equivalence) plus AC-normalization. Closes any (sub)goal that collapses to T.
    [<AdmissibleRule "Simplify expression.">]
    let simp = Theory.S.Rules.[30]

    /// Rewrite ¬/∨/⇒/⇐/≡ in terms of ⊕ (≢) and ∧, toward Boolean-ring normal form.
    [<AdmissibleRule "Rewrite expression with XOR and AND.">]
    let elim_to_xor = Theory.S.Rules.[31]

    /// Distribute ∧ over ⊕.
    [<AdmissibleRule "Distribute AND over XOR.">]
    let distrib_and_xor = Theory.S.Rules.[32]

    /// Normalize a ∧ monomial: flatten, F-annihilate, drop T, dedup atoms (idempotence), sort.
    [<AdmissibleRule "Normalize AND monomial.">]
    let and_normalize = Theory.S.Rules.[33]

    /// Normalize a ⊕ chain: flatten, cancel x⊕x, drop F, sort.
    [<AdmissibleRule "Normalize XOR terms.">]
    let xor_normalize = Theory.S.Rules.[34]

    (* Tactics for rules *)

    /// If A is a theorem then replace A with T.
    [<Tactic("If A is a theorem then replace A with T.")>]
    let Taut :Theorem->Rule=  
        let ieq p = 
            let stmt = <@@ ((%%p) = %T.Expr) = (%%p) @@> in Theorem(stmt, Proof (stmt, prop_calculus, [apply_left commute; apply right_assoc], true)) |> Ident  
        Tactics.Taut ieq
    
    /// If A = B is a theorem then replace (A = B) with T.
    [<Tactic("If A = B is a theorem then replace (A = B) with T.")>]
    let Taut' t = 
        let ieq p = Theorem(<@@ ((%%p) = %T.Expr) = (%%p) @@>, Proof (<@@ (%%p = %T.Expr) = %%p @@>, prop_calculus, [apply_left commute; apply right_assoc], true)) |> Ident 
        Tactics.Taut' ieq t
            
    /// If A = B is a theorem then so is B = A.
    [<Tactic("If A = B is a theorem then so is B = A.")>]
    let Commute = Tactics.Commute commute
    
    /// If (L = R) = B is a theorem then so is (R = L) = B.
    [<Tactic("If (L = R) = B is a theorem then so is (R = L) = B.")>]
    let CommuteL = Tactics.CommuteL commute

    /// If A = (L = R) is a theorem then so is A = (R = L).
    [<Tactic("If A = (L = R) is a theorem then so is A = (R = L).")>]
    let CommuteR = Tactics.CommuteR commute

    [<Tactic("If A1 = (A2 =  A3) is a theorem then so is (A1 = A2) = A3.")>]
    let LeftAssoc = Tactics.LeftAssoc right_assoc

    [<Tactic("If (A1 = (A2 = A3)) = A4 is a theorem then so is ((A1 = A2) = A3) = A4.")>]
    let LeftAssocBranchLeft = Tactics.LeftAssocRecurseLeft right_assoc

    [<Tactic("If A1 = (A2 = (A3 = A4)) is a theorem then so is A1 = ((A2 = A3) = A4).")>]
    let LeftAssocBranchRight = Tactics.LeftAssocRecurseRight right_assoc

    [<Tactic(" If (A1 = A2) = A3 is a theorem then so is A1 = (A2 = A3).")>]
    let RightAssoc = Tactics.RightAssoc left_assoc

    [<Tactic("If ((A1 = A2) = A3) = A4 is a theorem then so is (A1 = (A2 = A3)) = A4.")>]
    let RightAssocBranchLeft = Tactics.RightAssocRecurseLeft left_assoc

    [<Tactic("If A1 = ((A2 = A3) = A4) is a theorem then so is A1 = (A2 = (A3 = A4)).")>]
    let RightAssocBranchRight = Tactics.RightAssocRecurseRight left_assoc

    (* Tactics for proofs *)

    let MutualImplication stmt = Tactics.MutualImplication prop_calculus Taut mutual_implication reduce stmt

    (* Automation *)

    /// Bounded best-first proof search for a propositional goal. Simplifies with `simp`
    /// between structural moves (golden rule, def of ⇒, mutual implication, distribute/
    /// collect, double negation), deduping states and capped by a search budget. Returns a
    /// replayable, checkable step list (feed to `proof`/`theorem`/`ident`); throws if no
    /// proof is found within budget. Incomplete by design — handles the routine, not everything.
    let autoproof (e: Prop) : Proof  =        
        let moves =
            [ applyfirst golden_rule
              applyfirst def_implies
              applyfirst mutual_implication
              applyfirst distrib
              applyfirst collect
              applyfirst double_neg ]
        autoproof e prop_calculus simp moves 800 
        
    let autoident (e:Prop) = Proof.autoident autoproof e

    let autodeduce (e:Prop) = Proof.autodeduce autoproof e

    let auto (e:Prop) = Proof.auto Taut autoproof e

    let Auto = Proof.Auto Taut autoproof |> RuleApplication.Auto

    /// Complete, trace-emitting propositional prover via Boolean-ring normal form (ANF): drive the
    /// goal to canonical form with the local admitted rewrites (eliminate ¬/∨/⇒/≡ into ⊕/∧,
    /// distribute ∧ over ⊕, normalize ∧ monomials and ⊕ chains, reduce constants), greedily to a
    /// fixpoint, returning a REAL replayable proof — a valid propositional goal collapses to T.
    /// Complete for the propositional fragment (unlike the heuristic `autoproof` search); unlike
    /// the `valid` oracle it produces a checkable derivation. Throws if the goal is not a
    /// propositional theorem. (Candidate fallback for a complete hybrid `autoproof` — see notes.)
    let autoproof_anf (e: Prop) : Proof =
        let goal = expand e.Expr
        let isComplete x = prop_calculus.AxEquiv x || Proof.Logic.AxEquiv x
        let moves =
            [ applyfirst elim_to_xor
              applyfirst distrib_and_xor
              applyfirst and_normalize
              applyfirst xor_normalize
              applyfirst reduce ]
        match normalize_trace isComplete moves 2000 goal with
        | Some steps -> proof prop_calculus e steps
        | None -> failwithf "decide could not normalize %s to a proof (is it a propositional theorem?)." (prop_calculus.PrintFormula goal)

    /// Decision TOOL (not a proof step): does a proof of this propositional goal exist?
    /// Complete via algebraic normal form — use it to check that an identity is valid before
    /// investing in a hand proof or an `auto` search. It is NOT part of the trusted base and
    /// never closes a proof itself; a proof must still be a real derivation.
    let valid (e:Prop) : bool = EquationalLogic.Anf.is_tautology (expand e.Expr)

    /// Decision TOOL: are two propositional formulas equivalent (does a proof of a = b exist)?
    let equiv (a:Prop) (b:Prop) : bool = EquationalLogic.Anf.equivalent (expand a.Expr) (expand b.Expr)

    (* Derived rules *)
    
    /// T = (p = p)  (Gries 3.3)
    [<DerivedRule "T = (p = p)">]
    let def_true (p:Prop) = id_ax prop_calculus (T == (p == p))  
        
    /// F = (¬p = p)  (Gries 3.15)
    [<DerivedRule "F = (¬p = p)">]
    let def_false (p:Prop) = ident prop_calculus (F == (!!p == p)) [
        apply_right collect
        def_true p |> Commute |> apply_right
    ] 

    /// (p = T) = p  (Gries 3.3)
    [<DerivedRule "(p = T) = p">]
    let ident_eq (p:Prop) = ident prop_calculus ((p == T) == p)  [
        apply_left commute
        apply right_assoc
    ]

    /// p = q = q = p  (Gries 3.2)
    [<DerivedRule "p = q = q = p">]
    let commute_eq (p:Prop) (q:Prop) = ident prop_calculus ( (p == q) == (q == p) ) [apply left_assoc]

    /// p = (q = r) = p = q = r  (Gries 3.1)
    [<DerivedRule "p = (q = r) = p = q = r">]
    let left_assoc_eq (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus ( (p == (q == r)) == (p == q == r)) [apply_right right_assoc]

    /// (p = q) = r = p = (q = r)  (Gries 3.1)
    [<DerivedRule "(p = q) = r = p = (q = r)">]
    let right_assoc_eq (p:Prop) (q:Prop) (r:Prop) = id_ax prop_calculus (((p == q) == r) == (p == (q == r)))

    /// ¬F = T  (Gries 3.13)
    [<DerivedRule "¬F = T">]
    let not_false = ident prop_calculus (!!F == T) [
        apply commute
        def_true F  |> apply_left
        apply right_assoc
        apply_right commute
        apply_right collect
        def_true F |> Commute |> apply_right  
    ]

    /// ¬¬p = p  (Gries 3.12)
    [<DerivedRule "¬¬p = p">]
    let double_negation (p:Prop) = ident prop_calculus ((!!(!!p)) == p) [
         apply collect
         def_false p |> Commute |> apply         
    ]

    /// ¬p = q = p = ¬q  (Gries 3.11)
    [<DerivedRule "¬p = q = p = ¬q">]
    let symm_not_eq (p:Prop) (q:Prop) = ident prop_calculus (!!p == q == p == !!q) [
        collect |> apply_left
        right_assoc |> apply
        collect |> apply_left
        commute |> apply_right
        collect |> apply_right
        commute_eq q p |> apply_right
    ]

    /// (p = q) = (¬p = ¬q)
    [<DerivedRule "(p = q) = (¬p = ¬q)">]
    let symm_eq_not_eq (p:Prop) (q:Prop) = ident prop_calculus (p == q == (!!p == !!q) ) [
        left_assoc |> apply
        commute_eq (p == q) !!p |> apply_left
        commute_eq p q |> apply_left
        left_assoc |> apply_left
        symm_not_eq p q |> Taut' |> apply
    ]

    /// ((p = q) = (r = s)) = ((p = r) = (q = s))
    [<DerivedRule "((p = q) = (r = s)) = ((p = r) = (q = s))">]
    let commute_eq_eq (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p == q) == (r == s)) == ((p == r) == (q == s))) [
        // pure AC rearrangement of ≡ (Gries 3.1/3.2, both axiomatic); normalize collapses it
        apply normalize
    ]

    /// p ∨ q = q ∨ p  (Gries 3.24)
    [<DerivedRule "p ∨ q = q ∨ p">]
    let commute_or (p:Prop) (q:Prop) = id_ax prop_calculus ((p + q) == (q + p))
 
    /// p ∨ (q ∨ r) = p ∨ q ∨ r  (Gries 3.25)
    [<DerivedRule "p ∨ (q ∨ r) = p ∨ q ∨ r">]
    let left_assoc_or (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus ( (p + (q + r)) == ((p + q) + r) ) [apply normalize]

    /// (p ∨ q) ∨ r = p ∨ (q ∨ r)  (Gries 3.25)
    [<DerivedRule "(p ∨ q) ∨ r = p ∨ (q ∨ r)">]
    let right_assoc_or p q r = left_assoc_or p q r |> Commute

    /// ((p ∨ q) ∨ (r ∨ s)) = ((p ∨ r) ∨ (q ∨ s))
    [<DerivedRule "((p ∨ q) ∨ (r ∨ s)) = ((p ∨ r) ∨ (q ∨ s))">]
    let commute_or_or (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p + q) + (r + s)) == ((p + r) + (q + s))) [
        // pure AC rearrangement of ∨; normalize collapses the reassociate/commute chain
        apply normalize
    ]

    /// p ∨ (q = r) = (p ∨ q) = (p ∨ r)  (Gries 3.27)
    [<DerivedRule "p ∨ (q = r) = (p ∨ q) = (p ∨ r)">]
    let distrib_or_eq (p:Prop) (q:Prop) (r:Prop) = id_ax prop_calculus ((p + (q == r)) == ((p + q) == (p + r)))

    /// (p ∨ q) = (p ∨ r) = p ∨ (q ∨ r)  (Gries 3.27)
    [<DerivedRule "(p ∨ q) = (p ∨ r) = p ∨ (q ∨ r)">]
    let collect_or_eq p q r = distrib_or_eq p q r |> Commute

    /// (p ∨ p) = p  (Gries 3.26)
    [<DerivedRule "(p ∨ p) = p">]
    let idemp_or p =  id_ax prop_calculus ((p + p) == p) 
  
    /// (p and p) = p  (Gries 3.38)
    [<DerivedRule "(p and p) = p">]
    let idemp_and p = ident prop_calculus ((p * p) == p) [
        apply_left golden_rule
        apply right_assoc
        idemp_or p |> Taut' |> apply_right
        apply commute 
    ] 

    /// p ∨ T = T  (Gries 3.29)
    [<DerivedRule "p ∨ T = T">]
    let zero_or p = ident prop_calculus ((p + T) == T) [
        def_true p |> apply_right |> left_branch
        distrib |> apply_left
        commute |> apply
    ]

    /// p ∨ F = p  (Gries 3.30)
    [<DerivedRule "p ∨ F = p">]
    let ident_or (p:Prop) = ident prop_calculus ((p + F) == p) [
        def_false p |> apply_right |> left_branch
        apply_left distrib
        apply right_assoc
        idemp_or p |> apply_right
        apply_left excluded_middle
    ]

    /// (p ∨ q) = (p ∨ ¬q = p)  (Gries 3.32)
    [<DerivedRule "(p ∨ q) = (p ∨ ¬q = p)">]
    let ident_or_or_not (p:Prop) q = ident prop_calculus ((p + q) == ((p + !!q) == p)) [
        apply left_assoc
        collect_or_eq p q !!q  |> apply_left
        commute_eq q !!q |> apply_left
        def_false q  |> Commute |> apply_left
        ident_or p  |> apply_left      
    ]

    /// (p ∨ ¬q) = (p = (p ∨ q))
    [<DerivedRule "(p ∨ ¬q) = (p = (p ∨ q))">]
    let ident_or_not_or (p:Prop) (q:Prop) = ident prop_calculus ((p + !!q) == (p == (p + q))) [
        commute |> apply_right
        apply left_assoc
        collect_or_eq p !!q q  |> apply_left
        def_false q |> Commute |> apply_left
        ident_or p |> apply_left
    ]

    
    /// p ∨ (q ∨ r) = ((p ∨ q) ∨ (p ∨ r))  (Gries 3.31)
    [<DerivedRule "p ∨ (q ∨ r) = ((p ∨ q) ∨ (p ∨ r))">]
    let distrib_or_or (p:Prop) (q:Prop) (r:Prop) =  ident prop_calculus ((p + (q + r)) == ((p + q) + (p + r))) [
        idemp_or p |> Commute |> apply_left
        right_assoc |> apply_left
        left_assoc_or p q r |> apply_left
        commute_or p q |> apply_left
        right_assoc_or q p r |> apply_left
        left_assoc |> apply_left
    ]

    /// (p ∨ q) = (p ∨ r) = p ∨ (q ∨ r)  (Gries 3.31)
    [<DerivedRule "(p ∨ q) = (p ∨ r) = p ∨ (q ∨ r)">]
    let collect_or_or p q r = distrib_or_or p q r |> Commute

    /// ¬(p = q) = ¬p = q  (Gries 3.9)
    [<DerivedRule "¬(p = q) = ¬p = q">]
    let distrib_not (p:Prop) (q:Prop) = ident prop_calculus ((-(p == q)) == (-p == q)) [apply right_assoc]

    /// (¬p = q) = ¬(p = q)  (Gries 3.9)
    [<DerivedRule "(¬p = q) = ¬(p = q)">]
    let collect_not p q = distrib_not p q |> Commute

    /// p ≠ q = ¬(p = q)  (Gries 3.10)
    [<DerivedRule "p ≠ q = ¬(p = q)">]
    let def_not_eq (p:Prop) (q:Prop) = ident prop_calculus ((p != q) == (-(p == q))) [
        right_assoc |> apply
    ]

    /// p ≠ q = q ≠ p  (Gries 3.16)
    [<DerivedRule "p ≠ q = q ≠ p">]
    let commute_not_eq (p:Prop) (q:Prop) = ident prop_calculus ((p != q) == (q != p)) [
        def_not_eq p q |> apply_left
        def_not_eq q p |> apply_right
        commute_eq q p |> apply_right
    ]

    /// (p ≠ q) ≠ r = p ≠ (q ≠ r)  (Gries 3.17)
    [<DerivedRule "(p ≠ q) ≠ r = p ≠ (q ≠ r)">]
    let right_assoc_not_eq p q r = ident prop_calculus (((p != q) != r) == (p != (q != r))) [
        def_not_eq p q  |> apply_left
        def_not_eq (!!(p == q)) r |> apply_left
        def_not_eq q r |> apply_right
        def_not_eq p (!!(q == r)) |> apply_right
        distrib_not q r |> apply_right
        left_assoc_eq p !!q r |> apply_right
        commute_eq p !!q |> apply_right
        collect_not q p |> apply_right
        commute_eq q p |> apply_right
    ]

    /// p ≠ (q ≠ r) = (p ≠ q) ≠ r  (Gries 3.17)
    [<DerivedRule "p ≠ (q ≠ r) = (p ≠ q) ≠ r">]
    let left_assoc_not_eq p q r = right_assoc_not_eq p q r |> Commute

    /// (p ≠ q) = (¬p = q)  (Gries 3.14)
    [<DerivedRule "(p ≠ q) = (¬p = q)">]
    let distrib_not_not_eq (p:Prop) (q:Prop) = ident prop_calculus ((p != q) == (!!p == q)) [
        def_not_eq p q |> apply_left
        distrib_not p q |> apply_left
    ]

    /// (p ≠ q) = r = p ≠ (q = r)  (Gries 3.18)
    [<DerivedRule "(p ≠ q) = r = p ≠ (q = r)">]
    let mutual_assoc_not_eq (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus (((p != q) == r) == (p != (q == r))) [
        def_not_eq p q |> apply_left |> left_branch
        distrib_not p q |> apply_left |> left_branch
        def_not_eq p (q == r) |> apply_right
        distrib_not p (q == r) |> apply_right
        right_assoc_eq (!!p) q r |> apply_left
    ]

    /// (p ≠ q) = r = (p = q) ≠ r  (Gries 3.19)
    [<DerivedRule "(p ≠ q) = r = (p = q) ≠ r">]
    let mutual_interchange_not_eq (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus (((p != q) == r) == ((p == q) != r)) [
        def_not_eq p q |> apply_left |> left_branch
        distrib_not p q |> apply_left |> left_branch
        def_not_eq (p == q) r |> apply_right
        distrib_not (p == q) r |> apply_right
        distrib_not p q |> apply_right
    ]


    /// p ∨ ¬p = T  (Gries 3.28)
    [<DerivedRule "p ∨ ¬p = T">]
    let excluded_middle' (p:Prop) = ident prop_calculus ((p + (-p)) == T) [ident_eq (p + (-p)) |> apply]
    
    /// p ∧ q = ((p = q) = (p ∨ q))  (Gries 3.35)
    [<DerivedRule "p ∧ q = ((p = q) = (p ∨ q))">]
    let golden_rule' (p:Prop) (q:Prop) = id_ax prop_calculus ((p * q) == (p == q == (p + q)))

    /// (p ∨ (p ∧ q)) = p  (Gries 3.43b)
    [<DerivedRule "(p ∨ (p ∧ q)) = p">]
    let absorb_or (p:Prop) (q:Prop) = ident prop_calculus (p + (p * q) == p)  [
        golden_rule |> apply_right |> left_branch
        distrib |> apply_left
        left_assoc_or p p q |> apply_left
        idemp_or p |> apply_left
        distrib_or_eq p p q |> apply_left
        idemp_or p |> apply_left
    ]

     /// p ∧ q = q ∧ p  (Gries 3.36)
    [<DerivedRule "p ∧ q = q ∧ p">]
    let commute_and (p:Prop) (q:Prop) = ident prop_calculus ((p * q) == (q * p))  [
        golden_rule' p q |> apply_left
        golden_rule' q p |> apply_right
        commute_or q p |> apply_right
        commute_eq q p |> apply_right
    ]
        
    /// p ∧ q ∧ r == (p == q == r == (p ∨ q) = (q ∨ r) = (r ∨ p) = (p ∨ q ∨ r))  (Gries 3.55)
    [<DerivedRule "p ∧ q ∧ r == (p == q == r == (p ∨ q) == (q ∨ r) == (r ∨ p) == (p ∨ q ∨ r))">]
    let ident_and_eq_all p q r = ident prop_calculus ((p * q * r) == (p == q == r == (p + q) == (q + r) == (r + p) == (p + q + r))) [
        golden_rule' p q |> apply_left |> left_branch
        golden_rule' ( (p == q) == (p + q) ) r |> apply_left 
        commute_or ( ((p == q) == (p + q)) ) r |> apply_left
        distrib_or_eq r ( p == q ) ( p + q ) |> apply_left
        distrib_or_eq r p q |> apply_left
        right_assoc_eq ( p == q ) ( p + q ) r |> apply_left
        commute_eq ( p + q ) r |> apply_left        
        commute_or r q |> apply_right |> left_branch |> right_branch |> left_branch        
        commute_eq ( r + p ) ( q + r ) |> apply_left
        commute_or r ( p + q ) |> apply_left
        left_assoc_eq ( p == q ) r ( p + q ) |> apply_left
        left_assoc |> apply_left        
        left_assoc_eq (p == q == r == (p + q)) (q + r) (r + p) |> apply_left |> left_branch
    ]
    
    /// p ∧ q ∧ r == p ∧ (q ∧ r)  (Gries 3.37)
    [<DerivedRule "p ∧ q ∧ r == p ∧ (q ∧ r)">]
    let right_assoc_and p q r = ident prop_calculus ((p * q * r) == (p * (q * r))) [
        ident_and_eq_all p q r |> apply_left
        commute_and p ( q * r ) |> apply_right
        ident_and_eq_all q r p |> apply_right
        commute_eq ( q == r ) p |> apply_right
        left_assoc_eq ( p == q == r == (p + q) ) ( q + r ) ( r + p ) |> apply_left
        left_assoc_eq p q r |> apply_right
        commute_or ( q + r ) p |> apply_right
        left_assoc_or p q r |> apply_right
        right_assoc_eq ( p == q == r ) ( q + r ) ( r + p ) |> apply_right
        left_assoc_eq ( p == q == r )  ( q + r ) ( r + p ) |> apply_right
        right_assoc_eq ( p == q == r == (q + r) ) ( r + p ) ( p + q ) |> apply_right
        commute_eq ( (r + p) ) ( p + q ) |> apply_right
        left_assoc |> apply_right
        left_assoc_eq ( p == q == r == (q + r) ) ( p + q  ) ( r + p ) |> apply_right
        right_assoc_eq ( p == q == r )  ( q + r ) ( p + q ) |> apply_right
        commute_eq ( q + r ) ( p + q ) |> apply_right
        left_assoc_eq ( p == q == r ) ( p + q )  ( (q + r) ) |> apply_right
    ]

    /// p ∧ (q ∧ r) = p ∧ q ∧ r  (Gries 3.37)
    [<DerivedRule "p ∧ (q ∧ r) = p ∧ q ∧ r">]
    let left_assoc_and p q r = right_assoc_and p q r |> Commute
        
    /// p ∧ true = p  (Gries 3.39)
    [<DerivedRule "p ∧ true = p">]
    let ident_and p = ident prop_calculus ( (p * T) == p ) [
        apply_left golden_rule
        apply right_assoc
        zero_or p |> apply_right
        apply_right commute
    ]

    /// p ∧ false = false  (Gries 3.40)
    [<DerivedRule "p ∧ false = false">]
    let zero_and p = ident prop_calculus ( (p * F) == F ) [
      golden_rule' p F |> apply_left
      ident_or p |> apply_left
      apply right_assoc
    ]

    /// p ∧ (q ∧ r) = (p ∧ q) ∧ (p ∧ r)  (Gries 3.41)
    [<DerivedRule "p ∧ (q ∧ r) = (p ∧ q) ∧ (p ∧ r)">]
    let distrib_and p q r = ident prop_calculus ( (p * (q * r)) == ((p * q) * (p * r)) ) [
        idemp_and p |> Commute |> apply_left |> left_branch
        right_assoc |> apply_left
        left_assoc_and p q r |> apply_right |> left_branch
        commute_and p q |> apply_right |> left_branch
        right_assoc_and q p r |> apply_right |> left_branch
        left_assoc |> apply_left
    ]

    /// p ∧ ¬p = F  (Gries 3.42)
    [<DerivedRule "p ∧ ¬p = F">]
    let contr p = ident prop_calculus ( p * -p == F) [
        golden_rule |> apply_left
        excluded_middle |> apply_right |> left_branch
        commute_eq p ( !!p ) |> apply_left
        def_false p |> Commute |> apply_left
        commute_eq F T  |> apply_left
        right_assoc |> apply
    ]

    /// (p ∧ (p ∨ q)) = p  (Gries 3.43a)
    [<DerivedRule "(p ∧ (p ∨ q)) = p">]
    let absorb_and p q = ident prop_calculus ( (p * (p + q)) == p ) [
        apply_left golden_rule
        left_assoc_or p  p  q |> apply_left
        idemp_or p |> apply_left
    ]
    
    /// p ∧ (-p ∨ q) = (p ∧ q)  (Gries 3.44a)
    [<DerivedRule "p ∧ (-p ∨ q) = (p ∧ q)">]
    let absorb_and_not (p:Prop) q = ident prop_calculus (p * ((-p) + q) == (p * q)) [
        golden_rule |> apply_left
        left_assoc_or p -p q |> apply_left
        excluded_middle' p |> apply_left
        zero_or q |> CommuteL |> apply_left
        ident_eq ( p == (-p + q) ) |> apply_left
        commute_or ( !! p ) q |> apply_left
        ident_or_not_or q p |> apply_left
        left_assoc |> apply_left
        commute_or q p |> apply_left
        golden_rule' p q |> Commute |> apply_left
    ]

    /// p ∨ (-p ∧ q) = (p ∨ q)  (Gries 3.44b)
    [<DerivedRule "p ∨ (-p ∧ q) = (p ∨ q)">]
    let absorb_or_not p q = ident prop_calculus (p + (-p * q) == (p + q)) [
        distrib |> apply_left
        excluded_middle |> apply_left |> left_branch
        commute |> apply_left
        ident_and ( p + q ) |> apply_left
    ]
    
    /// p ∨ (q ∧ r) = ((p ∨ q) ∧ (p ∨ r))  (Gries 3.45)
    [<DerivedRule "p ∨ (q ∧ r) = ((p ∨ q) ∧ (p ∨ r))">]
    let distrib_or_and (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus (p + (q * r) == ((p + q) * (p + r))) [
        golden_rule |> apply_right |> left_branch
        distrib |> apply_left
        distrib |> apply_left |> left_branch
        distrib_or_or p q r |> apply_left
        golden_rule' ( p + q ) ( p + r ) |> Commute |> apply_left
    ]

    /// ((p ∨ q) ∧ (p ∨ r)) = p ∨ (q ∧ r)  (Gries 3.45)
    [<DerivedRule "((p ∨ q) ∧ (p ∨ r)) = p ∨ (q ∧ r)">]
    let collect_or_and p q r = distrib_or_and p q r |> Commute

    /// p ∧ (q ∨ r) = ((p ∧ q) ∨ (p ∧ r))  (Gries 3.46)
    [<DerivedRule "p ∧ (q ∨ r) = ((p ∧ q) ∨ (p ∧ r))">]
    let distrib_and_or p q r =  ident prop_calculus ( p * (q + r) == ((p * q) + (p * r)) ) [
        distrib_or_and ( p * q ) p r|> apply_right
        absorb_or p q |> CommuteL |> apply_right
        distrib_or_and r p q |> CommuteL |> apply_right
        left_assoc |> apply_right
        commute_or r p |>apply_right
        absorb_and p r |> apply_right
        commute |> apply_right |> right_branch
    ]
    /// -(p ∧ q) = -p ∨ -q  (Gries 3.47a)
    [<DerivedRule "-(p ∧ q) = -p ∨ -q">]
    let distrib_not_and (p:Prop) (q:Prop) = ident prop_calculus (-(p * q) == (-p + -q)) [
        golden_rule |> apply |> ApplyUnary |> left_branch
        distrib |> apply_left
        distrib |> apply_left |> left_branch 
        ident_or_or_not ( -p ) ( -q ) |> apply_right
        double_negation q |> apply_right
        ident_or_not_or q p |> CommuteL |> apply_right
        commute |> apply_right
        commute_or q p |> apply_right
    ]

    /// -p ∨ -q == -(p ∧ q)   (Gries 3.47a)
    [<DerivedRule "-p ∨ -q = -(p ∧ q)">]
    let collect_not_and p q = distrib_not_and p q |> Commute

    /// -(p ∨ q) = -p ∧ -q  (Gries 3.47b)
    [<DerivedRule "-(p ∨ q) = -p ∧ -q">]
    let distrib_not_or (p:Prop) (q:Prop) = ident prop_calculus (-(p + q) == (-p * -q)) [
        golden_rule' p q |> Commute |> CommuteL |> RightAssoc |> apply_left
        commute |> apply |> ApplyUnary |> left_branch
        distrib |> apply_left
        distrib_not_and p q |> apply_left
        commute |> apply
        symm_eq_not_eq p q |> apply_right
        commute |> apply_right
    ]

    /// -p ∧ -q == -(p ∨ q)  (Gries 3.47b)
    [<DerivedRule "-p ∧ -q = -(p ∨ q)">]
    let collect_not_or p q = distrib_not_or p q |> Commute
    
    /// p ∨ q == (p ∨ -q == p)  (Gries 3.32)
    [<DerivedRule "p ∨ q = (p ∨ -q = p)">]
    let ident_or_or_not_eq (p:Prop) (q:Prop) = ident prop_calculus ( (p + q) == (p + (-q) == p) ) [
        left_assoc |> apply
        collect_or_eq p q (-q)  |> apply
        commute_eq q ( -q ) |> apply_left
        def_false q |> Commute |> apply_left
        ident_or p |> apply_left
    ]

    /// p == q == ((p ∧ q) ∨ (-p ∧ -q))  (Gries 3.52)
    [<DerivedRule "p = q = ((p ∧ q) ∨ (-p ∧ -q))">]
    let ident_eq_and_or_not (p:Prop) (q:Prop) = ident prop_calculus (p == q == ((p * q) + (-p * -q))) [
        ident_or_or_not ( p * q ) ( -p * -q ) |> apply_right
        distrib_not_and ( -p ) ( -q ) |> apply_right
        double_negation p |> apply_right
        double_negation q |> apply_right
        distrib |> apply_left |> right_branch
        absorb_or p q |> CommuteL |> apply_right
        commute_and p q |> apply_right
        absorb_or q p |> CommuteL |> apply_right
        commute_and q p |> apply_right
        left_assoc |> apply
        commute |> apply
    ]

    /// p ≠ q = ((¬p ∧ q) ∨ (p ∧ ¬q))  (Gries 3.53)
    [<DerivedRule "p ≠ q = ((¬p ∧ q) ∨ (p ∧ ¬q))">]
    let ident_not_eq_and_or_not (p:Prop) (q:Prop) = ident prop_calculus ((p != q) == ((!!p * q) + (p * !!q))) [
        double_negation p |> Commute |> apply_right |> right_branch
        ident_eq_and_or_not (!!p) q |> Commute |> apply_right
        distrib_not_not_eq p q |> Commute |> apply_right
    ]

    /// (p = q) ∧ (r = p) = (p = q) ∧ (r = q)  (Gries 3.51)
    [<DerivedRule "(p = q) ∧ (r = p) = (p = q) ∧ (r = q)">]
    let replace_eq (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus (((p == q) * (r == p)) == ((p == q) * (r == q))) [
        subst_and |> apply_left
    ]

    /// p ∧ q == (p ∧ -q == -p)  (Gries 3.48)
    [<DerivedRule "p ∧ q = (p ∧ -q = -p)">]
    let ident_and_and_not (p:Prop) (q:Prop) = ident prop_calculus ((p * q) == (p * -q == -p)) [
        left_assoc |> apply
        golden_rule |> apply_left |> left_branch
        golden_rule' p ( -q ) |> apply_left
        commute |> apply_right |> left_branch
        left_assoc |> apply_right |> left_branch
        ident_or_or_not_eq p q |> Commute |> apply_left
        left_assoc |> apply_left
        right_assoc |> apply_left |> left_branch
        def_true ( p + q ) |> Commute |> apply_left
        commute |> apply_left |> left_branch
        right_assoc |> apply
        commute |> apply_right
        right_assoc |> apply
        symm_eq_not_eq p q |> apply_right
    ]

    /// p ∧ (q == r) = ((p ∧ q) = (p ∧ r) = p)  (Gries 3.49)
    [<DerivedRule "p ∧ (q = r) = ((p ∧ q) = (p ∧ r) = p)">]
    let distrib_and_eq p q r = ident prop_calculus (p * (q == r) == ((p * q) == (p * r) == p)) [
        golden_rule |> apply_left
        distrib |> apply_right |> left_branch
        left_assoc |> apply_left |> left_branch
        left_assoc |> apply_left
        right_assoc |> apply_left
        commute_eq_eq ( p == q ) r ( p + q ) ( p + r ) |> apply_left
        golden_rule' p q |> LeftAssoc |> apply_left
        golden_rule' p r |> LeftAssoc |> LeftAssocBranchLeft |> RightAssoc |> Commute |> apply_left 
        golden_rule' p q |> Commute |> apply_left
        left_assoc |> apply_left
    ]

    /// p ∧ (q == p) = (p ∧ q)  (Gries 3.50)
    [<DerivedRule "p ∧ (q = p) = (p ∧ q)">]
    let ident_and_eq p q  = ident prop_calculus (p * (q == p) == (p * q)) [
        golden_rule |> apply_left
        distrib |> apply_right |> left_branch
        left_assoc |> apply_left |> left_branch
        left_assoc |> apply_left
        right_assoc |> apply_left
        idemp_or p |> apply_left
        commute |> apply_left |> left_branch
        left_assoc |> apply_left |> left_branch
        def_true p |> Commute |> apply_left |> left_branch
        ident_eq q |> CommuteL |> apply_left
        commute |> apply_left
        golden_rule' p q |> Commute |> CommuteL |> LeftAssocBranchLeft |> apply_left
    ]

    /// p ∧ q ∧ (r ∧ s) = p ∧ r ∧ (q ∧ s) 
    [<DerivedRule "p ∧ q ∧ (r ∧ s) = p ∧ r ∧ (q ∧ s)">]
    let commute_and_and (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p * q) * (r * s)) == ((p * r) * (q * s))) [
        // pure AC rearrangement of ∧; normalize collapses the reassociate/commute chain
        apply normalize
    ]

    /// p ⇒ q = (p ∨ q = q)  (Gries 3.57)
    [<DerivedRule "p ⇒ q = (p ∨ q = q)">]
    let def_implies' (p:Prop) (q:Prop) = id_ax prop_calculus ( (p ==> q) == (p + q == q) )

    /// p ⇒ q = (¬p ∨ q)  (Gries 3.59)
    [<DerivedRule "p ⇒ q = (¬p ∨ q)">]
    let ident_implies_not_or p q = ident prop_calculus ( p ==> q == (-p + q) ) [
        def_implies |> apply_left
        ident_or_not_or q p |> CommuteL |> apply_right
        commute |> apply_right
        commute |> apply_left |> right_branch
    ]

    /// p ⇒ q = ((p ∧ q) = p)  (Gries 3.60)
    [<DerivedRule "p ⇒ q = ((p ∧ q) = p)">]
    let ident_implies_eq_and_eq p q = ident prop_calculus (p ==> q == ((p * q) == p)) [
        def_implies |> apply_left
        commute |> apply
        right_assoc |> apply
        commute |> apply_right |> right_branch 
        left_assoc |> apply_right 
    ]

    /// p ∧ (p ⇒ q) = (p ∧ q)  (Gries 3.66)
    [<DerivedRule "p ∧ (p ⇒ q) = (p ∧ q)">]
    let ident_and_implies (p:Prop) (q:Prop) = ident prop_calculus ( p * (p ==> q) == (p * q) ) [
        ident_implies_eq_and_eq p q |> apply_left
        distrib_and_eq p ( p * q ) p |> apply_left
        left_assoc |> apply_left |> left_branch |> left_branch
        // two p ∧ p occurrences; first-match Subst rewrites one per step (see docs/prover-automation.md)
        idemp_and p |> apply_left
        idemp_and p |> apply_left
    ]

    /// p ∨ (q ⇒ p) = (q ⇒ p)  (Gries 3.69)
    [<DerivedRule "p ∨ (q ⇒ p) = (q ⇒ p)">]
    let ident_or_conseq (p:Prop) (q:Prop) = ident prop_calculus ( p + (q ==> p) == (q ==> p) ) [
        def_implies |> apply_right |> left_branch
        distrib |> apply_left
        commute_or q p |> apply_left
        left_assoc_or p p q |> apply_left
        // two p ∨ p occurrences; first-match Subst rewrites one per step
        idemp_or p |> apply_left
        idemp_or p |> apply_left
        commute |> apply
        commute_or p q |> apply_right
    ]

    /// p ∧ (q ⇒ p) = p  (Gries 3.67)
    [<DerivedRule "p ∧ (q ⇒ p) = p">]
    let ident_and_conseq (p:Prop) (q:Prop) = ident prop_calculus (p * (q ==> p) == p) [
        ident_implies_not_or q p |> apply_left
        commute_or (!!q) p |> apply_left
        absorb_and p (!!q) |> apply_left
    ]

    /// (p ∨ q ⇒ p ∧ q) = (p = q)  (Gries 3.70)
    [<DerivedRule "(p ∨ q ⇒ p ∧ q) = (p = q)">]
    let ident_or_implies_and_eq (p:Prop) (q:Prop) = ident prop_calculus (((p + q) ==> (p * q)) == (p == q)) [
        ident_implies_not_or (p + q) (p * q) |> apply_left
        distrib_not_or p q |> apply_left
        commute_or (!!p * !!q) (p * q) |> apply_left
        ident_eq_and_or_not p q |> Commute |> apply_left
    ]

    /// p ⇒ q = (¬q ⇒ ¬p)  (Gries 3.61)
    [<DerivedRule "p ⇒ q = (¬q ⇒ ¬p)">]
    let def_implies_contr p q = ident prop_calculus (p ==> q == (-q ==> -p)) [
        def_implies |> apply_right
        commute |> apply_right
        commute |> apply_right |> right_branch
        distrib_not_and p q |> Commute |> apply_right |> right_branch
        symm_eq_not_eq p ( p * q ) |> Commute |> apply_right 
        commute |> apply_right
        ident_implies_eq_and_eq p q |> Taut' |> apply
    ]

    /// p ⇒ (q = r) = ((p ∧ q) = (p ∧ r))  (Gries 3.62)
    [<DerivedRule "p ⇒ (q = r) = ((p ∧ q) = (p ∧ r))">]
    let distrib_implies_eq_and p q r =
        ident prop_calculus ( p ==> (q == r) == ((p * q) == (p * r))) [
            ident_implies_eq_and_eq p ( q == r ) |> apply_left
            distrib_and_eq p q r |> apply_left
    ]

    /// p ⇒ (q = r) = ((p ⇒ q) = (p ⇒ r))  (Gries 3.63)
    [<DerivedRule "p ⇒ (q = r) = ((p ⇒ q) = (p ⇒ r))">]
    let distrib_implies_eq_implies p q r = ident prop_calculus ( p ==> (q == r) == ((p ==> q) == (p ==> r))) [
        distrib_implies_eq_and p q r |> apply_left
        ident_implies_eq_and_eq p q |> apply_left |> right_branch
        ident_implies_eq_and_eq p r |> apply_right |> right_branch
        commute |> apply_right |> right_branch
        left_assoc |> apply_right 
        right_assoc |> apply_left |> right_branch
        def_true p |> Commute |> apply_right
        ident_eq ( p * q ) |> apply_left |> right_branch
    ]

    /// p ⇒ (q ⇒ r) = ((p ⇒ q) ⇒ (p ⇒ r))  (Gries 3.64)
    [<DerivedRule "p ⇒ (q ⇒ r) = ((p ⇒ q) ⇒ (p ⇒ r))">]
    let self_distrib_implies (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus ((p ==> (q ==> r)) == ((p ==> q) ==> (p ==> r))) [
        ident_implies_not_or p (q ==> r) |> apply_left
        ident_implies_not_or q r |> apply_left
        left_assoc_or (!!p) (!!q) r |> apply_left
        ident_implies_not_or (p ==> q) (p ==> r) |> apply_right
        ident_implies_not_or p q |> apply_right
        ident_implies_not_or p r |> apply_right
        distrib_not_or (!!p) q |> apply_right
        double_negation p |> apply_right
        left_assoc_or (p * !!q) (!!p) r |> apply_right
        commute_or (p * !!q) (!!p) |> apply_right
        distrib_or_and (!!p) p (!!q) |> apply_right
        commute_or (!!p) p |> apply_right
        excluded_middle' p |> apply_right
        commute |> apply_left |> right_branch
        ident_and (!!p + !!q) |> apply_right
    ]

    /// (T ⇒ p) = p  (Gries 3.73)
    [<DerivedRule "(T ⇒ p) = p">]
    let ident_conseq_true p = ident prop_calculus ((T ==> p) == p) [
        def_implies |> apply_left
        zero_or p |> CommuteL |> apply_left
        right_assoc |> apply
        commute |> apply
    ]

    /// p ⇒ F = ¬p  (Gries 3.74)
    [<DerivedRule "p ⇒ F = ¬p">]
    let ident_implies_false_not (p:Prop) = ident prop_calculus ((p ==> F) == -p) [
        def_implies |> apply_left
        ident_or p |> apply_left
        commute |> apply
        left_assoc |> apply
        commute |> apply
        def_false p |> apply
    ]
    
    /// (¬p ⇒ F) = p  (reductio: Gries 3.74 with p:=¬p, then double negation) — the identity
    /// underpinning proof by contradiction.
    [<DerivedRule "(¬p ⇒ F) = p">]
    let contradiction_id (p:Prop) = ident prop_calculus ((!!p ==> F) == p) [
        ident_implies_false_not (!!p) |> apply_left
        double_negation p |> apply_left
    ]

    /// p ∧ q ⇒ r = (p ⇒ (q ⇒ r))  (Gries 3.65)
    [<DerivedRule "p ∧ q ⇒ r = (p ⇒ (q ⇒ r))">]
    let shunt' p q r = ident prop_calculus (p * q ==> r == (p ==> (q ==> r))) [
        ident_implies_eq_and_eq ( p * q ) r |> apply_left
        ident_implies_eq_and_eq q r |> apply_right
        ident_implies_eq_and_eq p ( q * r == q ) |> apply_right
        distrib_and_eq p ( q * r ) q |> apply_right
        left_assoc_and p q r |> apply_right
        right_assoc |> apply_right
        def_true p |> Commute |> apply_right
        left_assoc |> apply
        commute |> apply
    ]
    
    /// (p ⇒ r) ∧ (q ⇒ r) = (p ∨ q ⇒ r)  (Gries 3.78)
    [<DerivedRule "(p ⇒ r) ∧ (q ⇒ r) = (p ∨ q ⇒ r)">]
    let case_analysis_1 p q r = ident prop_calculus (( p ==> r) * (q ==> r) == (p + q  ==> r) ) [
        ident_implies_not_or ( p + q ) r |> apply_right
        distrib|> apply_left |> right_branch
        distrib_or_and r ( -p ) ( -q ) |> CommuteL |> apply_right
        commute |> apply_left |> right_branch
        commute |> apply_right |> right_branch
        ident_implies_not_or p r |> Commute |> apply_right
        ident_implies_not_or q r |> Commute |> apply_right
    ]

    /// (p ⇒ r) ∧ (¬p ⇒ r) = r  (Gries 3.79)
    [<DerivedRule "(p ⇒ r) ∧ (¬p ⇒ r) = r">]
    let case_analysis_2 p r = ident prop_calculus ((p ==> r) * (-p ==> r) == r) [
        case_analysis_1 p -p r |> apply
        excluded_middle |> apply_left |> left_branch
        ident_conseq_true r |> Taut' |> apply
    ]

    /// Proof by contradiction (Gries §4.2): from a proof of ¬P ⇒ F, conclude the theorem P.
    let Contradiction (t:Theorem) =
        Tactics.Contradiction prop_calculus Taut Commute (fun pe -> contradiction_id (pe |> expand_as<bool> |> Prop)) t

    /// Proof by cases (Gries 3.79 / §4.2): from proofs of Q ⇒ P and ¬Q ⇒ P, conclude the theorem P.
    let Cases (t1:Theorem) (t2:Theorem) =
        Tactics.Cases prop_calculus Taut Commute reduce (fun qe pe -> case_analysis_2 (qe |> expand_as<bool> |> Prop) (pe |> expand_as<bool> |> Prop)) t1 t2

    /// (p ⇒ q) ∧ (q ⇒ p) = (p == q)  (Gries 3.80)
    [<DerivedRule "(p ⇒ q) ∧ (q ⇒ p) = (p = q)">]
    let mutual_implication' (p:Prop) (q:Prop) = ident prop_calculus (((p ==> q) * (q ==> p)) == (p == q)) [
        right_assoc |> apply
        ident_implies_not_or p q |> apply_left
        ident_implies_not_or q p |> apply_left  
        distrib |> apply_left  
        commute |> apply_left |> left_branch |> left_branch
        commute |> apply_right |> left_branch
        distrib |> apply_left |> left_branch
        distrib |> apply_right |> left_branch
        distrib |> apply_left |> left_branch |> left_branch
        commute |> apply_left |> left_branch
        distrib |> apply_left |> left_branch
        contr q |> CommuteL |> apply_left
        contr p |> apply_left
        ident_or ( p * q ) |> CommuteL |> apply_left
        ident_or ( -q * -p ) |> CommuteL |> apply_left
        commute |> apply_left |> left_branch
        commute |> apply
        commute |> apply_right
        ident_eq_and_or_not p q |> apply_left
    ]
        
    (* Theorems *)

    /// p ∨ (p ⇒ q)  (Gries 3.68)
    [<Theorem "p ∨ (p ⇒ q)">]
    let or_implies (p:Prop) (q:Prop) = theorem prop_calculus ( (p + (p ==> q)) == T ) [
        def_implies |> apply_right |> left_branch
        distrib |> apply_left
        left_assoc |> apply_left |> left_branch
        idemp_or p |> apply_left
        ident_eq ((p + q) == (p + q)) |> apply
    ]

    /// p ⇒ p  (Gries 3.71)
    [<Theorem "p ⇒ p">]
    let reflex_implies p = theorem prop_calculus ( p ==> p ) [
        def_implies |> apply
    ]
        
    /// p ⇒ true  (Gries 3.72)
    [<Theorem "p ⇒ true">]
    let implies_true p = theorem prop_calculus (p ==> T) [
        def_implies |> apply
        zero_or p |> apply_left
    ]

    /// false ⇒ p  (Gries 3.75)
    [<Theorem "false ⇒ p">]
    let conseq_false (p:Prop) = theorem prop_calculus (F ==> p) [
        def_implies |> apply
        ident_or p |> CommuteL |> Taut' |> apply
    ]

    /// (p ∧ q) ⇒ p  (Gries 3.76b)
    [<Theorem "(p ∧ q) ⇒ p">]
    let strengthen_and p q = theorem prop_calculus ((p * q) ==> p) [
        ident_eq ( ((p * q ) ==> p) ) |> apply
        def_implies |> apply
        commute |> apply_left
        absorb_or p q |> Taut' |> apply
    ]
    
    /// p ⇒ p ∨ q   (Gries 3.76a)
    [<Theorem "p ⇒ p ∨ q">]
    let weaken_or p q = theorem prop_calculus ( p ==> (p + q) ) [
        ident_eq ( (p ==> (p + q)) ) |> apply
        def_implies |> apply
        left_assoc |> apply_left
        idemp_or p |> apply_left
    ]

    /// p ∧ q ⇒ p ∨ q  (Gries 3.76c)
    [<Theorem "p ∧ q ⇒ p ∨ q">]
    let weaken_and_or (p:Prop) (q:Prop) = theorem prop_calculus ( p * q ==> p + q ) [
        def_implies |> apply
        left_assoc_or ( p * q ) p q |> apply_left
        commute_or ( p * q ) p |> apply_left |> left_branch
        absorb_or p q |> apply_left |> left_branch
    ]

    /// (p ∨ (q ∧ r)) ⇒ (p ∨ q)  (Gries 3.76d)
    [<Theorem "(p ∨ (q ∧ r)) ⇒ (p ∨ q)">]
    let weaken_or_and (p:Prop) q r = theorem prop_calculus ( (p + (q * r)) ==> (p + q) ) [
        distrib |> apply_left
        strengthen_and ( p + q ) ( p + r ) |> Taut |> apply
    ]

    /// (p ∧ q) ⇒ (p ∧ (q ∨ r))  (Gries 3.76e)
    [<Theorem "(p ∧ q) ⇒ (p ∧ (q ∨ r))">]
    let weaken_and_and_or p (q:Prop) (r:Prop) = theorem prop_calculus ((p * q)  ==> (p * (q + r)) ) [
        distrib |> apply_right
        weaken_or ( p * q ) ( p * r ) |> Taut |> apply
    ]

    /// p ∧ (p ⇒ q) ⇒ q  (Gries 3.77)
    [<Theorem "p ∧ (p ⇒ q) ⇒ q">]
    let modus_ponens p q = theorem prop_calculus ( p * (p ==> q) ==> q ) [
        ident_and_implies p q |> apply_left
        commute_and p q |> apply
        strengthen_and q p |> Taut |> apply
    ]
    /// (p ⇒ q) ∧ (q ⇒ p) ⇒ (p = q)  (Gries 3.81)
    [<Theorem "(p ⇒ q) ∧ (q ⇒ p) ⇒ (p = q)">]
    let antisymm_implies p q = theorem prop_calculus ((p ==> q) * (q ==> p) ==> (p == q)) [
        mutual_implication' p q |> apply_left  
        reflex_implies ( p == q ) |> Taut |> apply
    ]

    /// (p ⇒ q) ∧ (q ⇒ r) ⇒ (p ⇒ r)  (Gries 3.82a)
    [<Theorem "(p ⇒ q) ∧ (q ⇒ r) ⇒ (p ⇒ r)">]
    let trans_implies p q r = theorem prop_calculus ((p ==> q) * (q ==> r) ==> (p ==> r)) [
        rshunt |> apply
        commute |> apply_left
        left_assoc |> apply_left
        ident_and_implies p q |> apply_left
        right_assoc |> apply_left
        ident_and_implies q r |> apply_left
        commute |> apply_left
        commute |> apply_left |> left_branch
        right_assoc |> apply_left
        strengthen_and r ( q * p ) |> Taut |> apply
    ]

    /// (p = q) ∧ (q ⇒ r) ⇒ (p ⇒ r)  (Gries 3.82b)
    [<Theorem "(p = q) ∧ (q ⇒ r) ⇒ (p ⇒ r)">]
    let trans_implies_eq (p:Prop) (q:Prop) (r:Prop) = theorem prop_calculus ((p == q) * (q ==> r) ==> (p ==> r)) [
        mutual_implication' p q |> Commute |> apply_left
        rshunt |> apply
        commute |> apply_left
        left_assoc |> apply_left
        left_assoc |> apply_left |> left_branch
        ident_and_implies p q |> apply_left |> left_branch
        right_assoc |> apply_left |> left_branch
        ident_and_implies q p |> apply_left |> left_branch
        commute |> apply_right |> left_branch |> left_branch
        left_assoc |> apply_left |> left_branch
        idemp_and p |> apply_left |> left_branch
        right_assoc |> apply_left
        ident_and_implies q r |> apply_left
        left_assoc |> apply_left
        commute |> apply_left
        strengthen_and r ( p * q ) |> Taut |> apply
    ]

    /// (p ⇒ q) ∧ (q = r) ⇒ (p ⇒ r)  (Gries 3.82c)
    [<Theorem "(p ⇒ q) ∧ (q = r) ⇒ (p ⇒ r)">]
    let trans_implies_eq_conseq (p:Prop) (q:Prop) (r:Prop) = theorem prop_calculus ((p ==> q) * (q == r) ==> (p ==> r)) [
        mutual_implication' q r |> Commute |> apply_left
        left_assoc_and (p ==> q) (q ==> r) (r ==> q) |> apply_left
        commute |> apply_left
        shunt |> apply
        trans_implies p q r |> Taut |> apply_right
    ]


    /// p ⇒ (q ⇒ p)
    [<Theorem "p ⇒ (q ⇒ p)">]
    let trans_implies_implies p q = theorem prop_calculus (p ==> (q ==> p)) [
        def_implies |> apply_right
        def_implies |> apply
        commute |> apply_left |> right_branch |> left_branch 
        distrib |> apply_left
        left_assoc |> apply_left |> left_branch
        idemp_or p |> apply_left |> left_branch |> left_branch
        commute |> apply_left |> left_branch 
        idemp_or p |> apply_right |> left_branch
    ]

    /// (p ⇒ q) ⇒ ((p ∨ r) ⇒ (q ∨ r))
    [<Theorem "(p ⇒ q) ⇒ ((p ∨ r) ⇒ (q ∨ r))">]
    let mono_or (p:PropVar) (q:Prop) (r:PropVar) = theorem prop_calculus ((p ==> q) ==> ((p + r) ==> (q + r))) [
        def_implies |> apply_right
        commute_or_or p r q r |> apply_left |> right_branch
        idemp_or r |> apply_left |> right_branch
        commute_or ( p + q ) r |> apply_left |> right_branch
        commute_or q r |> apply_right |> right_branch
        collect_or_eq r ( p + q ) q |> apply_right
        commute |> apply_right
        def_implies' p q |> Commute |> apply_right
        weaken_or ( p ==> q ) r |> Taut |> apply
    ]

    // NOTE: Shannon's expansion (Gries 3.89), E_z = (p ∧ E[z:=true]) ∨ (¬p ∧ E[z:=false]),
    // is intentionally NOT formalized here. It is schematic in an arbitrary expression E with
    // the textual-substitution metavariable E[z:=…], which the prover does not yet support as
    // first-class machinery (the subst_true/subst_false/subst_or_and admissible rules only cover
    // restricted, structurally-matched cases). It is not needed by any theorem in this module and
    // its uses can be discharged by other propositional theorems, so it is left unimplemented
    // pending metavariable support in the kernel.

    (* Module information members *)

    type private IModuleTypeLocator = interface end
    
    let Type = match typeof<IModuleTypeLocator>.DeclaringType with | NonNull m -> m | _ -> failwith "Failed to locate module type."

    let Axioms = [|
        "T"        
        "F = ¬T"
        "p = p" 
        "p <> q = ¬(p = q)"
        "(p = q) = r = p = (q = r)"
        "(p ∨ q) ∨ r = p ∨ (q ∨ r)"
        "p = q = q = p"
        "p ∨ q = q ∨ p"
        "p ∨ (q = r) = (p ∨ q) = (p ∨ r)"  
        "¬(p = q) = ¬p = q"
        "p ∨ p = p"
        "p ∨ ¬p"
        "p ∧ q = p = q = p ∨ q"
        "p ⇒ q = ((p ∨ q) = q)"
        "(e = f) ⇒ E(e) = E(f)"
    |]