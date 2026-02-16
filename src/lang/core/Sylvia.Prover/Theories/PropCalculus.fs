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
     
    (* Tactics for rules *)

    /// The constant true is a theorem
    let Truth = Tactics.Truth commute

    /// If A is a theorem then so is A = true.
    let Taut :Theorem->Rule=  
        let ieq p = 
            let stmt = <@@ ((%%p) = true) = (%%p) @@> in Theorem(stmt, Proof (stmt, prop_calculus, [apply_left commute; apply right_assoc], true)) |> Ident  
        Tactics.Taut ieq
    
    /// If A = B is a theorem then so is (A = B) = true.
    let Taut' t = 
        let ieq p = Theorem(<@@ ((%%p) = true) = (%%p) @@>, Proof (<@@ (%%p = true) = %%p @@>, prop_calculus, [apply_left commute; apply right_assoc], true)) |> Ident 
        Tactics.Taut' ieq t
        
    let Lemma :Theorem->RuleApplication = Taut >> Truth >> Apply
    
    let Lemma' = Taut' >> Truth >> Apply

    /// If A = B is a theorem then so is B = A.
    let Commute = Tactics.Commute commute
    
    /// If (L = R) = B is a theorem then so is (R = L) = B.
    let CommuteL = Tactics.CommuteL commute

    /// If A = (L = R) is a theorem then so is A = (R = L).
    let CommuteR = Tactics.CommuteR commute

    let LeftAssoc = Tactics.LeftAssoc right_assoc

    let LeftAssocRecurseLeft = Tactics.LeftAssocRecurseLeft right_assoc

    let LeftAssocRecurseRight = Tactics.LeftAssocRecurseRight right_assoc

    let RightAssoc = Tactics.RightAssoc left_assoc

    let RightAssocRecurseLeft = Tactics.RightAssocRecurseLeft left_assoc

    let RightAssocRecurseRight = Tactics.RightAssocRecurseRight left_assoc

    (* Tactics for proofs *)
    
    let MutualImplication stmt = Tactics.MutualImplication prop_calculus Taut mutual_implication stmt

    (* Derived rules *)
    
    /// true = (p = p)
    [<DerivedRule "true = (p = p)">]
    let def_true (p:Prop) = id_ax prop_calculus (T == (p == p))  
        
    /// false = (-p = p)
    [<DerivedRule "false = (-p = p)">]
    let def_false (p:Prop) = ident prop_calculus (F == (!!p == p)) [
        apply_right collect
        def_true p |> Commute |> apply_right
    ] 

    /// (p = true) = p
    [<DerivedRule "(p = true) = p">]
    let ident_eq (p:Prop) = ident prop_calculus ((p == T) == p)  [
        apply_left commute
        apply right_assoc
    ]

    /// p = q = q = p
    [<DerivedRule "p = q = q = p">]
    let commute_eq (p:Prop) (q:Prop) = ident prop_calculus ( (p == q) == (q == p) ) [apply left_assoc]

    /// p = (q = r) = p = q = r
    [<DerivedRule "p = (q = r) = p = q = r">]
    let left_assoc_eq (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus ( (p == (q == r)) == ((p == q) == r) ) [apply_right right_assoc]

    /// (p = q) = r = p = (q = r)
    [<DerivedRule "(p = q) = r = p = (q = r)">]
    let right_assoc_eq (p:Prop) (q:Prop) (r:Prop) = id_ax prop_calculus (((p == q) == r) == (p == (q == r)))

    /// -false = true
    [<DerivedRule "-false = true">]
    let not_false = ident prop_calculus (!!F == T) [
        apply commute
        def_true F  |> apply_left
        apply right_assoc
        apply_right commute
        apply_right collect
        def_true F |> Commute |> apply_right  
    ]

    /// --p = p
    [<DerivedRule "--p = p">]
    let double_negation (p:Prop) = ident prop_calculus ((!!(!!p)) == p) [
         apply collect
         def_false p |> Commute |> apply
         not_false |> Truth |> apply
    ]

    /// -p = q = p = -q
    [<DerivedRule "-p = q = p = -q">]
    let symm_not_eq (p:Prop) (q:Prop) = ident prop_calculus (!!p == q == p == !!q) [
        collect |> apply_left
        right_assoc |> apply
        collect |> apply_left
        commute |> apply_right
        collect |> apply_right
        commute_eq q p |> apply_right
    ]

    /// (p = q) = (-p = -q)
    [<DerivedRule "(p = q) = (-p = -q)">]
    let symm_eq_not_eq (p:Prop) (q:Prop) = ident prop_calculus (p == q == (!!p == !!q) ) [
        left_assoc |> apply
        commute_eq (p == q) !!p |> apply_left
        commute_eq p q |> apply_left
        left_assoc |> apply_left
        symm_not_eq p q |> Lemma'
    ]

    /// ((p = q) = (r = s)) = ((p = r) = (q = s))
    [<DerivedRule "((p = q) = (r = s)) = ((p = r) = (q = s))">]
    let commute_eq_eq (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p == q) == (r == s)) == ((p == r) == (q == s))) [
        left_assoc_eq (p == q) r s |> apply_left
        right_assoc_eq p q r |> apply_left
        commute_eq q r |> apply_left
        left_assoc_eq p r q |> apply_left
    ]

    /// p ∨ q = q ∨ p
    [<DerivedRule "p ∨ q = q ∨ p">]
    let commute_or (p:Prop) (q:Prop) = id_ax prop_calculus ((p + q) == (q + p))
 
    /// p ∨ (q ∨ r) = p ∨ q ∨ r
    [<DerivedRule "p ∨ (q ∨ r) = p ∨ q ∨ r">]
    let left_assoc_or (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus ( (p + (q + r)) == ((p + q) + r) ) [apply left_assoc; apply commute]

    /// (p ∨ q) ∨ r = p ∨ (q ∨ r)
    [<DerivedRule "(p ∨ q) ∨ r = p ∨ (q ∨ r)">]
    let right_assoc_or p q r = left_assoc_or p q r |> Commute

    /// ((p ∨ q) ∨ (r ∨ s)) = ((p ∨ r) ∨ (q ∨ s))
    [<DerivedRule "((p ∨ q) ∨ (r ∨ s)) = ((p ∨ r) ∨ (q ∨ s))">]
    let commute_or_or (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p + q) + (r + s)) == ((p + r) + (q + s))) [
        left_assoc_or (p + q) r s |> apply_left
        right_assoc_or p q r |> apply_left
        commute_or q r |> apply_left
        left_assoc_or p r q |> apply_left
    ]

    /// p ∨ (q = r) = (p ∨ q) = (p ∨ r)
    [<DerivedRule "p ∨ (q = r) = (p ∨ q) = (p ∨ r)">]
    let distrib_or_eq (p:Prop) (q:Prop) (r:Prop) = id_ax prop_calculus ((p + (q == r)) == ((p + q) == (p + r)))

    /// (p ∨ q) = (p ∨ r) = p ∨ (q ∨ r)
    [<DerivedRule "(p ∨ q) = (p ∨ r) = p ∨ (q ∨ r)">]
    let collect_or_eq p q r = distrib_or_eq p q r |> Commute

    /// (p ∨ p) = p
    [<DerivedRule "(p ∨ p) = p">]
    let idemp_or p =  id_ax prop_calculus ((p + p) == p) 
  
    /// (p and p) = p
    [<DerivedRule "(p and p) = p">]
    let idemp_and p = ident prop_calculus ((p * p) == p) [
        apply_left golden_rule
        apply right_assoc
        idemp_or p |> Taut' |> apply_right
        apply commute 
    ] 

    /// p ∨ true = true
    [<DerivedRule "p ∨ true = true">]
    let zero_or p = ident prop_calculus ((p + T) == T) [
        def_true p |> apply_right |> branch_left
        distrib |> apply_left
        commute |> apply
    ]

    /// p ∨ false = p
    [<DerivedRule "p ∨ false = p">]
    let ident_or (p:Prop) = ident prop_calculus ((p + F) == p) [
        def_false p |> apply_right |> branch_left
        apply_left distrib
        apply right_assoc
        idemp_or p |> apply_right
        apply_left excluded_middle
    ]

    /// (p ∨ q) = (p ∨ -q = p)
    [<DerivedRule "(p ∨ q) = (p ∨ -q = p)">]
    let ident_or_or_not (p:Prop) q = ident prop_calculus ((p + q) == ((p + !!q) == p)) [
        apply left_assoc
        collect_or_eq p q !!q  |> apply_left
        commute_eq q !!q |> apply_left
        def_false q  |> Commute |> apply_left
        ident_or p  |> apply_left      
    ]

    /// (p ∨ -q) = (p = (p or q))
    [<DerivedRule "(p ∨ -q) = (p = (p or q))">]
    let ident_or_not_or (p:Prop) (q:Prop) = ident prop_calculus ((p + !!q) == (p == (p + q))) [
        commute |> apply_right
        apply left_assoc
        collect_or_eq p !!q q  |> apply_left
        def_false q |> Commute |> apply_left
        ident_or p |> apply_left
    ]

    
    /// p ∨ (q ∨ r) = ((p ∨ q) ∨ (p ∨ r))
    [<DerivedRule "p ∨ (q ∨ r) = ((p ∨ q) ∨ (p ∨ r))">]
    let distrib_or_or (p:Prop) (q:Prop) (r:Prop) =  ident prop_calculus ((p + (q + r)) == ((p + q) + (p + r))) [
        idemp_or p |> Commute |> apply_left
        right_assoc |> apply_left
        left_assoc_or p q r |> apply_left
        commute_or p q |> apply_left
        right_assoc_or q p r |> apply_left
        left_assoc |> apply_left
    ]

    /// (p ∨ q) = (p ∨ r) = p ∨ (q ∨ r)
    [<DerivedRule "(p ∨ q) = (p ∨ r) = p ∨ (q ∨ r)">]
    let collect_or_or p q r = distrib_or_or p q r |> Commute

    /// -(p = q) = -p = q
    [<DerivedRule "-(p = q) = -p = q">]
    let distrib_not (p:Prop) (q:Prop) = ident prop_calculus ((-(p == q)) == (-p == q)) [apply right_assoc]

    /// (-p = q) = -(p = q) 
    [<DerivedRule "(-p = q) = -(p = q)">]
    let collect_not p q = distrib_not p q |> Commute

    /// p <> q = -(p = q)
    [<DerivedRule "p <> q = -(p = q)">]
    let def_not_eq (p:Prop) (q:Prop) = ident prop_calculus ((p != q) == (-(p == q))) [
        right_assoc |> apply
    ]

    /// p <> q = q <> p
    [<DerivedRule "p <> q = q <> p">]
    let commute_not_eq (p:Prop) (q:Prop) = ident prop_calculus ((p != q) == (q != p)) [
        def_not_eq p q |> apply_left
        def_not_eq q p |> apply_right
        commute_eq q p |> apply_right
    ]

    /// (p <> q) <> r = p <> (q <> r)
    [<DerivedRule "(p <> q) <> r = p <> (q <> r)">]
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

    /// p <> (q <> r) = (p <> q) <> r  
    [<DerivedRule "p <> (q <> r) = (p <> q) <> r">]
    let left_assoc_not_eq p q r = right_assoc_not_eq p q r |> Commute

    
    /// p ∨ -p = true
    [<DerivedRule "p ∨ -p = true">]
    let excluded_middle' (p:Prop) = ident prop_calculus ((p + (-p)) == T) [ident_eq (p + (-p)) |> apply]
    
    /// p ∧ q = ((p = q) = (p ∨ q))
    [<DerivedRule "p ∧ q = ((p = q) = (p ∨ q))">]
    let golden_rule' (p:Prop) (q:Prop) = id_ax prop_calculus ((p * q) == (p == q == (p + q)))

    /// (p ∨ (p ∧ q)) = p
    [<DerivedRule "(p ∨ (p ∧ q)) = p">]
    let absorb_or (p:Prop) (q:Prop) = ident prop_calculus (p + (p * q) == p)  [
        golden_rule |> apply_right |> branch_left
        distrib |> apply_left
        left_assoc_or p p q |> apply_left
        idemp_or p |> apply_left
        distrib_or_eq p p q |> apply_left
        idemp_or p |> apply_left
    ]

     /// p ∧ q = q ∧ p
    [<DerivedRule "p ∧ q = q ∧ p">]
    let commute_and (p:Prop) (q:Prop) = ident prop_calculus ((p * q) == (q * p))  [
        golden_rule' p q |> apply_left
        golden_rule' q p |> apply_right
        commute_or q p |> apply_right
        commute_eq q p |> apply_right
    ]
        
    /// p ∧ q ∧ r == (p == q == r == (p ∨ q) = (q ∨ r) = (r ∨ p) = (p ∨ q ∨ r))
    [<DerivedRule "p ∧ q ∧ r == (p == q == r == (p ∨ q) = (q ∨ r) = (r ∨ p) = (p ∨ q ∨ r))">]
    let ident_and_eq_all p q r = ident prop_calculus ( (p * q * r) == (p == q == r == (p + q) == (q + r) == (r + p) == (p + q + r)) ) [
        golden_rule' p q |> apply_left
        golden_rule' ( (p == q) == (p + q) ) r |> apply_left 
        commute_or ( ((p == q) == (p + q)) ) r |> apply_left
        distrib_or_eq r ( p == q ) ( p + q ) |> apply_left
        distrib_or_eq r p q |> apply_left
        right_assoc_eq ( p == q ) ( p + q ) r |> apply_left
        commute_eq ( p + q ) r |> apply_left
        commute_or r q |> apply_left
        commute_eq ( r + p ) ( q + r ) |> apply_left
        commute_or r ( p + q ) |> apply_left
        left_assoc_eq ( p == q ) r ( p + q ) |> apply_left
        left_assoc |> apply_left
        left_assoc_eq ( p == q == r == (p + q) ) ( q + r ) ( r + p ) |> apply_left
    ]
    
    /// p ∧ q ∧ r == p ∧ (q ∧ r)
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

    /// p ∧ (q ∧ r) = p ∧ q ∧ r
    [<DerivedRule "p ∧ (q ∧ r) = p ∧ q ∧ r">]
    let left_assoc_and p q r = right_assoc_and p q r |> Commute
        
    /// p ∧ true = p
    [<DerivedRule "p ∧ true = p">]
    let ident_and p = ident prop_calculus ( (p * T) == p ) [
        apply_left golden_rule
        apply right_assoc
        zero_or p |> apply_right
        apply_right commute
    ]

    /// p ∧ false = false
    [<DerivedRule "p ∧ false = false">]
    let zero_and p = ident prop_calculus ( (p * F) == F ) [
      golden_rule' p F |> apply_left
      ident_or p |> apply_left
      apply right_assoc
    ]

    /// p ∧ (q ∧ r) = (p ∧ q) ∧ (p ∧ r)
    [<DerivedRule "p ∧ (q ∧ r) = (p ∧ q) ∧ (p ∧ r)">]
    let distrib_and p q r = ident prop_calculus ( (p * (q * r)) == ((p * q) * (p * r)) ) [
        idemp_and p |> Commute |> apply_left |> branch_left
        right_assoc |> apply_left
        left_assoc_and p q r |> apply_right |> branch_left
        commute_and p q |> apply_right |> branch_left
        right_assoc_and q p r |> apply_right |> branch_left
        left_assoc |> apply_left
    ]

    /// p ∧ -p == false
    [<DerivedRule "p ∧ -p == false">]
    let contr p = ident prop_calculus ( p * -p == F) [
        golden_rule |> apply_left
        excluded_middle |> apply_right |> branch_left
        commute_eq p ( !!p ) |> apply_left
        def_false p |> Commute |> apply_left
        commute_eq F T  |> apply_left
        right_assoc |> apply
    ]

    /// (p ∧ (p ∨ q)) = p
    [<DerivedRule "(p ∧ (p ∨ q)) = p">]
    let absorb_and p q = ident prop_calculus ( (p * (p + q)) == p ) [
        apply_left golden_rule
        left_assoc_or p  p  q |> apply_left
        idemp_or p |> apply_left
    ]
    
    /// p ∧ (-p ∨ q) = (p ∧ q)
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

    /// p ∨ (-p ∧ q) = (p ∨ q)
    [<DerivedRule "p ∨ (-p ∧ q) = (p ∨ q)">]
    let absorb_or_not p q = ident prop_calculus (p + (-p * q) == (p + q)) [
        golden_rule |> apply_left
        commute_or ( -p ) q  |> apply_left
        right_assoc_eq ( -p ) q  ( q + -p ) |> apply_left
        ident_or_or_not q  p |> Commute |> CommuteL |> apply_left
        distrib |> apply_left 
        commute_or q p |> apply_left
        left_assoc_or p p q |> apply_left
        idemp_or p |> apply_left
        excluded_middle |> apply_left
        ident_eq ( p + q ) |> CommuteL |> apply_left
    ]
    
    /// p ∨ (q ∧ r) = ((p ∨ q) ∧ (p ∨ r))
    [<DerivedRule "p ∨ (q ∧ r) = ((p ∨ q) ∧ (p ∨ r))">]
    let distrib_or_and (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus (p + (q * r) == ((p + q) * (p + r))) [
        golden_rule |> apply_right |> branch_left
        distrib |> apply_left
        distrib |> apply_left |> branch_left
        distrib_or_or p q r |> apply_left
        golden_rule' ( p + q ) ( p + r ) |> Commute |> apply_left
    ]

    /// ((p ∨ q) ∧ (p ∨ r)) = p ∨ (q ∧ r)
    [<DerivedRule "((p ∨ q) ∧ (p ∨ r)) = p ∨ (q ∧ r)">]
    let collect_or_and p q r = distrib_or_and p q r |> Commute

    /// p ∧ (q ∨ r) = ((p ∧ q) ∨ (p ∧ r))
    [<DerivedRule "p ∧ (q ∨ r) = ((p ∧ q) ∨ (p ∧ r))">]
    let distrib_and_or p q r =  ident prop_calculus ( p * (q + r) == ((p * q) + (p * r)) ) [
        distrib_or_and ( p * q ) p r|> apply_right
        absorb_or p q |> CommuteL |> apply_right
        distrib_or_and r p q |> CommuteL |> apply_right
        left_assoc |> apply_right
        commute_or r p |>apply_right
        absorb_and p r |> apply_right
        commute |> apply_right |> branch_right
    ]
    /// -(p ∧ q) = -p ∨ -q
    [<DerivedRule "-(p ∧ q) = -p ∨ -q">]
    let distrib_not_and (p:Prop) (q:Prop) = ident prop_calculus (-(p * q) == (-p + -q)) [
        golden_rule |> apply |> ApplyUnary |> branch_left
        distrib |> apply_left
        distrib |> apply_left |> branch_left 
        ident_or_or_not ( -p ) ( -q ) |> apply_right
        double_negation q |> apply_right
        ident_or_not_or q p |> CommuteL |> apply_right
        commute |> apply_right
        commute_or q p |> apply_right
    ]

    /// -p ∨ -q == -(p ∧ q) 
    [<DerivedRule "-p ∨ -q = -(p ∧ q)">]
    let collect_not_and p q = distrib_not_and p q |> Commute

    /// -(p ∨ q) = -p ∧ -q
    [<DerivedRule "-(p ∨ q) = -p ∧ -q">]
    let distrib_not_or (p:Prop) (q:Prop) = ident prop_calculus (-(p + q) == (-p * -q)) [
        golden_rule' p q |> Commute |> CommuteL |> RightAssoc |> apply_left
        commute |> apply |> ApplyUnary |> branch_left
        distrib |> apply_left
        distrib_not_and p q |> apply_left
        commute |> apply
        symm_eq_not_eq p q |> apply_right
        commute |> apply_right
    ]

    /// -p ∧ -q == -(p ∨ q)
    [<DerivedRule "-p ∧ -q = -(p ∨ q)">]
    let collect_not_or p q = distrib_not_or p q |> Commute
    
    /// p ∨ q == (p ∨ -q == p)
    [<DerivedRule "p ∨ q = (p ∨ -q = p)">]
    let ident_or_or_not_eq (p:Prop) (q:Prop) = ident prop_calculus ( (p + q) == (p + (-q) == p) ) [
        left_assoc |> apply
        collect_or_eq p q (-q)  |> apply
        commute_eq q ( -q ) |> apply_left
        def_false q |> Commute |> apply_left
        ident_or p |> apply_left
    ]

    /// p == q == ((p ∧ q) ∨ (-p ∧ -q))
    [<DerivedRule "p = q = ((p ∧ q) ∨ (-p ∧ -q))">]
    let ident_eq_and_or_not (p:Prop) (q:Prop) = ident prop_calculus (p == q == ((p * q) + (-p * -q))) [
        ident_or_or_not ( p * q ) ( -p * -q ) |> apply_right
        distrib_not_and ( -p ) ( -q ) |> apply_right
        double_negation p |> apply_right
        double_negation q |> apply_right
        distrib |> apply_left |> branch_right
        absorb_or p q |> CommuteL |> apply_right
        commute_and p q |> apply_right
        absorb_or q p |> CommuteL |> apply_right
        commute_and q p |> apply_right
        left_assoc |> apply
        commute |> apply
    ]

    /// p ∧ q == (p ∧ -q == -p)
    [<DerivedRule "p ∧ q = (p ∧ -q = -p)">]
    let ident_and_and_not (p:Prop) (q:Prop) = ident prop_calculus ((p * q) == (p * -q == -p)) [
        left_assoc |> apply
        golden_rule |> apply_left |> branch_left
        golden_rule' p ( -q ) |> apply_left
        commute |> apply_right |> branch_left
        left_assoc |> apply_right |> branch_left
        ident_or_or_not_eq p q |> Commute |> apply_left
        left_assoc |> apply_left
        right_assoc |> apply_left |> branch_left
        def_true ( p + q ) |> Commute |> apply_left
        commute |> apply_left |> branch_left
        right_assoc |> apply
        commute |> apply_right
        right_assoc |> apply
        symm_eq_not_eq p q |> apply_right
    ]

    /// p ∧ (q == r) = ((p ∧ q) = (p ∧ r) = p)
    [<DerivedRule "p ∧ (q = r) = ((p ∧ q) = (p ∧ r) = p)">]
    let distrib_and_eq p q r = ident prop_calculus (p * (q == r) == ((p * q) == (p * r) == p)) [
        golden_rule |> apply_left
        distrib |> apply_right |> branch_left
        left_assoc |> apply_left |> branch_left
        left_assoc |> apply_left
        right_assoc |> apply_left
        commute_eq_eq ( p == q ) r ( p + q ) ( p + r ) |> apply_left
        golden_rule' p q |> LeftAssoc |> apply_left
        golden_rule' p r |> LeftAssoc |> LeftAssocRecurseLeft |> RightAssoc |> Commute |> apply_left 
        golden_rule' p q |> Commute |> apply_left
        left_assoc |> apply_left
    ]

    /// p ∧ (q == p) = (p ∧ q)
    [<DerivedRule "p ∧ (q = p) = (p ∧ q)">]
    let ident_and_eq p q  = ident prop_calculus (p * (q == p) == (p * q)) [
        golden_rule |> apply_left
        distrib |> apply_right |> branch_left
        left_assoc |> apply_left |> branch_left
        left_assoc |> apply_left
        right_assoc |> apply_left
        idemp_or p |> apply_left
        commute |> apply_left |> branch_left
        left_assoc |> apply_left |> branch_left
        def_true p |> Commute |> apply_left |> branch_left
        ident_eq q |> CommuteL |> apply_left
        commute |> apply_left
        golden_rule' p q |> Commute |> CommuteL |> LeftAssocRecurseLeft |> apply_left
    ]

    /// p ∧ q ∧ (r ∧ s) = p ∧ r ∧ (q ∧ s) 
    [<DerivedRule "p ∧ q ∧ (r ∧ s) = p ∧ r ∧ (q ∧ s)">]
    let commute_and_and (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p * q) * (r * s)) == ((p * r) * (q * s))) [
        right_assoc_and p q ( r * s ) |> apply_left
        left_assoc_and q r s |>  apply_left
        commute_and q r |> apply_left
        right_assoc_and r q s |> apply_left
        left_assoc_and p r ( q * s ) |> apply_left
    ]

    /// p ⇒ q == (p ∨ q = q)
    [<DerivedRule "p ⇒ q = (p ∨ q == q)">]
    let def_implies' p q = id_ax prop_calculus ( (p ==> q) == (p + q == q) )

    /// p ⇒ q == (-p ∨ q)
    [<DerivedRule "p ⇒ q = (-p ∨ q)">]
    let ident_implies_not_or p q = ident prop_calculus ( p ==> q == (-p + q) ) [
        def_implies |> apply_left
        ident_or_not_or q p |> CommuteL |> apply_right
        commute |> apply_right
        commute |> apply_left |> branch_right
    ]

    /// p ⇒ q == ((p ∧ q) = p)
    [<DerivedRule "p ⇒ q = ((p ∧ q) = p)">]
    let ident_implies_eq_and_eq p q = ident prop_calculus ( p ==> q == ((p * q) = p) ) [
        def_implies |> apply_left
        commute |> apply
        right_assoc |> apply
        commute |> apply_right |> branch_right 
        left_assoc |> apply_right 
    ]

    /// p ∧ (p ⇒ q) = (p ∧ q)
    [<DerivedRule "p ∧ (p ⇒ q) = (p ∧ q)">]
    let ident_and_implies (p:Prop) (q:Prop) = ident prop_calculus ( p * (p ==> q) == (p * q) ) [
        ident_implies_eq_and_eq p q |> apply_left
        distrib_and_eq p ( p * q ) p |> apply_left
        left_assoc |> apply_left |> branch_left |> branch_left
        idemp_and p |> apply_left
    ]

    /// p ∨ (q ⇒ p) = (q ⇒ p)
    [<DerivedRule "p ∨ (q ⇒ p) = (q ⇒ p)">]
    let ident_or_conseq (p:Prop) (q:Prop) = ident prop_calculus ( p + (q ==> p) == (q ==> p) ) [
        def_implies |> apply_right |> branch_left
        distrib |> apply_left
        commute_or q p |> apply_left
        left_assoc_or p p q |> apply_left 
        idemp_or p |> apply_left
        commute |> apply
        commute_or p q |> apply_right
    ]

    /// p ⇒ q == (-q ⇒ -p)
    [<DerivedRule "p ⇒ q = (-q ⇒ -p)">]
    let def_implies_contr p q = ident prop_calculus (p ==> q == (-q ==> -p)) [
        def_implies |> apply_right
        commute |> apply_right
        commute |> apply_right |> branch_right
        distrib_not_and p q |> Commute |> apply_right |> branch_right
        symm_eq_not_eq p ( p * q ) |> Commute |> apply_right 
        commute |> apply_right
        ident_implies_eq_and_eq p q |> Lemma'
    ]

    /// p ⇒ (q == r) = ((p ∧ q) = (p ∧ r))
    [<DerivedRule "p ⇒ (q = r) = ((p ∧ q) = (p ∧ r))">]
    let distrib_implies_eq_and p q r =
        ident prop_calculus ( p ==> (q == r) == ((p * q) == (p * r))) [
            ident_implies_eq_and_eq p ( q == r ) |> apply_left
            distrib_and_eq p q r |> apply_left
    ]

    /// p ⇒ (q == r) = ((p ⇒ q) = (p ⇒ r))
    [<DerivedRule "p ⇒ (q = r) = ((p ⇒ q) = (p ⇒ r))">]
    let distrib_implies_eq_implies p q r = ident prop_calculus ( p ==> (q == r) == ((p ==> q) = (p ==> r))) [
        distrib_implies_eq_and p q r |> apply_left
        ident_implies_eq_and_eq p q |> apply_left |> branch_right
        ident_implies_eq_and_eq p r |> apply_right |> branch_right
        commute |> apply_right |> branch_right
        left_assoc |> apply_right 
        right_assoc |> apply_left |> branch_right
        def_true p |> Commute |> apply_right
        ident_eq ( p * q ) |> apply_left |> branch_right
    ]

    /// p ∨ (p ⇒ q)
    [<DerivedRule "p ∨ (p ⇒ q)">]
    let or_implies (p:Prop) (q:Prop) = theorem prop_calculus ( (p + (p ==> q)) == T ) [
        def_implies |> apply_right |> branch_left
        distrib |> apply_left
        left_assoc |> apply_left |> branch_left
        idemp_or p |> apply_left
        ident_eq ((p + q) == (p + q)) |> apply
    ]

    /// p ⇒ p
    [<DerivedRule "p ⇒ p">]
    let reflex_implies p = theorem prop_calculus ( p ==> p ) [
        def_implies |> apply
    ]
        
    /// p ⇒ true
    [<DerivedRule "p ⇒ true">]
    let implies_true p = theorem prop_calculus (p ==> T) [
        def_implies |> apply
        zero_or p |> apply_left
    ]

    /// false ⇒ p
    [<DerivedRule "false ⇒ p">]
    let conseq_false (p:Prop) = theorem prop_calculus (F ==> p) [
        def_implies |> apply
        ident_or p |> CommuteL |> Lemma'
    ]

    /// (true ⇒ p) = p
    [<DerivedRule "(true ⇒ p) = p">]
    let ident_conseq_true p = ident prop_calculus ((T ==> p) == p) [
        def_implies |> apply_left
        zero_or p |> CommuteL |> apply_left
        right_assoc |> apply
        commute |> apply
    ]

    /// p ⇒ false = (-p)
    [<DerivedRule "p ⇒ false = (-p)">]
    let ident_implies_false_not (p:Prop) = ident prop_calculus ((p ==> F) == -p) [
        def_implies |> apply_left
        ident_or p |> apply_left
        commute |> apply
        left_assoc |> apply
        commute |> apply
        def_false p |> apply
    ]
    
    /// p ∧ q ⇒ r = (p ⇒ (q ⇒ r))
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

    /// (p ∧ q) ⇒ p
    [<DerivedRule "(p ∧ q) ⇒ p">]
    let strengthen_and p q = theorem prop_calculus ((p * q) ==> p) [
        ident_eq ( ((p * q ) ==> p) ) |> apply
        def_implies |> apply
        commute |> apply_left
        absorb_or p q |> Lemma'
    ]
    
    /// p ⇒ p ∨ q 
    [<DerivedRule "p ⇒ p ∨ q">]
    let weaken_or p q = theorem prop_calculus ( p ==> (p + q) ) [
        ident_eq ( (p ==> (p + q)) ) |> apply
        def_implies |> apply
        left_assoc |> apply_left
        idemp_or p |> apply_left
    ]

    /// p ∧ q ⇒ p ∨ q
    [<DerivedRule "p ∧ q ⇒ p ∨ q">]
    let weaken_and_or (p:Prop) (q:Prop) = theorem prop_calculus ( p * q ==> p + q ) [
        def_implies |> apply_left
        commute |> apply_left |> branch_left
        distrib |> apply_left |> branch_left
        commute |> apply
        idemp_or p |> apply
        distrib |> apply_left |> branch_right
        idemp_and p |> apply
        distrib |> apply
        distrib |> apply_right |> branch_left
        distrib |> apply_left
        idemp_or p |> apply_left
        distrib |> apply_left
        commute_or q p |> apply_left
        idemp_and ( p + q ) |> apply_left
        commute |> apply_left |> branch_left
        distrib |> apply_left |> branch_left
        idemp_and q |> apply_left
        absorb_or q p |> CommuteL |> apply_left
        commute |> apply_right |> branch_left
        left_assoc |> apply_left
        idemp_or q |> apply_left
    ]

    /// (p ∨ (q ∧ r)) ⇒ (p ∨ q)
    [<DerivedRule "(p ∨ (q ∧ r)) ⇒ (p ∨ q)">]
    let weaken_or_and (p:Prop) q r = theorem prop_calculus ( (p + (q * r)) ==> (p + q) ) [
        distrib |> apply_left
        strengthen_and ( p + q ) ( p + r ) |> Lemma
    ]

    /// (p ∧ q) ⇒ (p ∧ (q ∨ r))
    [<DerivedRule "(p ∧ q) ⇒ (p ∧ (q ∨ r))">]
    let weaken_and_and_or p (q:Prop) (r:Prop) = theorem prop_calculus ((p * q)  ==> (p * (q + r)) ) [
        distrib |> apply_right
        weaken_or ( p * q ) ( p * r ) |> Lemma
    ]

    /// p ∧ (p ⇒ q) ⇒ q
    [<DerivedRule "p ∧ (p ⇒ q) ⇒ q">]
    let modus_ponens p q = theorem prop_calculus ( p * (p ==> q) ==> q ) [
        ident_and_implies p q |> apply_left
        commute_and p q |> apply
        strengthen_and q p |> Lemma
    ]

    /// (p ⇒ r) ∧ (q ⇒ r) = (p ∨ q ⇒ r)
    [<DerivedRule "(p ⇒ r) ∧ (q ⇒ r) = (p ∨ q ⇒ r)">]
    let case_analysis_1 p q r = ident prop_calculus (( p ==> r) * (q ==> r) == (p + q  ==> r) ) [
        ident_implies_not_or ( p + q ) r |> apply_right
        distrib|> apply_left |> branch_right
        distrib_or_and r ( -p ) ( -q ) |> CommuteL |> apply_right
        commute |> apply_left |> branch_right
        commute |> apply_right |> branch_right
        ident_implies_not_or p r |> Commute |> apply_right
        ident_implies_not_or q r |> Commute |> apply_right
    ]

    /// (p ⇒ r) ∧ (-p ⇒ r) = r
    [<DerivedRule "(p ⇒ r) ∧ (-p ⇒ r) = r">]
    let case_analysis_2 p r = ident prop_calculus ((p ==> r) * (-p ==> r) == r) [
        case_analysis_1 p -p r |> apply
        excluded_middle |> apply_left |> branch_left
        ident_conseq_true r |> Lemma'
    ]

    /// (p ⇒ q) ∧ (q ⇒ p) = (p == q)
    [<DerivedRule "(p ⇒ q) ∧ (q ⇒ p) = (p == q)">]
    let mutual_implication' (p:Prop) (q:Prop) = ident prop_calculus (((p ==> q) * (q ==> p)) == (p == q)) [
        right_assoc |> apply
        ident_implies_not_or p q |> apply_left
        ident_implies_not_or q p |> apply_left  
        distrib |> apply_left  
        commute |> apply_left |> branch_left |> branch_left
        commute |> apply_right |> branch_left
        distrib |> apply_left |> branch_left
        distrib |> apply_right |> branch_left
        distrib |> apply_left |> branch_left |> branch_left
        commute |> apply_left |> branch_left
        distrib |> apply_left |> branch_left
        contr q |> CommuteL |> apply_left
        contr p |> apply_left
        ident_or ( p * q ) |> CommuteL |> apply_left
        ident_or ( -q * -p ) |> CommuteL |> apply_left
        commute |> apply_left |> branch_left
        commute |> apply
        commute |> apply_right
        ident_eq_and_or_not p q |> apply_left
    ]

    /// (p ⇒ q) ∧ (q ⇒ p) ⇒ (p == q)
    [<DerivedRule "(p ⇒ q) ∧ (q ⇒ p) ⇒ (p == q)">]
    let antisymm_implies p q = theorem prop_calculus ((p ==> q) * (q ==> p) ==> (p == q)) [
        mutual_implication' p q |> apply_left  
        reflex_implies ( p == q ) |> Lemma
    ]

    /// (p ⇒ q) ∧ (q ⇒ r) ⇒ (p ⇒ r)
    [<DerivedRule "(p ⇒ q) ∧ (q ⇒ r) ⇒ (p ⇒ r)">]
    let trans_implies p q r = theorem prop_calculus ((p ==> q) * (q ==> r) ==> (p ==> r)) [
        rshunt |> apply
        commute |> apply_left
        left_assoc |> apply_left
        ident_and_implies p q |> apply_left
        right_assoc |> apply_left
        ident_and_implies q r |> apply_left
        commute |> apply_left
        commute |> apply_left |> branch_left
        right_assoc |> apply_left
        strengthen_and r ( q * p ) |> Lemma
    ]

    /// (p == q) ∧ (q ⇒ r) ⇒ (p ⇒ r)
    [<DerivedRule "(p == q) ∧ (q ⇒ r) ⇒ (p ⇒ r)">]
    let trans_implies_eq (p:Prop) (q:Prop) (r:Prop) = theorem prop_calculus ((p == q) * (q ==> r) ==> (p ==> r)) [
        mutual_implication' p q |> Commute |> apply_left
        rshunt |> apply
        commute |> apply_left
        left_assoc |> apply_left
        left_assoc |> apply_left |> branch_left
        ident_and_implies p q |> apply_left |> branch_left
        right_assoc |> apply_left |> branch_left
        ident_and_implies q p |> apply_left |> branch_left
        commute |> apply_right |> branch_left |> branch_left
        left_assoc |> apply_left |> branch_left
        idemp_and p |> apply_left |> branch_left
        right_assoc |> apply_left
        ident_and_implies q r |> apply_left
        left_assoc |> apply_left
        commute |> apply_left
        strengthen_and r ( p * q ) |> Lemma
    ]

    /// p ⇒ (q ⇒ p)
    [<DerivedRule "p ⇒ (q ⇒ p)">]
    let trans_implies_implies p q = theorem prop_calculus (p ==> (q ==> p)) [
        def_implies |> apply_right
        def_implies |> apply
        commute |> apply_left |> branch_right |> branch_left 
        distrib |> apply_left
        left_assoc |> apply_left |> branch_left
        idemp_or p |> apply_left |> branch_left |> branch_left
        commute |> apply_left |> branch_left 
        idemp_or p |> apply_right |> branch_left
    ]

    /// (p ⇒ q) ⇒ ((p ∨ r) ⇒ (q ∨ r)
    [<DerivedRule "(p ⇒ q) ⇒ ((p ∨ r) ⇒ (q ∨ r)">]
    let mono_or (p:PropVar) (q:Prop) (r:PropVar) = theorem prop_calculus ((p ==> q) ==> ((p + r) ==> (q + r))) [
        def_implies |> apply_right
        commute_or_or p r q r |> apply_left |> branch_right
        idemp_or r |> apply_left |> branch_right
        commute_or ( p + q ) r |> apply_left |> branch_right
        commute_or q r |> apply_right |> branch_right
        collect_or_eq r ( p + q ) q |> apply_right
        commute |> apply_right
        def_implies' p q |> Commute |> apply_right
        weaken_or ( p ==> q ) r |> Lemma
    ]
    
    (* Module information members *)

    type private IModuleTypeLocator = interface end
    
    let Type = match typeof<IModuleTypeLocator>.DeclaringType with | NonNull m -> m | _ -> failwith "Failed to locate module type."

    let Axioms = [|
        "true = p = p"    
        "false = not true"
        "p <> q = not (p = q)"
        "(p = q) = r = p = (q = r)"
        "(p ∨ q) ∨ r = p ∨ (q ∨ r)"
        "p = q = q = p"
        "p ∨ q = q ∨ p"
        "p ∨ (q = r) = (p ∨ q) = (p ∨ r)"  
        "not (p = q) = not p = q"
        "p ∨ p = p"
        "p ∨ not p"
        "p ∧ q = p = q = p ∨ q"
        "p ⇒ q = ((p ∨ q) = q)"
        "(e = f) ⇒ E(e) = E(f)"
    |]