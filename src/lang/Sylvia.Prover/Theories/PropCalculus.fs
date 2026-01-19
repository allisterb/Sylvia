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
    let reduce = Theory.S.Rules.[0]

    /// Logical expression is left associative.
    let left_assoc = Theory.S.Rules.[1]

    /// Logical expression is right associative.
    let right_assoc = Theory.S.Rules.[2]
  
    /// Logical expression is commutative.
    let commute = Theory.S.Rules.[3]

    /// Distribute logical terms in expression.
    let distrib = Theory.S.Rules.[4]

    /// Collect distributed logical terms in expression.
    let collect = Theory.S.Rules.[5]

    /// Logical operators are idempotent.
    let idemp = Theory.S.Rules.[6]

    /// Logical expression satisfies law of excluded middle.
    let excluded_middle = Theory.S.Rules.[7]

    /// Logical expression satisfies golden rule.
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
            let stmt = <@@ ((%%p) = true) = (%%p) @@> in Theorem(stmt, Proof (stmt, prop_calculus, [ApplyLeft commute; Apply right_assoc], true)) |> Ident  
        Tactics.Taut ieq
    
    /// If A = B is a theorem then so is (A = B) = true.
    let Taut' t = 
        let ieq p = Theorem(<@@ ((%%p) = true) = (%%p) @@>, Proof (<@@ (%%p = true) = %%p @@>, prop_calculus, [ApplyLeft commute; Apply right_assoc], true)) |> Ident 
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

    (* Theorems *)
    
    /// true = (p = p)
    let def_true (p:Prop) = id_ax prop_calculus (true == (p == p))  
        
    /// false = (-p = p)
    let def_false (p:Prop) = ident prop_calculus (false == (!!p == p)) [
        ApplyRight collect
        def_true p |> Commute |> ApplyRight
    ] 

    /// (p = true) = p
    let ident_eq (p:Prop) = ident prop_calculus ((p == true) == p)  [
        ApplyLeft commute
        Apply right_assoc
    ]

    /// p = q = q = p
    let commute_eq (p:Prop) (q:Prop) = ident prop_calculus ( (p == q) == (q == p) ) [Apply left_assoc]

    /// p = (q = r) = p = q = r
    let left_assoc_eq (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus ( (p == (q == r)) == ((p == q) == r) ) [ApplyRight right_assoc]

    /// (p = q) = r = p = (q = r)
    let right_assoc_eq (p:Prop) (q:Prop) (r:Prop) = id_ax prop_calculus (((p == q) == r) == (p == (q == r)))

    /// -false = true
    let not_false = ident prop_calculus (!!F == T) [
        Apply commute
        def_true F  |> ApplyLeft
        Apply right_assoc
        ApplyRight commute
        ApplyRight collect
        def_true F |> Commute |> ApplyRight  
    ]

    /// --p = p
    let double_negation (p:Prop) = ident prop_calculus ((!!(!!p)) == p) [
         Apply collect
         def_false p |> Commute |> Apply
         not_false |> Truth |> Apply
    ]

    /// -p = q = p = -q
    let symm_not_eq (p:Prop) (q:Prop) = ident prop_calculus (!!p == q == p == !!q) [
        collect |> ApplyLeft
        right_assoc |> Apply
        collect |> ApplyLeft
        commute |> ApplyRight
        collect |> ApplyRight
        commute_eq q p |> ApplyRight
    ]

    /// (p = q) = (-p = -q)
    let symm_eq_not_eq (p:Prop) (q:Prop) = ident prop_calculus (p == q == (!!p == !!q) ) [
        left_assoc |> Apply
        commute_eq (p == q) !!p |> ApplyLeft
        commute_eq p q |> ApplyLeft
        left_assoc |> ApplyLeft
        symm_not_eq p q |> Lemma'
    ]

    /// ((p = q) = (r = s)) = ((p = r) = (q = s))
    let commute_eq_eq (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p == q) == (r == s)) == ((p == r) == (q == s))) [
        left_assoc_eq (p == q) r s |> ApplyLeft
        right_assoc_eq p q r |> ApplyLeft
        commute_eq q r |> ApplyLeft
        left_assoc_eq p r q |> ApplyLeft
    ]

    /// p ∨ q = q ∨ p
    let commute_or (p:Prop) (q:Prop) = id_ax prop_calculus ((p ||| q) == (q ||| p))
 
    /// p ∨ (q ∨ r) = p ∨ q ∨ r
    let left_assoc_or (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus ( (p ||| (q ||| r)) == ((p ||| q) ||| r) ) [Apply left_assoc; Apply commute]

    /// (p ∨ q) ∨ r = p ∨ (q ∨ r)
    let right_assoc_or p q r = left_assoc_or p q r |> Commute

    /// ((p ∨ q) ∨ (r ∨ s)) = ((p ∨ r) ∨ (q ∨ s))
    let commute_or_or (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p ||| q) ||| (r ||| s)) == ((p ||| r) ||| (q ||| s))) [
        left_assoc_or (p ||| q) r s |> ApplyLeft
        right_assoc_or p q r |> ApplyLeft
        commute_or q r |> ApplyLeft
        left_assoc_or p r q |> ApplyLeft
    ]

    /// p ∨ (q = r) = (p ∨ q) = (p ∨ r)
    let distrib_or_eq (p:Prop) (q:Prop) (r:Prop) = id_ax prop_calculus ((p ||| (q == r)) == ((p ||| q) == (p ||| r)))

    /// (p ∨ q) = (p ∨ r) = p ∨ (q ∨ r)
    let collect_or_eq p q r = distrib_or_eq p q r |> Commute

    /// (p ∨ p) = p
    let idemp_or p =  id_ax prop_calculus (p ||| p == p) 
  
    /// (p and p) = p
    let idemp_and p = ident prop_calculus ((p &&& p) == p) [
        ApplyLeft golden_rule
        Apply right_assoc
        idemp_or p |> Taut' |> ApplyRight
        Apply commute 
    ] 

    /// p ∨ true = true
    let zero_or p = ident prop_calculus ((p ||| T) == T) [
        def_true p |> ApplyRight |> RecurseLeft
        distrib |> ApplyLeft
        commute |> Apply
    ]

    /// p ∨ false = p
    let ident_or (p:Prop) = ident prop_calculus ((p ||| F) == p) [
        def_false p |> ApplyRight |> RecurseLeft
        ApplyLeft distrib
        Apply right_assoc
        idemp_or p |> ApplyRight
        ApplyLeft excluded_middle
    ]

    /// (p ∨ q) = (p ∨ -q = p)
    let ident_or_or_not (p:Prop) q = ident prop_calculus ((p ||| q) == ((p ||| !!q) == p)) [
        Apply left_assoc
        collect_or_eq p q !!q  |> ApplyLeft
        commute_eq q !!q |> ApplyLeft
        def_false q  |> Commute |> ApplyLeft
        ident_or p  |> ApplyLeft      
    ]

    /// (p ∨ -q) = (p = (p or q))
    let ident_or_not_or (p:Prop) (q:Prop) = ident prop_calculus ((p ||| !!q) == (p == (p ||| q))) [
        commute |> ApplyRight
        Apply left_assoc
        collect_or_eq p !!q q  |> ApplyLeft
        def_false q |> Commute |> ApplyLeft
        ident_or p |> ApplyLeft
    ]

    
    /// p ∨ (q ∨ r) = ((p ∨ q) ∨ (p ∨ r))
    let distrib_or_or (p:Prop) (q:Prop) (r:Prop) =  ident prop_calculus ((p ||| (q ||| r)) == ((p ||| q) ||| (p ||| r))) [
        idemp_or p |> Commute |> ApplyLeft
        right_assoc |> ApplyLeft
        left_assoc_or p q r |> ApplyLeft
        commute_or p q |> ApplyLeft
        right_assoc_or q p r |> ApplyLeft
        left_assoc |> ApplyLeft
    ]

    /// (p ∨ q) = (p ∨ r) = p ∨ (q ∨ r)
    let collect_or_or p q r = distrib_or_or p q r |> Commute

    /// -(p = q) = -p = q
    let distrib_not (p:Prop) (q:Prop) = ident prop_calculus ((-(p == q)) == (-p == q)) [Apply right_assoc]

    /// (-p = q) = -(p = q) 
    let collect_not p q = distrib_not p q |> Commute

    /// p <> q = -(p = q)
    let def_not_eq (p:Prop) (q:Prop) = ident prop_calculus ((p != q) == (-(p == q))) [
        right_assoc |> Apply
    ]

    /// p <> q = q <> p
    let commute_not_eq (p:Prop) (q:Prop) = ident prop_calculus ((p != q) == (q != p)) [
        def_not_eq p q |> ApplyLeft
        def_not_eq q p |> ApplyRight
        commute_eq q p |> ApplyRight
    ]

    /// (p <> q) <> r = p <> (q <> r)
    let right_assoc_not_eq p q r = ident prop_calculus (((p != q) != r) == (p != (q != r))) [
        def_not_eq p q  |> ApplyLeft
        def_not_eq (!!(p == q)) r |> ApplyLeft
        def_not_eq q r |> ApplyRight
        def_not_eq p (!!(q == r)) |> ApplyRight
        distrib_not q r |> ApplyRight
        left_assoc_eq p !!q r |> ApplyRight
        commute_eq p !!q |> ApplyRight
        collect_not q p |> ApplyRight
        commute_eq q p |> ApplyRight
    ]

    /// p <> (q <> r) = (p <> q) <> r  
    let left_assoc_not_eq p q r = right_assoc_not_eq p q r |> Commute

    
    /// p ∨ -p = true
    let excluded_middle' (p:Prop) = ident prop_calculus ((p ||| (-p)) == T) [ident_eq (p ||| (-p)) |> Apply]
    
    /// p ∧ q = ((p = q) = (p ∨ q))
    let golden_rule' (p:Prop) (q:Prop) = id_ax prop_calculus ((p &&& q) == (p == q == (p ||| q)))

    /// (p ||| (p |&| q)) = p
    let absorb_or (p:Prop) (q:Prop) = ident prop_calculus (p ||| (p &&& q) == p)  [
        golden_rule |> ApplyRight |> RecurseLeft
        distrib |> ApplyLeft
        left_assoc_or p p q |> ApplyLeft
        idemp_or p |> ApplyLeft
        distrib_or_eq p p q |> ApplyLeft
        idemp_or p |> ApplyLeft
    ]

     /// p |&| q = q |&| p
    let commute_and (p:Prop) (q:Prop) = ident prop_calculus ((p &&& q) == (q &&& p))  [
        golden_rule' p q |> ApplyLeft
        golden_rule' q p |> ApplyRight
        commute_or q p |> ApplyRight
        commute_eq q p |> ApplyRight
    ]
        
    /// p |&| q |&| r == (p == q == r == (p ||| q) = (q ||| r) = (r ||| p) = (p ||| q ||| r))
    let ident_and_eq_all p q r = ident prop_calculus ( (p |&| q |&| r) == (p == q == r == (p ||| q) == (q ||| r) == (r ||| p) == (p ||| q ||| r)) ) [
        golden_rule' p q |> ApplyLeft
        golden_rule' ( (p == q) == (p ||| q) ) r |> ApplyLeft 
        commute_or ( ((p == q) == (p ||| q)) ) r |> ApplyLeft
        distrib_or_eq r ( p == q ) ( p ||| q ) |> ApplyLeft
        distrib_or_eq r p q |> ApplyLeft
        right_assoc_eq ( p == q ) ( p ||| q ) r |> ApplyLeft
        commute_eq ( p ||| q ) r |> ApplyLeft
        commute_or r q |> ApplyLeft
        commute_eq ( r ||| p ) ( q ||| r ) |> ApplyLeft
        commute_or r ( p ||| q ) |> ApplyLeft
        left_assoc_eq ( p == q ) r ( p ||| q ) |> ApplyLeft
        left_assoc |> ApplyLeft
        left_assoc_eq ( p == q == r == (p ||| q) ) ( q ||| r ) ( r ||| p ) |> ApplyLeft
    ]
    
    /// p |&| q |&| r == p |&| (q |&| r)
    let right_assoc_and p q r = ident prop_calculus ((p &&& q &&& r) == (p &&& (q &&& r))) [
        ident_and_eq_all p q r |> ApplyLeft
        commute_and p ( q |&| r ) |> ApplyRight
        ident_and_eq_all q r p |> ApplyRight
        commute_eq ( q == r ) p |> ApplyRight
        left_assoc_eq ( p == q == r == (p ||| q) ) ( q ||| r ) ( r ||| p ) |> ApplyLeft
        left_assoc_eq p q r |> ApplyRight
        commute_or ( q ||| r ) p |> ApplyRight
        left_assoc_or p q r |> ApplyRight
        right_assoc_eq ( p == q == r ) ( q ||| r ) ( r ||| p ) |> ApplyRight
        left_assoc_eq ( p == q == r )  ( q ||| r ) ( r ||| p ) |> ApplyRight
        right_assoc_eq ( p == q == r == (q ||| r) ) ( r ||| p ) ( p ||| q ) |> ApplyRight
        commute_eq ( (r ||| p) ) ( p ||| q ) |> ApplyRight
        left_assoc |> ApplyRight
        left_assoc_eq ( p == q == r == (q ||| r) ) ( p ||| q  ) ( r ||| p ) |> ApplyRight
        right_assoc_eq ( p == q == r )  ( q ||| r ) ( p ||| q ) |> ApplyRight
        commute_eq ( q ||| r ) ( p ||| q ) |> ApplyRight
        left_assoc_eq ( p == q == r ) ( p ||| q )  ( (q ||| r) ) |> ApplyRight
    ]

    /// p |&| (q |&| r) = p |&| q |&| r
    let left_assoc_and p q r = right_assoc_and p q r |> Commute
        
    /// p |&| true = p
    let ident_and p = ident prop_calculus ( (p &&& T) == p ) [
        ApplyLeft golden_rule
        Apply right_assoc
        zero_or p |> ApplyRight
        ApplyRight commute
    ]

    /// p |&| false = false
    let zero_and p = ident prop_calculus ( (p &&& F) == F ) [
      golden_rule' p F |> ApplyLeft
      ident_or p |> ApplyLeft
      Apply right_assoc
    ]

    /// p |&| (q |&| r) = (p |&| q) |&| (p |&| r)
    let distrib_and p q r = ident prop_calculus ( (p &&& (q &&& r)) == ((p &&& q) &&& (p &&& r)) ) [
        idemp_and p |> Commute |> ApplyLeft |> RecurseLeft
        right_assoc |> ApplyLeft
        left_assoc_and p q r |> ApplyRight |> RecurseLeft
        commute_and p q |> ApplyRight |> RecurseLeft
        right_assoc_and q p r |> ApplyRight |> RecurseLeft
        left_assoc |> ApplyLeft
    ]

    /// p |&| -p == false
    let contr p = ident prop_calculus ( p &&& -p == F) [
        golden_rule |> ApplyLeft
        excluded_middle |> ApplyRight |> RecurseLeft
        commute_eq p ( !!p ) |> ApplyLeft
        def_false p |> Commute |> ApplyLeft
        commute_eq F T  |> ApplyLeft
        right_assoc |> Apply
    ]

    /// (p |&| (p ||| q)) = p
    let absorb_and p q = ident prop_calculus ( (p &&& (p ||| q)) == p ) [
        ApplyLeft golden_rule
        left_assoc_or p  p  q |> ApplyLeft
        idemp_or p |> ApplyLeft
    ]
    
    /// p |&| (-p ||| q) = (p |&| q)
    let absorb_and_not (p:Prop) q = ident prop_calculus (p &&& ((-p) ||| q) == (p &&& q)) [
        golden_rule |> ApplyLeft
        left_assoc_or p -p q |> ApplyLeft
        excluded_middle' p |> ApplyLeft
        zero_or q |> CommuteL |> ApplyLeft
        ident_eq ( p == (-p ||| q) ) |> ApplyLeft
        commute_or ( !! p ) q |> ApplyLeft
        ident_or_not_or q p |> ApplyLeft
        left_assoc |> ApplyLeft
        commute_or q p |> ApplyLeft
        golden_rule' p q |> Commute |> ApplyLeft
    ]

    /// p ||| (-p |&| q) = (p ||| q)
    let absorb_or_not p q = ident prop_calculus (p ||| (-p &&& q) == (p ||| q)) [
        golden_rule |> ApplyLeft
        commute_or ( -p ) q  |> ApplyLeft
        right_assoc_eq ( -p ) q  ( q ||| -p ) |> ApplyLeft
        ident_or_or_not q  p |> Commute |> CommuteL |> ApplyLeft
        distrib |> ApplyLeft 
        commute_or q p |> ApplyLeft
        left_assoc_or p p q |> ApplyLeft
        idemp_or p |> ApplyLeft
        excluded_middle |> ApplyLeft
        ident_eq ( p ||| q ) |> CommuteL |> ApplyLeft
    ]
    
    /// p ||| (q |&| r) = ((p ||| q) |&| (p ||| r))
    let distrib_or_and (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus (p ||| (q &&& r) == ((p ||| q) &&& (p ||| r))) [
        golden_rule |> ApplyRight |> RecurseLeft
        distrib |> ApplyLeft
        distrib |> ApplyLeft |> RecurseLeft
        distrib_or_or p q r |> ApplyLeft
        golden_rule' ( p ||| q ) ( p ||| r ) |> Commute |> ApplyLeft
    ]

    /// ((p ||| q) |&| (p ||| r)) = p ||| (q |&| r)
    let collect_or_and p q r = distrib_or_and p q r |> Commute

    /// p |&| (q ||| r) = ((p |&| q) ||| (p |&| r))
    let distrib_and_or p q r =  ident prop_calculus ( p &&& (q ||| r) == ((p &&& q) ||| (p &&& r)) ) [
        distrib_or_and ( p |&| q ) p r|> ApplyRight
        absorb_or p q |> CommuteL |> ApplyRight
        distrib_or_and r p q |> CommuteL |> ApplyRight
        left_assoc |> ApplyRight
        commute_or r p |>ApplyRight
        absorb_and p r |> ApplyRight
        commute |> ApplyRight |> RecurseRight
    ]
    /// -(p |&| q) = -p ||| -q
    let distrib_not_and (p:Prop) (q:Prop) = ident prop_calculus (-(p &&& q) == (-p ||| -q)) [
        golden_rule |> Apply |> ApplyUnary |> RecurseLeft
        distrib |> ApplyLeft
        distrib |> ApplyLeft |> RecurseLeft 
        ident_or_or_not ( -p ) ( -q ) |> ApplyRight
        double_negation q |> ApplyRight
        ident_or_not_or q p |> CommuteL |> ApplyRight
        commute |> ApplyRight
        commute_or q p |> ApplyRight
    ]

    /// -p ||| -q == -(p |&| q) 
    let collect_not_and p q = distrib_not_and p q |> Commute

    /// -(p ||| q) = -p |&| -q
    let distrib_not_or (p:Prop) (q:Prop) = ident prop_calculus (-(p ||| q) == (-p &&& -q)) [
        golden_rule' p q |> Commute |> CommuteL |> RightAssoc |> ApplyLeft
        commute |> Apply |> ApplyUnary |> RecurseLeft
        distrib |> ApplyLeft
        distrib_not_and p q |> ApplyLeft
        commute |> Apply
        symm_eq_not_eq p q |> ApplyRight
        commute |> ApplyRight
    ]

    /// -p |&| -q == -(p ||| q)
    let collect_not_or p q = distrib_not_or p q |> Commute
    
    /// p ||| q == (p ||| -q == p)
    let ident_or_or_not_eq (p:Prop) (q:Prop) = ident prop_calculus ( (p ||| q) == (p ||| (-q) == p) ) [
        left_assoc |> Apply
        collect_or_eq p q (-q)  |> Apply
        commute_eq q ( -q ) |> ApplyLeft
        def_false q |> Commute |> ApplyLeft
        ident_or p |> ApplyLeft
    ]

    /// p == q == ((p |&| q) ||| (-p |&| -q))
    let ident_eq_and_or_not (p:Prop) (q:Prop) = ident prop_calculus (p == q == ((p &&& q) ||| (-p &&& -q))) [
        ident_or_or_not ( p |&| q ) ( -p |&| -q ) |> ApplyRight
        distrib_not_and ( -p ) ( -q ) |> ApplyRight
        double_negation p |> ApplyRight
        double_negation q |> ApplyRight
        distrib |> ApplyLeft |> RecurseRight
        absorb_or p q |> CommuteL |> ApplyRight
        commute_and p q |> ApplyRight
        absorb_or q p |> CommuteL |> ApplyRight
        commute_and q p |> ApplyRight
        left_assoc |> Apply
        commute |> Apply
    ]

    /// p |&| q == (p |&| -q == -p)
    let ident_and_and_not (p:Prop) (q:Prop) = ident prop_calculus ((p &&& q) == (p &&& -q == -p)) [
        left_assoc |> Apply
        golden_rule |> ApplyLeft |> RecurseLeft
        golden_rule' p ( -q ) |> ApplyLeft
        commute |> ApplyRight |> RecurseLeft
        left_assoc |> ApplyRight |> RecurseLeft
        ident_or_or_not_eq p q |> Commute |> ApplyLeft
        left_assoc |> ApplyLeft
        right_assoc |> ApplyLeft |> RecurseLeft
        def_true ( p ||| q ) |> Commute |> ApplyLeft
        commute |> ApplyLeft |> RecurseLeft
        right_assoc |> Apply
        commute |> ApplyRight
        right_assoc |> Apply
        symm_eq_not_eq p q |> ApplyRight
    ]

    /// p |&| (q == r) = ((p |&| q) = (p |&| r) = p)
    let distrib_and_eq p q r = ident prop_calculus (p &&& (q == r) == ((p &&& q) == (p &&& r) == p)) [
        golden_rule |> ApplyLeft
        distrib |> ApplyRight |> RecurseLeft
        left_assoc |> ApplyLeft |> RecurseLeft
        left_assoc |> ApplyLeft
        right_assoc |> ApplyLeft
        commute_eq_eq ( p == q ) r ( p ||| q ) ( p ||| r ) |> ApplyLeft
        golden_rule' p q |> LeftAssoc |> ApplyLeft
        golden_rule' p r |> LeftAssoc |> LeftAssocRecurseLeft |> RightAssoc |> Commute |> ApplyLeft 
        golden_rule' p q |> Commute |> ApplyLeft
        left_assoc |> ApplyLeft
    ]

    /// p |&| (q == p) = (p |&| q)
    let ident_and_eq p q  = ident prop_calculus (p &&& (q == p) == (p &&& q)) [
        golden_rule |> ApplyLeft
        distrib |> ApplyRight |> RecurseLeft
        left_assoc |> ApplyLeft |> RecurseLeft
        left_assoc |> ApplyLeft
        right_assoc |> ApplyLeft
        idemp_or p |> ApplyLeft
        commute |> ApplyLeft |> RecurseLeft
        left_assoc |> ApplyLeft |> RecurseLeft
        def_true p |> Commute |> ApplyLeft |> RecurseLeft
        ident_eq q |> CommuteL |> ApplyLeft
        commute |> ApplyLeft
        golden_rule' p q |> Commute |> CommuteL |> LeftAssocRecurseLeft |> ApplyLeft
    ]

    /// p |&| q |&| (r |&| s) = p |&| r |&| (q |&| s) 
    let commute_and_and (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p &&& q) &&& (r &&& s)) == ((p &&& r) &&& (q &&& s))) [
        right_assoc_and p q ( r &&& s ) |> ApplyLeft
        left_assoc_and q r s |>  ApplyLeft
        commute_and q r |> ApplyLeft
        right_assoc_and r q s |> ApplyLeft
        left_assoc_and p r ( q &&& s ) |> ApplyLeft
    ]

    /// p ===> q == (p ||| q == q)
    let def_implies' p q = id_ax prop_calculus ( (p ==> q) == (p ||| q == q) )

    /// p ===> q == (-p ||| q)
    let ident_implies_not_or p q = ident prop_calculus ( p ==> q == (-p ||| q) ) [
        def_implies |> ApplyLeft
        ident_or_not_or q p |> CommuteL |> ApplyRight
        commute |> ApplyRight
        commute |> ApplyLeft |> RecurseRight
    ]

    /// p ===> q == ((p |&| q) = p)
    let ident_implies_eq_and_eq p q = ident prop_calculus ( p ==> q == ((p &&& q) = p) ) [
        def_implies |> ApplyLeft
        commute |> Apply
        right_assoc |> Apply
        commute |> ApplyRight |> RecurseRight 
        left_assoc |> ApplyRight 
    ]

    /// p |&| (p ===> q) = (p |&| q)
    let ident_and_implies (p:Prop) (q:Prop) = ident prop_calculus ( p &&& (p ==> q) == (p &&& q) ) [
        ident_implies_eq_and_eq p q |> ApplyLeft
        distrib_and_eq p ( p |&| q ) p |> ApplyLeft
        left_assoc |> ApplyLeft |> RecurseLeft |> RecurseLeft
        idemp_and p |> ApplyLeft
    ]

    /// p ||| (q ===> p) = (q ===> p)
    let ident_or_conseq (p:Prop) (q:Prop) = ident prop_calculus ( p ||| (q ==> p) == (q ==> p) ) [
        def_implies |> ApplyRight |> RecurseLeft
        distrib |> ApplyLeft
        commute_or q p |> ApplyLeft
        left_assoc_or p p q |> ApplyLeft 
        idemp_or p |> ApplyLeft
        commute |> Apply
        commute_or p q |> ApplyRight
    ]

    /// p ===> q == (-q ===> -p)
    let def_implies_contr p q = ident prop_calculus (p ==> q == (-q ==> -p)) [
        def_implies |> ApplyRight
        commute |> ApplyRight
        commute |> ApplyRight |> RecurseRight
        distrib_not_and p q |> Commute |> ApplyRight |> RecurseRight
        symm_eq_not_eq p ( p |&| q ) |> Commute |> ApplyRight 
        commute |> ApplyRight
        ident_implies_eq_and_eq p q |> Lemma'
    ]

    /// p ===> (q == r) = ((p |&| q) = (p |&| r))
    let distrib_implies_eq_and p q r =
        ident prop_calculus ( p ==> (q == r) == ((p &&& q) == (p &&& r))) [
            ident_implies_eq_and_eq p ( q == r ) |> ApplyLeft
            distrib_and_eq p q r |> ApplyLeft
    ]

    /// p ===> (q == r) = ((p ===> q) = (p ===> r))
    let distrib_implies_eq_implies p q r = ident prop_calculus ( p ==> (q == r) == ((p ==> q) = (p ==> r))) [
        distrib_implies_eq_and p q r |> ApplyLeft
        ident_implies_eq_and_eq p q |> ApplyLeft |> RecurseRight
        ident_implies_eq_and_eq p r |> ApplyRight |> RecurseRight
        commute |> ApplyRight |> RecurseRight
        left_assoc |> ApplyRight 
        right_assoc |> ApplyLeft |> RecurseRight
        def_true p |> Commute |> ApplyRight
        ident_eq ( p |&| q ) |> ApplyLeft |> RecurseRight
    ]

    /// p ||| (p ===> q)
    let or_implies (p:Prop) (q:Prop) = theorem prop_calculus ( (p ||| (p ==> q)) == T ) [
        def_implies |> ApplyRight |> RecurseLeft
        distrib |> ApplyLeft
        left_assoc |> ApplyLeft |> RecurseLeft
        idemp_or p |> ApplyLeft
        ident_eq ((p ||| q) == (p ||| q)) |> Apply
    ]

    /// p ==> p
    let reflex_implies p = theorem prop_calculus ( p ==> p ) [
        def_implies |> Apply
    ]
        
    /// p ===> true
    let implies_true p = theorem prop_calculus (p ==> T) [
        def_implies |> Apply
        zero_or p |> ApplyLeft
    ]

    /// false ==> p
    let conseq_false (p:Prop) = theorem prop_calculus (F ==> p) [
        def_implies |> Apply
        ident_or p |> CommuteL |> Lemma'
    ]

    /// (true ==> p) = p
    let ident_conseq_true p = ident prop_calculus ((T ==> p) == p) [
        def_implies |> ApplyLeft
        zero_or p |> CommuteL |> ApplyLeft
        right_assoc |> Apply
        commute |> Apply
    ]

    /// p ===> false = (-p)
    let ident_implies_false_not (p:Prop) = ident prop_calculus ((p ==> F) == -p) [
        def_implies |> ApplyLeft
        ident_or p |> ApplyLeft
        commute |> Apply
        left_assoc |> Apply
        commute |> Apply
        def_false p |> Apply
    ]
    
    /// p |&| q ===> r == (p ===> (q ===> r))
    let shunt' p q r = ident prop_calculus (p &&& q ==> r == (p ==> (q ==> r))) [
        ident_implies_eq_and_eq ( p |&| q ) r |> ApplyLeft
        ident_implies_eq_and_eq q r |> ApplyRight
        ident_implies_eq_and_eq p ( q |&| r == q ) |> ApplyRight
        distrib_and_eq p ( q |&| r ) q |> ApplyRight
        left_assoc_and p q r |> ApplyRight
        right_assoc |> ApplyRight
        def_true p |> Commute |> ApplyRight
        left_assoc |> Apply
        commute |> Apply
    ]

    /// (p |&| q) ==> p
    let strengthen_and p q = theorem prop_calculus ((p &&& q) ==> p) [
        ident_eq ( ((p |&| q ) ==> p) ) |> Apply
        def_implies |> Apply
        commute |> ApplyLeft
        absorb_or p q |> Lemma'
    ]
    
    /// p ===> p ||| q 
    let weaken_or p q = theorem prop_calculus ( p ==> (p ||| q) ) [
        ident_eq ( (p ==> (p ||| q)) ) |> Apply
        def_implies |> Apply
        left_assoc |> ApplyLeft
        idemp_or p |> ApplyLeft
    ]

    /// p |&| q ===> p ||| q
    let weaken_and_or (p:Prop) (q:Prop) = theorem prop_calculus ( p &&& q ==> p ||| q ) [
        def_implies |> ApplyLeft
        commute |> ApplyLeft |> RecurseLeft
        distrib |> ApplyLeft |> RecurseLeft
        commute |> Apply
        idemp_or p |> Apply
        distrib |> ApplyLeft |> RecurseRight
        idemp_and p |> Apply
        distrib |> Apply
        distrib |> ApplyRight |> RecurseLeft
        distrib |> ApplyLeft
        idemp_or p |> ApplyLeft
        distrib |> ApplyLeft
        commute_or q p |> ApplyLeft
        idemp_and ( p ||| q ) |> ApplyLeft
        commute |> ApplyLeft |> RecurseLeft
        distrib |> ApplyLeft |> RecurseLeft
        idemp_and q |> ApplyLeft
        absorb_or q p |> CommuteL |> ApplyLeft
        commute |> ApplyRight |> RecurseLeft
        left_assoc |> ApplyLeft
        idemp_or q |> ApplyLeft
    ]

    /// (p ||| (q |&| r)) ==> (p ||| q)
    let weaken_or_and p q r = theorem prop_calculus ( (p ||| (q &&& r)) ==> (p ||| q) ) [
        distrib |> ApplyLeft
        strengthen_and ( p ||| q ) ( p ||| r ) |> Lemma
    ]

    /// (p |&| q) ==> (p |&| (q ||| r))
    let weaken_and_and_or p q r = theorem prop_calculus ( (p &&& q)  ==> (p |&| (q ||| r)) ) [
        distrib |> ApplyRight
        weaken_or ( p |&| q ) ( p |&| r ) |> Lemma
    ]

    /// p |&| (p ===> q) ==> q
    let modus_ponens p q = theorem prop_calculus ( p &&& (p ==> q) ==> q ) [
        ident_and_implies p q |> ApplyLeft
        commute_and p q |> Apply
        strengthen_and q p |> Lemma
    ]

    /// (p ===> r) |&| (q ===> r) = (p ||| q ===> r)
    let case_analysis_1 p q r = ident prop_calculus (( p ==> r) &&& (q ==> r) == (p ||| q  ==> r) ) [
        ident_implies_not_or ( p ||| q ) r |> ApplyRight
        distrib|> ApplyLeft |> RecurseRight
        distrib_or_and r ( -p ) ( -q ) |> CommuteL |> ApplyRight
        commute |> ApplyLeft |> RecurseRight
        commute |> ApplyRight |> RecurseRight
        ident_implies_not_or p r |> Commute |> ApplyRight
        ident_implies_not_or q r |> Commute |> ApplyRight
    ]

    /// (p ===> r) |&| (-p ===> r) = r
    let case_analysis_2 p r = ident prop_calculus ((p ==> r) &&& (-p ==> r) == r) [
        case_analysis_1 p -p r |> Apply
        excluded_middle |> ApplyLeft |> RecurseLeft
        ident_conseq_true r |> Lemma'
    ]

    /// (p ===> q) |&| (q ===> p) = (p == q)
    let mutual_implication' (p:Prop) (q:Prop) = ident prop_calculus (((p ==> q) &&& (q ==> p)) == (p == q)) [
        right_assoc |> Apply
        ident_implies_not_or p q |> ApplyLeft
        ident_implies_not_or q p |> ApplyLeft  
        distrib |> ApplyLeft  
        commute |> ApplyLeft |> RecurseLeft |> RecurseLeft
        commute |> ApplyRight |> RecurseLeft
        distrib |> ApplyLeft |> RecurseLeft
        distrib |> ApplyRight |> RecurseLeft
        distrib |> ApplyLeft |> RecurseLeft |> RecurseLeft
        commute |> ApplyLeft |> RecurseLeft
        distrib |> ApplyLeft |> RecurseLeft
        contr q |> CommuteL |> ApplyLeft
        contr p |> ApplyLeft
        ident_or ( p |&| q ) |> CommuteL |> ApplyLeft
        ident_or ( -q |&| -p ) |> CommuteL |> ApplyLeft
        commute |> ApplyLeft |> RecurseLeft
        commute |> Apply
        commute |> ApplyRight
        ident_eq_and_or_not p q |> ApplyLeft
    ]

    /// (p ===> q) |&| (q ===> p) ==> (p == q)
    let antisymm_implies p q = theorem prop_calculus ((p ==> q) &&& (q ==> p) ==> (p == q)) [
        mutual_implication' p q |> ApplyLeft  
        reflex_implies ( p == q ) |> Lemma
    ]

    /// (p ===> q) |&| (q ===> r) ==> (p ===> r)
    let trans_implies p q r = theorem prop_calculus ((p ==> q) &&& (q ==> r) ==> (p ==> r)) [
        rshunt |> Apply
        commute |> ApplyLeft
        left_assoc |> ApplyLeft
        ident_and_implies p q |> ApplyLeft
        right_assoc |> ApplyLeft
        ident_and_implies q r |> ApplyLeft
        commute |> ApplyLeft
        commute |> ApplyLeft |> RecurseLeft
        right_assoc |> ApplyLeft
        strengthen_and r ( q |&| p ) |> Lemma
    ]

    /// (p == q) |&| (q ==> r) ==> (p ==> r)
    let trans_implies_eq (p:Prop) (q:Prop) (r:Prop) = theorem prop_calculus ((p == q) &&& (q ==> r) ==> (p ==> r)) [
        mutual_implication' p q |> Commute |> ApplyLeft
        rshunt |> Apply
        commute |> ApplyLeft
        left_assoc |> ApplyLeft
        left_assoc |> ApplyLeft |> RecurseLeft
        ident_and_implies p q |> ApplyLeft |> RecurseLeft
        right_assoc |> ApplyLeft |> RecurseLeft
        ident_and_implies q p |> ApplyLeft |> RecurseLeft
        commute |> ApplyRight |> RecurseLeft |> RecurseLeft
        left_assoc |> ApplyLeft |> RecurseLeft
        idemp_and p |> ApplyLeft |> RecurseLeft
        right_assoc |> ApplyLeft
        ident_and_implies q r |> ApplyLeft
        left_assoc |> ApplyLeft
        commute |> ApplyLeft
        strengthen_and r ( p |&| q ) |> Lemma
    ]

    /// p ===> (q ===> p)
    let trans_implies_implies p q = theorem prop_calculus (p ==> (q ==> p)) [
        def_implies |> ApplyRight
        def_implies |> Apply
        commute |> ApplyLeft |> RecurseRight |> RecurseLeft 
        distrib |> ApplyLeft
        left_assoc |> ApplyLeft |> RecurseLeft
        idemp_or p |> ApplyLeft |> RecurseLeft |> RecurseLeft
        commute |> ApplyLeft |> RecurseLeft 
        idemp_or p |> ApplyRight |> RecurseLeft
    ]

    /// (p ===> q) ==> ((p ||| r) ==> (q ||| r)
    let mono_or p q r = theorem prop_calculus ( (p ==> q) ==> ((p ||| r) ==> (q ||| r)) ) [
        def_implies |> ApplyRight
        commute_or_or p r q r |> ApplyLeft |> RecurseRight
        idemp_or r |> ApplyLeft |> RecurseRight
        commute_or ( p ||| q ) r |> ApplyLeft |> RecurseRight
        commute_or q r |> ApplyRight |> RecurseRight
        collect_or_eq r ( p ||| q ) q |> ApplyRight
        commute |> ApplyRight
        def_implies' p q |> Commute |> ApplyRight
        weaken_or ( p ==> q ) r |> Lemma
    ]
    