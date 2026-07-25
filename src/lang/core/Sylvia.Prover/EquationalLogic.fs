namespace Sylvia

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Formula
open Patterns
open Descriptions

/// Formalizes the default equational logic used by Sylph called S.
/// Based on E: https://www.cs.cornell.edu/fbs/publications/94-1455.pdf
///             http://www.cs.cornell.edu/home/gries/Logic/Equational.html
/// The number after each axiom corresponds to the number of the axiom in the textbook A Logical Approach to Discrete Math by Gries et.al.
module EquationalLogic =
    let private desc = axiom_desc "Equational Logic"

    // The propositional truth constants. Rule functions produce these named
    // constants (T/F), never bare bool literals, so that propositions are only
    // ever compared to other propositions.
    let private tE : Expr = T.Expr.Raw
    let private fE : Expr = F.Expr.Raw
    let private boolConst b = if b then tE else fE

    // Hoisted quantifier templates for `call`: a literal passed inline is
    // re-deserialized on every call (see docs/expressions-perf.md). Instantiated at
    // obj — the same defaulting the inline literals resolved to; only the MethodInfo's
    // name matters downstream (the ForAll/Exists patterns and sequal are name-based).
    let private forall_template : Expr<obj -> bool -> bool -> bool> = <@ forall_expr @>
    let private exists_template : Expr<obj -> bool -> bool -> bool> = <@ exists_expr @>

    (* Axioms *)

    /// true = p = p
    let (|DefTrue|_|) =
        function
        | True -> pattern_desc' (lazy "Definition of true") (lazy <@fun x -> x = x = true @>) |> Some
        | Not(False) -> pattern_desc' (lazy "Definition of true") (lazy <@fun x -> x = x = true @>) |> Some
        | Equals(True, Equals(a1, a2)) when sequal a1 a2 -> pattern_desc' (lazy "Definition of true") (lazy <@fun x -> x = x = true @>) |> Some
        | _ -> None

    /// false = not true
    let (|DefFalse|_|) =
        function
        | Equals(False, Not(True)) -> pattern_desc' (lazy "Definition of false") (lazy <@ false = not true @>) |> Some
        | _ -> None

    /// not (p = q) = not p = q
    let (|DistribNot|_|) =
        function
        | Equals(Not(Equals(a1, a2)), Equals(Not(b1), b2)) when sequal2 a1 a2 b1 b2 -> pattern_desc' (lazy "Distributivity") (lazy <@fun x y -> not(x = y) = (not x = y) @>) |> Some
        | _ -> None

    /// p || not p
    let (|ExcludedMiddle|_|) =
        function
        | Or(a1, Not(a2)) when sequal a1 a2 -> pattern_desc' (lazy "the Excluded Middle") (lazy <@fun x -> x || not x @>) |> Some
        | _ -> None

    
    let (|IdempotencyOr|_|) =
        function
        | Equals(Or(a1,a2), a3) when sequal a1 a2 && sequal a1 a3 -> pattern_name "Idempotency" |> Some  
        | _ -> None

    /// p && q = p = q = p || q 
    let (|GoldenRule|_|) =
        function
        | Equals(And(p1, q1), Equals(Equals(p2, q2), Or(p3, q3))) when sequal p1 p2 && sequal p2 p3 && sequal q1 q2 && sequal q2 q3 -> 
                                                                pattern_desc' (lazy "the Golden Rule") (lazy <@fun x y -> x && y = (x = y) = (x || y) @>) |> Some
        | _ -> None

    /// p ===> q = ((p || q) = q)
    let (|Implication|_|) =
        function
        | Equals(Implies(a1, a2), Equals(Or(a3, a4), a5)) when sequal a1 a3 && sequal a2 a4 && sequal a4 a5 -> 
                                                                pattern_desc' (lazy "Implication") (lazy <@fun x y-> (x ===> y) = ((x || y) = y)@>) |> Some
        | Equals(Conseq(a1, a2), Implies(a3, a4)) when sequal2 a1 a2 a4 a3 -> 
                                                                pattern_desc' (lazy "Consequence") (lazy <@fun x y -> (x <=== y) = (y ===> x) @>) |> Some
        
        // The following two axioms aren't in the original axioms for E but are included in S for convenience
        /// p ===> true
        | Implies(_, True) -> pattern_desc' (lazy "Implication") (lazy <@fun x -> x ===> true @>) |> Some
        /// p ===> p
        | Implies(E, E') when sequal E E' -> pattern_desc' (lazy "Implication") (lazy <@fun x -> x ===> x @>) |> Some
        | _ -> None

    /// (e = f) ===> E(e) = E(f) 
    let (|Leibniz|_|) =
        function
        | Implies(Equals(Var e, Var f), Equals(Ee, Ef)) when sequal (replace_var_var e f Ee) Ef -> pattern_desc' (lazy "Leibniz") (lazy <@fun e f E -> (e = f) ===> E(e) = E(f)@>) |> Some
        | Implies(Equals(Var p, Var q), Equals(Quantifier(_, _, R, P), Quantifier(_, _, R', P'))) when sequal (replace_var_var p q R) R' && sequal (replace_var_var p q P) P' -> 
            pattern_desc' (lazy "Leibniz") (lazy <@fun p q E  -> (p = q) ===> E(p) = E(q) @>) |> Some
        | Implies(Implies(R , Equals(Var p, Var q)), Equals(Quantifier(_, _, R1, P), Quantifier(_, _, R2, P'))) when sequal R R1 && sequal R1 R2 && sequal (replace_var_var p q P) P' -> 
            pattern_desc' (lazy "Leibniz") (lazy <@fun p q R E  -> (R ===> (p = q)) ===> E(p) = E(q) @>) |> Some
        | _ -> None
    
    let (|EmptyRange|_|) =
        function
        | ForAll(_,_,False,_) -> pattern_desc' (lazy "Empty Range") (lazy <@ fun x -> not x @>) |> Some
        | Equals(Exists(_,_,False,_), False) -> pattern_desc' (lazy "Empty Range") (lazy <@ fun x -> not x @>) |> Some
        | _ -> None
    
    let (|QuantifierCollect|_|) =
        function
        | Equals(And(ForAll(_, b1, R1, P), ForAll(_, b2, R2, Q)), ForAll(_, b3, R3, PQ)) 
            when 
                vequal' b1 b2 && vequal' b2 b3 && sequal R1 R2 && sequal R2 R3 && sequal (mk_and P Q) PQ ->
                pattern_desc' (lazy "Distributivity of \u2200") (lazy <@ fun x -> not x @>) |> Some
        | Equals(Or(Exists(_, b1, R1, P), Exists(_, b2, R2, Q)), Exists(_, b3, R3, PQ)) 
            when 
                vequal' b1 b2 && vequal' b2 b3 && sequal R1 R2 && sequal R2 R3 && sequal (mk_or P Q) PQ ->
                pattern_desc' (lazy "Distributivity of \u2203") (lazy <@ fun x -> not x @>) |> Some
        | _ -> None

    let (|RangeSplit|_|) =
        function
        | Equals(ForAll(_, b1, Or(R1, S1), P1), And(ForAll(_,b2, R, P2), ForAll(_,b3, S, P3))) 
            when vequal' b1 b2 && vequal' b2 b3 && sequal2 R1 S1 R S && sequal P1 P2 && sequal P2 P3 -> pattern_name "Range Split"  |> Some
        | Equals(Exists(_,b1, Or(R1, S1), P1), Or(Exists(_,b2, R, P2), Exists(_,b3, S, P3))) 
            when vequal' b1 b2 && vequal' b2 b3 && sequal2 R1 S1 R S && sequal P1 P2 && sequal P2 P3 -> pattern_name "Range Split" |> Some
        | _ -> None

    let (|Interchange|_|) =
        function
        | Equals(ForAll(_, x, R, ForAll(_, y, Q, P)), ForAll(_, y', Q', ForAll(_ ,x', R' ,P')))
            when 
                vequal' x x' && vequal' y y' && sequal3 P Q R P' Q' R' && not_occurs_free y R && not_occurs_free x Q
                -> pattern_desc' (lazy "the Interchange of Variables") (lazy <@ fun x -> not x @>) |> Some
        | Equals(Exists(_, x, R, Exists(_, y, Q, P)), Exists(_, y', Q', Exists(_ ,x', R' ,P')))
            when 
                vequal' x x' && vequal' y y' && sequal P P' && sequal Q Q' && sequal R R' && not_occurs_free y R && not_occurs_free x Q
                -> pattern_desc' (lazy "the Interchange of Variables") (lazy <@ fun x -> not x @>) |> Some
        | _ -> None

    let (|Trading|_|) =
        function
        | Equals(ForAll(_, x, R, P), ForAll(_, x', True, Implies(R', P'))) when vequal' x x' && sequal2 R P R' P'-> pattern_desc' (lazy "Trading") (lazy <@ fun x -> not x @>) |> Some
        | _ -> None
              
    let (|ForAllDistribOr|_|) =
        function
        | Equals(Or(P1, ForAll(_, x1, R1, Q1)), ForAll(_, x2, R2, Or(P2, Q2))) 
            when not_occurs_free x1 P1 && vequal' x1 x2 && sequal P1 P2 && sequal2 Q1 R1 Q2 R2 -> pattern_desc' (lazy "Distributivity of forall") (lazy <@ fun x -> not x @>) |> Some
        | _ -> None
    
    let (|GeneralizedDeMorgan|_|) =
        function
        | Equals(Exists(_, x, R, P), Not(ForAll(_, x', R', Not(P')))) when vequal' x x' && sequal2 R P R' P'-> pattern_desc' (lazy "Generalized De Morgan") (lazy <@ fun x -> not x @>) |> Some
        | _ -> None
    
    let (|UniversalInstantiation|_|) =
        function
        // Instantiation (Gries 9.13): (∀x |: P) ⇒ P[x:=E]. Holds for any term E, so there is no
        // ¬occurs side condition — `is_inst_expr` verifies P' is P with x textually replaced.
        // (A former `not_occurs_free [x] P` guard here was spurious: P is the body and DOES
        // contain x, so with occurs_free corrected the guard would block every instantiation.)
        | Implies(ForAll(_, [x], True, P), P') when is_inst_expr x P P' ->
                pattern_desc' (lazy "Universal Instantiation") (lazy <@ fun x P P' -> (forall_expr x true P) = P' @>) |> Some
        | _ -> None

    // Hoisted operator templates: a quotation literal written inline in a match arm is
    // re-deserialized on EVERY probe of equational_logic_axioms — one of the hottest
    // paths in the prover (see docs/expressions-perf.md). Same quotation values,
    // evaluated once at module initialization.
    let private eq_op : Expr<bool->bool->bool> = <@ (=) @>
    let private neq_op : Expr<bool->bool->bool> = <@ (<>) @>
    let private or_op : Expr<bool->bool->bool> = <@ (||) @>
    let private not_op : Expr<bool->bool> = <@ not @>

    let equational_logic_axioms =
        function
        | SEqual x
        | DefTrue x // (3.3)
        | DefFalse x //(3.8)
        | BinaryOpDef eq_op neq_op eq_op not_op x // (3.10)

        | Assoc eq_op eq_op x  // (3.1)
        | Assoc eq_op or_op x // (3.25)

        | Symm eq_op x // (3.2)
        | Commute eq_op or_op x // (3.24)

        | Distrib eq_op or_op eq_op x  // (3.27)
        | DistribNot x // (3.9)
             
        | IdempotencyOr x // (3.26)
        
        | ExcludedMiddle x // (3.28)
        | GoldenRule x // (3.35)

        | Implication x  // (3.57 and 3.58)
        | Leibniz x  // (3.83)
        
        | EmptyRange x // (8.13)
        | OnePoint x // (8.14)
        | Nesting x
        | Renaming x
        | QuantifierCollect x 
        | RangeSplit x 
        | Interchange x 
        | Trading x 
        | ForAllDistribOr x 
        | GeneralizedDeMorgan x 
        | UniversalInstantiation x -> Some (desc x) 
        | _ -> None

    (* Expression functions for admissible rules *) 
       
    /// Reduce logical constants.
    let _reduce_constants  =
        function
        | Equals(Bool l, Bool r) -> boolConst (l = r)
        | NotEquals(Bool l, Bool r) -> boolConst (l <> r)
        | Not(Bool l) -> boolConst (not l)
        | Or(Bool l, Bool r) -> boolConst (l || r)
        | And(Bool l, Bool r) -> boolConst (l && r)
        | Implies(Bool l, Bool r) -> boolConst (l ===> r)
        | expr -> expr
    
    // NOTE for all the rewrite functions below: results are built with the mk_* Expr
    // builders (FsExpr/Formula), which produce exactly the same tree shapes as the
    // spliced quotation literals they replace — a <@@ … %%a … @@> literal re-deserializes
    // its pickled template on EVERY evaluation, and these functions run on every rule
    // application (see docs/expressions-perf.md).

    /// Binary logical operators are right associative.
    let _right_assoc =
        function
        | Equals(Equals(a1, a2), a3) -> mk_eq_bool a1 (mk_eq_bool a2 a3)
        | Or(Or(a1, a2), a3) -> mk_or a1 (mk_or a2 a3)
        | And(And(a1, a2), a3) -> mk_and a1 (mk_and a2 a3)
        | expr -> expr

    /// Binary logical operators are left associative.
    let _left_assoc =
        function
        | Equals(a1, Equals(a2, a3)) -> mk_eq_bool (mk_eq_bool a1 a2) a3
        | Or(a1, Or(a2, a3)) -> mk_or (mk_or a1 a2) a3
        | And(a1, And(a2, a3)) -> mk_and (mk_and a1 a2) a3
        | expr -> expr

    /// Binary logical operators commute.
    let _commute =
        function
        | Equals(a1, a2) -> mk_eq_bool a2 a1
        | Or(a1, a2) -> mk_or a2 a1
        | And(a1, a2) -> mk_and a2 a1
        | expr -> expr

    /// Distribute logical terms.
    let _distrib =
        function
        | Not(Equals(a1, a2)) -> mk_eq_bool (mk_not a1) a2
        | Or(a1, Equals(a2, a3)) -> mk_eq_bool (mk_or a1 a2) (mk_or a1 a3)
        | Or(p, And(q, r)) -> mk_and (mk_or p q) (mk_or p r)
        | Or(p, Or(q, r)) -> mk_or (mk_or p q) (mk_or p r)
        | And(p, Or(q, r)) -> mk_or (mk_and p q) (mk_and p r)
        | Not(Or(a1, a2)) -> mk_and (mk_not a1) (mk_not a2)
        | Not(And(a1, a2)) -> mk_or (mk_not a1) (mk_not a2)
        | expr -> expr

    /// Collect distributed logical terms.
    let _collect =
        function
        | Equals(Not a1, a2)  -> mk_not (mk_eq_bool a1 a2)
        | Equals(Or(a1, a2), Or(a3, a4)) when sequal a1 a3 -> mk_or a1 (mk_eq_bool a2 a4)
        | And(Or(a1, a2), Or(a3, a4)) when sequal a1 a3 -> mk_or a1 (mk_and a2 a4)
        | Or(And(a1, a2), And(a3, a4)) when sequal a1 a3 -> mk_and a1 (mk_or a2 a4)
        | Or(Or(a1, a2), Or(a3, a4)) when sequal a1 a3 -> mk_or a1 (mk_or a2 a4)
        | Or(Not p , Not q) -> mk_not (mk_and p q)
        | And(Not p , Not q) -> mk_not (mk_or p q)
        | expr -> expr

    /// || and && operators are idempotent.
    // NB: = (equivalence) is NOT idempotent: p = p is true (Reflexivity, Gries 3.5),
    // not p. Reflexivity of = is captured by the DefTrue axiom, so idemp leaves = alone.
    let _idemp =
        function
        | Or(a1, a2) when sequal a1 a2 -> a2
        | And(a1, a2) when sequal a1 a2 -> a2
        | expr ->expr

    let _excluded_middle =
        function
        | Or(a1, Not(a2)) when sequal a1 a2 -> tE
        | expr -> expr

    let _golden_rule =
        function
        | And(p, q) -> mk_eq_bool (mk_eq_bool p q) (mk_or p q)
        | expr -> expr

    let _def_implies =
        function
        | Implies(p, q) -> mk_eq_bool (mk_or p q) q
        | expr -> expr

    let _shunt =
        function
        | Implies(And(p, q), r) -> mk_implies p (mk_implies q r)
        | expr -> expr

    let _rshunt =
        function
        | Implies(p, Implies(q, r)) -> mk_implies (mk_and p q) r
        | expr -> expr

    let _mutual_implication =
        function
        | Equals(p, q) -> mk_and (mk_implies p q) (mk_implies q p)
        | expr -> expr

    let _subst_and =
        function
        | And(Equals(Var e, Var f), E) when E |> occurs [e] ->
            let E' = replace_var_var e f E
            mk_and (mk_eq_bool (Expr.Var e) (Expr.Var f)) E'
        | expr -> expr

    let _subst_implies =
        function
        | Implies(Equals(Var e, Var f), E) when E |> occurs [e] ->
            let E' = replace_var_var e f E
            mk_implies (mk_eq_bool (Expr.Var e) (Expr.Var f)) E'
        | expr -> expr

    let _subst_and_implies =
        function
        | Implies(And(q, Equals(Var e, Var f)), E) when E |> occurs [e] ->
            let E' = replace_var_var e f E
            mk_implies (mk_and q (mk_eq_bool (Expr.Var e) (Expr.Var f))) E'
        | expr -> expr

    let _subst_true =
        function
        | Implies(Var p,  E) when E |> occurs [p] ->
            let E' = replace_var_expr p tE E in
            mk_implies (Expr.Var p) E'
        | Implies(And(Var q, Var p),  E) when E |> occurs [p] ->
            let E' = replace_var_expr p tE E in
            mk_implies (mk_and (Expr.Var q) (Expr.Var p)) E'
        | And(Var p, E) when E |> occurs [p] ->
            let E' = replace_var_expr p tE E in
            mk_and (Expr.Var p) E'
        | expr -> expr

    let _subst_false =
        function
        | Implies(E, Var p) when E |> occurs [p] ->
            let E' = replace_var_expr p fE E in
            mk_implies E' (Expr.Var p)
        | Implies(E, Or(Var p, Var q)) when E |> occurs [p] ->
            let E' = replace_var_expr p fE E in
            mk_implies E' (mk_or (Expr.Var p) (Expr.Var q))
        | Or(Var p, E) when E |> occurs [p] ->
            let E' = replace_var_expr p fE E in
            mk_or (Expr.Var p) E'
        | expr -> expr

    let _subst_or_and = 
        function
        | Equals(E, Or(And(Var p1, Et), And(Not(Var p2), Ef))) when E |> occurs [p1] && p1.Name = p2.Name && Et = replace_var_expr p1 tE E && Ef = replace_var_expr p2 fE E ->
            tE
        | expr -> expr

    let rec _dual =
        function
        | False -> tE
        | True -> fE
        | Not p -> mk_not (_dual p)
        | And(p, q) -> mk_or (_dual p) (_dual q)
        | Or(p, q) -> mk_and (_dual p) (_dual q)
        | Equals _
        | NotEquals _
        | Implies _
        | Conseq _ 
        | ForAll _
        | Exists _ as expr -> failwithf "Expression %s not supported for dual operator." <| src expr
        | expr -> traverse expr _dual 

    /// Rewrite an expression as an equivalent double negation, distributing the inner
    /// negation with De Morgan's laws (and generalized De Morgan for quantifiers, so
    /// forall <-> exists). Unlike _dual, this preserves equivalence.
    let rec _double_neg =
        function
        | True -> mk_not fE
        | False -> mk_not tE
        | Equals(p, q) -> mk_not (mk_neq_bool (_double_neg p) (_double_neg q))
        | Not(NotEquals(p, q)) -> mk_eq_bool (_double_neg p) (_double_neg q)
        // p ==> q = not (p && not q)  (since not (p ==> q) = p && not q)
        | Implies(p, q) -> mk_not (mk_and (_double_neg p) (mk_not (_double_neg q)))
        | Not(Implies(p, q)) -> mk_and (_double_neg p) (mk_not (_double_neg q))
        // p <=== q = q ==> p = not (q && not p)
        | Conseq(p, q) -> mk_not (mk_and (_double_neg q) (mk_not (_double_neg p)))
        | Not(Conseq(p, q)) -> mk_and (_double_neg q) (mk_not (_double_neg p))
        | And(p, q) -> mk_not (mk_or (mk_not (_double_neg p)) (mk_not (_double_neg q)))
        | Or(p, q) -> mk_not (mk_and (mk_not (_double_neg p)) (mk_not (_double_neg q)))
        | ForAll(_, bound, range, body) -> let v = vars_to_tuple bound in mk_not (call exists_template (v::range::(mk_not body)::[]))
        | Exists(_, bound, range, body) -> let v = vars_to_tuple bound in mk_not (call forall_template (v::range::(mk_not body)::[]))
        | expr -> traverse expr _double_neg

    let _distrib_implies =
        function
        | And(p1, Implies(p2, q)) when sequal p1 p2 -> mk_and p1 q
        | And(p1, Implies(_, p2)) when sequal p1 p2 -> p1
        | Or(p1, Implies(p2, q)) when sequal p1 p2 -> tE
        | Or(p1, Implies(q, p2)) when sequal p1 p2 -> mk_implies q p1
        | Implies(Or(p1,  q1), And(p2,  q2)) when sequal2 p1 q1 p2 q2 -> mk_eq_bool p1 q1
        | expr -> expr
        
    let _empty_range = 
        function
        | ForAll(_,_,False,_) -> tE
        | Exists(_,_,False,_) -> fE
        | expr -> expr

    let _collect_forall_and =
        function
        | And(ForAll(_, b1, R, P), ForAll(_, b2, R', Q)) when vequal' b1 b2 && sequal R R' ->
            let t = vars_to_tuple b1 in call forall_template (t::R::(mk_and P Q)::[])
        | expr -> expr

    let _collect_exists_or =
        function
        | Or(Exists(_, b1, R, P), Exists(_, b2, R', Q)) when vequal' b1 b2 && sequal R R' ->
            let t = vars_to_tuple b1 in call exists_template (t::R::(mk_or P Q)::[])
        | expr -> expr

    let _trade_body =
        function
        | ForAll(_, x, R, P) -> let v = vars_to_tuple x in call forall_template (v::tE::(mk_implies R P)::[])
        | Exists(_, x, R, P) -> let v = vars_to_tuple x in call exists_template (v::tE::(mk_and R P)::[])
        | expr -> expr

    let _distrib_or_forall =
        function
        | Or(P, ForAll(_, x, N, Q)) when not_occurs_free x P -> let v = vars_to_tuple x in call forall_template (v::N::(mk_or P Q)::[])
        | expr -> expr

    let _split_range_forall =
        function
        | ForAll(_, x, Or(R1, R2), P) ->
            let c1 = let v = vars_to_tuple x in call forall_template (v::R1::P::[])
            let c2 = let v = vars_to_tuple x in call forall_template (v::R2::P::[])
            mk_and c1 c2
        | expr -> expr

    let _split_range_exists =
        function
        | Exists(_, x, Or(R1, R2), P) ->
            let c1 = let v = vars_to_tuple x in call exists_template (v::R1::P::[])
            let c2 = let v = vars_to_tuple x in call exists_template (v::R2::P::[])
            mk_or c1 c2
        | expr -> expr

    /// Shared core of the associativity/AC normalizers. Flattens maximal chains of
    /// a propositional operator (≡ over bool operands, ∨, ∧) and rebuilds them
    /// right-associated. When `sort` is true, the operands of each chain are reordered
    /// into a canonical order (full associative-commutative normalization); when false,
    /// operand order is preserved (associativity only).
    ///
    /// Equivalence-preserving because ≡/∨/∧ are associative (and, when sorting,
    /// commutative). The ≡ case is restricted to bool operands, so a non-bool `=`
    /// (e.g. an integer equality) is treated as an atom and never reassociated. Pure
    /// structural normalization: no idempotent/absorption simplification is performed.
    let private _normalize_with (sort: bool) : Expr -> Expr =
        // Collect the maximal chain of operands joined by a single operator.
        let rec flatten split e =
            match split e with
            | Some (l, r) -> flatten split l @ flatten split r
            | None -> [e]
        let splitOr  = function Or (l, r)  -> Some (l, r) | _ -> None
        let splitAnd = function And (l, r) -> Some (l, r) | _ -> None
        let splitEq  = function Equals (l, r) when l.Type = typeof<bool> -> Some (l, r) | _ -> None
        // Recursively normalize bottom-up; optionally sort operands by their
        // normalized text, then rebuild right-associated so the form is deterministic.
        let rec norm (e: Expr) =
            let canon split rebuild =
                flatten split e
                |> List.map norm
                |> (if sort then List.sortBy (fun (x: Expr) -> x.ToString()) else id)
                |> List.reduceBack rebuild
            match e with
            | Or _  -> canon splitOr  mk_or
            | And _ -> canon splitAnd mk_and
            | Equals (l, _) when l.Type = typeof<bool> -> canon splitEq mk_eq_bool
            | expr -> traverse expr norm
        function
        // The goal's top-level ≡ separates the two sides being proved equal:
        // normalize each side independently so an equivalent goal becomes C ≡ C
        // (closed by SEqual), instead of merging both sides into one ≡-chain.
        | Equals (l, r) when l.Type = typeof<bool> ->
            mk_eq_bool (norm l) (norm r)
        | expr -> norm expr

    /// Full associative-commutative normalization: flatten maximal ≡/∨/∧ chains,
    /// SORT their operands, and rebuild right-associated into a canonical form. A single
    /// application collapses the runs of left_assoc/right_assoc/commute bookkeeping
    /// otherwise needed to shape an expression into a rule or lemma's exact left-hand
    /// side: two AC-equivalent expressions normalize to the identical term, so SEqual
    /// then recognizes them and closes the proof. Best as a closing / shape-to-canonical
    /// move; because it reorders operands it can disturb a specific arrangement that a
    /// longer hand-derivation's later steps depend on (use _normalize_assoc there).
    let _normalize (e: Expr) : Expr = _normalize_with true e

    /// Associativity-only normalization: flatten maximal ≡/∨/∧ chains and rebuild
    /// right-associated, PRESERVING operand order (no commutative reordering). Reshapes
    /// association without disturbing operand order, so it can slot into a longer
    /// hand-derivation where full AC normalization (which sorts) would derail a later step.
    let _normalize_assoc (e: Expr) : Expr = _normalize_with false e

    /// Schematic single-node propositional simplification laws: identity, annihilator,
    /// complement, idempotence, double negation, and constant/reflexive equivalence.
    /// Every case is equivalence-preserving and size-non-increasing (so it drives a
    /// terminating fixpoint), and produces the named truth constants T/F. Matches only
    /// the top node; `_simp` applies it at every position. The ≡ cases are bool-typed by
    /// construction (the operands are propositions).
    let _simp_laws =
        function
        // identity and annihilator for the logical constants
        | And(True, p) | And(p, True) -> p
        | And(False, _) | And(_, False) -> fE
        | Or(False, p) | Or(p, False) -> p
        | Or(True, _) | Or(_, True) -> tE
        // complement
        | And(p, Not q) | And(Not q, p) when sequal p q -> fE
        | Or(p, Not q) | Or(Not q, p) when sequal p q -> tE
        // idempotence
        | And(p, q) when sequal p q -> p
        | Or(p, q) when sequal p q -> p
        // absorption
        | And(a, Or(b, c)) | And(Or(b, c), a) when sequal a b || sequal a c -> a
        | Or(a, And(b, c)) | Or(And(b, c), a) when sequal a b || sequal a c -> a
        // negation of constants and double negation
        | Not True -> fE
        | Not False -> tE
        | Not(Not p) -> p
        // equivalence with a constant, and reflexivity
        | Equals(True, p) | Equals(p, True) -> p
        | Equals(False, p) | Equals(p, False) -> mk_not p
        | Equals(p, q) when sequal p q -> tE
        | expr -> expr

    /// Whole-CHAIN simplification of a ∨ or ∧: apply idempotence, complement, and the
    /// identity/annihilator constants to the FLATTENED operand list instead of to one node.
    ///
    /// `_simp_laws` sees a single node at a time, so it cannot spot a repeated or complementary
    /// operand that association has separated — `p ∨ (q ∨ p)` and `(p ∨ q) ∨ ¬q` both survive it.
    /// That makes `_simp` NON-CONFLUENT on clauses: two AC-equal clauses can reduce to different
    /// normal forms (one collapsing to T, the other not), so an equality between them fails to
    /// close. Flattening first removes the dependence on association entirely.
    ///
    /// Equivalence-preserving (every case is an instance of idempotence, complement, identity or
    /// annihilation, modulo associativity/commutativity) and size-reducing, so it drives a
    /// terminating fixpoint. Operand ORDER is preserved and the chain is left untouched when
    /// nothing is removed, so it never disturbs a shape that `_simp` would otherwise have kept.
    let _chain_simp : Expr -> Expr =
        let rec flatten split e =
            match split e with
            | Some (l, r) -> flatten split l @ flatten split r
            | None -> [ e ]
        let dedup ops = ops |> List.fold (fun acc x -> if acc |> List.exists (sequal x) then acc else acc @ [x]) []
        let go split rebuild isUnit isAnnih unit_ annih e =
            let ops = flatten split e
            // an annihilator operand anywhere, or a complementary pair anywhere, collapses the chain
            if ops |> List.exists isAnnih then annih
            elif ops |> List.exists (fun x -> match x with Not a -> ops |> List.exists (sequal a) | _ -> false) then annih
            else
                let kept = ops |> List.filter (isUnit >> not) |> dedup
                if List.length kept = List.length ops then e            // nothing removed: keep the shape
                elif List.isEmpty kept then unit_
                else List.reduceBack rebuild kept
        let isTrue = function True -> true | _ -> false
        let isFalse = function False -> true | _ -> false
        function
        | Or _ as e -> go (function Or (l, r) -> Some(l, r) | _ -> None) mk_or isFalse isTrue fE tE e
        | And _ as e -> go (function And (l, r) -> Some(l, r) | _ -> None) mk_and isTrue isFalse tE fE e
        | e -> e

    /// Deterministic propositional simplifier: apply the size-reducing rules
    /// (`_simp_laws`, `_reduce_constants`, `_chain_simp`) at every subterm position bottom-up,
    /// then AC-normalize the whole term, iterating to a fixpoint. Closes any (sub)goal that
    /// collapses to T (or to a canonical identity `x = x`); leaves anything it cannot
    /// reduce unchanged. Terminating (each law is size-non-increasing, `_normalize` is
    /// idempotent) with a hard iteration cap as a safety net. Sound by construction: it
    /// only composes verified equivalence-preserving rules.
    let _simp : Expr -> Expr =
        // Apply the local laws bottom-up (implicit position enumeration via traverse).
        let rec localpass (e: Expr) =
            let e' = traverse e localpass
            e' |> _reduce_constants |> _simp_laws |> _chain_simp
        let onepass e = e |> localpass |> _normalize |> _reduce_constants |> _simp_laws |> _chain_simp
        let rec fix n e =
            if n <= 0 then e
            else let e' = onepass e in if sequal e' e then e else fix (n - 1) e'
        fix 200

    // ---- Boolean-ring (ANF) rewrite rules (PROTOTYPE, temporarily admitted) --------------
    // Drive a formula toward canonical XOR-of-AND normal form using local rewrites.

    /// Eliminate ¬, ∨, ⇒, ⇐, ≡ in favour of ⊕ (<>) and ∧.
    let _elim_to_xor =
        function
        | Not a -> mk_neq_bool a tE
        | Or (a, b) -> mk_neq_bool (mk_neq_bool (mk_and a b) a) b
        | Implies (a, b) -> mk_neq_bool (mk_neq_bool (mk_and a b) a) tE
        | Conseq (a, b) -> mk_neq_bool (mk_neq_bool (mk_and a b) b) tE
        | Equals (a, b) when a.Type = typeof<bool> -> mk_neq_bool (mk_neq_bool a b) tE
        | expr -> expr

    /// Distribute ∧ over ⊕: a ∧ (b ⊕ c) = (a∧b) ⊕ (a∧c).
    let _distrib_and_xor =
        function
        | And (a, NotEquals (b, c)) -> mk_neq_bool (mk_and a b) (mk_and a c)
        | And (NotEquals (b, c), a) -> mk_neq_bool (mk_and b a) (mk_and c a)
        | expr -> expr

    /// Normalize a ∧ monomial: flatten, F annihilates, drop T, DEDUP atoms (idempotence
    /// x∧x=x), sort. Needed because AC-normalize alone does not collapse repeated atoms in a
    /// nested ∧ (e.g. p ∧ (p ∧ q)), which would then fail to cancel at the ⊕ level.
    let _and_normalize =
        let rec flatten e = match e with | And (l, r) -> flatten l @ flatten r | x -> [x]
        function
        | And (_, _) as e ->
            let ops = flatten e
            if ops |> List.exists (fun x -> sequal x fE) then fE
            else
                let kept =
                    ops
                    |> List.filter (fun x -> not (sequal x tE))
                    |> List.distinctBy (fun (x: Expr) -> x.ToString())
                    |> List.sortBy (fun (x: Expr) -> x.ToString())
                match kept with
                | [] -> tE
                | [ x ] -> x
                | xs -> xs |> List.reduce mk_and
        | expr -> expr

    /// Normalize a ⊕ chain: flatten, cancel equal pairs (x⊕x = 0), drop the ⊕-identity F, sort.
    let _xor_normalize =
        let rec flatten e = match e with | NotEquals (l, r) -> flatten l @ flatten r | x -> [x]
        function
        | NotEquals (_, _) as e ->
            let ops = flatten e |> List.filter (fun x -> not (sequal x fE))
            let counts = ops |> List.countBy (fun (x: Expr) -> x.ToString())
            let kept =
                ops
                |> List.distinctBy (fun (x: Expr) -> x.ToString())
                |> List.filter (fun (x: Expr) -> (counts |> List.find (fun (k, _) -> k = x.ToString()) |> snd) % 2 = 1)
                |> List.sortBy (fun (x: Expr) -> x.ToString())
            match kept with
            | [] -> fE
            | [ x ] -> x
            | xs -> xs |> List.reduce mk_neq_bool
        | expr -> expr

    /// Algebraic normal form (ANF / Zhegalkin) over the Boolean ring (⊕, ∧, true), the
    /// canonical form on which propositional E-equivalence is DECIDABLE: every formula is a
    /// unique XOR of AND-monomials, and two formulas are equivalent iff their ANFs are equal.
    /// A monomial is a set of atom keys (∧ is idempotent, so x∧x=x → set union); an ANF is a
    /// set of monomials (⊕ is its own inverse, so a repeated monomial cancels → symmetric
    /// difference). Bool subterms that are not known connectives (variables, comparisons, …)
    /// are opaque atoms keyed by their text — treating them as free variables is sound: a
    /// formula valid for all atom assignments is valid regardless of what the atoms mean.
    module Anf =
        type private Monomial = Set<string>
        type private Poly = Set<Monomial>
        let private zero : Poly = Set.empty
        let private one : Poly = Set.singleton Set.empty
        let private xor (a: Poly) (b: Poly) : Poly = Set.union (Set.difference a b) (Set.difference b a)
        let private toggle (m: Monomial) (a: Poly) : Poly = if Set.contains m a then Set.remove m a else Set.add m a
        let private mul (a: Poly) (b: Poly) : Poly =
            Set.fold (fun acc ma -> Set.fold (fun acc2 mb -> toggle (Set.union ma mb) acc2) acc b) zero a
        let rec private poly (e: Expr) : Poly =
            match e with
            | True -> one
            | False -> zero
            | Bool b -> if b then one else zero
            | Not a -> xor (poly a) one
            | And (a, b) -> mul (poly a) (poly b)
            | Or (a, b) -> let x, y = poly a, poly b in xor (xor x y) (mul x y)
            | Implies (a, b) -> let x, y = poly a, poly b in xor one (xor x (mul x y))   // 1 ⊕ a ⊕ ab
            | Conseq (a, b) -> let x, y = poly a, poly b in xor one (xor y (mul x y))     // b ⇒ a = 1 ⊕ b ⊕ ab
            | NotEquals (a, b) -> xor (poly a) (poly b)                                    // XOR
            | Equals (a, b) when a.Type = typeof<bool> -> xor (xor (poly a) (poly b)) one  // a ≡ b = a ⊕ b ⊕ 1
            | atom -> Set.singleton (Set.singleton (atom.ToString()))

        /// True iff the bool expression is a propositional tautology (ANF = constant true).
        /// A decision TOOL, deliberately NOT a proof rule: it tells you whether a proof
        /// exists (complete for the propositional fragment) so you don't invest in proving a
        /// non-theorem — it is not part of the trusted base and never closes a proof itself.
        let is_tautology (e: Expr) : bool = e.Type = typeof<bool> && poly e = one

        /// True iff two bool expressions are propositionally equivalent (equal ANF), i.e. a
        /// proof of the identity a = b exists. Same tool character as is_tautology.
        let equivalent (a: Expr) (b: Expr) : bool =
            a.Type = typeof<bool> && b.Type = typeof<bool> && poly a = poly b