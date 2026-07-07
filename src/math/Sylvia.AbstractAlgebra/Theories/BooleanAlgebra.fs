namespace Sylvia

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Formula
open Patterns
open Descriptions

/// Theory of Boolean algebra on a set closed under 2 binary operations that are associative, commutative, and idempotent,
/// with identity elements zero and one, and a unary inverse or complement operation.
module BooleanAlgebra =
    let private desc = axiom_desc "Boolean Algebra" 
    
    (* Axioms *)

    let boolean_algebra_axioms (theoryName:string) (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) (zero: Expr<'t>) (one: Expr<'t>) (comp: Expr<'t->'t>) = 
        function
        | Assoc <@(=)@> join x
        | Assoc <@(=)@> meet x
                
        | Commute <@(=)@> join x
        | Commute <@(=)@> meet x

        | Idempotency <@(=)@> join x
        | Idempotency <@(=)@> meet x

        | Identity <@(=)@> join zero x
        | Identity <@(=)@> meet one x
                
        | Inverse <@(=)@> join comp one x
        | Inverse <@(=)@> meet comp zero x

        | Distrib <@(=)@> join meet x 
        | Distrib <@(=)@> meet join x -> desc x |> set_axiom_desc_theory theoryName |> Some
        | _ -> None

    (* Expression functions for admitted rules *)
    
    let rec _right_assoc (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) =
        function
        | Binary join (Binary join (a1, a2), a3) -> <@@ (%join) %%a1  ((%join) %%a2 %%a3) @@>
        | Binary meet (Binary meet (a1, a2), a3) -> <@@ (%join) %%a1  ((%join) %%a2 %%a3) @@>
        | expr -> traverse expr (_right_assoc join meet)

    let rec _left_assoc (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) =
        function
        | Binary join (a1, Binary join (a2, a3)) -> <@@ (%join) ((%join) %%a1 %%a2) %%a3 @@>
        | Binary meet (a1, Binary meet (a2, a3)) -> <@@ (%meet) ((%meet) %%a1 %%a2) %%a3 @@>
        | expr -> traverse expr (_left_assoc join meet)

    let rec _commute (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) =
        function
        | Binary join (a1, a2) -> <@@ (%join) %%a2 %%a1 @@>
        | Binary meet (a1, a2) -> <@@ (%meet) %%a2 %%a1 @@>
        | expr -> traverse expr (_commute join meet)

    let rec _idemp (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) (zero: Expr<'t>)  (one: Expr<'t>) (comp:Expr<'t -> 't>) =
         function
         | Binary join (a1, a2) when sequal a1 a2 -> <@@ %%a1 @@>
         | Binary meet (a1, a2) when sequal a1 a2 -> <@@ %%a1 @@> 
         | expr -> traverse expr (_idemp join meet zero one comp)

    let rec _ident (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) (zero: Expr<'t>)  (one: Expr<'t>) (comp:Expr<'t -> 't>) =
         function
         | Binary join (a1, Val zero _) -> <@@ %%a1 @@>
         | Binary meet (a1, Val one _) -> <@@ %%a1 @@>
         | expr -> traverse expr (_ident join meet zero one comp)

    let rec _comp (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) (zero: Expr<'t>) (one: Expr<'t>) (comp:Expr<'t -> 't>) =
         function
         // a ∪ ~a = one (e.g. the universe U);  a ∩ ~a = zero (e.g. the empty set ∅)
         | Binary join (a1, Unary comp a2) when sequal a1 a2 -> <@@ %one @@>
         | Binary meet (a1, Unary comp a2) when sequal a1 a2 -> <@@ %zero @@>
         | expr -> traverse expr (_comp join meet zero one comp)

    let rec _distrib (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) =
        function
        // x * ( y + z) == x * y + x * z
        | Binary meet (a1, Binary join (a2, a3)) -> <@@ (%join) ((%meet) %%a1 %%a2)  ((%meet) %%a1 %%a3) @@> 
        // x + ( y * z) == x + y * x * z
        | Binary join (a1, Binary meet (a2, a3)) -> <@@ (%meet) ((%join) %%a1 %%a2)  ((%join) %%a1 %%a3) @@> 
        | expr -> traverse expr (_distrib join meet)

    (* Admitted rules *)

    /// Expression is left-associative.
    let left_assoc join meet = Admit("(expression) is left-associative", _left_assoc join meet)
    
    /// Expression is right associative.
    let right_assoc join meet = Admit("(expression) is right-associative", _right_assoc join meet)

    /// Expression is commutative.
    let commute join meet = Admit("(expression) is commutative", _commute join meet)

    /// Idempotent operation in (expression) can be simplified.
    let idemp join meet zero one comp = Admit("Idempotent operation in (expression) can be simplified", _idemp join meet zero one comp)
    
    /// Identity in (expression) can be simplified.
    let ident_ join meet zero one comp = Admit("Identity in (expression) can be simplified", _ident join meet zero one comp)

    /// Complement operation in (expression) can be reduced.
    let complement join meet zero one comp = Admit("Complement operation in (expression) can be simplified", _comp join meet zero one comp)
    
    /// (expression) is distributive
    let distrib join meet = Admit("(expression) is distributive", _distrib join meet)

    /// Compose two axiom recognizers: try `extra` first, then fall back to `base'`. Used to layer a
    /// subclass's extra axioms on top of the base Boolean-algebra axioms rather than dropping them.
    let combine_axioms (extra: Axioms) (base': Axioms) : Axioms =
        fun e -> match extra e with | Some d -> Some d | None -> base' e

    type BooleanAlgebra<'t when 't: equality>(theoryName: string, join: Expr<'t->'t->'t>, meet: Expr<'t->'t->'t>, zero: Expr<'t>, one: Expr<'t>, comp: Expr<'t->'t>,
        ?axioms:Axioms, ?rules:Rules, ?formula_printer:Expr->string) =
        // A subclass may supply extra axioms/rules; compose them with (not discard) the Boolean-algebra
        // ones. Extra axioms are tried first (so a subclass can specialize); extra rules are appended
        // AFTER the seven built-ins so their fixed indices (used by SetAlgebra) are preserved.
        inherit Theory(
            combine_axioms (defaultArg axioms (fun _ -> None)) (boolean_algebra_axioms theoryName join meet zero one comp),
            [
                left_assoc join meet
                right_assoc join meet
                commute join meet
                idemp join meet zero one comp
                ident_ join meet zero one comp
                complement join meet zero one comp
                distrib join meet
            ] @ (defaultArg rules []),
            ?formula_printer = formula_printer)