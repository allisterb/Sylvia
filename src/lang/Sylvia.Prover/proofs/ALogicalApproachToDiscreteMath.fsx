#load "Include.fsx"

open Sylvia
open PropCalculus

let p,q,r = boolvar3 "p" "q" "r"

let a = bool
//proof prop_calculus <@(true = true) = true@> [LR Commute]
let ``3.12`` = proof prop_calculus ((!!p) == p)  [
        apply collect 
        //commute_or (p * p) |> apply
        def_false p |> Commute |> apply
        not_false |> Truth |> apply
        
        //def_true <@ p @> |> LR
]


let ``3.31`` = proof prop_calculus (((p ||| (q ||| r)) == ((p ||| q) ||| (p ||| r)))) [
    idemp_or p |> Commute |> apply_left
    right_assoc |> apply_left
    left_assoc_or p q r |> apply_left
    commute_or p q |> apply_left
    right_assoc_or q p r |> apply_left
    apply_left left_assoc
]


/// p ∨ (q ∧ r) = ((p ∨ q) ∧ (p ∨ r))
let ``ddd`` = ident prop_calculus (p ||| (q &&& r) == ((p ||| q) &&& (p ||| r)))  [
    golden_rule |> ApplyRight |> RecurseLeft
    distrib |> ApplyLeft
    distrib |> ApplyLeft |> RecurseLeft
    distrib_or_or p q r |> ApplyLeft
    golden_rule' (p ||| q)  (p ||| r)  |> Commute |> ApplyLeft
]

let ``3.76c`` = theorem prop_calculus ((p &&& q) ==> (p ||| q)) [
    def_implies |> apply
    commute |> apply_left |> recurse_left
    distrib |> apply_left |> recurse_left
    commute |> apply
    idemp_or p |> apply
    distrib |> apply_left |> recurse_right
    idemp_and p |> apply
    
    distrib |> apply
    distrib |> apply_right |> recurse_left
    distrib |> apply_left
    idemp_or p |> apply_left
    distrib |> apply_left
    
    commute_or q p |> apply_left
    idemp_and (p + q) |> apply_left
    commute |> apply_left |> recurse_left
    distrib |> apply_left |> recurse_left
    idemp_and q |> apply_left

    absorb_or q p |> CommuteL |> apply_left
    commute |> apply_right |> recurse_left
    left_assoc |> apply_left
    idemp_or q |> apply_left
] 
