#load "Include.fsx"

open Sylvia
open PropCalculus

let p,q,r = boolvar3 "p" "q" "r"

//proof prop_calculus <@(true = true) = true@> [LR Commute]
let ``3.12`` = proof prop_calculus ((!! (!!p)) == p)  [
        apply collect 
        //commute_or (p * p) |> apply
        def_false p |> Commute |> apply
        not_false |> Truth |> apply
        
        //def_true <@ p @> |> LR
]


let ``3.31`` = proof prop_calculus ((p + (q + r)) == ((p + q) + (p + r))) [
    idemp_or p |> Commute |> apply_left
    right_assoc |> apply_left
    left_assoc_or p q r |> apply_left
    commute_or p q |> apply_left
    right_assoc_or q p r |> apply_left
    apply_left left_assoc
]
