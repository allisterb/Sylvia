#load "Include.fsx"

// Set print width to 120 characters
fsi.PrintWidth <- 420

// Set print length for collections (e.g., to 1000 items)
fsi.PrintLength <- 1000

open System.Reflection
open Sylvia
open PropCalculus
open PredCalculus

let p,q,r = boolvar3 "p" "q" "r"

let ident_and_eq_all2 p q r = proof prop_calculus ((p * q * r) == (p == q == r == (p + q) == (q + r) == (r + p) == (p + q + r))) [
        golden_rule' p q |> apply_left |> branch_left
        golden_rule' ( (p == q) == (p + q) ) r |> apply_left 
        commute_or ( ((p == q) == (p + q)) ) r |> apply_left
        distrib_or_eq r ( p == q ) ( p + q ) |> apply_left
        distrib_or_eq r p q |> apply_left
        right_assoc_eq ( p == q ) ( p + q ) r |> apply_left
        commute_eq ( p + q ) r |> apply_left
        
        commute_or r q |> apply_right |> branch_left |> branch_right |> branch_left
        
        commute_eq ( r + p ) ( q + r ) |> apply_left
        commute_or r ( p + q ) |> apply_left
        left_assoc_eq ( p == q ) r ( p + q ) |> apply_left
        left_assoc |> apply_left
        
        left_assoc_eq (p == q == r == (p + q)) (q + r) (r + p) |> apply_left |> branch_left
        
        
    ]

let pr = ident_and_eq_all2 p q (p * q)
pr

(left_state pr |> src) + "\n" + (right_state pr |> src)

//pr |> left_state |> expand_right |> expand_left |> expand_right |> src

//(left_state pr |> src) + "\n" + (right_state pr |> src)



