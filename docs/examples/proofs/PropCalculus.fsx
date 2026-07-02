#load "Include.fsx"

// Set print width to 120 characters
fsi.PrintWidth <- 420

// Set print length for collections (e.g., to 1000 items)
fsi.PrintLength <- 1000

open Sylvia
open PropCalculus


let p,q,r = boolvar3 "p" "q" "r"

 // p ∨ (p ∧ q) = p
proof prop_calculus (p + (p * q) == p) [
    golden_rule |> apply_right |> left_branch    
    distrib |> apply_left
    left_assoc_or p p q |> apply_left
    idemp_or p |> apply_left
    distrib_or_eq p p q |> apply_left
    idemp_or p |> apply_left
]


