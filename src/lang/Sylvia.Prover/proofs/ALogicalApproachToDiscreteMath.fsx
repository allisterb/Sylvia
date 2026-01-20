#load "Include.fsx"

open Sylvia
open PropCalculus

let p,q,r = boolvar3 "p" "q" "r"

let x = intvar "x"
let P = Pred<int> "P"

let p1 = proof prop_calculus ((p * q) ==> (p + q)) [
    def_implies |> apply
    left_assoc_or (p * q) p q |> apply_left
    commute_or (p * q) p |> apply_left |> recurse_left
    absorb_or p q |> apply_left |> recurse_left
]

let j = def prop_calculus (p == r)

let p2 = proof prop_calculus (p == r) [
    apply j
]