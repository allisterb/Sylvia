#load "Include.fsx"

open System.Reflection
open Sylvia
open PropCalculus
open PredCalculus

open ProofModules

getModuleTheorems PropCalculus.Type

let p,q,r = boolvar3 "p" "q" "r"

let x = intvar "x"

let c = intconst "c"
x + 3
let P = Pred<int> (symbol="P")
let N = symbolic_pred<int> "N"
let N1 = symbolic_pred<int> "N1"
let N2 = symbolic_pred<int> "N2"
let Q = Pred<int> (<@ fun x -> x > 2 @>, symbol="Q")

theorem pred_calculus (forall (x, N, P) == (P[x] + forall'(x, -N))) [
    distrib_or_forall |> apply_right
    commute_or P[x] -N[x] |> apply_right
    ident_implies_not_or N[x] P[x] |> Commute |> apply_right 
]

theorem pred_calculus (forall (x, (N1 + N2), P) == (forall (x, N1, P)) * (forall (x, N2, P))) []
 
split_range_forall' x N1 N2 P

id_ax pred_calculus (P[x] + forall(x, N, Q) == (forall(x, N, (P + Q))))