#load "Include.fsx"

open System.Reflection
open Sylvia
open PropCalculus
open PredCalculus


PropCalculus.Type

let p,q,r = boolvar3 "p" "q" "r"


id_ax prop_calculus ((p + p) == p) 
(p + p == p).Expr
(p + p).Expr
((p + F)).Expr 
theorem prop_calculus ((p + F) == p) [
        def_false p |> apply_right |> branch_left
        apply_left distrib
        apply right_assoc
        idemp_or p |> apply_right
        apply_left excluded_middle
    ]
//ident_implies_not_or p q

let x = intvar "x"

let c = intconst "c"
x + 3
let P = Pred<int> (symbol="P")
let N = symbolic_pred<int> "N"
let Q = Pred<int> (<@ fun x -> x > 2 @>, symbol="Q")

N.Prop.Expr
//(P ==> Q).F
    /// forall x N P = (P ||| forall x true (not N))
theorem pred_calculus (forall(x, N, P) == (P[x] + forall' (x, (-N)))) [
        distrib_or_forall |> apply_right
        commute_or P[x] -N[x] |> apply_right
        ident_implies_not_or N[x] P[x] |> Commute |> apply_right
    ]
  