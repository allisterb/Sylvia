#load "Include.fsx"

open Sylvia
open PropCalculus
open PredCalculus


let p,q,r = boolvar3 "p" "q" "r"

let x = intvar "x"

let c = intconst "c"
x + 3
let P = Pred<int> (symbol="P")
let N = symbolic_pred<int> "N"
let Q = Pred<int> (<@ fun x -> x > 2 @>, symbol="Q")

N.Prop.Expr
//(P ==> Q).F
theorem pred_calculus (forall (x, N, P) == forall' (x, N ==> P)) []