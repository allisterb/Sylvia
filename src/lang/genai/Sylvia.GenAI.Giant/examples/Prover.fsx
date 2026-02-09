#load "Include.fsx"

open Sylvia.GenAI.Giant.Examples

(* GIANT Prover function calling examples begin here *)

let p = proof "prop_calculus" "((p != q) == (-(p == q)))" []

p.Stmt