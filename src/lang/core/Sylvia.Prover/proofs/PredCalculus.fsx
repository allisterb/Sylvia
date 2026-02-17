#load "Include.fsx"

open System.Reflection
open Sylvia
open PropCalculus
open PredCalculus

let p,q,r = boolvar3 "p" "q" "r"

//ident_and_eq_all p p q
proof prop_calculus ((p * q * r) == (p == q == r == (p + q) == (q + r) == (r + p) == (p + q + r))) [
    golden_rule' p q |> apply_left
    golden_rule' ( (p == q) == (p + q) ) r |> apply_left 
    commute_or ( ((p == q) == (p + q)) ) r |> apply_left
    distrib_or_eq r ( p == q ) ( p + q ) |> apply_left
    distrib_or_eq r p q |> apply_left
    right_assoc_eq ( p == q ) ( p + q ) r |> apply_left
    commute_eq ( p + q ) r |> apply_left
    commute_or r q |> apply_left
    commute_eq ( r + p ) ( q + r ) |> apply_left
    commute_or r ( p + q ) |> apply_left
    left_assoc_eq ( p == q ) r ( p + q ) |> apply_left
    left_assoc |> apply_left
    left_assoc_eq ( p == q == r == (p + q) ) ( q + r ) ( r + p ) |> apply
]

((p ==> q) == ((p * q) == p))

ident prop_calculus ((p ==> q) == ((p * q) = p)) [
        def_implies |> apply_left
        commute |> apply
        right_assoc |> apply
        commute |> apply_right |> branch_right 
        left_assoc |> apply_right 
    ]

proof prop_calculus ((p * q) ==> (p * (q + r)) == F) [
    ident_implies_eq_and_eq (p * q) (p * (q + r)) |> apply
]
proof prop_calculus ((p &&& q) == (q * p)) [
    golden_rule' p q |> apply_left
    golden_rule' q p |> apply_right
    commute_or q p |> apply_right
    commute_eq q p |> apply_right
]

//open ProofModules

//PropCalculus.Type.GetMembers()

//getModuleFields PropCalculus.Type typeof<Rule> |> Array.filter(fun f -> f.Name = "get_reduce") |> Array.map(fun m -> match (m.GetCustomAttributes(typeof<AdmittedRuleAttribute>, false) |> List.ofArray) with | a::[] -> "ll" | _ -> "" )
//getModuleDerivedRules PropCalculus.Type



p * q


ident prop_calculus (((p * q) ==> (p * (q + r))))

ident prop_calculus ( p ==> q == ((p * q) == p) ) [
        def_implies |> apply_left
        commute |> apply
        right_assoc |> apply
        commute |> apply_right |> branch_right 
        left_assoc |> apply_right 
    ]

(*
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
*)