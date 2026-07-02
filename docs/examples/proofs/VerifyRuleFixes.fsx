#load "Include.fsx"

// Verification harness for the three equational-logic (S) rule fixes:
//   1. _idemp  : = (equivalence) is NOT idempotent  (p = p is true, not p)
//   2. _double_neg : correct rewrite of ==> and <===
//   3. _subst_false : E must stay the antecedent of ==>
//
// Each rule must be equivalence-preserving. We check that by evaluating the
// input and the rewritten output over every boolean assignment to their free
// variables, using an INDEPENDENT truth-table oracle (defined below), so the
// check does not rely on any of the prover's own semantics.

open Sylvia
open Sylvia.Formula
open PropCalculus
open FSharp.Quotations

let p, q, r = boolvar3 "p" "q" "r"

// --- Independent boolean oracle over the raw quotation structure ------------
let rec eval (env: Map<string, bool>) (e: Expr) : bool =
    match e with
    | DerivedPatterns.Bool b            -> b
    | Patterns.Var v                    -> env.[v.Name]
    | True                              -> true
    | False                             -> false
    | Not a                             -> not (eval env a)
    | And (a, b)                        -> eval env a && eval env b
    | Or  (a, b)                        -> eval env a || eval env b
    | Equals (a, b)                     -> eval env a = eval env b
    | NotEquals (a, b)                  -> eval env a <> eval env b
    | Implies (a, b)                    -> (not (eval env a)) || eval env b
    | Conseq (a, b)                     -> (not (eval env b)) || eval env a   // p <=== q  ==  q ==> p
    | _ -> failwithf "oracle: unsupported node %s" (src e)

let freeNames (e: Expr) = e.GetFreeVars() |> Seq.map (fun v -> v.Name) |> Seq.toList

let equivalent (a: Expr) (b: Expr) =
    let names = (freeNames a @ freeNames b) |> List.distinct
    let n = List.length names
    seq {
        for mask in 0 .. (1 <<< n) - 1 ->
            let env = names |> List.mapi (fun i nm -> nm, (mask >>> i) &&& 1 = 1) |> Map.ofList
            eval env a = eval env b
    }
    |> Seq.forall id

let check name (input: Expr) (rule: Expr -> Expr) =
    let input = expand input
    let output = rule input
    let ok = equivalent input output
    printfn "[%s] %s" (if ok then "PASS" else "FAIL") name
    printfn "      in : %s" (src input)
    printfn "      out: %s" (src output)
    printfn "      equivalence-preserving: %b" ok
    printfn ""
    ok

printfn "=== Verifying S rule fixes (post-fix build) ===\n"

let mutable allOk = true

// Fix 1: idemp on (p = p). Must NOT become p; it is true. Rule should leave it
// unchanged (equivalence-preserving) rather than rewrite to p (which was unsound).
allOk <- check "idemp on (p = p)" (p == p).Expr EquationalLogic._idemp && allOk

// Sanity: idemp still works on || and && (should rewrite, staying equivalent).
allOk <- check "idemp on (p || p)" (p + p).Expr EquationalLogic._idemp && allOk
allOk <- check "idemp on (p && p)" (p * p).Expr EquationalLogic._idemp && allOk

// Fix 2: double_neg on (p ==> q) and on the consequence operator.
allOk <- check "double_neg on (p ==> q)"  (p ==> q).Expr        EquationalLogic._double_neg && allOk
allOk <- check "double_neg on !(p ==> q)" (!! (p ==> q)).Expr   EquationalLogic._double_neg && allOk

// Fix 3: subst_false on (E ==> (p || q)) with E containing p.
allOk <- check "subst_false on (p ==> (p || q))"        (p ==> (p + q)).Expr        EquationalLogic._subst_false && allOk
allOk <- check "subst_false on ((p && r) ==> (p || q))" ((p * r) ==> (p + q)).Expr  EquationalLogic._subst_false && allOk

printfn "=== %s ===" (if allOk then "ALL CHECKS PASSED" else "SOME CHECKS FAILED")
exit (if allOk then 0 else 1)
