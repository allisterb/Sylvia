#load "Include.fsx"
#nowarn "3391"   // a bare rule as a proof step is the implicit "apply to the whole expression"

// Predicate calculus (Gries & Schneider, "A Logical Approach to Discrete Math", ch. 8-9).
// Constructing, displaying and machine-checking quantified (∀ / ∃) theorems in Sylvia.

fsi.PrintWidth <- 420
fsi.PrintLength <- 1000

open Sylvia
open FSharp.Quotations
open PropCalculus
open PredCalculus

// A bound variable of type int, and some symbolic predicates over int: the application N.[x]
// is the proposition "N holds of x". `pp` is an ordinary proposition that does NOT mention the
// dummy x — the distributivity/trading laws require such an x-free operand (¬occurs(x, pp)).
let x = intvar "x"
let y = intvar "y"
let N = symbolic_pred<int> "N"
let P = symbolic_pred<int> "P"
let Q = symbolic_pred<int> "Q"
let R = symbolic_pred<int> "R"
let pp = boolvar "pp"

let show (title: string) (p: Prop) = printfn "  %-34s %s" title (Theory.S.PrintFormula (expand p.Expr))

// Each theorem below is checked simply by CONSTRUCTING it: the `ident`/`theorem` builders replay
// and verify the proof and throw if it does not close, so reaching `✓` means the derivation holds.
// The builders log every step as they go; the catalog silences that (see the worked proof at the
// end for a full trace) and just reports whether each theorem checked.
Proof.LogLevel <- 0
let check (name: string) (f: unit -> unit) =
    let saved = System.Console.Out
    try
        System.Console.SetOut System.IO.TextWriter.Null
        f ()
        System.Console.SetOut saved
        printfn "  ✓  %s" name
    with e ->
        System.Console.SetOut saved
        printfn "  ✗  %s\n        %s" name (e.Message.Split('\n').[0])

printfn "===== Building and displaying quantified formulas ====="
show "universal, with a range:"        (forall (x, N, P))
show "universal, empty (true) range:"  (forall' (x, P))
show "existential:"                    (exists (x, N, P))
show "mixed with connectives:"         (exists (x, N, P) ==> forall (x, N, P))
show "generalized De Morgan RHS:"      (- (forall (x, N, -P)))

printfn "\n===== Universal quantification (Gries 9.2-9.13) ====="
check "9.2   Trading:            (∀x|N:P) = (∀x|: N⇒P)"                  (fun () -> trade_forall_implies x N P |> ignore)
check "9.5   ∨ over ∀:           pp ∨ (∀x|N:Q) = (∀x|N: pp∨Q)"          (fun () -> distrib_or_forall' x N pp Q |> ignore)
check "9.6   trade out:          (∀x|N:pp) = pp ∨ (∀x|:¬N)"             (fun () -> trade_forall_or_not x N pp |> ignore)
check "8.15  ∀ over ∧ (collect): (∀x|N:P) ∧ (∀x|N:Q) = (∀x|N: P∧Q)"     (fun () -> collect_forall_and' x N P Q |> ignore)
check "9.8   empty body:         (∀x|N:true) = true"                    (fun () -> ident_forall_true x N |> ignore)
check "9.9   distribute =:       (∀x|N:P=Q) ⇒ ((∀x|N:P)=(∀x|N:Q))"      (fun () -> distrib_forall_body x N P Q |> ignore)
check "9.10  range strengthen:   (∀x|N1∨N2:P) ⇒ (∀x|N1:P)"              (fun () -> strengthen_forall_range_or x N P Q |> ignore)
check "9.11  body strengthen:    (∀x|N:P∧Q) ⇒ (∀x|N:P)"                 (fun () -> strengthen_forall_body_and x N P Q |> ignore)
check "9.12  monotonicity of ∀:  (∀x|N:Q⇒P) ⇒ ((∀x|N:Q)⇒(∀x|N:P))"      (fun () -> mono_forall_body x N Q P |> ignore)
check "9.13  instantiation:      (∀x|:P) ⇒ P[x:=3]"                     (fun () -> inst x P (Scalar<int> 3) |> ignore)
check "9.16  ⇐ metatheorem:      pp ⇒ (∀x|:pp)"                         (fun () -> forall_conseq x pp |> ignore)

printfn "\n===== Existential quantification (Gries 9.17-9.29) ====="
check "9.17  De Morgan:          (∃x|N:P) = ¬(∀x|N:¬P)"                 (fun () -> ident_exists_not_forall x N P |> ignore)
check "9.18b De Morgan:          ¬(∃x|N:P) = (∀x|N:¬P)"                 (fun () -> ident_not_exists_forall_not x N P |> ignore)
check "9.19  Trading:            (∃x|N:P) = (∃x|: N∧P)"                 (fun () -> trade_exists_and x N P |> ignore)
check "8.15  ∃ over ∨ (collect): (∃x|N:P) ∨ (∃x|N:Q) = (∃x|N: P∨Q)"     (fun () -> collect_exists_or' x N P Q |> ignore)
check "9.21  ∧ over ∃:           pp ∧ (∃x|N:Q) = (∃x|N: pp∧Q)"          (fun () -> distrib_and_exists_and x N pp Q |> ignore)
check "9.22  split off:          (∃x|N:pp) = pp ∧ (∃x|:N)"              (fun () -> distrib_and_exists x N pp |> ignore)
check "9.24  empty body:         (∃x|N:false) = false"                 (fun () -> ident_exists_false x N |> ignore)
check "9.25  range weaken:       (∃x|N:P) ⇒ (∃x|Q∨N:P)"                 (fun () -> weaken_exists_range x N P Q |> ignore)
check "9.26  body weaken:        (∃x|N:P) ⇒ (∃x|N: P∨Q)"                (fun () -> weaken_exists_body x N P Q |> ignore)
check "9.27  monotonicity of ∃:  (∀x|N:Q⇒P) ⇒ ((∃x|N:Q)⇒(∃x|N:P))"      (fun () -> mono_exists x N Q P |> ignore)
check "9.28  ∃-introduction:     P[x:=3] ⇒ (∃x|:P)"                     (fun () -> exists_intro x P (Scalar<int> 3) |> ignore)
let q2 = Var("q2", typeof<int -> int -> bool>)
let pxy = Expr.Application(Expr.Application(Expr.Var q2, x.Expr), y.Expr) |> expand_as<bool> |> Prop
check "9.29  interchange:        (∃x|:∀y|:P) ⇒ (∀y|:∃x|:P)"             (fun () -> exists_forall_interchange x y pxy |> ignore)

// A worked calculational derivation, printed step by step: trading a conjunctive range
// (Gries 9.4a),   (∀x | Q∧N : P) = (∀x | Q : N⇒P),
// by Trading the whole range in (9.2), Shunting in the body, then Trading Q back out.
printfn "\n===== A worked proof: trading a conjunctive range (Gries 9.4a) ====="
Proof.LogLevel <- 1
trade_forall_and_implies x Q N P |> ignore

// ---------------------------------------------------------------------------
// Exercise 9.1. Derive Distributivity of ∨ over ∀ (9.5) for an arbitrary range,
//   pp ∨ (∀x|R:Q)  =  (∀x|R: pp∨Q)     (pp x-free),
// from ONLY the true-range version   pp ∨ (∀x|:B) = (∀x|: pp∨B)   plus Trading —
// i.e. show the general axiom follows from the simpler one. The recipe (per Gries):
// trade the range into the body, apply the given true-range law, then trade back.
printfn "\n===== Exercise 9.1: derive (9.5) from its true-range version ====="
Proof.LogLevel <- 0

// The propositional body identity used along the way:  pp ∨ (R⇒Q) = R ⇒ (pp∨Q).
let bodyLemma (p: Prop) (r: Prop) (q: Prop) =
    ident prop_calculus ((p + (r ==> q)) == (r ==> (p + q))) [
        ident_implies_not_or r q |> at [left_branch; right_branch]
        ident_implies_not_or r (p + q) |> at_right
        normalize
    ]

// The GIVEN simpler axiom (distributivity over a true range only):
let trueRange (x: TermVar<'t>) (P: Prop) (B: Pred<'t>) =
    id_ax pred_calculus ((P + forall'(x, B)) == qall x T (P + B[x]))

let ex_9_1 (x: TermVar<'t>) (R: Pred<'t>) (Q: Pred<'t>) (P: Prop) =
    ident pred_calculus ((P + forall(x, R, Q)) == qall x R[x] (P + Q[x])) [
        trade_forall_implies x R Q |> at [left_branch; right_branch]  // ∀x|R:Q → ∀x|: R⇒Q   (Trading)
        trueRange x P (R ==> Q) |> at_left                            // pull pp inside (the given law)
        bodyLemma P R[x] Q[x] |> at [left_branch; select_body]        // pp∨(R⇒Q) → R⇒(pp∨Q)
        trade_body |> at_right                                       // RHS ∀x|R:(pp∨Q) → ∀x|: R⇒(pp∨Q)
    ]

check "9.1  general (9.5) from true-range: pp ∨ (∀x|R:Q) = (∀x|R: pp∨Q)" (fun () -> ex_9_1 x R Q pp |> ignore)

printfn "\nDone."
