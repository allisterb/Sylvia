#load "Include.fsx"

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
let N = intpred "N"
let P = intpred "P"
let P' = boolvar "P"
let Q = intpred "Q"
let Q' = boolvar "Q"
let R = intpred "R"
let pp = boolvar "pp"
let qq = boolvar "qq"

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
check "9.7   ∧ over ∀ (cond):     ¬(∀x|:¬N) ⇒ ((∀x|N:pp∧Q) = pp∧(∀x|N:Q))" (fun () -> distrib_forall_and_cond x N pp Q |> ignore)
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

printfn "9.1  general (9.5) from true-range: pp ∨ (∀x|R:Q) = (∀x|R: pp∨Q)"
let ex_9_1 = ident pred_calculus ((P' + forall(x, R, Q)) == qall x R[x] (P' + Q[x])) [
        trade_forall_implies x R Q |> at [left_branch; right_branch]  // ∀x|R:Q → ∀x|: R⇒Q   (Trading)
        trueRange x P' (R ==> Q) |> at_left                            // pull pp inside (the given law)
        bodyLemma P' R[x] Q[x] |> at [left_branch; select_body]        // pp∨(R⇒Q) → R⇒(pp∨Q)
        trade_body |> at_right                                       // RHS ∀x|R:(pp∨Q) → ∀x|: R⇒(pp∨Q)
    ]
// Exercise 9.6. Range split for ∀ WITHOUT the range-split axiom (8.18):
//   (∀x|R:P) ∧ (∀x|Q:P) = (∀x|R∨Q:P),  from Trading (9.2), Distributivity (8.15) and Case analysis (3.78).
printfn "9.6  range split w/o axiom 8.18: (∀x|R:P)∧(∀x|Q:P) = (∀x|R∨Q:P)"
let ex_9_6 = ident pred_calculus (((forall(x,R,P)) * (forall(x,Q,P))) == forall(x, R + Q, P)) [
        trade_forall_implies x R P |> at [left_branch; left_branch]
        trade_forall_implies x Q P |> at [left_branch; right_branch]
        collect_forall_and' x truepred (R ==> P) (Q ==> P) |> at_left    // (∀x|:A)∧(∀x|:B) → ∀x|:(A∧B)
        case_analysis_1 R[x] Q[x] P[x] |> at [left_branch; select_body]  // (R⇒P)∧(Q⇒P) → (R∨Q⇒P)
        trade_forall_implies x (R + Q) P |> at_right
    ]


// Exercise 9.27.  (∃x|R:P) ⇒ Q  =  (∀x|R: P⇒Q)   (Q x-free) — an ∃ in an antecedent becomes a ∀.
printfn "9.27 ∃-antecedent: (∃x|R:P) ⇒ Q = (∀x|R: P⇒Q)"
let ex_9_27 = ident pred_calculus (((exists(x,R,P)) ==> Q') == qall x R[x] (P[x] ==> Q')) [
        ident_implies_not_or (exists(x,R,P)) Q' |> at_left                  // → ¬(∃x|R:P) ∨ Q
        ident_not_exists_forall_not x R P |> at [left_branch; left_branch] // ¬(∃x|R:P) → (∀x|R:¬P)
        commute_or (forall(x,R,-P)) Q' |> at_left
        distrib_or_forall' x R Q' (-P) |> at_left                          // Q ∨ (∀x|R:¬P) → (∀x|R: Q∨¬P)
        commute_or Q' (-(P[x])) |> at [left_branch; select_body]
        ident_implies_not_or P[x] Q' |> Commute |> at [left_branch; select_body]  // ¬P∨Q → P⇒Q
    ]

// Exercise 9.20 → Trading ∨ out of ∃ (9.23), a CONDITIONAL identity discharged with a `Deduce`
// on the range-nonempty assumption:  (∃x|:R) ⇒ ((∃x|R:pp∨Q) = pp ∨ (∃x|R:Q))   (pp x-free).
let collectMixed (x: TermVar<'t>) (R: Pred<'t>) (P: Prop) (Q: Pred<'t>) =
    id_ax pred_calculus (((qex x R[x] P) + exists(x,R,Q)) == qex x R[x] (P + Q[x]))

printfn "9.23 trade ∨ out of ∃ (conditional): (∃x|:R) ⇒ ((∃x|R:pp∨Q) = pp∨(∃x|R:Q))"
let ex_9_23 = theorem pred_calculus (exists'(x, R) ==> ((qex x R[x] (P' + Q[x])) == (P' + exists(x,R,Q)))) [
        collectMixed x R P' Q |> Commute |> at [right_branch; left_branch]
        distrib_and_exists x R P' |> at [right_branch; left_branch; left_branch]  // ∃x|R:pp → pp ∧ (∃x|:R)
        axiom pred_calculus (exists'(x,R) ==> exists'(x,R)) |> Deduce |> at [right_branch; left_branch; left_branch; right_branch]  // assume (∃x|:R)
        ident_and P' |> at [right_branch; left_branch; left_branch]
        def_true (P' + exists(x,R,Q)) |> Commute |> at_right
        implies_true (exists'(x, R)) |> Taut |> apply
    ]

// Exercise 9.36.  Socrates: soundness of  (∀m|man:mortal) ∧ man.S ⇒ mortal.S,
// by Shunting, Trading, and Universal Instantiation at the constant S.
printfn "9.36 Socrates: (∀m|man:mortal) ∧ man.Socrates ⇒ mortal.Socrates"
let ex_9_36 = 
    let man = intpred "man" 
    let mortal = intpred "mortal" 
    let m = intvar "m" 
    let socrates = intvar "Socrates" 

    theorem pred_calculus ((forall(m, man, mortal) * man[socrates]) ==> mortal[socrates]) [
            shunt                                         // (A∧B)⇒C → A⇒(B⇒C)
            trade_forall_implies m man mortal |> at_left  // ∀m|man:mortal → ∀m|:(man⇒mortal); Instantiation then closes
    ]
