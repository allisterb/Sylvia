#load "Include.fsx"

// Inductively defined functions, and strong induction (Gries & Schneider, "A Logical Approach to
// Discrete Math", ch. 12, §12.2). Companion to Induction.fsx, which covers §12.1.
//
// What this script verifies:
//   (A) an inductive definition is a set of AXIOMS, and as an unconditional scheme a recurrence is
//       consistent only if it can be run BACKWARDS. f(n+1) = f.n + 2 and Fibonacci can; n! and bⁿ
//       cannot — at e := −1 the unconditional (e+1)! = (e+1)·e! proves 0! = 0 beside 0! = 1 — so
//       their inductive equation is stated under 0 ≤ e, which is how Gries states it (n ≥ 0);
//   (B) (b·c)ⁿ = bⁿ·cⁿ by induction, the conditional definition used under the step's 0 ≤ n;
//   (C) the geometric sum  (b−1)·(Σi | 1 ≤ i ≤ n : bⁱ) = bⁿ⁺¹ − b;
//   (D) Fibonacci:  (Σi | 1 ≤ i ≤ n : Fᵢ) = Fₙ₊₂ − 1;
//   (E) the order facts and the case split a STRONG induction step needs, all theorems;
//   (F) strong induction:  F(m+n+1) = F(m+1)·F(n+1) + F(m)·F(n), whose step uses P.n AND P(n−1);
//   (G) 0 ≤ m ⇒ b^(m+n) = bᵐ·bⁿ, whose step needs 0 ≤ m + n — a theorem of the ordered domain.
//
// Run:  dotnet fsi examples/proofs/InductiveDefinitions.fsx

open Sylvia
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FsExpr
open Formula
open Descriptions
open PropCalculus
open PredCalculus
open Sums
open Integers
open Induction

fsi.PrintWidth <- 300
Proof.LogLevel <- 0

let mutable failures = 0
let ok label cond =
    if not cond then failures <- failures + 1
    printfn "  %s  %s" (if cond then "✓" else "✗") label
let why what (e: exn) = printfn "     [%s] %s" what (e.Message.Split('\n').[0]); false
let proves (what: string) (f: unit -> Theorem) = try (f ()).Proof.Complete with e -> why what e
let named (label: string) (f: unit -> Theorem) = ok label (proves label f)
let fails (label: string) (f: unit -> Theorem) = ok label (try not ((f ()).Proof.Complete) with _ -> true)

let n, i, j, m = IntVar "n", IntVar "i", IntVar "j", IntVar "m"
let b, c = IntVar "b", IntVar "c"

// An uninterpreted integer function, applied: the defining equations are the theory's axioms.
let app1 (v: Var) (x: Term<int>) : IntTerm = IntTerm(Expr.Application(Expr.Var v, x.Expr) |> expand_as<int>)
let (|App1|_|) (name: string) = function Application(Var v, a) when v.Name = name -> Some a | _ -> None

let factv = Var("fact", typeof<int -> int>)
let inline fact x : IntTerm = app1 factv (int_term x)

let fibv = Var("fib", typeof<int -> int>)
let inline fib x : IntTerm = app1 fibv (int_term x)

let powv = Var("pow", typeof<int -> int -> int>)
let inline pow x e : IntTerm =
    let x, e = int_term x, int_term e
    IntTerm(Expr.Application(Expr.Application(Expr.Var powv, x.Expr), e.Expr) |> expand_as<int>)
let (|PowApp|_|) = function Application(Application(Var v, x), e) when v.Name = "pow" -> Some(x, e) | _ -> None

printfn "\n===== (A) An inductive definition is a set of axioms, and must be consistent ====="
// 0! = 1,  (e+1)! = (e+1)·e!  — as UNCONDITIONAL schemes, which is how Induction.fsx (L) first stated them.
let fact_unconditional =
    function
    | Equals(App1 "fact" (Int32 0), Int32 1) -> axiom_name "fact" "Definition of ! (base)" |> Some
    | Equals(App1 "fact" (Add(a, Int32 1)), Multiply(Add(a', Int32 1), App1 "fact" a'')) when sequal a a' && sequal a' a'' ->
        axiom_name "fact" "Definition of ! (inductive)" |> Some
    | _ -> None
let thbad = Induction(fact_unconditional) :> Theory
let m1p1 = IntTerm(-1) + 1                                  // (−1) + 1, unfolded
let zero_by_def (th: Theory) =
    theorem th (fact m1p1 == 0) [
        ax_ident th (fact m1p1 == (m1p1 * fact (-1))) |> at_left        // (e+1)! = (e+1)·e! at e := −1
        Integers.reduce |> at_left                                       // → 0 · (−1)!
        Taut (ring_ident th (IntTerm 0 * fact (-1)) (IntTerm 0)) |> apply
    ]
named "the unconditional scheme proves 0! = 1 ..."    (fun () -> theorem thbad (fact m1p1 == 1) [ Integers.reduce |> at_left ])
named "... AND 0! = 0: it is INCONSISTENT"            (fun () -> zero_by_def thbad)

// The fix: the inductive equation holds for 0 ≤ e. A recurrence that can be run backwards —
// f.e = f(e+1) − 2, F.e = F(e+2) − F(e+1) — has a model on all of ℤ and needs no condition; n! and
// bⁿ would have to divide by e+1 and by b.
let fact_axioms =
    function
    | Equals(App1 "fact" (Int32 0), Int32 1) -> axiom_name "fact" "Definition of ! (base)" |> Some
    | Implies(LessEq(Int32 0, e), Equals(App1 "fact" (Add(e', Int32 1)), Multiply(Add(e'', Int32 1), App1 "fact" e''')))
        when sequal e e' && sequal e' e'' && sequal e'' e''' -> axiom_name "fact" "Definition of ! (inductive)" |> Some
    | _ -> None

// b⁰ = 1,  0 ≤ e ⇒ b^(e+1) = b·bᵉ
let pow_axioms =
    function
    | Equals(PowApp(_, Int32 0), Int32 1) -> axiom_name "pow" "Definition of bⁿ (base)" |> Some
    | Implies(LessEq(Int32 0, e), Equals(PowApp(x, Add(e', Int32 1)), Multiply(x', PowApp(x'', e''))))
        when sequal e e' && sequal e' e'' && sequal x x' && sequal x' x'' -> axiom_name "pow" "Definition of bⁿ (inductive)" |> Some
    | _ -> None

// F₀ = 0,  F₁ = 1,  F(e+2) = F(e+1) + F.e   — unconditional: it runs backwards.
let fib_axioms =
    function
    | Equals(App1 "fib" (Int32 0), Int32 0) -> axiom_name "fib" "Definition of F (F₀)" |> Some
    | Equals(App1 "fib" (Int32 1), Int32 1) -> axiom_name "fib" "Definition of F (F₁)" |> Some
    | Equals(App1 "fib" (Add(e, Int32 2)), Add(App1 "fib" (Add(e', Int32 1)), App1 "fib" e'')) when sequal e e' && sequal e' e'' ->
        axiom_name "fib" "Definition of F (inductive)" |> Some
    | _ -> None

let th = Induction(BooleanAlgebra.combine_axioms fact_axioms (BooleanAlgebra.combine_axioms pow_axioms fib_axioms)) :> Theory

ok "0 ≤ e ⇒ (e+1)! = (e+1)·e! is an axiom"            (th.AxEquiv ((leq 0 n) ==> (fact (n + 1) == (n + 1) * fact n)).Expr)
ok "... the bare equation is not"                      (not (th.AxEquiv (fact (n + 1) == (n + 1) * fact n).Expr))
ok "... nor the equation under another hypothesis"     (not (th.AxEquiv ((leq 1 n) ==> (fact (n + 1) == (n + 1) * fact n)).Expr))
ok "the instance at e := −1 is vacuous: (0 ≤ −1) = false" (th.AxEquiv ((leq 0 (-1)) == F).Expr)
fails "0! = 0 is no longer provable that way"          (fun () -> zero_by_def th)
ok "F(e+2) = F(e+1) + F.e is an axiom, unconditionally" (th.AxEquiv ((fib (n + 2)) == (fib (n + 1) + fib n)).Expr)
ok "F(e+2) = F(e+1) + F(e+1) is not"                   (not (th.AxEquiv ((fib (n + 2)) == (fib (n + 1) + fib (n + 1))).Expr))

/// 0 ≤ e ⇒ b^(e+1) = b·bᵉ, the definition as a theorem for `Deduce'`.
let inline pow_succ x e =
    let x, e = int_term x, int_term e
    axiom th ((leq 0 e) ==> (pow x (IntTerm(e.Expr) + 1) == IntTerm(x.Expr) * pow x e))
/// The definition at a literal k ≥ 0, detached from its (literally true) condition.
let pow_succ_lit x (k: int) = mp (pow_succ x k) (theorem th (leq 0 k) [ ax_ident th ((leq 0 k) == T) |> apply ])

printfn "\n===== (B) (b·c)ⁿ = bⁿ·cⁿ ====="
// The step uses the definition three times, each under the step's own 0 ≤ n.
let bc = b * c
let Pmul = pred_of n ((pow bc n) == (pow b n * pow c n))
let a0 = leq 0 n
let ihB, pn1B = hypothesis 0 n i Pmul, Pmul.[n + 1]
let baseB =
    theorem th (base_case 0 Pmul) [
        ax_ident th ((pow bc 0) == 1) |> at_left
        ax_ident th ((pow b 0) == 1)  |> at_right
        ax_ident th ((pow c 0) == 1)  |> at_right
        Integers.reduce               |> at_right
    ]
let bodyB =
    theorem th (a0 ==> (ihB ==> pn1B)) [
        (shunt' a0 ihB pn1B |> Commute) |> apply                     // (0 ≤ n ∧ IH) ⇒ P(n+1)
        Deduce' (pow_succ bc n)               |> apply_right           // (bc)ⁿ⁺¹ → bc·(bc)ⁿ
        Deduce' (hypothesis_at th 0 n i Pmul) |> apply_right           // (bc)ⁿ → bⁿ·cⁿ — THE INDUCTIVE HYPOTHESIS
        Deduce' (pow_succ b n)                |> apply_right           // bⁿ⁺¹ → b·bⁿ
        Deduce' (pow_succ c n)                |> apply_right           // cⁿ⁺¹ → c·cⁿ
        Taut (ring_ident th (bc * (pow b n * pow c n)) ((b * pow b n) * (c * pow c n))) |> apply_right
    ]
named "base case       (b·c)⁰ = b⁰·c⁰"                    (fun () -> baseB)
named "step body       0 ≤ n ⇒ (IH ⇒ P(n+1))"             (fun () -> bodyB)
let stepB = generalize_step th 0 n i Pmul bodyB
named "inductive case  (generalize_step)"                 (fun () -> stepB)
let thmB = by_induction th 0 n i Pmul baseB stepB
printfn "  %s" (th.PrintFormula (expand thmB.Stmt))
named "BY INDUCTION    (∀n | 0 ≤ n : (b·c)ⁿ = bⁿ·cⁿ)"   (fun () -> thmB)
fails "the step does not close without the inductive hypothesis" (fun () ->
    theorem th (a0 ==> (ihB ==> pn1B)) [
        (shunt' a0 ihB pn1B |> Commute) |> apply
        Deduce' (pow_succ bc n) |> apply_right
        Deduce' (pow_succ b n)  |> apply_right
        Deduce' (pow_succ c n)  |> apply_right
        Taut (ring_ident th (bc * (pow b n * pow c n)) ((b * pow b n) * (c * pow c n))) |> apply_right
    ])
fails "the definition cannot be used without 0 ≤ n" (fun () ->
    theorem th (pow b (n + 1) == b * pow b n) [ Deduce' (pow_succ b n) |> apply_right ])

printfn "\n===== (C) The geometric sum  (b−1)·(Σi | 1 ≤ i ≤ n : bⁱ) = bⁿ⁺¹ − b ====="
// Split off the last term, distribute, use the hypothesis; unfolding b^((n+1)+1) needs 0 ≤ n+1,
// which `le_step` gets from 0 ≤ n.
let Sum1 (hi: Term<int>) = qsum i ((leq 1 i) * (leq i hi)) (pow b i)
let Pgeo = pred_of n (((b - 1) * Sum1 n) == (pow b (n + 1) - b))
let ihC, pn1C = hypothesis 0 n j Pgeo, Pgeo.[n + 1]     // P binds i (in Σ), so the hypothesis dummy is j
let X = pow b (n + 1)
let baseC =
    theorem th (base_case 0 Pgeo) [
        Ident (empty_sum th 1 0 i (pow b i)) |> at_left               // Σ over 1..0 → 0
        Ident (pow_succ_lit b 0)             |> at_right              // b^(0+1) → b·b⁰
        ax_ident th ((pow b 0) == 1)         |> at_right
        Taut (ring_ident th ((b - 1) * 0) (b * 1 - b)) |> apply
    ]
let bodyC =
    theorem th (a0 ==> (ihC ==> pn1C)) [
        (shunt' a0 ihC pn1C |> Commute) |> apply
        Establish (shift_bound th 0 n)                 |> apply                 // 1 ≤ n+1, Split off term's proviso
        Deduce' (split_off_term th 1 n i (pow b i))    |> apply_right           // Σ(1..n+1) → Σ(1..n) + bⁿ⁺¹
        Ident (ring_ident th ((b - 1) * (Sum1 n + X)) ((b - 1) * Sum1 n + (b - 1) * X)) |> apply_right
        Deduce' (hypothesis_at th 0 n j Pgeo)          |> apply_right           // (b−1)·Σ(1..n) → bⁿ⁺¹ − b — THE IH
        Establish (le_step th 0 n)                     |> apply                 // 0 ≤ n+1
        Deduce' (pow_succ b (n + 1))                   |> apply_right           // b^((n+1)+1) → b·bⁿ⁺¹
        Taut (ring_ident th ((X - b) + (b - 1) * X) (b * X - b)) |> apply_right
    ]
named "base case       (b−1)·(Σi | 1 ≤ i ≤ 0 : bⁱ) = b^(0+1) − b"   (fun () -> baseC)
named "step body"                                                    (fun () -> bodyC)
let thmC = by_induction th 0 n j Pgeo baseC (generalize_step th 0 n j Pgeo bodyC)
printfn "  %s" (th.PrintFormula (expand thmC.Stmt))
named "BY INDUCTION    the geometric sum"                            (fun () -> thmC)

printfn "\n===== (D) Fibonacci:  (Σi | 1 ≤ i ≤ n : Fᵢ) = Fₙ₊₂ − 1 ====="
let Pfs = pred_of n ((qsum i ((leq 1 i) * (leq i n)) (fib i)) == (fib (n + 2) - 1))
let ihD, pn1D = hypothesis 0 n j Pfs, Pfs.[n + 1]
let baseD =
    theorem th (base_case 0 Pfs) [
        Ident (empty_sum th 1 0 i (fib i)) |> at_left                                     // Σ over 1..0 → 0
        ax_ident th ((fib (IntTerm 0 + 2)) == (fib (IntTerm 0 + 1) + fib 0)) |> at_right  // F(0+2) → F(0+1) + F₀
        Integers.reduce                    |> at_right
        ax_ident th ((fib 1) == 1)         |> at_right
        ax_ident th ((fib 0) == 0)         |> at_right
        Integers.reduce                    |> at_right                                    // (1 + 0) − 1 → 1 − 1
        Integers.reduce                    |> at_right                                    // → 0  (one fold per pass)
    ]
let bodyD =
    theorem th (a0 ==> (ihD ==> pn1D)) [
        (shunt' a0 ihD pn1D |> Commute) |> apply
        Establish (shift_bound th 0 n)              |> apply
        Deduce' (split_off_term th 1 n i (fib i))   |> apply_right                  // Σ(1..n+1) → Σ(1..n) + Fₙ₊₁
        Deduce' (hypothesis_at th 0 n j Pfs)        |> apply_right                  // Σ(1..n) → Fₙ₊₂ − 1 — THE IH
        ax_ident th ((fib ((n + 1) + 2)) == (fib ((n + 1) + 1) + fib (n + 1))) |> apply_right
        Ident (ring_ident th ((n + 1) + 1) (n + 2)) |> apply_right
        Taut (ring_ident th ((fib (n + 2) - 1) + fib (n + 1)) ((fib (n + 2) + fib (n + 1)) - 1)) |> apply_right
    ]
named "base case       (Σi | 1 ≤ i ≤ 0 : Fᵢ) = F(0+2) − 1"     (fun () -> baseD)
named "step body"                                              (fun () -> bodyD)
let thmD = by_induction th 0 n j Pfs baseD (generalize_step th 0 n j Pfs bodyD)
printfn "  %s" (th.PrintFormula (expand thmD.Stmt))
named "BY INDUCTION    (∀n | 0 ≤ n : (Σi | 1 ≤ i ≤ n : Fᵢ) = Fₙ₊₂ − 1)"   (fun () -> thmD)

printfn "\n===== (E) What a strong induction step needs: order facts and a case split ====="
// A step that uses P(n−1) must show n−1 is in the hypothesis's range 0 ≤ i ≤ n — which it is only
// when 1 ≤ n — so the step splits on whether n is the base point. All of these are theorems of the
// order axioms of `Integers` and the ring normalizer.
named "le_succ         n ≤ n + 1"                              (fun () -> le_succ th n)
named "le_step         0 ≤ n ⇒ 0 ≤ n + 1"                      (fun () -> le_step th 0 n)
named "le_of_succ_le   1 ≤ n ⇒ 0 ≤ n"                          (fun () -> le_of_succ_le th 0 n)
named "le_of_succ_le   m + 1 ≤ n ⇒ m ≤ n   (symbolic bound)"   (fun () -> le_of_succ_le th m n)
named "pred_translate  (0 ≤ n − 1) = (1 ≤ n)"                  (fun () -> pred_translate th 0 n)
named "le_pred         1 ≤ n ⇒ 0 ≤ n − 1"                      (fun () -> le_pred th 0 n)
named "pred_le         n − 1 ≤ n"                              (fun () -> pred_le th n)
named "lower_split     (0 ≤ n) = (1 ≤ n ∨ 0 = n)"              (fun () -> lower_split th 0 n)
named "lower_split     (m ≤ n) = (m + 1 ≤ n ∨ m = n)"          (fun () -> lower_split th m n)
named "eq_sym          (0 = n) ⇒ (n = 0)"                      (fun () -> eq_sym th (IntTerm 0 :> Term<int>) (n :> Term<int>))
// eq_sym with modus ponens FLIPS a proved integer identity, which nothing else does.
let flipped = mp (eq_sym th ((n + 1) * 2 :> Term<int>) (2 * n + 2 :> Term<int>)) (ring_ident th ((n + 1) * 2) (2 * n + 2))
named "mp ∘ eq_sym     ⊢ (n+1)·2 = 2n + 2  gives  ⊢ 2n + 2 = (n+1)·2"   (fun () -> flipped)
ok "... and it is the flipped statement"
   (sequal (expand flipped.Stmt) (expand ((2 * n + 2 :> Term<int>) == ((n + 1) * 2 :> Term<int>)).Expr))
fails "le_succ does not give n + 1 ≤ n"  (fun () -> theorem th (leq (n + 1) n) [ Taut (le_succ th n) |> apply ])

let Pfib = pred_of n ((fib (m + n + 1)) == (fib (m + 1) * fib (n + 1) + fib m * fib n))
named "hypothesis_at_point  ((0 ≤ n−1 ∧ n−1 ≤ n) ∧ IH) ⇒ P(n−1)"   (fun () -> hypothesis_at_point th 0 n i Pfib (n - 1))
named "hypothesis_below     (1 ≤ n ∧ IH) ⇒ P(n−1)"                  (fun () -> hypothesis_below th 0 n i Pfib)

printfn "\n===== (F) Strong induction:  F(m+n+1) = F(m+1)·F(n+1) + F(m)·F(n) ====="
// Induction on n, m arbitrary. The step for P(n+1) unfolds F(m+n+2) into F(m+n+1) + F(m+n) and uses
// the hypothesis at BOTH n and n−1 — so for n = 0, where n−1 is out of range, it proves P(1) outright.
let ihF, pn1F = hypothesis 0 n i Pfib, Pfib.[n + 1]
let nm1 = n - 1
let baseF =
    theorem th (base_case 0 Pfib) [
        Integers.ident_add            |> at_left           // F((m+0)+1) → F(m+1)
        Integers.reduce               |> at_right          // F(0+1) → F₁
        ax_ident th ((fib 1) == 1)    |> at_right
        ax_ident th ((fib 0) == 0)    |> at_right
        Taut (ring_ident th (fib (m + 1)) (fib (m + 1) * 1 + fib m * 0)) |> apply
    ]

// n ≥ 1: both hypotheses are in range.
let lhsF = (fib (m + 1) * fib (n + 1) + fib m * fib n) + (fib (m + 1) * fib n + fib m * fib nm1)
let rhsF = fib (m + 1) * (fib (n + 1) + fib n) + fib m * (fib n + fib nm1)
let aboveF =
    theorem th ((ihF * (leq 1 n)) ==> pn1F) [
        Establish (le_of_succ_le th 0 n)                                    |> apply        // 0 ≤ n, for P.n
        Ident (ring_ident th ((m + (n + 1)) + 1) ((m + n) + 2))             |> apply_right
        ax_ident th ((fib ((m + n) + 2)) == (fib ((m + n) + 1) + fib (m + n))) |> apply_right   // F(m+n+2) → F(m+n+1) + F(m+n)
        Deduce' (hypothesis_at th 0 n i Pfib)                               |> apply_right  // P.n
        Ident (ring_ident th (m + n) ((m + nm1) + 1))                       |> apply_right
        Deduce' (hypothesis_below th 0 n i Pfib)                            |> apply_right  // P(n−1) — the STRONG hypothesis
        Ident (ring_ident th ((n + 1) + 1) (n + 2))                         |> apply_right
        ax_ident th ((fib (n + 2)) == (fib (n + 1) + fib n))                |> apply_right  // F(n+2) → F(n+1) + Fₙ
        Ident (ring_ident th (n + 1) (nm1 + 2)) |> at [right_branch; right_branch; right_branch; right_branch]
        ax_ident th ((fib (nm1 + 2)) == (fib (nm1 + 1) + fib nm1))          |> apply_right  // F(n+1) → Fₙ + Fₙ₋₁
        Ident (ring_ident th (nm1 + 1) n)                                   |> apply_right
        Ident (ring_ident th (nm1 + 1) n)                                   |> apply_right
        Taut (ring_ident th lhsF rhsF)                                      |> apply_right
    ]

// n = 0: P(1) outright, by the definition.
let eq0 = (IntTerm 0 :> Term<int>) == (n :> Term<int>)
let n_is_0 = eq_sym th (IntTerm 0 :> Term<int>) (n :> Term<int>)
let atBaseF =
    theorem th ((ihF * eq0) ==> pn1F) [
        Deduce' n_is_0 |> apply_right                                                       // n → 0, three times
        Deduce' n_is_0 |> apply_right
        Deduce' n_is_0 |> apply_right
        Ident (ring_ident th ((m + (IntTerm 0 + 1)) + 1) (m + 2))           |> apply_right
        ax_ident th ((fib (m + 2)) == (fib (m + 1) + fib m))                |> apply_right
        Ident (ring_ident th ((IntTerm 0 + 1) + 1) (IntTerm 0 + 2))         |> apply_right
        ax_ident th ((fib (IntTerm 0 + 2)) == (fib (IntTerm 0 + 1) + fib 0)) |> apply_right
        Integers.reduce                                                     |> apply_right
        ax_ident th ((fib 1) == 1)                                          |> apply_right
        ax_ident th ((fib 1) == 1)                                          |> apply_right
        ax_ident th ((fib 0) == 0)                                          |> apply_right
        Taut (ring_ident th (fib (m + 1) + fib m) (fib (m + 1) * (IntTerm 1 + 0) + fib m * 1)) |> apply_right
    ]
named "base case       P.0"                                         (fun () -> baseF)
named "case 1 ≤ n      (IH ∧ 1 ≤ n) ⇒ P(n+1)   — uses P.n and P(n−1)"  (fun () -> aboveF)
named "case n = 0      (IH ∧ 0 = n) ⇒ P(n+1)"                       (fun () -> atBaseF)
let bodyF = step_by_cases th 0 n i Pfib aboveF atBaseF
named "step body       (step_by_cases)"                             (fun () -> bodyF)
let thmF = by_induction th 0 n i Pfib baseF (generalize_step th 0 n i Pfib bodyF)
printfn "  %s" (th.PrintFormula (expand thmF.Stmt))
named "BY STRONG INDUCTION  (∀n | 0 ≤ n : F(m+n+1) = F(m+1)·F(n+1) + F(m)·F(n))"  (fun () -> thmF)
ok "it is the statement claimed"
   (sequal (expand thmF.Stmt) (expand (qall n (leq 0 n) ((fib (m + n + 1)) == (fib (m + 1) * fib (n + 1) + fib m * fib n))).Expr))
ok "step_by_cases refuses the two cases swapped"
   (try step_by_cases th 0 n i Pfib atBaseF aboveF |> ignore; false with _ -> true)
fails "the n ≥ 1 case does not close without P(n−1)" (fun () ->
    theorem th ((ihF * (leq 1 n)) ==> pn1F) [
        Establish (le_of_succ_le th 0 n)                                    |> apply
        Ident (ring_ident th ((m + (n + 1)) + 1) ((m + n) + 2))             |> apply_right
        ax_ident th ((fib ((m + n) + 2)) == (fib ((m + n) + 1) + fib (m + n))) |> apply_right
        Deduce' (hypothesis_at th 0 n i Pfib)                               |> apply_right
        Ident (ring_ident th ((n + 1) + 1) (n + 2))                         |> apply_right
        ax_ident th ((fib (n + 2)) == (fib (n + 1) + fib n))                |> apply_right
        Taut (ring_ident th lhsF rhsF)                                      |> apply_right
    ])
fails "hypothesis_below is refused without 1 ≤ n (from 0 ≤ n alone)" (fun () ->
    theorem th (((leq 0 n) * ihF) ==> Pfib.[nm1]) [ Deduce (hypothesis_below th 0 n i Pfib) |> apply_right ])

printfn "\n===== (G) b^(m+n) = bᵐ·bⁿ — needs the ORDERED DOMAIN ====="
// The step unfolds b^((m+n)+1), so the conditional definition needs 0 ≤ m + n. Translation by 1
// cannot give that from 0 ≤ m and 0 ≤ n; `Integers.nonneg_add` — Monotonicity (15.42) and
// Transitivity (15.41d) of the ordered domain `Integers` is — can. P.n carries 0 ≤ m as a premise.
let am = leq 0 m
let Pexp = pred_of n (am ==> ((pow b (m + n)) == (pow b m * pow b n)))
let ihG, pn1G = hypothesis 0 n i Pexp, Pexp.[n + 1]
let qG = (pow b (m + n)) == (pow b m * pow b n)        // P.n's consequent
let eqG = (pow b (m + (n + 1))) == (pow b m * pow b (n + 1))   // P(n+1)'s consequent
let baseG =
    theorem th (base_case 0 Pexp) [
        Integers.ident_add             |> at [right_branch; left_branch]      // b^(m+0) → bᵐ
        ax_ident th ((pow b 0) == 1)   |> at [right_branch; right_branch]
        Integers.ident_mul             |> at [right_branch; right_branch]     // bᵐ·1 → bᵐ
        Taut (axiom th ((pow b m) == (pow b m))) |> apply_right
    ]
let bodyG =
    theorem th (a0 ==> (ihG ==> pn1G)) [
        (shunt' a0 ihG pn1G |> Commute) |> apply                                  // (0 ≤ n ∧ IH) ⇒ (0 ≤ m ⇒ …)
        (shunt' (a0 * ihG) am eqG |> Commute) |> apply                            // ((0 ≤ n ∧ IH) ∧ 0 ≤ m) ⇒ …
        Establish (Integers.nonneg_add th m n)          |> apply                  // 0 ≤ m + n — the ordered domain
        Establish (hypothesis_at th 0 n i Pexp)         |> apply                  // P.n = (0 ≤ m ⇒ b^(m+n) = bᵐ·bⁿ)
        Establish (modus_ponens am qG)                  |> apply                  // b^(m+n) = bᵐ·bⁿ — THE IH, detached
        Ident (ring_ident th (m + (n + 1)) ((m + n) + 1)) |> apply_right
        Deduce' (pow_succ b (m + n))                    |> apply_right            // b^((m+n)+1) → b·b^(m+n), under 0 ≤ m+n
        Deduce' (reflex_implies qG)                     |> apply_right            // b^(m+n) → bᵐ·bⁿ
        Deduce' (pow_succ b n)                          |> apply_right            // bⁿ⁺¹ → b·bⁿ
        Taut (ring_ident th (b * (pow b m * pow b n)) (pow b m * (b * pow b n))) |> apply_right
    ]
named "base case       0 ≤ m ⇒ b^(m+0) = bᵐ·b⁰"                     (fun () -> baseG)
named "step body       uses 0 ≤ m + n from nonneg_add"              (fun () -> bodyG)
let thmG = by_induction th 0 n i Pexp baseG (generalize_step th 0 n i Pexp bodyG)
printfn "  %s" (th.PrintFormula (expand thmG.Stmt))
named "BY INDUCTION    (∀n | 0 ≤ n : 0 ≤ m ⇒ b^(m+n) = bᵐ·bⁿ)"     (fun () -> thmG)
fails "the step does not close without 0 ≤ m + n" (fun () ->
    theorem th (a0 ==> (ihG ==> pn1G)) [
        (shunt' a0 ihG pn1G |> Commute) |> apply
        (shunt' (a0 * ihG) am eqG |> Commute) |> apply
        Establish (hypothesis_at th 0 n i Pexp)         |> apply
        Establish (modus_ponens am qG)                  |> apply
        Ident (ring_ident th (m + (n + 1)) ((m + n) + 1)) |> apply_right
        Deduce' (pow_succ b (m + n))                    |> apply_right
        Deduce' (reflex_implies qG)                     |> apply_right
        Deduce' (pow_succ b n)                          |> apply_right
        Taut (ring_ident th (b * (pow b m * pow b n)) (pow b m * (b * pow b n))) |> apply_right
    ])

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
