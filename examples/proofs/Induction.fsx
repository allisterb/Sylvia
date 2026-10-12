#load "Include.fsx"

// Mathematical Induction (Gries & Schneider, "A Logical Approach to Discrete Math", ch. 12, §12.1).
//
// What this script verifies:
//   (A) (12.8), induction over {n₀, n₀+1, …}, is a recognized axiom of the `Induction` theory —
//       and Gries' (12.5), induction over ℕ, is its n₀ := 0 instance rather than a second axiom;
//   (B) every side condition on that axiom is CHECKED (each is exercised as a rejection);
//   (C) `hypothesis_at`, the inductive hypothesis at its own upper bound, which is the only place
//       a proof by induction touches the order on ℤ;
//   (D) a worked induction end to end: an inductively defined function (§12.2) `f.0 = 0`,
//       `f(n+1) = f.n + 2`, and `(∀n | 0 ≤ n : f.n = 2·n)`;
//   (E) `by_induction` refuses proofs of anything but the two obligations;
//   …
//   (K) the integral-domain theorems of Gries ch.15 §15.1, which the ring normalizer of (J) rests on.
//
// The dummies are `IntVar`s — integer variables that are ALSO integer terms — so the statements are
// written over the variables themselves: `2 * n`, `n + 1`, `f n`.
//
// Run:  dotnet fsi examples/proofs/Induction.fsx

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

let n = IntVar "n"
let i = IntVar "i"
let m = IntVar "m"
let k = IntVar "k"
let zero = IntTerm 0

printfn "\n===== (A) (12.8) Mathematical Induction over {n₀, n₀+1, …} ====="

let P = intpred "P"
let ind = induction :> Theory

// P.n₀ ∧ (∀n | n₀ ≤ n : (∀i | n₀ ≤ i ∧ i ≤ n : P.i) ⇒ P(n+1))  ⇒  (∀n | n₀ ≤ n : P.n)
let ax0 = induction_axiom zero n i P
printfn "  (12.5)  %s" (ind.PrintFormula (expand ax0.Expr))
ok "(12.5) induction over ℕ is an axiom of the theory"      (ind.AxEquiv ax0.Expr)
ok "(12.5) is NOT an axiom of the ambient logic"            (not (Theory.S.AxEquiv ax0.Expr))

// The base point is a parameter: (12.5) is the n₀ := 0 instance of (12.8), not a separate axiom.
ok "(12.8) at a symbolic base point n₀ = k"                 (ind.AxEquiv (induction_axiom k n i P).Expr)
ok "(12.8) at base point 1"                                 (ind.AxEquiv (induction_axiom (IntTerm 1) n i P).Expr)

// The API takes any `#ISymbolicVar<int>`: a dummy with no arithmetic of its own works as well.
ok "(12.8) with ElemVar dummies, which carry no arithmetic"
   (ind.AxEquiv (induction_axiom zero (ElemVar<int> "n") (ElemVar<int> "i") P).Expr)

printfn "\n===== (B) The side conditions are checked ====="
// `P` is not a syntactic argument of an axiom — the recognizer sees only the statement — so the
// four applications of P are tied to ONE predicate by reconstructing three of them from the
// conclusion's body. Each way of breaking that is rejected.

let mk (bse: Prop) (stp: Prop) (con: Prop) = ((bse * stp) ==> con)

ok "wrong base case (P.1 against a conclusion from 0) rejected"
   (not (ind.AxEquiv (mk P.[IntTerm 1] (inductive_case zero n i P) (conclusion zero n P)).Expr))
ok "wrong step consequent (P(n+2)) rejected"
   (not (ind.AxEquiv (mk P.[zero]
                         (qall n (leq zero n) ((hypothesis zero n i P) ==> P.[n + 2]))
                         (conclusion zero n P)).Expr))
ok "a strict hypothesis range (i < n) rejected"
   (not (ind.AxEquiv (mk P.[zero]
                         (qall n (leq zero n)
                               ((qall i ((leq zero i) * (Prop <@ %i.Expr < %n.Expr @>)) P.[i]) ==> P.[n + 1]))
                         (conclusion zero n P)).Expr))
ok "step and conclusion quantifying different dummies rejected"
   (not (ind.AxEquiv (mk P.[zero] (inductive_case zero m i P) (conclusion zero n P)).Expr))
ok "the two dummies being the SAME variable rejected"
   (not (ind.AxEquiv (mk P.[zero] (inductive_case zero n n P) (conclusion zero n P)).Expr))
ok "base points that disagree rejected"
   (not (ind.AxEquiv (mk P.[zero] (inductive_case (IntTerm 1) n i P) (conclusion zero n P)).Expr))
// A base point mentioning a dummy would be CAPTURED by the range that quantifies it.
ok "a base point mentioning the dummy rejected"
   (not (ind.AxEquiv (induction_axiom n n i P).Expr))
// A P mentioning `i` would have its `i` captured when the hypothesis body is formed.
let Pi = pred_of n (P.[i] * P.[n])
ok "a predicate mentioning the hypothesis dummy rejected"
   (not (ind.AxEquiv (induction_axiom zero n i Pi).Expr))
// A P that re-binds n inside itself would have that binder substituted under.
let Pbind = pred_of n (qall n (leq zero n) P.[n])
ok "a predicate re-binding the dummy rejected"
   (not (ind.AxEquiv (induction_axiom zero n i Pbind).Expr))

printfn "\n===== (C) The inductive hypothesis, at its own upper bound ====="
// (n₀ ≤ n ∧ (∀i | n₀ ≤ i ∧ i ≤ n : P.i)) ⇒ P.n — Trading (9.2), Instantiation (9.13), and the one
// order fact induction needs, reflexivity of ≤. Both premises are load-bearing: `n₀ ≤ n` is what
// discharges the instance's range obligation, and it can come from nowhere else.
named "hypothesis_at:  (n₀ ≤ n ∧ IH) ⇒ P.n"  (fun () -> hypothesis_at ind zero n i P)
named "hypothesis_at at a symbolic base point" (fun () -> hypothesis_at ind k n i P)
ok "reflexivity of ≤ is an axiom (Integers)"   (ind.AxEquiv ((leq n n) == T).Expr)
ok "≤ is NOT symmetric-by-accident: n ≤ m is not an axiom"
   (not (ind.AxEquiv ((leq n m) == T).Expr))

printfn "\n===== (D) A worked induction: an inductively defined function (§12.2) ====="
// f.0 = 0,  f(n+1) = f.n + 2   — two axioms, in the shape §12.2 calls an inductive definition.
// The caller's axioms are passed to the theory, so the base case, the inductive case and the
// induction axiom instance all live in ONE theory, which is what lets them be detached.
let fv = Var("f", typeof<int -> int>)
let f (x: Term<int>) : IntTerm = IntTerm(Expr.Application(Expr.Var fv, x.Expr) |> expand_as<int>)

let (|FApp|_|) =
    function
    | Application(Var v, a) when v.Name = "f" -> Some a
    | _ -> None

let f_axioms =
    function
    | Equals(FApp(Int32 0), Int32 0) -> axiom_name "f" "Definition of f (base)" |> Some
    | Equals(FApp(Add(a, Int32 1)), Add(FApp a', Int32 2)) when sequal a a' ->
        axiom_name "f" "Definition of f (inductive)" |> Some
    | _ -> None

let th = Induction(f_axioms) :> Theory
ok "f.0 = 0 is an axiom of the extended theory"        (th.AxEquiv ((f zero) == zero).Expr)
ok "f(n+1) = f.n + 2 is an axiom"                      (th.AxEquiv ((f (n + 1)) == (f n + 2)).Expr)
ok "(12.8) survives the extension"                     (th.AxEquiv ax0.Expr)
ok "the extension is not free: f.n = 0 is no axiom"    (not (th.AxEquiv ((f n) == zero).Expr))

// P.n :  f.n = 2·n
let Pf = pred_of n ((f n) == (2 * n))
let a = leq zero n
let ih = hypothesis zero n i Pf
let pn1 = Pf.[n + 1]
let rhs = 2 * n + 2

// Base case: f.0 = 2·0 — the definition, then constant folding.
let bse =
    theorem th (base_case zero Pf) [
        ax_ident th ((f zero) == zero) |> at_left
        Integers.reduce
    ]

// The body of the inductive case, for arbitrary n. Traded so that `0 ≤ n` is an ANTECEDENT, which
// is what makes it available to `Deduce'` — and `0 ≤ n` is exactly what `hypothesis_at` needs.
let body =
    theorem th (a ==> (ih ==> pn1)) [
        (shunt' a ih pn1 |> Commute) |> apply                                 // → (0 ≤ n ∧ IH) ⇒ P(n+1)
        ax_ident th ((f (n + 1)) == (f n + 2)) |> at [right_branch; left_branch]
        Deduce' (hypothesis_at th zero n i Pf) |> apply_right                 // f.n → 2·n, from the IH
        Integers.distrib_mul_add |> at [right_branch; right_branch]           // 2·(n+1) → 2·n + 2·1
        Integers.reduce |> at [right_branch; right_branch]                    // 2·1 → 2
        ax_ident th (T == (rhs == rhs)) |> Commute |> at_right
    ]

// Generalize the body over n: Trading (9.2) puts the range in the body, the body is replaced by its
// own theorem, and `(∀n |: T) = T` closes it.
let stp =
    theorem th (inductive_case zero n i Pf) [
        trade_forall_implies n (pred_of n a) (pred_of n (ih ==> pn1)) |> apply
        Taut body |> at [select_body]
        ident_forall_true' n
    ]

named "base case      f.0 = 2·0"                                  (fun () -> bse)
named "body of step   0 ≤ n ⇒ (IH ⇒ f(n+1) = 2·(n+1))"            (fun () -> body)
named "inductive case (∀n | 0 ≤ n : IH ⇒ P(n+1))"                 (fun () -> stp)
named "BY INDUCTION   (∀n | 0 ≤ n : f.n = 2·n)"                   (fun () -> by_induction th zero n i Pf bse stp)
ok "the conclusion is what was claimed"
   (sequal (expand (by_induction th zero n i Pf bse stp).Stmt) (expand (conclusion zero n Pf).Expr))
// Same thing through the ℕ-specific entry point.
named "by_induction_nat gives the same theorem"                   (fun () -> by_induction_nat th n i Pf bse stp)

// The step genuinely USES the inductive hypothesis: without `Deduce'` on it there is no way from
// f(n+1) to 2·(n+1), and the same proof with that step removed does not close.
ok "the step does not close without the inductive hypothesis"
   (not (proves "step without IH" (fun () ->
        theorem th (a ==> (ih ==> pn1)) [
            (shunt' a ih pn1 |> Commute) |> apply
            ax_ident th ((f (n + 1)) == (f n + 2)) |> at [right_branch; left_branch]
            Integers.distrib_mul_add |> at [right_branch; right_branch]
            Integers.reduce |> at [right_branch; right_branch]
            ax_ident th (T == (rhs == rhs)) |> Commute |> at_right
        ])))

printfn "\n===== (E) The combinator refuses anything but the two obligations ====="
let refuses (what: string) (f: unit -> Theorem) =
    ok what (try f () |> ignore; false with _ -> true)

refuses "a base case proved at the wrong point"
        (fun () -> by_induction th (IntTerm 1) n i Pf bse stp)
refuses "the base case and inductive case swapped"
        (fun () -> by_induction th zero n i Pf stp bse)
refuses "an inductive case for a different predicate"
        (fun () -> by_induction th zero n i P bse stp)

printfn "\n===== (F) IntVar: a variable that is also a term ====="
// `n + 1` from the operator and `succ n` from the theory build the SAME tree, which is what lets the
// statements above use either. A fresh witness of an IntVar is an IntVar (IFreshVar), so it keeps
// its arithmetic inside a subproof — it can be added to without being wrapped first.
ok "n + 1 is exactly succ n"                      (sequal (expand (n + 1).Expr) (expand (succ n).Expr))
let w = fresh_witness n [ expand (f n).Expr ]
ok "a fresh witness of an IntVar is an IntVar"   (w.GetType() = typeof<IntVar>)
ok "... with its arithmetic intact: w + 1"       ((th.PrintFormula (expand (w + 1).Expr)) = "n#0 + 1")
ok "an IntVar prints as its name"               (n.Display = "n")

printfn "\n===== (G) Split off term (Gries 8.23) for Σ, and what it rests on ====="
// A THEOREM, as in Gries: Σ's unconditional Range split (8.18) and Empty range (8.13), the One-point
// rule (8.14), Substitution (3.84a), and two facts about the integer order. Each foundation is checked
// to fire where it should and to refuse where it would be unsound.

// (3.84a) Substitution:  (z = e ∧ E) = (z = e ∧ E[z := e])  — an EXPRESSION for a variable, any type.
let ieq = (i :> Term<int>) == ((n + 1) :> Term<int>)
let subst_ok = ((ieq * (leq i n)) == (ieq * (leq (n + 1) n)))
ok "(3.84a) substitutes an expression for a variable"     (Theory.S.AxEquiv subst_ok.Expr)
ok "(3.84a) rejects a wrong substitution"
   (not (Theory.S.AxEquiv ((ieq * (leq i n)) == (ieq * (leq (n + 2) n))).Expr))
// A binder inside E over a variable FREE in e would capture it: (i = n+1 ∧ (∀n |: i ≤ n)).
ok "(3.84a) rejects capture of e's variables by a binder in E"
   (not (Theory.S.AxEquiv ((ieq * (qall n T (leq i n))) == (ieq * (qall n T (leq (n + 1) n)))).Expr))
// A binder inside E over z itself owns those occurrences: (i = n+1 ∧ (∀i |: i ≤ n)) — nothing to
// substitute, and a naive traversal would rewrite them anyway.
ok "(3.84a) rejects substituting under a binder of z"
   (not (Theory.S.AxEquiv ((ieq * (qall i T (leq i n))) == (ieq * (qall i T (leq (n + 1) n)))).Expr))

// The integer order: discreteness, and a successor is never ≤ its predecessor. Both are stated
// for `int` only — discreteness is FALSE over the reals.
let x, y = IntVar "x", IntVar "y"
ok "Discreteness:  (x ≤ y+1) = (x ≤ y ∨ x = y+1)"
   (ind.AxEquiv ((leq x (y + 1)) == ((leq x y) + ((x :> Term<int>) == ((y + 1) :> Term<int>)))).Expr)
ok "Discreteness does not hold for y+2"
   (not (ind.AxEquiv ((leq x (y + 2)) == ((leq x y) + ((x :> Term<int>) == ((y + 2) :> Term<int>)))).Expr))
ok "Discreteness is not an axiom over the reals"
   (not (ind.AxEquiv (Prop <@ ((%x.Expr |> float) <= (%y.Expr |> float) + 1.0) = (((%x.Expr |> float) <= (%y.Expr |> float)) || ((%x.Expr |> float) = (%y.Expr |> float) + 1.0)) @>).Expr))
ok "Successor:  (x+1 ≤ x) = false"                         (ind.AxEquiv ((leq (x + 1) x) == F).Expr)
ok "... but not (x+1 ≤ y) = false"                          (not (ind.AxEquiv ((leq (x + 1) y) == F).Expr))

// The two range lemmas and the theorem, at a symbolic lower bound and at 1.
named "range_extend    lo ≤ n+1 ⇒ (lo≤i≤n+1) = (lo≤i≤n ∨ i=n+1)"  (fun () -> range_extend ind k n i)
named "range_disjoint  ((lo≤i≤n) ∧ i=n+1) = false"                 (fun () -> range_disjoint ind k n i)
let sot = split_off_term ind (IntTerm 1) n i (2 * i - 1)
printfn "  %s" (ind.PrintFormula (expand sot.Stmt))
named "SPLIT OFF TERM  (Σi | 1≤i≤n+1 : 2i−1) = (Σi | 1≤i≤n : 2i−1) + (2(n+1)−1)"  (fun () -> sot)
named "... at a symbolic lower bound and body"                          (fun () -> split_off_term ind k n i (f i))
// The proviso is a real premise: the same identity WITHOUT it is not provable by this route, and it
// is false — at lo = n+2 the left range is empty while the right is {n+1}.
ok "the proviso is part of the theorem"
   (match expand sot.Stmt with Implies(LessEq(_, _), _) -> true | _ -> false)

printfn "\n===== (H) The base case of Gries (12.7):  (Σi | 1 ≤ i ≤ n : 2i−1) = n·n ====="
// At n = 0 the range 1 ≤ i ≤ 0 is empty by ARITHMETIC, which Σ's Empty range (8.13) cannot see — it
// matches the syntactic `false`. Two general order facts make every such range provably empty:
// Transitivity of ≤, and evaluation of a comparison between integer literals.
let z' = IntVar "z"
ok "Transitivity:  (x ≤ y ∧ y ≤ z) ⇒ x ≤ z"
   (ind.AxEquiv (((leq x y) * (leq y z')) ==> (leq x z')).Expr)
ok "Transitivity needs a shared middle term"
   (not (ind.AxEquiv (((leq x y) * (leq z' y)) ==> (leq x y)).Expr))
ok "Transitivity is an implication, not an equivalence"
   (not (ind.AxEquiv ((((leq x y) * (leq y z')) == (leq x z'))).Expr))
ok "(1 ≤ 0) = false, by evaluation"           (ind.AxEquiv ((leq (IntTerm 1) zero) == F).Expr)
ok "(0 ≤ 1) = true, by evaluation"            (ind.AxEquiv ((leq zero (IntTerm 1)) == T).Expr)
ok "... but only the CORRECT value: (1 ≤ 0) = true is no axiom"
   (not (ind.AxEquiv ((leq (IntTerm 1) zero) == T).Expr))
ok "... and only for literals: (1 ≤ n) = false is no axiom"
   (not (ind.AxEquiv ((leq (IntTerm 1) n) == F).Expr))

named "empty_range  (1 ≤ i ∧ i ≤ 0) = false"                      (fun () -> empty_range ind (IntTerm 1) zero i)
named "empty_range  (5 ≤ i ∧ i ≤ -3) = false"                     (fun () -> empty_range ind (IntTerm 5) (IntTerm -3) i)
named "empty_sum    (Σi | 1 ≤ i ≤ 0 : 2i−1) = 0"                  (fun () -> empty_sum ind (IntTerm 1) zero i (2 * i - 1))
ok "empty_range refuses a non-empty range (0..1)"
   (not (proves "empty_range 0..1" (fun () -> empty_range ind zero (IntTerm 1) i)))
ok "empty_range refuses a symbolic bound (1..n)"
   (not (proves "empty_range 1..n" (fun () -> empty_range ind (IntTerm 1) n i)))

// (12.7)  P.n : (Σi | 1 ≤ i ≤ n : 2i−1) = n·n  — the predicate exactly as Gries states it.
let P127 = pred_of n ((qsum i ((leq (IntTerm 1) i) * (leq i n)) (2 * i - 1)) == (n * n))
printfn "  P.0 = %s" (ind.PrintFormula (expand (base_case zero P127).Expr))
named "(12.7) BASE CASE  (Σi | 1 ≤ i ≤ 0 : 2i−1) = 0·0"
      (fun () -> theorem ind (base_case zero P127) [
                     Ident (empty_sum ind (IntTerm 1) zero i (2 * i - 1)) |> at_left   // Σ over 1..0 → 0
                     Integers.reduce                                                    // 0·0 → 0
                 ])

printfn "\n===== (I) The inductive step of (12.7), and (12.7) itself ====="
// The step, in Gries' own words: split off the last term, use the inductive hypothesis, arithmetic.
//   (Σi | 1 ≤ i ≤ n+1 : 2i−1)  =  (Σi | 1 ≤ i ≤ n : 2i−1) + (2(n+1)−1)   Split off term (8.23)
//                              =  n·n + (2(n+1)−1)                      inductive hypothesis P.n
//                              =  (n+1)·(n+1)                           arithmetic
// Split off term's proviso `1 ≤ n+1` comes from the step's `0 ≤ n` by Translation of ≤.

ok "Translation:  (x ≤ y) = (x+1 ≤ y+1)"        (ind.AxEquiv ((leq x y) == (leq (x + 1) (y + 1))).Expr)
ok "Translation does not hold by different amounts"
   (not (ind.AxEquiv ((leq x y) == (leq (x + 1) (y + 2))).Expr))

// REGRESSION: both right-associativity rules used to rewrite (a·b)·c to a·(b + c), so the kernel
// accepted (2·3)·4 = 2·(3+4), i.e. 24 = 14.
ok "right_assoc_mul no longer proves (2·3)·4 = 2·(3+4)"
   (not (proves "24 = 14" (fun () ->
        theorem ind (((IntTerm 2 * IntTerm 3) * IntTerm 4) == (IntTerm 2 * (IntTerm 3 + IntTerm 4))) [ Integers.right_assoc_mul ])))
named "right_assoc_mul proves (a·b)·c = a·(b·c)"
      (fun () -> theorem ind (((x * y) * z') == (x * (y * z'))) [ Integers.right_assoc_mul ])

// Every Integers rewrite rule must preserve the VALUE of what it rewrites — checked on random
// integer expressions, evaluated at random points. This is the check that would have caught the above.
let int_rules_preserve_value =
    let rnd = System.Random(20261011)
    let vs = [| Var("a", typeof<int>); Var("b", typeof<int>); Var("c", typeof<int>) |]
    let rec gen d : Expr =
        if d = 0 || rnd.Next 4 = 0 then (if rnd.Next 2 = 0 then Expr.Var vs.[rnd.Next 3] else Expr.Value(rnd.Next(-3, 4)))
        else let l, r = gen (d - 1), gen (d - 1) in (match rnd.Next 4 with 0 -> call_add l r | 1 -> call_sub l r | 2 -> <@@ - (%%l : int) @@> | _ -> call_mul l r)
    let eval (e: Expr) (v: int[]) =
        FSharp.Quotations.Evaluator.QuotationEvaluator.EvaluateUntyped
            (e.Substitute(fun x -> vs |> Array.tryFindIndex (fun y -> y.Name = x.Name) |> Option.map (fun k -> Expr.Value v.[k]))) :?> int
    [ Integers._reduce_constants; Integers._right_assoc_add; Integers._right_assoc_mul; Integers._left_assoc_add
      Integers._left_assoc_mul; Integers._commute_add; Integers._commute_mul; Integers._distrib_mul_add
      Integers._distrib_mul_sub; Integers._collect_mul_add; Integers._collect_mul_sub; Integers._ident_add
      Integers._ident_mul; Integers._assoc_add_sub ]
    |> List.forall (fun rule ->
        [ 1 .. 600 ] |> List.forall (fun _ ->
            let e = gen 4
            let e' = rule e
            sequal e e' || ([ 1 .. 4 ] |> List.forall (fun _ ->
                               let v = [| rnd.Next(-9, 10); rnd.Next(-9, 10); rnd.Next(-9, 10) |] in eval e v = eval e' v))))
ok "every Integers rewrite rule preserves value (14 rules × 600 random expressions)" int_rules_preserve_value

named "shift_bound  0 ≤ n ⇒ 1 ≤ n+1"                     (fun () -> shift_bound ind zero n)

// The arithmetic — Gries' "(Arithmetic)" step — by the ring normalizer (`Integers.ring_ident`, section J):
// both sides go to one canonical polynomial, and every rewrite is an existing rule at an explicit
// address, re-checked by the kernel. (It was sixteen hand-addressed rewrites before the normalizer.)
let algebra127 = ring_ident ind (n * n + (2 * (n + 1) - 1)) ((n + 1) * (n + 1))
named "algebra      n·n + (2(n+1) − 1) = (n+1)·(n+1)"     (fun () -> algebra127)

// The hypothesis dummy must not occur in P.n at all — and P.n binds `i` inside its Σ — so it is `j`.
let j = IntVar "j"
let E127 = 2 * i - 1
let a127 = leq zero n
let ih127 = hypothesis zero n j P127
let pn1_127 = P127.[n + 1]
let body127 =
    theorem ind (a127 ==> (ih127 ==> pn1_127)) [
        (shunt' a127 ih127 pn1_127 |> Commute) |> apply                     // (0 ≤ n ∧ IH) ⇒ P(n+1)
        Establish (shift_bound ind zero n) |> apply                         // 1 ≤ n+1, Split off term's proviso
        Deduce' (split_off_term ind (IntTerm 1) n i E127) |> apply_right    // Σ(1..n+1) → Σ(1..n) + (2(n+1)−1)
        Deduce' (hypothesis_at ind zero n j P127) |> apply_right            // Σ(1..n) → n·n — THE INDUCTIVE HYPOTHESIS
        Taut algebra127 |> apply_right                                      // n·n + (2(n+1)−1) = (n+1)·(n+1)
    ]
let step127 =
    theorem ind (inductive_case zero n j P127) [
        trade_forall_implies n (pred_of n a127) (pred_of n (ih127 ==> pn1_127)) |> apply
        Taut body127 |> at [select_body]
        ident_forall_true' n
    ]
let base127 =
    theorem ind (base_case zero P127) [
        Ident (empty_sum ind (IntTerm 1) zero i E127) |> at_left
        Integers.reduce
    ]
named "step body    0 ≤ n ⇒ (IH ⇒ P(n+1))"                 (fun () -> body127)
named "inductive case"                                      (fun () -> step127)
ok "the step does not close without the inductive hypothesis"
   (not (proves "step without IH" (fun () ->
        theorem ind (a127 ==> (ih127 ==> pn1_127)) [
            (shunt' a127 ih127 pn1_127 |> Commute) |> apply
            Establish (shift_bound ind zero n) |> apply
            Deduce' (split_off_term ind (IntTerm 1) n i E127) |> apply_right
            Taut algebra127 |> apply_right
        ])))

let thm127 = by_induction ind zero n j P127 base127 step127
printfn "  %s" (ind.PrintFormula (expand thm127.Stmt))
named "(12.7) BY INDUCTION   (∀n | 0 ≤ n : (Σi | 1 ≤ i ≤ n : 2i−1) = n·n)"   (fun () -> thm127)
ok "(12.7) is the statement Gries gives"
   (sequal (expand thm127.Stmt) (expand (qall n (leq zero n) ((qsum i ((leq (IntTerm 1) i) * (leq i n)) (2 * i - 1)) == (n * n))).Expr))

printfn "\n===== (J) Ring normalization: Gries' \"(Arithmetic)\" step as a tactic ====="
// `ring_ident th L R` rewrites both sides to one canonical polynomial and closes on syntactic
// equality. It PRODUCES A PROOF — every step an existing rule at an explicit address, re-checked by
// the kernel — so a bug in it can only make it fail. Completeness is what needs testing.
let ringProves (label: string) (L: Term<int>) (R: Term<int>) = named label (fun () -> ring_ident ind L R)
let ringRefuses (label: string) (L: Term<int>) (R: Term<int>) =
    ok label (not (proves label (fun () -> ring_ident ind L R)))

ringProves "square of a sum        (x+y)² = x² + 2xy + y²"   ((x + y) * (x + y)) (x * x + 2 * (x * y) + y * y)
ringProves "difference of squares  (x+y)(x−y) = x² − y²"     ((x + y) * (x - y)) (x * x - y * y)
ringProves "cube                   (x+1)³"                    ((x + 1) * ((x + 1) * (x + 1))) (x * (x * x) + 3 * (x * x) + 3 * x + 1)
ringProves "cancellation           (x − x) + y·0 = 0"        (x - x + (y * 0)) zero
ringProves "unary minus            −(x − y) = y − x"          (-(x - y)) (y - x)
ringProves "atoms are opaque       f(n)·2 + f(n) = 3·f(n)"    (f n * 2 + f n) (3 * f n)
ringRefuses "refuses an off-by-one  (x+1)² = x² + 2x"        ((x + 1) * (x + 1)) (x * x + 2 * x)
ringRefuses "refuses a wrong cross term"                       ((x + y) * (x + y)) (x * x + x * y + y * y)
ringRefuses "refuses x − y = y − x"                            (x - y) (y - x)

// Completeness, fuzzed: a random expression scrambled by random value-preserving rule applications
// must still be proved equal to itself; and on independent random pairs the tactic must succeed
// EXACTLY when the two agree by evaluation.
let ring_fuzz =
    let rnd = System.Random(20261012)
    let vs = [| Var("a", typeof<int>); Var("b", typeof<int>); Var("c", typeof<int>) |]
    let rec gen d : Expr =
        if d = 0 || rnd.Next 4 = 0 then (if rnd.Next 3 > 0 then Expr.Var vs.[rnd.Next 3] else Expr.Value(rnd.Next(-2, 4)))
        else
            match rnd.Next 7 with
            | 0 | 1 -> call_add (gen (d - 1)) (gen (d - 1))
            | 2 -> call_sub (gen (d - 1)) (gen (d - 1))
            | 3 -> <@@ - (%%(gen (d - 1)) : int) @@>
            | _ -> call_mul (gen (d - 1)) (gen (d - 1))
    let eval (e: Expr) (v: int[]) =
        FSharp.Quotations.Evaluator.QuotationEvaluator.EvaluateUntyped
            (e.Substitute(fun x -> vs |> Array.tryFindIndex (fun y -> y.Name = x.Name) |> Option.map (fun k -> Expr.Value v.[k]))) :?> int
    let sameValues e1 e2 = [ 1 .. 8 ] |> List.forall (fun _ -> let v = [| rnd.Next(-20, 21); rnd.Next(-20, 21); rnd.Next(-20, 21) |] in eval e1 v = eval e2 v)
    // a − b → a + b·(−1), locally: the normalizer gets this from (15.14) and (15.20), not from a rule
    let subToAddNeg (e: Expr) = match e with Subtract(a, b) -> <@@ (%%a: int) + (%%b: int) * -1 @@> | _ -> e
    let scramblers = [| Integers._commute_add; Integers._commute_mul; Integers._distrib_mul_add; Integers._right_assoc_add
                        Integers._left_assoc_add; Integers._right_assoc_mul; Integers._left_assoc_mul
                        Integers._collect_mul_add; Integers._reduce_constants; subToAddNeg |]
    let rec scrambleAt (e: Expr) : Expr =
        match e, rnd.Next 3 with
        | (Add _ | Subtract _ | Multiply _), 1 -> (match e with Patterns.Call(None, m, [l; r]) -> Expr.Call(m, [ scrambleAt l; r ]) | _ -> e)
        | (Add _ | Subtract _ | Multiply _), 2 -> (match e with Patterns.Call(None, m, [l; r]) -> Expr.Call(m, [ l; scrambleAt r ]) | _ -> e)
        | _ -> scramblers.[rnd.Next scramblers.Length] e
    let proves (e1: Expr) (e2: Expr) =
        try ring_ident ind (IntTerm(expand_as<int> e1)) (IntTerm(expand_as<int> e2)) |> ignore; true with _ -> false
    let equalPairs = [ 1 .. 100 ] |> List.forall (fun _ -> let e = gen 3 in proves e (Seq.fold (fun acc _ -> scrambleAt acc) e [ 1 .. 6 ]))
    let agree = [ 1 .. 100 ] |> List.forall (fun _ -> let e1, e2 = gen 2, gen 2 in proves e1 e2 = sameValues e1 e2)
    equalPairs, agree
ok "completeness: 100 scrambled pairs all proved equal"                (fst ring_fuzz)
ok "100 independent pairs: proved exactly when equal by evaluation"    (snd ring_fuzz)

printfn "\n===== (K) Integral domains (Gries ch.15 §15.1): the theorems behind (Arithmetic) ====="
// `Integers` is an `IntegralDomain<int>`: its ring axioms are Gries' (15.1)–(15.7), (15.13), (15.14),
// stated once for any carrier, and (15.8)–(15.29) are THEOREMS proved from them in `IntegralDomain.fs`.
// The ring normalizer of (J) gets minus and zero from three of them — (15.9), (15.14), (15.20) —
// where it previously used three trusted rewrite rules.
let dom = induction :> IntegralDomain.IntegralDomain<int>
let proved (label: string) (f: unit -> Rule) =
    ok label (try (match f () with Derive(_, p, _) -> p.Complete | _ -> false) with e -> why label e)
let xe, ye, ze, we = x.Expr, y.Expr, (IntVar "z").Expr, (IntVar "w").Expr
let xy = (x + y).Expr                                      // a compound argument, to catch mis-targeting
proved "(15.8)  cancellation   (a + b = a + c) = (b = c)"   (fun () -> IntegralDomain.cancel_add dom xe ye ze)
proved "(15.9)  zero           a·0 = 0"                     (fun () -> IntegralDomain.zero_mul dom xy)
proved "(15.10a) unique zero   (a + z = a) = (z = 0)"       (fun () -> IntegralDomain.unique_zero dom xe ze)
named  "(15.10b) unique one    a ≠ 0 ⇒ ((a·z = a) = (z = 1))" (fun () -> IntegralDomain.unique_one dom xe ze)
proved "(15.11) zero divisors  (a·b = 0) = (a = 0 ∨ b = 0)" (fun () -> IntegralDomain.zero_divisors dom xe ye)
named  "(15.12) unique inverse"                              (fun () -> IntegralDomain.unique_inverse dom xe ye ze)
proved "(15.15) (x + a = 0) = (x = −a)"                     (fun () -> IntegralDomain.add_eq_zero dom xe ye)
proved "(15.16) (−a = −b) = (a = b)"                        (fun () -> IntegralDomain.neg_eq dom xe ye)
proved "(15.17) −(−a) = a"                                  (fun () -> IntegralDomain.neg_neg dom xy)
proved "(15.18) −0 = 0"                                     (fun () -> IntegralDomain.neg_zero dom)
proved "(15.19) −(a + b) = (−a) + (−b)"                     (fun () -> IntegralDomain.neg_add dom xe ye)
proved "(15.20) −a = (−1)·a"                                (fun () -> IntegralDomain.neg_as_mul dom xy)
proved "(15.21) (−a)·b = a·(−b)"                            (fun () -> IntegralDomain.neg_mul_comm dom xe ye)
proved "(15.22) a·(−b) = −(a·b)"                            (fun () -> IntegralDomain.mul_neg dom xe ye)
proved "(15.23) (−a)·(−b) = a·b"                            (fun () -> IntegralDomain.neg_mul_neg dom xe ye)
proved "(15.24) a − 0 = a"                                  (fun () -> IntegralDomain.sub_zero dom xe)
proved "(15.25) (a − b) + (c − d) = (a + c) − (b + d)"      (fun () -> IntegralDomain.sub_add_sub dom xe ye ze we)
proved "(15.26) (a − b) − (c − d) = (a + d) − (b + c)"      (fun () -> IntegralDomain.sub_sub_sub dom xe ye ze we)
proved "(15.27) (a − b)·(c − d) = (a·c + b·d) − (a·d + b·c)" (fun () -> IntegralDomain.sub_mul_sub dom xe ye ze we)
proved "(15.28) (a − b = c − d) = (a + d = b + c)"          (fun () -> IntegralDomain.sub_eq_sub dom xe ye ze we)
proved "(15.29) (a − b)·c = a·c − b·c"                      (fun () -> IntegralDomain.sub_mul dom xe ye ze)
named  "used in an Integers proof   (x − y)·0 = 0"
    (fun () -> theorem ind ((x - y) * zero == zero) [ IntegralDomain.zero_mul dom (x - y).Expr |> at_left ])
ok "refuses x·0 = x by (15.9)"
    (not (proves "x·0 = x" (fun () -> theorem ind (x * zero == x) [ IntegralDomain.zero_mul dom xe |> at_left ])))
ok "(15.9) for x does not rewrite y·0"
    (not (proves "y·0 = 0 by x·0" (fun () -> theorem ind (y * zero == zero) [ IntegralDomain.zero_mul dom xe |> at_left ])))

// The theory is GENERIC: the same theorems, at a second carrier.
let ops64 : IntegralDomain.DomainOps<int64> =
    { Name = "Int64"; Add = <@ (+) @>; Mul = <@ (*) @>; Zero = <@ 0L @>; One = <@ 1L @>; Neg = <@ (~-) @>; Sub = <@ (-) @> }
let dom64 = IntegralDomain.IntegralDomain<int64>(ops64)
let p64, q64 = Expr.Var(Var("p", typeof<int64>)) |> Expr.Cast<int64>, Expr.Var(Var("q", typeof<int64>)) |> Expr.Cast<int64>
proved "(15.23) at int64   (−p)·(−q) = p·q"                 (fun () -> IntegralDomain.neg_mul_neg dom64 p64 q64)
proved "(15.11) at int64   (p·q = 0) = (p = 0 ∨ q = 0)"     (fun () -> IntegralDomain.zero_divisors dom64 p64 q64)
printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
