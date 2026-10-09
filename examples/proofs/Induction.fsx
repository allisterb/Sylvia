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
//   (E) `by_induction` refuses proofs of anything but the two obligations.
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

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
