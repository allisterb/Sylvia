#load "Include.fsx"

// Divisibility (Gries & Schneider, "A Logical Approach to Discrete Math", ch. 15, §15.4).
//
// What this script verifies:
//   (A) (15.77), the definition of ∣, is the theory's ONE new axiom — and its side conditions are checked;
//   (B) (15.78)–(15.80) by ∃-introduction, and a concrete divisibility, 3 ∣ 6;
//   (C) (15.81), the units, and (15.83);
//   (D) (15.82), (15.84), (15.85) and Gries' exercise 15.65 (with its − twin) by ∃-elimination;
//   (E) (15.86), and its instance ¬(2 ∣ 3);
//   (F) Theorem (15.87), the division theorem, from Well ordering (15.49);
//   (G) refusals: wrong witnesses, the wrong theory, captured dummies;
//   (H) every non-schema theorem at arguments that contain one another;
//   (I) which theories' theorems a proof may use, and Witness (9.30) inside this theory.
//
// Run:  dotnet fsi examples/proofs/Divisibility.fsx

open Sylvia
open FSharp.Quotations
open FsExpr
open Formula
open PropCalculus
open PredCalculus
open Integers
open Induction
open Divisibility

fsi.PrintWidth <- 300
Proof.LogLevel <- 0

let mutable failures = 0
let ok label cond =
    if not cond then failures <- failures + 1
    printfn "  %s  %s" (if cond then "✓" else "✗") label
let why what (e: exn) = printfn "     [%s] %s" what (e.Message.Split('\n').[0]); false
let refuses (what: string) (f: unit -> Theorem) = ok what (try f () |> ignore; false with _ -> true)
/// The theorem is complete and states exactly `p`.
let states (label: string) (p: Prop) (f: unit -> Theorem) =
    ok label (try (let t = f () in t.Proof.Complete && sequal (expand t.Stmt) (expand p.Expr)) with e -> why label e)

let th = divisibility :> Theory
let D = Integers.order_domain th
let zero, one = D.Zero, D.One
let b, c, d, k = IntVar "b", IntVar "c", IntVar "d", IntVar "k"
let t (v: IntVar) = D.Term v
let sw = System.Diagnostics.Stopwatch()
let timed (f: unit -> 'a) = sw.Restart(); let r = f () in r, sw.ElapsedMilliseconds

printfn "\n===== (A) (15.77) Definition of ∣ — the one new axiom ====="

let def77 = dvd c b == qex d T ((IntTerm c.Expr * d :> Term<int>) == b)
ok "c ∣ b = (∃d |: c·d = b) is an axiom of Divisibility"           (th.IsAxiom def77.Expr)
ok "... with any dummy fresh for c and b"                          (th.IsAxiom (dvd c b == qex k T ((IntTerm c.Expr * k :> Term<int>) == b)).Expr)
ok "it is NOT an axiom of plain Induction"                         (not ((induction :> Theory).IsAxiom def77.Expr))
ok "refused: the dummy occurs in b (c ∣ d = (∃d |: c·d = d))"      (not (th.IsAxiom (dvd c d == qex d T ((IntTerm c.Expr * d :> Term<int>) == d)).Expr))
ok "refused: the factors swapped (∃d |: d·c = b)"                  (not (th.IsAxiom (dvd c b == qex d T ((d * IntTerm c.Expr :> Term<int>) == b)).Expr))
ok "refused: a range (∃d | 0 ≤ d : c·d = b)"                       (not (th.IsAxiom (dvd c b == qex d (leq 0 d) ((IntTerm c.Expr * d :> Term<int>) == b)).Expr))
ok "refused: the wrong right-hand side (∃d |: c·d = c)"            (not (th.IsAxiom (dvd c b == qex d T ((IntTerm c.Expr * d :> Term<int>) == c)).Expr))
let shown = th.PrintFormula (dvd c b).Expr
printfn "     c ∣ b prints as: %s" shown
ok "∣ prints infix"                                                (shown = "c ∣ b")

printfn "\n===== (B) (15.78)–(15.80): ∃-introduction ====="

states "(15.78) c ∣ c"     (dvd c c)        (fun () -> dvd_refl th c)
states "(15.79) c ∣ 0"     (dvd c zero)     (fun () -> dvd_zero th c)
states "(15.80) 1 ∣ b"     (dvd one b)      (fun () -> one_dvd th b)
states "3 ∣ 6, witness 2"  (dvd (IntTerm 3) (IntTerm 6))
                           (fun () -> dvd_intro th (IntTerm 3) (IntTerm 6) (IntTerm 2) (ring_ident th (IntTerm 3 * IntTerm 2) (IntTerm 6)))

printfn "\n===== (C) (15.81) the units, and (15.83) ====="

let tc, ms81 = timed (fun () -> dvd_one th c)
states "(15.81) c ∣ 1 ⇒ c = 1 ∨ c = −1"   ((dvd c one) ==> ((t c == one) + (t c == -one))) (fun () -> tc)
let _, ms81b = timed (fun () -> dvd_one th (t b + one))
printfn "     (15.81): first call %d ms, then %d ms (a schema, instantiated)" ms81 ms81b
states "(15.81) at b + 1"                ((dvd (t b + one) one) ==> ((t b + one == one) + (t b + one == -one))) (fun () -> dvd_one th (t b + one))
states "the units: x·y = 1 ⇒ x = 1 ∨ x = −1" (((t b * t c) == one) ==> ((t b == one) + (t b == -one))) (fun () -> unit_cases th b c)
states "(15.83) b ∣ c ∧ c ∣ b ≡ b = c ∨ b = −c" (((dvd b c) * (dvd c b)) == ((t b == t c) + (t b == -(t c)))) (fun () -> dvd_antisym th b c)

printfn "\n===== (D) ∃-elimination: (15.82), (15.84), (15.85), ex.15.65 ====="

states "(15.82) d ∣ c ∧ c ∣ b ⇒ d ∣ b"     (((dvd d c) * (dvd c b)) ==> (dvd d b))                  (fun () -> dvd_trans th d c b)
states "(15.84) b ∣ c ⇒ b ∣ c·d"           ((dvd b c) ==> (dvd b (t c * t d)))                       (fun () -> dvd_mul th b c d)
states "(15.85) b ∣ c ⇒ b·d ∣ c·d"         ((dvd b c) ==> (dvd (t b * t d) (t c * t d)))             (fun () -> dvd_mul_both th b c d)
states "ex.15.65 d ∣ b ∧ d ∣ c ⇒ d ∣ (b + c)" (((dvd d b) * (dvd d c)) ==> (dvd d (t b + t c)))      (fun () -> dvd_add th d b c)
states "d ∣ b ∧ d ∣ c ⇒ d ∣ (b − c)"       (((dvd d b) * (dvd d c)) ==> (dvd d (t b - t c)))         (fun () -> dvd_sub th d b c)
// The ∃ dummy of (15.77) is chosen fresh: here the caller's own variable is called `d`.
states "(15.84) when the caller's own variable is called d: d ∣ c ⇒ d ∣ c·d" ((dvd d c) ==> (dvd d (t c * t d))) (fun () -> dvd_mul th d c d)

printfn "\n===== (E) (15.86), and ¬(2 ∣ 3) ====="

states "(15.86) 1 < b ∧ b ∣ c ⇒ ¬(b ∣ (c + 1))" (((one <. t b) * (dvd b c)) ==> !!(dvd b (t c + one))) (fun () -> not_dvd_succ th b c)
let one_lt_two =
    lemma th (lt 1 2) [
        Ident (lt_iff_succ_le th (IntTerm 1) (IntTerm 2))  |> apply              // (1 < 2) → (1 + 1 ≤ 2)
        Integers.reduce                                    |> at_left
        ax_ident th ((leq 2 2) == T)                       |> apply              // literal evaluation
    ]
let two = IntTerm 2
states "¬(2 ∣ 3): (15.86) at b, c := 2, 2, with 1 < 2 and (15.78)" (!!(dvd two (D.Term two + one)))
       (fun () -> mp (not_dvd_succ th two two) (conj one_lt_two (dvd_refl th two)))
states "(a < b) = (a + 1 ≤ b)"   ((t b <. t c) == ((t b + one) <=. t c))   (fun () -> lt_iff_succ_le th b c)
states "0 < x ⇒ 1 ≤ x"           ((zero <. t b) ==> (one <=. t b))           (fun () -> one_le_of_pos th b)

printfn "\n===== (F) Theorem (15.87): the division theorem, from Well ordering ====="

let q, r = IntVar "q", IntVar "r"
let division_stmt (bb: Term<int>) (cc: Term<int>) (q: IntVar) (r: IntVar) =
    let bb, cc = D.Term bb, D.Term cc
    (zero <. cc) ==> qex q T (exists(r, pred_of r ((zero <=. t r) * (t r <. cc)), pred_of r (bb == t q * cc + t r)))
let t87, ms87 = timed (fun () -> division_theorem th b c)
printfn "     %s   (%d ms)" (th.PrintFormula t87.Stmt) ms87
states "(15.87) 0 < c ⇒ (∃q |: (∃r | 0 ≤ r ∧ r < c : b = q·c + r))" (division_stmt b c q r) (fun () -> t87)
states "(15.87) at b, c := −7, 3"  (division_stmt (IntTerm -7) (IntTerm 3) q r)  (fun () -> division_theorem th (IntTerm -7) (IntTerm 3))
// A caller's variables named q and r do not capture: the quotient and remainder dummies are renamed.
let q1, r1 = IntVar "q#1", IntVar "r#1"
states "(15.87) at b, c := q, r — the dummies become q#1, r#1" (division_stmt q r q1 r1) (fun () -> division_theorem th q r)

printfn "\n===== (G) Refusals ====="

refuses "3 ∣ 7 with witness 2 (3·2 = 7 is not a ring identity)"
        (fun () -> dvd_intro th (IntTerm 3) (IntTerm 7) (IntTerm 2) (ring_ident th (IntTerm 3 * IntTerm 2) (IntTerm 7)))
refuses "dvd_intro with a theorem about a different product"
        (fun () -> dvd_intro th c c (IntTerm 1) (ring_ident th (IntTerm 1 * IntTerm c.Expr) c))
refuses "(15.78) in plain Induction (no (15.77) there)"     (fun () -> dvd_refl (induction :> Theory) c)
refuses "(15.81) in plain Induction"                         (fun () -> dvd_one (Induction.Induction() :> Theory) c)
refuses "exists_elim with the dummy free in the conclusion"
        (fun () -> exists_elim th k truepred (pred_of k (leq 0 k)) (leq 0 k) (lemma th (((T * leq 0 k)) ==> leq 0 k) [ commute_and T (leq 0 k) |> at_left; ident_and (leq 0 k) |> at_left ]))

printfn "\n===== (H) Overlapping arguments: each rewrite must hit the occurrence it addresses ====="

// A derivation step rewrites the leftmost-outermost match inside the subterm it addresses, so a
// step that addresses too coarsely rewrites the wrong occurrence once an argument contains another.
// The schema-cached theorems (15.81), (15.83), (15.86) are derived once at distinct metavariables and
// cannot be affected; these are the others, at arguments that contain one another.
let tt (x: Term<int>) = D.Term x
let trans_stmt (x: Term<int>) (y: Term<int>) (z: Term<int>) = ((dvd x y) * (dvd y z)) ==> (dvd x z)
for (lbl, x, y, z) in [ "c, c, c", tt c, tt c, tt c
                        "b, b·b, b", tt b, tt b * tt b, tt b
                        "c+1, c, c+1", tt c + one, tt c, tt c + one ] do
    states (sprintf "(15.82) at %s" lbl) (trans_stmt x y z) (fun () -> dvd_trans th x y z)
for (lbl, x, y, z) in [ "c, c, c", tt c, tt c, tt c
                        "c·d, c, d", tt c * tt d, tt c, tt d ] do
    states (sprintf "(15.84) at %s" lbl) ((dvd x y) ==> (dvd x (tt y * tt z))) (fun () -> dvd_mul th x y z)
    states (sprintf "(15.85) at %s" lbl) ((dvd x y) ==> (dvd (tt x * tt z) (tt y * tt z))) (fun () -> dvd_mul_both th x y z)
for (lbl, x, y, z) in [ "c, c, c", tt c, tt c, tt c
                        "b+c, b, c", tt b + tt c, tt b, tt c
                        "c, c, c·c", tt c, tt c, tt c * tt c ] do
    states (sprintf "ex.15.65 at %s" lbl) (((dvd x y) * (dvd x z)) ==> (dvd x (tt y + tt z))) (fun () -> dvd_add th x y z)
    states (sprintf "d ∣ (x − y) at %s" lbl) (((dvd x y) * (dvd x z)) ==> (dvd x (tt y - tt z))) (fun () -> dvd_sub th x y z)
states "(15.87) at b, c := c·c, c" (division_stmt (tt c * tt c) c q r) (fun () -> division_theorem th (tt c * tt c) c)
states "(15.87) at b, c := c, c"   (division_stmt c c q r)             (fun () -> division_theorem th c c)
states "(a < b) = (a + 1 ≤ b) at b − 1, b" ((tt b - one <. tt b) == ((tt b - one + one) <=. tt b)) (fun () -> lt_iff_succ_le th (tt b - one) b)
states "0 < x ⇒ 1 ≤ x at x := 1 + 1"  ((zero <. (one + one)) ==> (one <=. (one + one))) (fun () -> one_le_of_pos th (one + one))

printfn "\n===== (I) Which theorems a proof may use; ∃-elimination by Witness (9.30) inside a theory ====="

// A proof may use a theorem from the logic S, from its own theory, or from a theory INSTANCE its
// theory declares it extends: `Divisibility` extends the canonical `induction`, which extends the
// canonical `integers`. This was once a test on .NET classes, under which the first four refusals
// below all proved `false` (measured 2026-11-13).
let anything : Axioms = fun _ -> Some (Descriptions.axiom_name "Bogus" "anything")
let prove_false_with (src: Theory) (dst: Theory) () =
    let fEqT = axiom src (F == T)                          // ⊢_src  F = T
    lemma dst F [ Ident fEqT |> apply ]
refuses "refused: F = T from a plain Theory, in Divisibility"             (prove_false_with (Theory(anything, [])) th)
refuses "refused: F = T from Integers(extra axioms), in Integers"         (prove_false_with (Integers.Integers(anything)) integers)
refuses "refused: F = T from Integers(extra axioms), in Divisibility"     (prove_false_with (Integers.Integers(anything)) th)
refuses "refused: F = T from Induction(extra axioms), in Divisibility"    (prove_false_with (Induction.Induction(anything)) th)
refuses "refused: a Divisibility theorem, in Integers"                    (fun () -> lemma integers (dvd c c) [ Taut (dvd_refl th c) |> apply ])
states  "a theorem of the canonical integers, in Divisibility"           (D.Term c <=. D.Term c)
                                                                          (fun () -> lemma th (D.Term c <=. D.Term c) [ Integers.le_refl integers c |> apply ])
states  "a theorem of the canonical induction, in Divisibility"          (D.Term c <=. D.Term c)
                                                                          (fun () -> lemma th (D.Term c <=. D.Term c) [ Integers.le_refl induction c |> apply ])
ok "Divisibility includes induction and integers, not the reverse"        (th.Includes induction && th.Includes integers && not ((induction :> Theory).Includes th))

// Witness (9.30) used to build its conclusion in pred_calculus, which refused any subproof from a
// theory; the conclusion now lives in the subproof's theory.
let ex_dvd = qex k T ((IntTerm c.Expr * k :> Term<int>) == b)
let by_witness () =
    witness k truepred (pred_of k ((IntTerm c.Expr * k :> Term<int>) == b)) (dvd c b) (fun kh ->
        let bodyk = (IntTerm c.Expr * kh :> Term<int>) == b
        lemma th ((T * bodyk) ==> dvd c b) [
            dvd_def th c b                                                        |> at_right
            Deduce (exists_intro d (pred_of d ((IntTerm c.Expr * d :> Term<int>) == b)) kh) |> apply_right ])
states "witness: (∃k |: c·k = b) ⇒ c ∣ b, in Divisibility"                 (ex_dvd ==> dvd c b) by_witness
ok     "... and the conclusion is a Divisibility theorem"                 (try obj.ReferenceEquals((by_witness ()).Proof.Theory, th) with e -> why "witness" e)

printfn "\n%s" (if failures = 0 then "All divisibility checks passed." else sprintf "%d CHECK(S) FAILED." failures)
