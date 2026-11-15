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
//   (J) (15.89) uniqueness of quotient and remainder; ÷ and mod (15.90)–(15.91), and c ∣ b ≡ b mod c = 0.
//   (K) gcd: its definition (an axiom, justified by an existence proof that does not cite it), Bézout,
//       Gries' (15.92) as a theorem, (15.96)–(15.104), (15.106)–(15.107), and Euclid's algorithm as a proof.
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
open DomainTerms

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

printfn "\n===== (J) (15.89) uniqueness; ÷ and mod (15.90)–(15.91) ====="

let q2, r2 = IntVar "q2", IntVar "r2"          // q, r are (F)'s
let C0 = zero <. t c
/// `b = q·c + r ∧ 0 ≤ r ∧ r < c` at the dividend `bb` and divisor `cc`.
let QRat (bb: DomainTerm<int>) (cc: DomainTerm<int>) (qq: DomainTerm<int>) (rr: DomainTerm<int>) = ((bb == qq * cc + rr) * (zero <=. rr)) * (rr <. cc)
let QR (qq: IntVar) (rr: IntVar) = QRat (t b) (t c) (t qq) (t rr)
let (uq, ms89) = timed (fun () -> try Some (div_unique th b c q r q2 r2) with e -> why "div_unique" e |> ignore; None)
states "(15.89) (0 < c ∧ qr(q,r)) ∧ qr(q2,r2) ⇒ q = q2 ∧ r = r2"   (((C0 * QR q r) * QR q2 r2) ==> ((t q == t q2) * (t r == t r2))) (fun () -> uq.Value)
let (_, ms89') = timed (fun () -> div_unique th c b r q r2 q2)
printfn "     first use %d ms (one-time glue included); again, at other arguments, %d ms" ms89 ms89'

let bdc, bmc = D.Term (div_of b c), D.Term (mod_of b c)
let spec90 = C0 ==> (((t b == bdc * t c + bmc) * (zero <=. bmc)) * (bmc <. t c))
ok "(15.90) 0 < c ⇒ b = (b÷c)·c + b mod c ∧ 0 ≤ b mod c < c is an axiom of Divisibility" (th.IsAxiom spec90.Expr)
printfn "     it prints as: %s" (th.PrintFormula spec90.Expr)
ok "it is NOT an axiom of plain Induction"                                   (not ((induction :> Theory).IsAxiom spec90.Expr))
ok "refused: Gries' printed side condition c ≠ 0 (the range is empty at c < 0)" (not (th.IsAxiom ((t c != zero) ==> (((t b == bdc * t c + bmc) * (zero <=. bmc)) * (bmc <. t c))).Expr))
ok "refused: a remainder only bounded by r ≤ c"                              (not (th.IsAxiom (C0 ==> (((t b == bdc * t c + bmc) * (zero <=. bmc)) * (bmc <=. t c))).Expr))
ok "refused: a different dividend in the equation"                           (not (th.IsAxiom (C0 ==> (((t q == bdc * t c + bmc) * (zero <=. bmc)) * (bmc <. t c))).Expr))

states "(15.90) 0 < c ⇒ b = (b ÷ c)·c + b mod c"      (C0 ==> (t b == bdc * t c + bmc)) (fun () -> div_mod_def th b c)
states "(15.90) 0 < c ⇒ 0 ≤ b mod c"                   (C0 ==> (zero <=. bmc))           (fun () -> mod_nonneg th b c)
states "(15.90) 0 < c ⇒ b mod c < c"                   (C0 ==> (bmc <. t c))             (fun () -> mod_lt th b c)
states "(15.91) 0 < c ⇒ b = c·(b ÷ c) + b mod c"      (C0 ==> (t b == t c * bdc + bmc)) (fun () -> div_mod th b c)
states "(15.90) 0 < c ∧ qr(q,r) ⇒ b ÷ c = q"           ((C0 * QR q r) ==> (bdc == t q))  (fun () -> div_eq th b c q r)
states "(15.90) 0 < c ∧ qr(q,r) ⇒ b mod c = r"         ((C0 * QR q r) ==> (bmc == t r))  (fun () -> mod_eq th b c q r)
states "0 < c ⇒ (c ∣ b = (b mod c = 0))"               (C0 ==> (dvd c b == (bmc == zero))) (fun () -> dvd_iff_mod_zero th c b)

// A concrete instance: (−7) ÷ 2 = −4 and (−7) mod 2 = 1 — Euclidean, NOT F#'s truncating −3 and −1.
let L (n: int) = D.Term (IntTerm n)
let lit_lt (a: int) (bb: int) =
    lemma th (lt a bb) [
        Ident (lt_iff_succ_le th (IntTerm a) (IntTerm bb))  |> apply              // (a < b) → (a + 1 ≤ b)
        Integers.reduce                                     |> at_left
        ax_ident th ((leq (a + 1) bb) == T)                 |> apply ]            // literal evaluation
let lit_le (a: int) (bb: int) = lemma th (leq a bb) [ ax_ident th ((leq a bb) == T) |> apply ]
let qr_7_2 () = conj (lit_lt 0 2) (conj (conj (Integers.ring_ident th (L -7) (L -4 * L 2 + L 1)) (lit_le 0 1)) (lit_lt 1 2))
states "0 < 2 ∧ (−7 = (−4)·2 + 1 ∧ 0 ≤ 1 ∧ 1 < 2)"   ((zero <. L 2) * QRat (L -7) (L 2) (L -4) (L 1)) qr_7_2
states "(−7) ÷ 2 = −4"   (D.Term (div_of (IntTerm -7) (IntTerm 2)) == L -4) (fun () -> mp (div_eq th (IntTerm -7) (IntTerm 2) (IntTerm -4) (IntTerm 1)) (qr_7_2 ()))
states "(−7) mod 2 = 1"  (D.Term (mod_of (IntTerm -7) (IntTerm 2)) == L 1)  (fun () -> mp (mod_eq th (IntTerm -7) (IntTerm 2) (IntTerm -4) (IntTerm 1)) (qr_7_2 ()))
refuses "refused: the truncated pair q, r := −3, −1 (0 ≤ −1 does not evaluate to true)"
    (fun () -> mp (div_eq th (IntTerm -7) (IntTerm 2) (IntTerm -3) (IntTerm -1))
                  (conj (lit_lt 0 2) (conj (conj (Integers.ring_ident th (L -7) (L -3 * L 2 + L -1)) (lit_le 0 -1)) (lit_lt -1 2))))

// Arguments that contain one another (the mis-targeting class): non-schema theorems only.
let bb = t b * t b
states "(15.91) at b, c := c·c, c"                      ((zero <. t c) ==> ((t c * t c) == t c * D.Term (div_of (t c * t c) c) + D.Term (mod_of (t c * t c) c)))
                                                        (fun () -> div_mod th (t c * t c) c)
states "0 < b ⇒ (b ∣ b·b = (b·b mod b = 0))"          ((zero <. t b) ==> (dvd b bb == (D.Term (mod_of bb b) == zero))) (fun () -> dvd_iff_mod_zero th b bb)
states "0 < b + 1 ⇒ (b + 1 ∣ b = (b mod (b + 1) = 0))" ((zero <. t b + one) ==> (dvd (t b + one) b == (D.Term (mod_of b (t b + one)) == zero)))
                                                        (fun () -> dvd_iff_mod_zero th (t b + one) b)
states "(15.90) at b, c, q, r := b·b, b, b, 0"          (((zero <. t b) * QRat bb (t b) (t b) zero) ==> (D.Term (div_of bb b) == t b)) (fun () -> div_eq th bb b b zero)

printfn "\n===== (K) gcd: its definition, Bézout, and (15.92)–(15.107) ====="

let G = D.Term (gcd_of b c)
let gc (x: Term<int>) (y: Term<int>) = D.Term (gcd_of x y)
/// The definition of gcd at x, y with the dummy dv, as a user would write it.
let gcd_def (dv: IntVar) (x: Term<int>) (y: Term<int>) =
    let g, z = gc x y, t dv
    ((zero <=. g) * (dvd g x * dvd g y)) * qall dv (dvd z x * dvd z y) (dvd z g)
ok "(0 ≤ b gcd c ∧ (b gcd c ∣ b ∧ b gcd c ∣ c)) ∧ (∀d | d ∣ b ∧ d ∣ c : d ∣ b gcd c) is an axiom" (th.IsAxiom (gcd_def d b c).Expr)
printfn "     it prints as: %s" (th.PrintFormula (gcd_def d b c).Expr)
ok "... with any dummy fresh for b and c"                          (th.IsAxiom (gcd_def k b c).Expr)
ok "it is NOT an axiom of plain Induction"                         (not ((induction :> Theory).IsAxiom (gcd_def d b c).Expr))
ok "refused: the dummy occurs in b"                                 (not (th.IsAxiom (gcd_def b b c).Expr))
ok "refused: Gries' order form, d ≤ b gcd c in the body"            (not (th.IsAxiom ((((zero <=. G) * (dvd G (t b) * dvd G (t c))) * qall d (dvd (t d) (t b) * dvd (t d) (t c)) (t d <=. G))).Expr))
ok "refused: without 0 ≤ b gcd c (then −(b gcd c) qualifies too)"   (not (th.IsAxiom (((T * (dvd G (t b) * dvd G (t c))) * qall d (dvd (t d) (t b) * dvd (t d) (t c)) (dvd (t d) G))).Expr))

/// Does any step of the proof — or of a proof it rests on — use an axiom matched by `isAx`?
let cites (isAx: Expr -> bool) (thm: Theorem) =
    let seen = System.Collections.Generic.HashSet<Proof>(HashIdentity.Reference)
    let rec walk (p: Proof) =
        seen.Add p
        && (isAx p.Stmt
            || (match p.State with [] -> false | s -> isAx (fst (List.last s)))
            || p.Steps |> List.exists (fun st ->
                   match (try Some st.Rule with _ -> None) with
                   | Some (Derive(_, q, _)) | Some (Deduce(_, q, _)) | Some (Establish(_, q)) -> walk q
                   | _ -> false))
    walk thm.Proof
let isGcdAx e = match e with DefGcd _ -> true | _ -> false
let isDivModAx e = match e with DefDivMod _ -> true | _ -> false

let gv, dv' = IntVar "g", IntVar "d"
let exists_stmt = qex gv (zero <=. t gv) ((dvd (t gv) (t b) * dvd (t gv) (t c)) * qall dv' (dvd (t dv') (t b) * dvd (t dv') (t c)) (dvd (t dv') (t gv)))
let (ex, msEx) = timed (fun () -> try Some (gcd_exists th b c) with e -> why "gcd_exists" e |> ignore; None)
states "gcd exists: (∃g | 0 ≤ g : (g ∣ b ∧ g ∣ c) ∧ (∀d | d ∣ b ∧ d ∣ c : d ∣ g))" exists_stmt (fun () -> ex.Value)
printfn "     %d ms" msEx
ok "... and no step of its proof cites the definition of gcd"      (not (cites isGcdAx ex.Value))
ok "... though it does cite (15.90), the definition of ÷ and mod"  (cites isDivModAx ex.Value)
let xv, uv, vv = IntVar "x", IntVar "u", IntVar "v"
let NZ = !!((t b == zero) * (t c == zero))
states "Bézout's lemma: ¬(b = 0 ∧ c = 0) ⇒ (∃x | 0 < x ∧ (x ∣ b ∧ x ∣ c) : (∃u |: (∃v |: x = u·b + v·c)))"
    (NZ ==> qex xv ((zero <. t xv) * (dvd (t xv) (t b) * dvd (t xv) (t c))) (qex uv T (qex vv T (t xv == t uv * t b + t vv * t c))))
    (fun () -> bezout_least th b c)
let bez = qex uv T (qex vv T (G == t uv * t b + t vv * t c))
let (bz, msBz) = timed (fun () -> try Some (gcd_bezout th b c) with e -> why "gcd_bezout" e |> ignore; None)
states "Bézout's identity: (∃u |: (∃v |: b gcd c = u·b + v·c))" bez (fun () -> bz.Value)
ok "... which does cite the definition of gcd (the walk sees it)"   (cites isGcdAx bz.Value)

let a', e' = IntVar "a", IntVar "e"
let b_, c_, d_ = t b, t c, t d
states "0 ≤ b gcd c"                                     (zero <=. G)                                   (fun () -> gcd_nonneg th b c)
states "b gcd c ∣ b"                                     (dvd G b_)                                     (fun () -> gcd_dvd_l th b c)
states "b gcd c ∣ c"                                     (dvd G c_)                                     (fun () -> gcd_dvd_r th b c)
states "e ∣ b ∧ e ∣ c ⇒ e ∣ b gcd c"                    ((dvd (t e') b_ * dvd (t e') c_) ==> dvd (t e') G) (fun () -> dvd_gcd th e' b c)
states "(0 ≤ e ∧ (e ∣ b ∧ e ∣ c)) ∧ b gcd c ∣ e ⇒ b gcd c = e" ((((zero <=. t e') * (dvd (t e') b_ * dvd (t e') c_)) * dvd G (t e')) ==> (G == t e')) (fun () -> gcd_eq th b c e')
states "0 < c ∧ d ∣ c ⇒ d ≤ c"                           (((zero <. c_) * dvd d_ c_) ==> (d_ <=. c_))   (fun () -> dvd_le th c d)
states "b = 0 ∧ b ∣ c ⇒ c = 0"                           (((b_ == zero) * dvd b_ c_) ==> (c_ == zero))  (fun () -> zero_dvd th b c)
states "(0 ≤ b ∧ 0 ≤ c) ∧ (b ∣ c ∧ c ∣ b) ⇒ b = c"      ((((zero <=. b_) * (zero <=. c_)) * (dvd b_ c_ * dvd c_ b_)) ==> (b_ == c_)) (fun () -> dvd_antisym_nonneg th b c)
states "(d ∣ −b) = (d ∣ b)"                              (dvd d_ (-b_) == dvd d_ b_)                    (fun () -> dvd_neg_eq th d b)
states "abs.b ∣ b"                                       (dvd (abs_of b_) b_)                           (fun () -> abs_dvd th b)
states "(d ∣ abs.b) = (d ∣ b)"                           (dvd d_ (abs_of b_) == dvd d_ b_)              (fun () -> dvd_abs_eq th d b)
states "¬(b = 0 ∧ c = 0) ⇒ 0 < b gcd c"                  (NZ ==> (zero <. G))                           (fun () -> gcd_pos th b c)
states "(15.92) ¬(b = 0 ∧ c = 0) ∧ (d ∣ b ∧ d ∣ c) ⇒ d ≤ b gcd c — the greatest" ((NZ * (dvd d_ b_ * dvd d_ c_)) ==> (d_ <=. G)) (fun () -> gcd_greatest th d b c)
states "(15.92) 0 gcd 0 = 0"                             (gc zero zero == zero)                         (fun () -> gcd_zero_zero th)
states "(15.96) b gcd c = c gcd b"                       (G == gc c_ b_)                                (fun () -> gcd_sym th b c)
states "(15.97) (b gcd c) gcd d = b gcd (c gcd d)"       (gc (gc b_ c_) d_ == gc b_ (gc c_ d_))         (fun () -> gcd_assoc th b c d)
states "(15.98) b gcd b = abs.b"                         (gc b_ b_ == abs_of b_)                        (fun () -> gcd_self th b)
states "(15.99) 1 gcd b = 1"                             (gc one b_ == one)                             (fun () -> gcd_one th b)
states "(15.100) 0 gcd b = abs.b"                        (gc zero b_ == abs_of b_)                      (fun () -> gcd_zero th b)
states "(15.101) b gcd c = (abs.b) gcd (abs.c)"          (G == gc (abs_of b_) (abs_of c_))              (fun () -> gcd_abs th b c)
states "(15.102) b gcd c = b gcd (b + c)"                (G == gc b_ (b_ + c_))                         (fun () -> gcd_add th b c)
states "(15.102) b gcd c = b gcd (b − c)"                (G == gc b_ (b_ - c_))                         (fun () -> gcd_sub th b c)
states "(15.103) b = a·c + d ⇒ b gcd c = c gcd d"        ((b_ == t a' * c_ + d_) ==> (G == gc c_ d_))   (fun () -> gcd_lin th a' b c d)
let (_, ms104) = timed (fun () -> gcd_mul th d b c)
states "(15.104) 0 ≤ d ⇒ d·(b gcd c) = (d·b) gcd (d·c)"  ((zero <=. d_) ==> (d_ * G == gc (d_ * b_) (d_ * c_))) (fun () -> gcd_mul th d b c)
printfn "     (15.104) first use %d ms (Bézout inside); again: instantiation" ms104
states "(15.106) 0 < c ⇒ b gcd c = c gcd (b mod c)"      ((zero <. c_) ==> (G == gc c_ (D.Term (mod_of b c)))) (fun () -> gcd_mod th b c)
states "(15.107) b gcd 0 = abs.b"                        (gc b_ zero == abs_of b_)                      (fun () -> gcd_zero_right th b)

// Euclid's algorithm, run as a proof: 30 gcd 24 = 24 gcd 6 = 6 gcd 0 = abs.6 = 6.
let qr_lit (x: int) (y: int) (q: int) (r: int) =
    conj (lit_lt 0 y) (conj (conj (Integers.ring_ident th (L x) (L q * L y + L r)) (lit_le 0 r)) (lit_lt r y))
let euclid () =
    let step1 = mp (gcd_mod th (L 30) (L 24)) (lit_lt 0 24)                                          // 30 gcd 24 = 24 gcd (30 mod 24)
    let mod1 = mp (mod_eq th (L 30) (L 24) (L 1) (L 6)) (qr_lit 30 24 1 6)                          // 30 mod 24 = 6
    let step2 = mp (gcd_mod th (L 24) (L 6)) (lit_lt 0 6)                                            // 24 gcd 6 = 6 gcd (24 mod 6)
    let mod2 = mp (mod_eq th (L 24) (L 6) (L 4) (L 0)) (qr_lit 24 6 4 0)                            // 24 mod 6 = 0
    let abs6 = mp (OrderedDomain.abs_of_nonneg D (L 6)) (lit_le 0 6)                                 // abs.6 = 6
    lemma th (gc (L 30) (L 24) == L 6) [
        Ident step1                                     |> at_left
        Ident mod1                                      |> at [left_branch; right_branch]
        Ident step2                                     |> at_left
        Ident mod2                                      |> at [left_branch; right_branch]
        Ident (gcd_zero_right th (L 6))                 |> at_left
        Ident abs6                                      |> at_left
    ]
states "30 gcd 24 = 6, by Euclid's algorithm ((15.106), (15.107), and (15.90) at literals)" (gc (L 30) (L 24) == L 6) euclid

// Arguments that contain one another, and that clash with the library's dummies (x, u, v, g, d).
states "Bézout at b, c := b, b·b"                        (qex uv T (qex vv T (gc b_ bb == t uv * b_ + t vv * bb))) (fun () -> gcd_bezout th b bb)
states "gcd exists at b, c := x + 1, u (the core's dummy names)" (qex gv (zero <=. t gv) ((dvd (t gv) (t xv + one) * dvd (t gv) (t uv)) * qall dv' (dvd (t dv') (t xv + one) * dvd (t dv') (t uv)) (dvd (t dv') (t gv))))
                                                         (fun () -> gcd_exists th (t xv + one) uv)
states "e ∣ b ∧ e ∣ c ⇒ e ∣ b gcd c at e := b gcd c"     ((dvd G b_ * dvd G c_) ==> dvd G G)            (fun () -> dvd_gcd th G b c)
states "e ∣ b ∧ e ∣ c ⇒ e ∣ b gcd c at e := d (the definition's dummy)" ((dvd d_ b_ * dvd d_ c_) ==> dvd d_ G) (fun () -> dvd_gcd th d b c)
states "(15.103) at a, b, c, d := c, b, c, b"            ((b_ == c_ * c_ + b_) ==> (G == gc c_ b_))     (fun () -> gcd_lin th c b c b)

printfn "\n%s" (if failures = 0 then "All divisibility checks passed." else sprintf "%d CHECK(S) FAILED." failures)
