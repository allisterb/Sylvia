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
//   (L) lcm: its definition (gcd's dual, justified the same way), (15.94) as theorems, gcd·lcm = abs(b·c),
//       12 lcm 18 = 36; relatively prime (15.105), (15.111), (15.113); primes: the definition, ¬prime 4,
//       Euclid's lemma (15.112), a prime divisor of every n > 1, and infinitely many primes (15.116).
//   (M) congruences: the definition (15.118) and that ≡ is an equivalence, (15.119)–(15.127) — powers by
//       induction, cancellation, solvability of d·x ≡ b by Bézout — and Gries' examples: the 12-hour
//       clock, 4·2 ≡ 1·2 but not 4 ≡ 1 (mod 6), and 3·x ≡ 1 (mod 7).
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
states "(15.108) Bézout's identity: (∃u |: (∃v |: b gcd c = u·b + v·c))" bez (fun () -> bz.Value)
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

printfn "\n===== (L) lcm (15.94), relatively prime (15.105), primes (15.111)–(15.116) ====="

// --- The definition of lcm: gcd's dual, and an axiom justified the same way.
let Lc = D.Term (lcm_of b c)
let lc (x: Term<int>) (y: Term<int>) = D.Term (lcm_of x y)
let mv, lv = IntVar "m", IntVar "l"
let m_ = t mv
/// The definition of lcm at x, y with the dummy dv, as a user would write it.
let lcm_def (dv: IntVar) (x: Term<int>) (y: Term<int>) =
    let l, z = lc x y, t dv
    ((zero <=. l) * (dvd x l * dvd y l)) * qall dv (dvd x z * dvd y z) (dvd l z)
ok "(0 ≤ b lcm c ∧ (b ∣ b lcm c ∧ c ∣ b lcm c)) ∧ (∀m | b ∣ m ∧ c ∣ m : b lcm c ∣ m) is an axiom" (th.IsAxiom (lcm_def mv b c).Expr)
printfn "     it prints as: %s" (th.PrintFormula (lcm_def mv b c).Expr)
ok "... with any dummy fresh for b and c"                          (th.IsAxiom (lcm_def k b c).Expr)
ok "it is NOT an axiom of plain Induction"                         (not ((induction :> Theory).IsAxiom (lcm_def mv b c).Expr))
ok "refused: the dummy occurs in c"                                 (not (th.IsAxiom (lcm_def c b c).Expr))
ok "refused: m ∣ b lcm c in the body (the divisibility turned round)" (not (th.IsAxiom (((zero <=. Lc) * (dvd b_ Lc * dvd c_ Lc)) * qall mv (dvd b_ m_ * dvd c_ m_) (dvd m_ Lc)).Expr))
ok "refused: without 0 ≤ b lcm c (then −(b lcm c) qualifies too)"   (not (th.IsAxiom ((T * (dvd b_ Lc * dvd c_ Lc)) * qall mv (dvd b_ m_ * dvd c_ m_) (dvd Lc m_)).Expr))

let isLcmAx e = match e with DefLcm _ -> true | _ -> false
let lexists_stmt (x: Term<int>) (y: Term<int>) (dm: IntVar) =
    qex lv (zero <=. t lv) ((dvd x (t lv) * dvd y (t lv)) * qall dm (dvd x (t dm) * dvd y (t dm)) (dvd (t lv) (t dm)))
let (lex, msLex) = timed (fun () -> try Some (lcm_exists th b c) with e -> why "lcm_exists" e |> ignore; None)
states "lcm exists: (∃l | 0 ≤ l : (b ∣ l ∧ c ∣ l) ∧ (∀m | b ∣ m ∧ c ∣ m : l ∣ m))" (lexists_stmt b_ c_ mv) (fun () -> lex.Value)
printfn "     %d ms" msLex
ok "... and no step of its proof cites the definition of lcm"      (not (cites isLcmAx lex.Value))
ok "... nor that of gcd"                                            (not (cites isGcdAx lex.Value))
ok "... though it does cite (15.90), the definition of ÷ and mod"  (cites isDivModAx lex.Value)

// --- lcm's laws: Gries' (15.94) as theorems, and the product with gcd.
let NZo = !!((b_ == zero) + (c_ == zero))
let e_ = t e'
states "0 ≤ b lcm c"                                     (zero <=. Lc)                                  (fun () -> lcm_nonneg th b c)
states "b ∣ b lcm c"                                     (dvd b_ Lc)                                    (fun () -> dvd_lcm_l th b c)
states "c ∣ b lcm c"                                     (dvd c_ Lc)                                    (fun () -> dvd_lcm_r th b c)
states "b ∣ e ∧ c ∣ e ⇒ b lcm c ∣ e"                    ((dvd b_ e_ * dvd c_ e_) ==> dvd Lc e_)        (fun () -> lcm_dvd th e' b c)
states "(0 ≤ e ∧ (b ∣ e ∧ c ∣ e)) ∧ e ∣ b lcm c ⇒ b lcm c = e" ((((zero <=. e_) * (dvd b_ e_ * dvd c_ e_)) * dvd e_ Lc) ==> (Lc == e_)) (fun () -> lcm_eq th b c e')
states "(15.94) ¬(b = 0 ∨ c = 0) ⇒ 0 < b lcm c"          (NZo ==> (zero <. Lc))                         (fun () -> lcm_pos th b c)
states "(15.94) 0 < k ∧ (b ∣ k ∧ c ∣ k) ⇒ b lcm c ≤ k — the least" (((zero <. t k) * (dvd b_ (t k) * dvd c_ (t k))) ==> (Lc <=. t k)) (fun () -> lcm_least th k b c)
states "(15.94) b = 0 ∨ c = 0 ⇒ b lcm c = 0"             (((b_ == zero) + (c_ == zero)) ==> (Lc == zero)) (fun () -> lcm_zero th b c)
states "b lcm c = c lcm b"                               (Lc == lc c_ b_)                               (fun () -> lcm_sym th b c)
let (_, msGL) = timed (fun () -> gcd_lcm th b c)
states "(b gcd c)·(b lcm c) = abs(b·c) — Gries' b·c = (b gcd c)·(b lcm c), at every integer" ((G * Lc) == abs_of (b_ * c_)) (fun () -> gcd_lcm th b c)
printfn "     first use %d ms" msGL

// Gries' examples (p. 317): 3 lcm 9 = 9, directly; 12 lcm 18 = 36 from 12 gcd 18 = 6 and the product.
let lcm_3_9 () =
    let d39 = dvd_intro th (L 3) (L 9) (L 3) (Integers.ring_ident th (L 3 * L 3) (L 9))
    mp (lcm_eq th (L 3) (L 9) (L 9)) (conj (conj (lit_le 0 9) (conj d39 (dvd_refl th (L 9)))) (dvd_lcm_r th (L 3) (L 9)))
states "3 lcm 9 = 9"                                     (lc (L 3) (L 9) == L 9)                        lcm_3_9
let gcd_12_18 () =
    let s1 = mp (gcd_mod th (L 12) (L 18)) (lit_lt 0 18)                                             // 12 gcd 18 = 18 gcd (12 mod 18)
    let m1 = mp (mod_eq th (L 12) (L 18) (L 0) (L 12)) (qr_lit 12 18 0 12)
    let s2 = mp (gcd_mod th (L 18) (L 12)) (lit_lt 0 12)
    let m2 = mp (mod_eq th (L 18) (L 12) (L 1) (L 6)) (qr_lit 18 12 1 6)
    let s3 = mp (gcd_mod th (L 12) (L 6)) (lit_lt 0 6)
    let m3 = mp (mod_eq th (L 12) (L 6) (L 2) (L 0)) (qr_lit 12 6 2 0)
    let abs6 = mp (OrderedDomain.abs_of_nonneg D (L 6)) (lit_le 0 6)
    lemma th (gc (L 12) (L 18) == L 6) [
        Ident s1 |> at_left; Ident m1 |> at [left_branch; right_branch]
        Ident s2 |> at_left; Ident m2 |> at [left_branch; right_branch]
        Ident s3 |> at_left; Ident m3 |> at [left_branch; right_branch]
        Ident (gcd_zero_right th (L 6)) |> at_left; Ident abs6 |> at_left ]
let lcm_12_18 () =
    let L1218 = lc (L 12) (L 18)
    let abs216 = mp (OrderedDomain.abs_of_nonneg D (L 216)) (lit_le 0 216)
    let six = lemma th ((L 6 * L1218) == (L 6 * L 36)) [
                Ident (mp (OrderedDomain.eq_sym th (gc (L 12) (L 18)) (L 6)) (gcd_12_18 ())) |> at [left_branch; left_branch]   // 6 → 12 gcd 18
                Ident (gcd_lcm th (L 12) (L 18))                |> at_left                             // → abs(12·18)
                Integers.ring th (L 12 * L 18) (L 216)          |> at [left_branch; apply_unary]
                Ident abs216                                    |> at_left
                Integers.ring th (L 6 * L 36) (L 216)           |> at_right ]
    let nz6 = lemma th (!!(L 6 == zero)) [ ax_ident th ((L 6 == zero) == F) |> at [apply_unary]; not_false |> apply ]
    let cancel = mp (axiom th (!!(L 6 == zero) ==> (((L 6 * L1218) == (L 6 * L 36)) == (L1218 == L 36)))) nz6   // (15.7)
    lemma th (L1218 == L 36) [ Ident cancel |> Commute |> apply; Taut six |> apply ]
states "12 gcd 18 = 6, by Euclid's algorithm"            (gc (L 12) (L 18) == L 6)                      gcd_12_18
states "12 lcm 18 = 36: 6·(12 lcm 18) = abs(12·18) = 216, and Cancellation (15.7)" (lc (L 12) (L 18) == L 36) lcm_12_18

// --- Relatively prime: notation for b gcd c = 1, and what Bézout gives.
ok "(15.105) b ⊥ c is b gcd c = 1 — notation, no new symbol or axiom" (sequal (rel_prime b c).Expr.Raw (G == one).Expr.Raw)
states "(15.111) k ∣ b ∧ k ∣ c ≡ k ∣ (b gcd c)"           ((dvd (t k) b_ * dvd (t k) c_) == dvd (t k) G) (fun () -> dvd_gcd_iff th k b c)
let (_, ms113) = timed (fun () -> coprime_dvd th b c d)
states "(15.113) b ⊥ c ∧ c ∣ (b·d) ⇒ c ∣ d"             ((rel_prime b c * dvd c_ (b_ * d_)) ==> dvd c_ d_) (fun () -> coprime_dvd th b c d)
printfn "     first use %d ms (Bézout inside)" ms113

// --- Primes: an explicit definition.
let pv = IntVar "p"
let p_ = t pv
let prime_stmt (dv: IntVar) (x: DomainTerm<int>) (lo: Prop) (rng: Prop) =
    prime x == (lo * qall dv rng ((t dv == one) + (t dv == x)))
let prime_def_stmt (dv: IntVar) (x: DomainTerm<int>) = prime_stmt dv x (one <. x) ((zero <. t dv) * dvd (t dv) x)
ok "prime p = 1 < p ∧ (∀d | 0 < d ∧ d ∣ p : d = 1 ∨ d = p) is an axiom" (th.IsAxiom (prime_def_stmt d p_).Expr)
printfn "     it prints as: %s" (th.PrintFormula (prime_def_stmt d p_).Expr)
ok "... with any dummy fresh for p"                                 (th.IsAxiom (prime_def_stmt k p_).Expr)
ok "it is NOT an axiom of plain Induction"                         (not ((induction :> Theory).IsAxiom (prime_def_stmt d p_).Expr))
ok "refused: the dummy occurs in p"                                 (not (th.IsAxiom (prime_def_stmt d d_).Expr))
ok "refused: 0 ≤ d in the range (then d = 0 would have to be 1 or p)" (not (th.IsAxiom (prime_stmt d p_ (one <. p_) ((zero <=. d_) * dvd d_ p_)).Expr))
ok "refused: 0 < p for 1 < p (then 1 would be prime)"              (not (th.IsAxiom (prime_stmt d p_ (zero <. p_) ((zero <. d_) * dvd d_ p_)).Expr))

states "prime p ⇒ 1 < p"                                 (prime p_ ==> (one <. p_))                     (fun () -> prime_gt_one th p_)
states "prime p ∧ (0 < d ∧ d ∣ p) ⇒ d = 1 ∨ d = p"      ((prime p_ * ((zero <. d_) * dvd d_ p_)) ==> ((d_ == one) + (d_ == p_))) (fun () -> prime_dvd th p_ d)
let not_prime_4 () =
    let d24 = dvd_intro th (L 2) (L 4) (L 2) (Integers.ring_ident th (L 2 * L 2) (L 4))
    let under (h: Prop) (x: Theorem) = lemma th (h ==> Prop(Expr.Cast<bool> x.Stmt)) [ Taut x |> apply_right ]   // h ⇒ φ, from ⊢ φ
    let to_lits = lemma th (prime (L 4) ==> ((L 2 == one) + (L 2 == L 4))) [
                    Establish (under (prime (L 4)) (lit_lt 0 2))       |> apply
                    Establish (under (prime (L 4)) d24)                 |> apply
                    Deduce (prime_dvd th (L 4) (L 2))                   |> apply_right ]
    lemma th (!!(prime (L 4))) [
        ident_implies_false_not (prime (L 4)) |> Commute               |> apply                       // ¬prime 4 → (prime 4 ⇒ F)
        idemp_or F |> Commute                                          |> at_right                    // F → F ∨ F
        ax_ident th ((L 2 == one) == F) |> Commute                     |> at [right_branch; left_branch]
        ax_ident th ((L 2 == L 4) == F) |> Commute                     |> at [right_branch; right_branch]
        Taut to_lits                                                   |> apply ]
states "¬(prime 4) — 2 is a positive divisor of 4 that is neither 1 nor 4" (!!(prime (L 4)))             not_prime_4
states "prime p ∧ ¬(p ∣ b) ⇒ b ⊥ p"                      ((prime p_ * !!(dvd p_ b_)) ==> rel_prime b p_) (fun () -> prime_coprime th p_ b)
states "(15.112) Euclid's lemma: prime p ∧ p ∣ b·c ⇒ p ∣ b ∨ p ∣ c" ((prime p_ * dvd p_ (b_ * c_)) ==> (dvd p_ b_ + dvd p_ c_)) (fun () -> euclid_lemma th p_ b c)
let nv = IntVar "n"
let n_ = t nv
states "a prime divisor: 1 < n ⇒ (∃p | prime p : p ∣ n)" ((one <. n_) ==> qex pv (prime p_) (dvd p_ n_))  (fun () -> prime_divisor th n_)
let (inf, msInf) = timed (fun () -> try Some (primes_unbounded th n_) with e -> why "primes_unbounded" e |> ignore; None)
states "(15.116) infinitely many primes: 0 ≤ n ⇒ (∃p | prime p : n < p)" ((zero <=. n_) ==> qex pv (prime p_) (n_ <. p_)) (fun () -> inf.Value)
printfn "     %d ms (Euclid, with n! + 1)" msInf
let isInduction e = match e with NatInduction _ -> true | _ -> false
ok "... and the walk finds Mathematical Induction (12.8) in its proof (n! by induction)" (cites isInduction inf.Value)

// Arguments that contain one another, and that clash with the library's dummies (l, m, x, u, v, p, q, w).
let m1v, p1v = IntVar "m#1", IntVar "p#1"
states "lcm exists at b, c := x + 1, m"                  (lexists_stmt (t xv + one) m_ m1v)             (fun () -> lcm_exists th (t xv + one) mv)
states "b ∣ e ∧ c ∣ e ⇒ b lcm c ∣ e at e := m (the definition's dummy)" ((dvd b_ m_ * dvd c_ m_) ==> dvd Lc m_) (fun () -> lcm_dvd th mv b c)
states "(b gcd c)·(b lcm c) = abs(b·c) at b, c := b, b·c" ((gc b_ (b_ * c_) * lc b_ (b_ * c_)) == abs_of (b_ * (b_ * c_))) (fun () -> gcd_lcm th b (b_ * c_))
states "(15.113) at b, c, d := c, b, b·c"                ((rel_prime c b * dvd b_ (c_ * (b_ * c_))) ==> dvd b_ (b_ * c_)) (fun () -> coprime_dvd th c b (b_ * c_))
states "(15.112) at b, c := p, p"                        ((prime p_ * dvd p_ (p_ * p_)) ==> (dvd p_ p_ + dvd p_ p_)) (fun () -> euclid_lemma th p_ p_ p_)
states "(15.116) at n := p (the conclusion's own dummy)" ((zero <=. p_) ==> qex p1v (prime (t p1v)) (p_ <. t p1v)) (fun () -> primes_unbounded th p_)

printfn "\n===== (M) Congruences (15.118)–(15.127) ====="

// --- The definition: an explicit one, like prime.
let cg (x: Term<int>) (y: Term<int>) (mm: Term<int>) = cong x y mm
let cong_def_stmt (x: DomainTerm<int>) (y: DomainTerm<int>) (mm: DomainTerm<int>) = cg x y mm == dvd mm (y - x)
ok "(15.118) b ≡ c (mod m) = m ∣ (c − b) is an axiom"              (th.IsAxiom (cong_def_stmt b_ c_ m_).Expr)
printfn "     it prints as: %s" (th.PrintFormula (cong_def_stmt b_ c_ m_).Expr)
ok "≡ prints as Gries' text reads it: b ≡ c (mod m)"              (th.PrintFormula (cg b_ c_ m_).Expr = "b ≡ c (mod m)")
ok "... at any terms: b·b ≡ c + 1 (mod m − 1) = (m − 1) ∣ ((c + 1) − b·b)" (th.IsAxiom (cong_def_stmt bb (c_ + one) (m_ - one)).Expr)
ok "it is NOT an axiom of plain Induction"                         (not ((induction :> Theory).IsAxiom (cong_def_stmt b_ c_ m_).Expr))
ok "refused: m ∣ (b − c), the difference turned round"             (not (th.IsAxiom (cg b_ c_ m_ == dvd m_ (b_ - c_)).Expr))
ok "refused: c ∣ (m − b), the modulus in the wrong place"           (not (th.IsAxiom (cg b_ c_ m_ == dvd c_ (m_ - b_)).Expr))

// --- The theorems.
states "(15.118) reflexive: b ≡ b (mod m)"               (cg b_ b_ m_)                                  (fun () -> cong_refl th b mv)
states "(15.118) symmetric: b ≡ c (mod m) = c ≡ b (mod m)" (cg b_ c_ m_ == cg c_ b_ m_)                 (fun () -> cong_sym th b c mv)
states "(15.118) transitive: b ≡ c ∧ c ≡ e ⇒ b ≡ e (mod m)" ((cg b_ c_ m_ * cg c_ e_ m_) ==> cg b_ e_ m_) (fun () -> cong_trans th b c e' mv)
let (_, ms119) = timed (fun () -> cong_mod_eq th b c mv)
states "(15.119) 0 < m ⇒ (b ≡ c (mod m) = (b mod m = c mod m))" ((zero <. m_) ==> (cg b_ c_ m_ == (D.Term (mod_of b mv) == D.Term (mod_of c mv)))) (fun () -> cong_mod_eq th b c mv)
printfn "     first use %d ms (uniqueness of the remainder inside)" ms119
states "(15.120) b ≡ c = b + d ≡ c + d (mod m)"          (cg b_ c_ m_ == cg (b_ + d_) (c_ + d_) m_)    (fun () -> cong_add th b c d mv)
states "(15.121) b ≡ c = −b ≡ −c (mod m)"                (cg b_ c_ m_ == cg (-b_) (-c_) m_)            (fun () -> cong_neg th b c mv)
states "(15.122) b ≡ c ⇒ b·d ≡ c·d (mod m)"              (cg b_ c_ m_ ==> cg (b_ * d_) (c_ * d_) m_)   (fun () -> cong_mul th b c d mv)
let pw (x: Term<int>) (y: Term<int>) = D.Term (power x y)
printfn "     bⁿ is notation: %s" (th.PrintFormula (expand (pw b_ n_).Expr))
let (pow123, ms123) = timed (fun () -> try Some (cong_pow th b c n_ mv) with e -> why "cong_pow" e |> ignore; None)
states "(15.123) 0 ≤ n ⇒ (b ≡ c ⇒ bⁿ ≡ cⁿ (mod m))"     ((zero <=. n_) ==> (cg b_ c_ m_ ==> cg (pw b_ n_) (pw c_ n_) m_)) (fun () -> pow123.Value)
printfn "     %d ms" ms123
ok "... and the walk finds Mathematical Induction (12.8) in its proof" (cites isInduction pow123.Value)
let (_, ms124) = timed (fun () -> cong_cancel th b c d mv)
states "(15.124) d ⊥ m ⇒ (b·d ≡ c·d = b ≡ c (mod m))"    (rel_prime d m_ ==> (cg (b_ * d_) (c_ * d_) m_ == cg b_ c_ m_)) (fun () -> cong_cancel th b c d mv)
printfn "     first use %d ms" ms124
states "(15.125) 0 < d ⇒ (b·d ≡ c·d (mod m·d) = b ≡ c (mod m))" ((zero <. d_) ==> (cg (b_ * d_) (c_ * d_) (m_ * d_) == cg b_ c_ m_)) (fun () -> cong_cancel_mod th b c d mv)
let (sol126, ms126) = timed (fun () -> try Some (cong_solvable th d b mv) with e -> why "cong_solvable" e |> ignore; None)
states "(15.126) d ⊥ m ⇒ (∃x |: d·x ≡ b (mod m))"        (rel_prime d m_ ==> qex xv T (cg (d_ * t xv) b_ m_)) (fun () -> sol126.Value)
printfn "     %d ms" ms126
ok "... by Bézout: the walk finds the definition of gcd in its proof" (cites isGcdAx sol126.Value)
let yv = IntVar "y"
states "(15.127) d ⊥ m ⇒ (d·x ≡ b ∧ d·y ≡ b ⇒ x ≡ y (mod m))" (rel_prime d m_ ==> ((cg (d_ * t xv) b_ m_ * cg (d_ * t yv) b_ m_) ==> cg (t xv) (t yv) m_)) (fun () -> cong_unique th d xv yv b mv)

// --- Gries' examples. Hours on a 12-hour clock (p. 325): 14 ≡ 2 and 38 ≡ 2 (mod 12).
/// `x ≡ y (mod mm)`, from `mm·w = y − x` at literals.
let cong_lit (x: DomainTerm<int>) (y: DomainTerm<int>) (mm: DomainTerm<int>) (w: int) =
    lemma th (cg x y mm) [
        cong_def th x y mm                                          |> apply
        Taut (dvd_intro th mm (y - x) (L w) (Integers.ring_ident th (mm * L w) (y - x))) |> apply ]
states "14 ≡ 2 (mod 12)"                                 (cg (L 14) (L 2) (L 12))                       (fun () -> cong_lit (L 14) (L 2) (L 12) -1)
states "38 ≡ 2 (mod 12)"                                 (cg (L 38) (L 2) (L 12))                       (fun () -> cong_lit (L 38) (L 2) (L 12) -3)
let mod_14_2 () =
    let iff = mp (cong_mod_eq th (L 14) (L 2) (L 12)) (lit_lt 0 12)                                       // (15.119) at m := 12
    lemma th (D.Term (mod_of (L 14) (L 12)) == D.Term (mod_of (L 2) (L 12))) [
        Ident iff |> Commute                                        |> apply
        Taut (cong_lit (L 14) (L 2) (L 12) -1)                      |> apply ]
states "(15.119) at 14, 2, 12: 14 mod 12 = 2 mod 12"     (D.Term (mod_of (L 14) (L 12)) == D.Term (mod_of (L 2) (L 12))) mod_14_2
// Why (15.122) is not an equality (p. 326): 4·2 ≡ 1·2 (mod 6) but not 4 ≡ 1 (mod 6) — 2 is a factor of 6.
states "4·2 ≡ 1·2 (mod 6)"                               (cg (L 4 * L 2) (L 1 * L 2) (L 6))             (fun () -> cong_lit (L 4 * L 2) (L 1 * L 2) (L 6) -1)
let not_4_1 () =
    let X = cg (L 4) (L 1) (L 6)
    let under (h: Prop) (x: Theorem) = lemma th (h ==> Prop(Expr.Cast<bool> x.Stmt)) [ Taut x |> apply_right ]
    lemma th (!!X) [
        ident_implies_false_not X |> Commute                       |> apply                       // ¬X → (X ⇒ F)
        cong_def th (L 4) (L 1) (L 6)                              |> at_left                     // → 6 ∣ (1 − 4)
        Integers.ring th (L 1 - L 4) (-(L 3))                      |> at [left_branch; right_branch]
        Ident (dvd_neg_eq th (L 6) (L 3))                          |> at_left                     // 6 ∣ −3 → 6 ∣ 3
        ax_ident th ((L 6 <=. L 3) == F) |> Commute                |> at_right                    // F → 6 ≤ 3, literal evaluation
        Establish (under (dvd (L 6) (L 3)) (lit_lt 0 3))           |> apply
        Deduce (dvd_le th (L 3) (L 6))                             |> apply_right ]               // 0 < 3 ∧ 6 ∣ 3 ⇒ 6 ≤ 3
states "¬(4 ≡ 1 (mod 6))"                                (!!(cg (L 4) (L 1) (L 6)))                     not_4_1
// (15.126) at d, b, m := 3, 1, 7, from 3 gcd 7 = 1 by Euclid's algorithm — and a solution, x = 5.
let gcd_3_7 () =
    let s1 = mp (gcd_mod th (L 3) (L 7)) (lit_lt 0 7)                                                  // 3 gcd 7 = 7 gcd (3 mod 7)
    let m1 = mp (mod_eq th (L 3) (L 7) (L 0) (L 3)) (qr_lit 3 7 0 3)
    let s2 = mp (gcd_mod th (L 7) (L 3)) (lit_lt 0 3)
    let m2 = mp (mod_eq th (L 7) (L 3) (L 2) (L 1)) (qr_lit 7 3 2 1)
    let s3 = mp (gcd_mod th (L 3) (L 1)) (lit_lt 0 1)
    let m3 = mp (mod_eq th (L 3) (L 1) (L 3) (L 0)) (qr_lit 3 1 3 0)
    let abs1 = mp (OrderedDomain.abs_of_nonneg D one) (lit_le 0 1)
    lemma th (gc (L 3) (L 7) == one) [
        Ident s1 |> at_left; Ident m1 |> at [left_branch; right_branch]
        Ident s2 |> at_left; Ident m2 |> at [left_branch; right_branch]
        Ident s3 |> at_left; Ident m3 |> at [left_branch; right_branch]
        Ident (gcd_zero_right th one) |> at_left; Ident abs1 |> at_left ]
states "3 ⊥ 7, by Euclid's algorithm"                    (rel_prime (L 3) (L 7))                        gcd_3_7
states "(15.126) at 3, 1, 7: (∃x |: 3·x ≡ 1 (mod 7))"    (qex xv T (cg (L 3 * t xv) one (L 7)))         (fun () -> mp (cong_solvable th (L 3) one (L 7)) (gcd_3_7 ()))
states "... and x = 5 is one: 3·5 ≡ 1 (mod 7)"           (cg (L 3 * L 5) one (L 7))                     (fun () -> cong_lit (L 3 * L 5) one (L 7) -2)

// Arguments that contain one another, and that clash with the library's dummies (d, j, k, i, u, v, x).
let jv, j1v = IntVar "j", IntVar "j#1"
let x1v = IntVar "x#1"
let pj (dummy: IntVar) (x: Term<int>) (y: Term<int>) = D.Term (Sums.qprod dummy ((leq 1 (tm dummy)) * (leq (tm dummy) y)) x)
states "transitivity at b, c, e := b, b, b"              ((cg b_ b_ m_ * cg b_ b_ m_) ==> cg b_ b_ m_)  (fun () -> cong_trans th b b b mv)
states "(15.122) at d := b"                              (cg b_ c_ m_ ==> cg (b_ * b_) (c_ * b_) m_)    (fun () -> cong_mul th b c b mv)
states "(15.119) at c := b mod m"                        ((zero <. m_) ==> (cg b_ (D.Term (mod_of b mv)) m_ == (D.Term (mod_of b mv) == D.Term (mod_of (D.Term (mod_of b mv)) mv)))) (fun () -> cong_mod_eq th b (D.Term (mod_of b mv)) mv)
states "(15.123) at b := j (the product's dummy): Π over j#1" ((zero <=. n_) ==> (cg (t jv) c_ m_ ==> cg (pj j1v (t jv) n_) (pj j1v c_ n_) m_)) (fun () -> cong_pow th jv c n_ mv)
states "(15.123) at n, m := n, n"                        ((zero <=. n_) ==> (cg b_ c_ n_ ==> cg (pw b_ n_) (pw c_ n_) n_)) (fun () -> cong_pow th b c n_ nv)
states "(15.125) at m := d"                              ((zero <. d_) ==> (cg (b_ * d_) (c_ * d_) (d_ * d_) == cg b_ c_ d_)) (fun () -> cong_cancel_mod th b c d d)
states "(15.126) at b := x (the conclusion's dummy): ∃x#1" (rel_prime d m_ ==> qex x1v T (cg (d_ * t x1v) (t xv) m_)) (fun () -> cong_solvable th d xv mv)
states "(15.127) at x, y := y, x"                        (rel_prime d m_ ==> ((cg (d_ * t yv) b_ m_ * cg (d_ * t xv) b_ m_) ==> cg (t yv) (t xv) m_)) (fun () -> cong_unique th d yv xv b mv)

printfn "\n%s" (if failures = 0 then "All divisibility checks passed." else sprintf "%d CHECK(S) FAILED." failures)
