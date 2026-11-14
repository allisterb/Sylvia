namespace Sylvia
// A bare rule as a proof step implicitly means "apply to the whole expression" (Rule -> RuleApplication).
#nowarn "3391"

open FSharp.Quotations

open FsExpr
open Formula
open Patterns
open Descriptions

open PropCalculus
open PredCalculus
open DomainTerms
open Induction

/// Divisibility — Gries & Schneider ch.15, §15.4 (pp. 315–316).
///
/// ONE axiom, the definition (15.77) of the relation `∣`:
///
/// ```
/// c ∣ b  ≡  (∃d |: c·d = b)
/// ```
///
/// A definitional extension: `∣` is a fresh symbol, so the axiom says nothing about anything else
/// and cannot make the theory inconsistent. It is a SCHEME over `c`, `b` and the dummy `d`, which
/// must not occur in `c` or `b` (or the quantifier would capture it). Everything else in the section
/// is a theorem: (15.78)–(15.86), Gries' exercise 15.65 `d ∣ b ∧ d ∣ c ⇒ d ∣ (b + c)`, its `−` twin,
/// and Theorem (15.87), the division theorem, proved from Well ordering (15.49).
///
/// ∃-reasoning stays inside the theory. ∃-introduction is (9.28) `exists_intro`; ∃-elimination is
/// ex.9.27 — `((∃x|R:P) ⇒ Q) = (∀x|R: P ⇒ Q)` — followed by rewriting the ∀'s body to `T` with a
/// theorem proved about the dummy itself (generalization, the step `generalize_step` uses) and (9.8).
/// No new rule of inference: Metatheorem Witness (9.30), `PredCalculus.witness`, is not used here,
/// because it accepts only subproofs carried out in the predicate calculus.
///
/// The theory is `Induction` plus (15.77): (15.81) needs "no integer lies between 0 and 1", which is
/// induction, and (15.87) needs Well ordering, which is too.
module Divisibility =

    /// The divisibility relation `c ∣ b`, "c divides b". Uninterpreted: its meaning is (15.77), and it
    /// is never evaluated.
    [<Symbol "∣">]
    let divides (c: int) (b: int) : bool = invalidOp "divides is a symbolic relation; its meaning is the axiom (15.77)."

    let private div_op : Expr<int -> int -> bool> = <@ divides @>
    let private mul_op : Expr<int -> int -> int> = <@ (*) @>

    /// True iff `v` occurs in `e` at all — free or bound.
    let private occurs_at_all (v: Var) (e: Expr) = get_vars e |> List.exists (vequal v)

    (* Axiom *)

    /// **(15.77) Definition of ∣**: `c ∣ b = (∃d |: c·d = b)`, for any dummy `d` occurring in neither
    /// `c` nor `b`.
    let (|DefDivides|_|) =
        function
        | Equals(Binary div_op (c, b), Exists(_, [d], True, Equals(Binary mul_op (c', Var d'), b')))
            when vequal d d' && sequal c c' && sequal b b' && not (occurs_at_all d c) && not (occurs_at_all d b) ->
            Some (axiom_name "Divisibility" "Definition of ∣")
        | _ -> None

    let divisibility_axioms =
        function
        | DefDivides x -> Some x
        | _ -> None

    (* Theory *)

    /// Integer arithmetic, Σ and Π, induction (12.8) — everything `Induction` has — and (15.77). A
    /// caller's own axioms and rules go on top, as with `Induction`.
    type Divisibility(?axioms: Axioms, ?rules: Rules) =
        inherit Induction.Induction(BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None)) divisibility_axioms,
                                    defaultArg rules [])

    /// The theory: `Induction` and (15.77), with nothing extra.
    let divisibility = Divisibility()

    (* Building blocks *)

    /// `c ∣ b`, as a proposition.
    let dvd (c: Term<int>) (b: Term<int>) : Prop = Prop(<@ divides %c.Expr %b.Expr @> |> expand_as<bool>)

    /// An integer dummy named `name`, `name#1`, `name#2`, … — the first whose name occurs nowhere in
    /// `avoid`, free or bound. Stronger than freshness for free variables, which is what (15.77)'s side
    /// condition asks for.
    let fresh_int (name: string) (avoid: Expr list) : IntVar =
        let names = avoid |> List.collect (fun e -> get_vars (expand e)) |> List.map (fun v -> v.Name) |> Set.ofList
        let rec pick i =
            let n = if i = 0 then name else sprintf "%s#%d" name i
            if names.Contains n then pick (i + 1) else IntVar n
        pick 0

    let private it (t: Term<int>) = IntTerm t.Expr

    /// The body `c·k = b` of (15.77) at `k`.
    let private body (c: Term<int>) (b: Term<int>) (k: IntVar) : Prop = (it c * tm k :> Term<int>) == b

    /// `(c ∣ b) = (∃d |: c·d = b)` with the given dummy.
    let private dvd_def_with (th: Theory) (d: IntVar) (c: Term<int>) (b: Term<int>) : Rule =
        ax_ident th (dvd c b == qex d T (body c b d))

    /// **(15.77)** as a rewrite: `c ∣ b → (∃d |: c·d = b)`, with a dummy fresh for `c` and `b`.
    let dvd_def (th: Theory) (c: Term<int>) (b: Term<int>) : Rule = dvd_def_with th (fresh_int "d" [ c.Expr; b.Expr ]) c b

    /// From `⊢ c·k = b`, conclude `⊢ c ∣ b` — ∃-introduction (9.28) at the witness `k`.
    let dvd_intro (th: Theory) (c: Term<int>) (b: Term<int>) (k: Term<int>) (h: Theorem) : Theorem =
        let d = fresh_int "d" [ c.Expr; b.Expr ]
        let P = pred_of d (body c b d)
        let ex = mp (OrderedDomain.in_theory th (exists_intro d P k)) h
        lemma th (dvd c b) [
            dvd_def_with th d c b                                   |> apply
            Taut ex                                                 |> apply
        ]

    /// **∃-elimination for ∣.** From `bodyf k : ⊢ c·k = b ⇒ Q`, proved about the dummy `k` it is
    /// handed (fresh for `c`, `b` and `Q`), conclude `⊢ c ∣ b ⇒ Q`. ex.9.27 turns the ∃ antecedent
    /// into `(∀k |: c·k = b ⇒ Q)`, whose body the theorem rewrites to `T` — generalization — and (9.8)
    /// closes it. Sound for the reason any generalization is: `k` is a dummy, so the theorem holds
    /// at every `k`.
    let dvd_elim (th: Theory) (c: Term<int>) (b: Term<int>) (Q: Prop) (bodyf: IntVar -> Theorem) : Theorem =
        let d = fresh_int "d" [ c.Expr; b.Expr; Q.Expr ]
        let P = pred_of d (body c b d)
        lemma th (dvd c b ==> Q) [
            dvd_def_with th d c b                                   |> at_left
            ident_exists_implies d truepred P Q                     |> apply          // ex.9.27
            Taut (bodyf d)                                          |> at [select_body]
            ident_forall_true' d                                    |> apply          // (9.8)
        ]

    /// **∃-elimination**, for any range: from `⊢ (R.x ∧ P.x) ⇒ Q`, proved about the dummy `x`
    /// itself, conclude `⊢ (∃x | R : P) ⇒ Q`. `x` must not be free in `Q`. As `dvd_elim`: ex.9.27,
    /// Trading (9.2), the theorem as the ∀'s body, and (9.8).
    let exists_elim (th: Theory) (x: IntVar) (R: Pred<int>) (P: Pred<int>) (Q: Prop) (t: Theorem) : Theorem =
        if Patterns.occurs_free [ x.Var ] (expand Q.Expr) then
            failwithf "exists_elim: the dummy %s occurs free in the conclusion." x.Name
        let Rx, Px = R.[tm x], P.[tm x]
        let shunted = lemma th (Rx ==> (Px ==> Q)) [
                          shunt' Rx Px Q |> Commute                 |> apply
                          Taut t                                    |> apply ]
        lemma th (exists(x, R, P) ==> Q) [
            ident_exists_implies x R P Q                            |> apply          // ex.9.27
            trade_forall_implies x R (pred_of x (Px ==> Q))         |> apply          // (9.2)
            Taut shunted                                            |> at [select_body]
            ident_forall_true' x                                    |> apply          // (9.8)
        ]

    (* Propositional glue, and two local devices *)

    /// (a ⇒ g) ⇒ ((b ⇒ g) ⇒ ((a ∨ b) ⇒ g))
    let private glue_or_cases = OrderedDomain.taut3 "dv_or_cases" (fun a b g -> (a ==> g) ==> ((b ==> g) ==> ((a + b) ==> g)))
    /// (z ⇒ ¬h) ⇒ ((h ∧ z) ⇒ r)
    let private glue_absurd = OrderedDomain.taut3 "dv_absurd" (fun h z r -> (z ==> !!h) ==> ((h * z) ==> r))
    /// (n ⇒ (x = y)) ⇒ ((n ∧ x) ⇒ y)
    let private glue_detach : Prop -> Prop -> Prop -> Theorem =
        OrderedDomain.taut3 "dv_detach" (fun (n: Prop) (x: Prop) (y: Prop) -> (n ==> (x == y)) ==> ((n * x) ==> y))

    /// `((A ∨ B) ∧ C) ⇒ R`, from its two cases `C ∧ A ⇒ R` and `C ∧ B ⇒ R`.
    let private by_cases2 (th: Theory) (A: Prop) (B: Prop) (C: Prop) (R: Prop) (cCA: Theorem) (cCB: Theorem) : Theorem =
        lemma th (((A + B) * C) ==> R) [
            commute_and (A + B) C                                   |> at_left
            distrib_and_or C A B                                    |> at_left
            case_analysis_1 (C * A) (C * B) R |> Commute            |> apply          // (3.78)
            Taut cCA                                                |> at_left
            Taut cCB                                                |> at_right
            ident_and T                                             |> apply
        ]

    /// `0 < 1`.
    let zero_lt_one (th: Theory) : Theorem =
        let d = Integers.order_domain th
        lemma th (d.Zero <. d.One) [
            OrderedDomain.pos_lt d d.One |> Commute                 |> apply          // 0 < 1 → pos.1
            Taut (mp (OrderedDomain.one_pos d) (Integers.one_ne_zero th)) |> apply
        ]

    /// `a < b ⇒ a ≤ b` — (15.38).
    let private lt_le (th: Theory) (a: DomainTerm<int>) (b: DomainTerm<int>) : Theorem =
        lemma th ((a <. b) ==> (a <=. b)) [
            ax_ident th ((a <=. b) == ((a <. b) + (a == b)))       |> at_right       // (15.38)
            Deduce (weaken_or (a <. b) (a == b))                    |> apply_right
        ]

    (* Units: the integers whose product is 1 *)

    /// `0 < a ∧ a·k = 1 ⇒ a = 1`. Then `0 < k` too (15.43), so `1 ≤ a` and `1 ≤ k`; and
    /// `a = a·1 ≤ a·k = 1`, so Antisymmetry (15.45).
    let private unit_pos (th: Theory) (a: DomainTerm<int>) (k: DomainTerm<int>) : Theorem =
        let d = Integers.order_domain th
        let zero, one = d.Zero, d.One
        let P1, P2 = (zero <. a), (a * k == one)
        let kpos =
            lemma th ((P1 * P2) ==> (zero <. k)) [
                Deduce' (OrderedDomain.lt_mul d zero k a)           |> apply_right                        // 0 < k → 0·a < k·a
                Integers.ring th (zero * a) zero                    |> at [right_branch; left_branch]
                Integers.ring th (k * a) (a * k)                    |> at [right_branch; right_branch]
                Deduce' (reflex_implies P2)                         |> at [right_branch; right_branch]   // a·k → 1
                Taut (zero_lt_one th)                               |> apply_right
            ]
        let le1 =
            lemma th (((P2 * (zero <=. a)) * (one <=. k)) ==> (a <=. one)) [
                Integers.ring th a (a * one)                        |> at [right_branch; left_branch]
                Deduce' (OrderedDomain.eq_sym th (a * k) one)       |> at [right_branch; right_branch]   // 1 → a·k
                Deduce (OrderedDomain.le_mul_nonneg d a one k)      |> apply_right
            ]
        lemma th ((P1 * P2) ==> (a == one)) [
            Establish kpos                                          |> apply
            Establish (one_le_of_pos th k)                          |> apply
            Establish (lt_le th zero a)                             |> apply
            Establish le1                                           |> apply
            Establish (one_le_of_pos th a)                          |> apply
            Deduce (OrderedDomain.le_antisym d a one)               |> apply_right                       // (15.45)
        ]

    let private unit_cases_at (th: Theory) (x: Term<int>) (y: Term<int>) : Theorem =
        let d = Integers.order_domain th
        let zero, one = d.Zero, d.One
        let x, y = d.Term x, d.Term y
        let H = x * y == one
        let E1, E2 = (x == one), (x == -one)
        let R = E1 + E2
        let A, B, B1, Z = (zero <. x), (x <=. zero), (x <. zero), (x == zero)
        // 0 < x: x = 1.
        let cHA = lemma th ((H * A) ==> R) [
                    Establish (unit_pos th x y)                     |> apply
                    Deduce (weaken_or E1 E2)                        |> apply_right ]
        // x < 0: then 0 < −x and (−x)·(−y) = 1, so −x = 1.
        let neg_pos =
            lemma th (B1 ==> (zero <. -x)) [
                ax_ident th (B1 == pos_of (zero - x))               |> at_left                            // (15.36)
                ax_ident th ((zero <. -x) == pos_of (-x - zero))    |> at_right
                Integers.ring th (zero - x) (-x)                    |> at [left_branch; apply_unary]
                Integers.ring th (-x - zero) (-x)                   |> at [right_branch; apply_unary]
                Deduce (reflex_implies (pos_of (-x)))               |> apply_right ]
        let neg_prod =
            lemma th (H ==> ((-x) * (-y) == one)) [
                Integers.ring th ((-x) * (-y)) (x * y)              |> at [right_branch; left_branch]
                Deduce' (reflex_implies H)                          |> at [right_branch; left_branch]
                IntegralDomain.refl_true th one                     |> apply_right ]
        let NX = (-x) == one
        let from_neg =
            lemma th (NX ==> R) [
                Integers.ring th x (-(-x))                          |> at [right_branch; right_branch; left_branch]
                Deduce' (reflex_implies NX)                         |> at [right_branch; right_branch; left_branch; apply_unary]
                IntegralDomain.refl_true th (-one)                  |> at [right_branch; right_branch]
                zero_or E1                                          |> apply_right ]
        let cHB1 = lemma th ((H * B1) ==> R) [
                    Establish neg_pos                               |> apply
                    Establish neg_prod                              |> apply
                    Establish (unit_pos th (-x) (-y))               |> apply
                    Deduce from_neg                                 |> apply_right ]
        // x = 0: then x·y = 0 ≠ 1.
        let z_not_h =
            lemma th (Z ==> !!H) [
                Deduce' (reflex_implies Z)                          |> at [right_branch; apply_unary; left_branch; left_branch]   // x → 0
                Integers.ring th (zero * y) zero                    |> at [right_branch; apply_unary; left_branch]
                ax_ident th ((zero == one) == F)                    |> at [right_branch; apply_unary]     // literal evaluation
                not_false                                           |> apply_right ]
        let cHZ = mp (OrderedDomain.in_theory th (glue_absurd H Z R)) z_not_h
        let cHB = lemma th ((H * B) ==> R) [
                    ax_ident th (B == (B1 + Z))                     |> at [left_branch; right_branch]     // (15.38)
                    commute_and H (B1 + Z)                          |> at_left
                    Taut (by_cases2 th B1 Z H R cHB1 cHZ)           |> apply ]
        let cover = lemma th (A + B) [
                    OrderedDomain.not_lt d zero x |> Commute        |> at_right                           // x ≤ 0 → ¬(0 < x)
                    excluded_middle' A                              |> apply ]
        lemma th (H ==> R) [
            ident_and H |> Commute                                  |> at_left
            Taut cover |> Commute                                   |> at [left_branch; right_branch]
            commute_and H (A + B)                                   |> at_left
            Taut (by_cases2 th A B H R cHA cHB)                     |> apply
        ]

    (* The theorems (15.78)–(15.86) *)

    /// **(15.78)** `c ∣ c` — witness 1 (15.4).
    let dvd_refl (th: Theory) (c: Term<int>) : Theorem =
        dvd_intro th c c (IntTerm 1) (Integers.ring_ident th (it c * IntTerm 1) c)

    /// **(15.79)** `c ∣ 0` — witness 0 (15.9).
    let dvd_zero (th: Theory) (c: Term<int>) : Theorem =
        dvd_intro th c (IntTerm 0) (IntTerm 0) (Integers.ring_ident th (it c * IntTerm 0) (IntTerm 0))

    /// **(15.80)** `1 ∣ b` — witness `b` (15.4).
    let one_dvd (th: Theory) (b: Term<int>) : Theorem =
        dvd_intro th (IntTerm 1) b b (Integers.ring_ident th (IntTerm 1 * it b) b)

    let private dvd_one_at (th: Theory) (unit_cases: Term<int> -> Term<int> -> Theorem) (c: Term<int>) : Theorem =
        let d = Integers.order_domain th
        let c' = d.Term c
        dvd_elim th c (IntTerm 1) ((c' == d.One) + (c' == -d.One)) (fun k -> unit_cases c k)

    /// `(a·m) ∣ b ⇒ a ∣ b`.
    let private dvd_of_mul_dvd (th: Theory) (a: Term<int>) (m: Term<int>) (b: Term<int>) : Theorem =
        let am = it a * it m
        let Q = dvd a b
        dvd_elim th am b Q (fun k ->
            let amk = am * tm k
            let target = dvd_intro th a amk (it m * tm k) (Integers.ring_ident th (it a * (it m * tm k)) amk)
            lemma th (body am b k ==> Q) [
                Deduce' (OrderedDomain.eq_sym th amk b)             |> at [right_branch; right_branch]    // b → (a·m)·k
                Taut target                                         |> apply_right
            ])

    /// **(15.82)** `d ∣ c ∧ c ∣ b ⇒ d ∣ b` — Transitivity: with `d·k = c`, `c ∣ b` is `(d·k) ∣ b`.
    let dvd_trans (th: Theory) (d: Term<int>) (c: Term<int>) (b: Term<int>) : Theorem =
        let A, B, C = dvd d c, dvd c b, dvd d b
        let inner =
            dvd_elim th d c (B ==> C) (fun k ->
                let dk = it d * tm k
                lemma th (body d c k ==> (B ==> C)) [
                    Deduce' (OrderedDomain.eq_sym th dk c)          |> at [right_branch; left_branch; left_branch]   // c → d·k
                    Taut (dvd_of_mul_dvd th d (tm k) b)             |> apply_right
                ])
        lemma th ((A * B) ==> C) [
            shunt' A B C                                            |> apply
            Taut inner                                              |> apply
        ]

    /// **(15.84)** `b ∣ c ⇒ b ∣ (c·d)` — with `b·k = c`, the witness is `k·d`.
    let dvd_mul (th: Theory) (b: Term<int>) (c: Term<int>) (d: Term<int>) : Theorem =
        let Q = dvd b (it c * it d)
        dvd_elim th b c Q (fun k ->
            let bk = it b * tm k
            let target = dvd_intro th b (bk * it d) (tm k * it d) (Integers.ring_ident th (it b * (tm k * it d)) (bk * it d))
            lemma th (body b c k ==> Q) [
                Deduce' (OrderedDomain.eq_sym th bk c)              |> at [right_branch; right_branch; left_branch]   // c → b·k
                Taut target                                         |> apply_right
            ])

    /// **(15.85)** `b ∣ c ⇒ (b·d) ∣ (c·d)` — with `b·k = c`, the witness is `k`.
    let dvd_mul_both (th: Theory) (b: Term<int>) (c: Term<int>) (d: Term<int>) : Theorem =
        let Q = dvd (it b * it d) (it c * it d)
        dvd_elim th b c Q (fun k ->
            let bk = it b * tm k
            let target = dvd_intro th (it b * it d) (bk * it d) (tm k) (Integers.ring_ident th ((it b * it d) * tm k) (bk * it d))
            lemma th (body b c k ==> Q) [
                Deduce' (OrderedDomain.eq_sym th bk c)              |> at [right_branch; right_branch; left_branch]
                Taut target                                         |> apply_right
            ])

    /// `d ∣ x ∧ d ∣ y ⇒ d ∣ (x ∘ y)`, for `∘` either `+` or `−`: with `d·k₁ = x` and `d·k₂ = y`,
    /// the witness is `k₁ ∘ k₂`.
    let private dvd_combine (th: Theory) (op: DomainTerm<int> -> DomainTerm<int> -> DomainTerm<int>) (d: Term<int>) (x: Term<int>) (y: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let d, x, y = D.Term d, D.Term x, D.Term y
        let A, B, C = dvd d x, dvd d y, dvd d (op x y)
        let inner =
            dvd_elim th d x (B ==> C) (fun k1 ->
                let dk1 = d * D.Term (tm k1)
                let Q2 = dvd d (op dk1 y)
                let inner2 =
                    dvd_elim th d y Q2 (fun k2 ->
                        let dk2 = d * D.Term (tm k2)
                        let w = op (D.Term (tm k1)) (D.Term (tm k2))
                        let target = dvd_intro th d (op dk1 dk2) w (Integers.ring_ident th (d * w) (op dk1 dk2))
                        lemma th (body d y k2 ==> Q2) [
                            Deduce' (OrderedDomain.eq_sym th dk2 y) |> at [right_branch; right_branch; right_branch]   // y → d·k₂
                            Taut target                             |> apply_right
                        ])
                lemma th (body d x k1 ==> (B ==> C)) [
                    Deduce' (OrderedDomain.eq_sym th dk1 x)         |> at [right_branch; right_branch; right_branch; left_branch]   // x → d·k₁
                    Taut inner2                                     |> apply_right
                ])
        lemma th ((A * B) ==> C) [
            shunt' A B C                                            |> apply
            Taut inner                                              |> apply
        ]

    /// `d ∣ x ∧ d ∣ y ⇒ d ∣ (x + y)` — Gries' exercise 15.65.
    let dvd_add (th: Theory) (d: Term<int>) (x: Term<int>) (y: Term<int>) : Theorem = dvd_combine th (fun a b -> a + b) d x y

    /// `d ∣ x ∧ d ∣ y ⇒ d ∣ (x − y)`.
    let dvd_sub (th: Theory) (d: Term<int>) (x: Term<int>) (y: Term<int>) : Theorem = dvd_combine th (fun a b -> a - b) d x y

    let private dvd_antisym_at (th: Theory) (unit_cases: Term<int> -> Term<int> -> Theorem) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let b', c' = D.Term b, D.Term c
        let E1, E2 = (b' == c'), (b' == -c')
        let R = E1 + E2
        let LB, LC = dvd b' c', dvd c' b'
        // ⇐ : substitute, then (15.78), and (−c)·(−1) = c, c·(−1) = −c.
        let c1 = lemma th (E1 ==> (LB * LC)) [
                    Deduce' (reflex_implies E1)                     |> at [right_branch; left_branch; left_branch]     // b ∣ c → c ∣ c
                    Deduce' (reflex_implies E1)                     |> at [right_branch; right_branch; right_branch]   // c ∣ b → c ∣ c
                    Taut (dvd_refl th c')                           |> at [right_branch; left_branch]
                    Taut (dvd_refl th c')                           |> at [right_branch; right_branch]
                    ident_and T                                     |> apply_right ]
        let c2 = lemma th (E2 ==> (LB * LC)) [
                    Deduce' (reflex_implies E2)                     |> at [right_branch; left_branch; left_branch]
                    Deduce' (reflex_implies E2)                     |> at [right_branch; right_branch; right_branch]
                    Taut (dvd_intro th (-c') c' (-one) (Integers.ring_ident th ((-c') * (-one)) c'))  |> at [right_branch; left_branch]
                    Taut (dvd_intro th c' (-c') (-one) (Integers.ring_ident th (c' * (-one)) (-c')))  |> at [right_branch; right_branch]
                    ident_and T                                     |> apply_right ]
        let bwd = mp (mp (OrderedDomain.in_theory th (glue_or_cases E1 E2 (LB * LC))) c1) c2
        // ⇒ : under b·k₁ = c and c·k₂ = b. If c = 0 then b = c·k₂ = 0 = c. Otherwise c·(k₂·k₁) = c, so
        // k₂·k₁ = 1 (15.10b), so k₂ = ±1 (the units), so b = c·k₂ = ±c.
        let core (k1: IntVar) (k2: IntVar) : Theorem =
            let k1, k2 = D.Term (tm k1), D.Term (tm k2)
            let H1, H2 = (b' * k1 == c'), (c' * k2 == b')
            let C = H1 * H2
            let Z = c' == zero
            let left_true = [ commute_or T E2 |> apply_right; zero_or E2 |> apply_right ]
            let cZ = lemma th ((C * Z) ==> R) ([
                        Deduce' (OrderedDomain.eq_sym th (c' * k2) b')   |> at [right_branch; left_branch; left_branch]                // b → c·k₂
                        Deduce' (reflex_implies Z)                       |> at [right_branch; left_branch; left_branch; left_branch]   // c → 0
                        Deduce' (reflex_implies Z)                       |> at [right_branch; left_branch; right_branch]
                        Integers.ring th (zero * k2) zero                |> at [right_branch; left_branch; left_branch]
                        IntegralDomain.refl_true th zero                 |> at [right_branch; left_branch] ] @ left_true)
            let NZ = c' != zero
            let fact = lemma th (C ==> (c' * (k2 * k1) == c')) [
                        Integers.ring th (c' * (k2 * k1)) ((c' * k2) * k1)   |> at [right_branch; left_branch]
                        Deduce' (reflex_implies H2)                      |> at [right_branch; left_branch; left_branch]   // c·k₂ → b
                        Deduce' (reflex_implies H1)                      |> at [right_branch; left_branch]                // b·k₁ → c
                        IntegralDomain.refl_true th c'                   |> apply_right ]
            let cancel1 = mp (OrderedDomain.in_theory th (glue_detach NZ (c' * (k2 * k1) == c') ((k2 * k1) == one)))
                             (IntegralDomain.unique_one D c' (k2 * k1))                                                    // (15.10b)
            let K1, K2 = (k2 == one), (k2 == -one)
            let kc1 = lemma th ((H2 * K1) ==> R) ([
                        Deduce' (OrderedDomain.eq_sym th (c' * k2) b')   |> at [right_branch; left_branch; left_branch]
                        Deduce' (reflex_implies K1)                      |> at [right_branch; left_branch; left_branch; right_branch]
                        Integers.ring th (c' * one) c'                   |> at [right_branch; left_branch; left_branch]
                        IntegralDomain.refl_true th c'                   |> at [right_branch; left_branch] ] @ left_true)
            let kc2 = lemma th ((H2 * K2) ==> R) [
                        Deduce' (OrderedDomain.eq_sym th (c' * k2) b')   |> at [right_branch; right_branch; left_branch]
                        Deduce' (reflex_implies K2)                      |> at [right_branch; right_branch; left_branch; right_branch]
                        Integers.ring th (c' * (-one)) (-c')             |> at [right_branch; right_branch; left_branch]
                        IntegralDomain.refl_true th (-c')                |> at [right_branch; right_branch]
                        zero_or E1                                       |> apply_right ]
            let cNZ = lemma th ((C * NZ) ==> R) [
                        Establish fact                                   |> apply
                        Establish cancel1                                |> apply
                        Establish (unit_cases k2 k1)                     |> apply
                        Deduce (by_cases2 th K1 K2 H2 R kc1 kc2)         |> apply_right ]
            lemma th (C ==> R) [
                ident_and C |> Commute                                   |> at_left
                excluded_middle' Z |> Commute                            |> at [left_branch; right_branch]
                commute_and C (Z + !!Z)                                  |> at_left
                Taut (by_cases2 th Z (!!Z) C R cZ cNZ)                   |> apply ]
        let fwd_inner =
            dvd_elim th b' c' (LC ==> R) (fun k1 ->
                let H1 = body b' c' k1
                let inner = dvd_elim th c' b' (H1 ==> R) (fun k2 ->
                                let H2 = body c' b' k2
                                lemma th (H2 ==> (H1 ==> R)) [
                                    shunt' H2 H1 R |> Commute            |> apply
                                    commute_and H2 H1                    |> at_left
                                    Taut (core k1 k2)                    |> apply ])
                lemma th (H1 ==> (LC ==> R)) [
                    shunt' H1 LC R |> Commute                            |> apply
                    commute_and H1 LC                                    |> at_left
                    shunt' LC H1 R                                       |> apply
                    Taut inner                                           |> apply ])
        let fwd = lemma th ((LB * LC) ==> R) [ shunt' LB LC R |> apply; Taut fwd_inner |> apply ]
        lemma th ((LB * LC) == R) [
            mutual_implication' (LB * LC) R |> Commute                   |> apply
            Taut (conj fwd bwd)                                          |> apply
        ]

    let private not_dvd_succ_at (th: Theory) (dvd_one: Term<int> -> Theorem) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let one = D.One
        let b', c' = D.Term b, D.Term c
        let L, A, S = (one <. b'), dvd b' c', dvd b' (c' + one)
        let E1, E2 = (b' == one), (b' == -one)
        let n1 = lemma th (E1 ==> !!L) [
                    Deduce' (reflex_implies E1)                          |> at [right_branch; apply_unary; right_branch]
                    OrderedDomain.lt_irrefl D one                        |> at [right_branch; apply_unary]
                    not_false                                            |> apply_right ]
        let n2 = lemma th (E2 ==> !!L) [
                    Deduce' (reflex_implies E2)                          |> at [right_branch; apply_unary; right_branch]
                    OrderedDomain.not_lt D one (-one)                    |> apply_right                       // ¬(1 < −1) → −1 ≤ 1
                    Integers.reduce                                      |> at [right_branch; left_branch]
                    ax_ident th ((leq -1 1) == T)                        |> apply_right ]                     // literal evaluation
        let not_unit = mp (mp (OrderedDomain.in_theory th (glue_or_cases E1 E2 (!!L))) n1) n2
        let to_one = lemma th (dvd b' ((c' + one) - c') ==> dvd b' one) [
                        Integers.ring th ((c' + one) - c') one           |> at [left_branch; right_branch]
                        Deduce (reflex_implies (dvd b' one))             |> apply_right ]
        lemma th ((L * A) ==> !!S) [
            ident_implies_false_not S |> Commute                         |> at_right                          // ¬S → (S ⇒ F)
            shunt' (L * A) S F |> Commute                                |> apply
            Establish (dvd_sub th b' (c' + one) c')                      |> apply                             // b ∣ ((c + 1) − c)
            Establish to_one                                             |> apply
            Establish (dvd_one b')                                       |> apply                             // (15.81)
            Establish not_unit                                           |> apply
            Deduce (lemma th ((L * !!L) ==> F) [ contr L |> at_left ])   |> apply_right
        ]

    // The proofs of (15.81), (15.83) and (15.86) run a case analysis each (≈ 2 s); they are derived once
    // per theory at schema metavariables and instantiated per call. Their statements are binder-free.
    type private DivLemmas =
        { UnitCases: Expr<int> -> Expr<int> -> Theorem
          DvdOne: Expr<int> -> Theorem
          Antisym: Expr<int> -> Expr<int> -> Theorem
          NotDvdSucc: Expr<int> -> Expr<int> -> Theorem }

    let private div_lemmas = System.Runtime.CompilerServices.ConditionalWeakTable<Theory, DivLemmas>()

    let rec private lemmas_of (th: Theory) : DivLemmas =
        div_lemmas.GetValue(th, fun _ ->
            let t (e: Expr<int>) = IntTerm e :> Term<int>
            let uc x y = (lemmas_of th).UnitCases (x: Term<int>).Expr (y: Term<int>).Expr
            let d1 x = (lemmas_of th).DvdOne (x: Term<int>).Expr
            { UnitCases = Tactics.Schema.th2 "unit_cases" (fun x y -> unit_cases_at th (t x) (t y))
              DvdOne = Tactics.Schema.th1 "dvd_one" (fun c -> dvd_one_at th uc (t c))
              Antisym = Tactics.Schema.th2 "dvd_antisym" (fun b c -> dvd_antisym_at th uc (t b) (t c))
              NotDvdSucc = Tactics.Schema.th2 "not_dvd_succ" (fun b c -> not_dvd_succ_at th d1 (t b) (t c)) })

    /// `x·y = 1 ⇒ (x = 1 ∨ x = −1)` — the units of ℤ. By cases on the sign of `x`: positive, `x = 1`;
    /// negative, the same for `−x`; zero, `x·y = 0`.
    let unit_cases (th: Theory) (x: Term<int>) (y: Term<int>) : Theorem = (lemmas_of th).UnitCases x.Expr y.Expr

    /// **(15.81)** `c ∣ 1 ⇒ c = 1 ∨ c = −1` — a witness `k` has `c·k = 1` (`unit_cases`).
    let dvd_one (th: Theory) (c: Term<int>) : Theorem = (lemmas_of th).DvdOne c.Expr

    /// **(15.83)** `b ∣ c ∧ c ∣ b ≡ b = c ∨ b = −c` — Gries' manual, answer 15.60: case `c = 0`, and
    /// otherwise the two witnesses multiply to 1.
    let dvd_antisym (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (lemmas_of th).Antisym b.Expr c.Expr

    /// **(15.86)** `1 < b ∧ b ∣ c ⇒ ¬(b ∣ (c + 1))` — otherwise `b` divides `(c + 1) − c = 1`, and
    /// (15.81) leaves only `b = ±1`.
    let not_dvd_succ (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (lemmas_of th).NotDvdSucc b.Expr c.Expr

    (* The division theorem *)

    /// **(15.87) Division theorem** (existence), as Gries' manual corrects it — `c > 0`, since at
    /// `c = 0` the range `0 ≤ r < c` is empty:
    ///
    /// ```
    /// 0 < c ⇒ (∃q |: (∃r | 0 ≤ r ∧ r < c : b = q·c + r))
    /// ```
    ///
    /// Gries proves it with a loop (15.88); this is the textbook proof from Well ordering (15.49). The
    /// set `S = {x | 0 ≤ x ∧ (∃k |: x = b − k·c)}` is non-empty — it contains `b + abs.b·c` (quotient
    /// `−abs.b`) — so it has a least element `x` (`least_element`), with quotient `k`. Then `x < c`, for
    /// otherwise `x − c` (quotient `k + 1`) is a smaller element of S. So `q, r := k, x`.
    let division_theorem (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let avoid = [ b.Expr.Raw; c.Expr.Raw ]
        let q = fresh_int "q" avoid
        let r = fresh_int "r" (q.Expr.Raw :: avoid)
        let x = fresh_int "x" (q.Expr.Raw :: r.Expr.Raw :: avoid)
        let y = fresh_int "y" (q.Expr.Raw :: r.Expr.Raw :: x.Expr.Raw :: avoid)
        let k = fresh_int "k" (q.Expr.Raw :: r.Expr.Raw :: x.Expr.Raw :: y.Expr.Raw :: avoid)
        let tv (v: IntVar) = D.Term (tm v)
        let b', c' = D.Term b, D.Term c
        let X = tv x
        let C0 = zero <. c'
        let ex1 (v: IntVar) (p: Pred<int>) = qex v T p.[tm v]
        let Pk (v: DomainTerm<int>) = pred_of k (v == b' - tv k * c')
        let Pof (v: DomainTerm<int>) = ex1 k (Pk v)
        let P = pred_of x (Pof X)
        let Rr = pred_of r ((zero <=. tv r) * (tv r <. c'))
        let Bq = pred_of r (b' == tv q * c' + tv r)
        let G = qex q T (exists(r, Rr, Bq))
        let wrap (t: Theorem) = lemma th (C0 ==> Prop(Expr.Cast<bool> t.Stmt)) [ Taut t |> apply_right ]
        // S is non-empty: e = b + abs.b·c ≥ 0, since −b ≤ abs.b ≤ abs.b·c.
        let ab = abs_of b'
        let e = b' + ab * c'
        let Pe = mp (OrderedDomain.in_theory th (exists_intro k (Pk e) (-ab))) (Integers.ring_ident th e (b' - (-ab) * c'))
        let ab_le = lemma th (((zero <=. ab) * (one <=. c')) ==> (ab <=. ab * c')) [
                        Integers.ring th ab (ab * one)                   |> at [right_branch; left_branch]
                        Deduce (OrderedDomain.le_mul_nonneg D ab one c') |> apply_right ]
        let negb =
            let A1, A2 = (b' <=. ab), (-b' <=. ab)
            mp (OrderedDomain.in_theory th (lemma prop_calculus ((A1 * A2) ==> A2) [
                    commute_and A1 A2 |> at_left
                    Taut (strengthen_and A2 A1) |> apply ])) (OrderedDomain.abs_ge D b')
        let translate = lemma th ((-b' <=. ab * c') ==> (zero <=. e)) [
                        OrderedDomain.le_translate D (-b') (ab * c') b'  |> at_left
                        Integers.ring th (-b' + b') zero                 |> at [left_branch; left_branch]
                        Integers.ring th (ab * c' + b') e                |> at [left_branch; right_branch]
                        Deduce (reflex_implies (zero <=. e))             |> apply_right ]
        let nonneg = lemma th (C0 ==> (zero <=. e)) [
                        Establish (one_le_of_pos th c')                  |> apply
                        Establish (wrap (OrderedDomain.abs_nonneg D b')) |> apply
                        Establish ab_le                                  |> apply
                        Establish (wrap negb)                            |> apply
                        Establish (OrderedDomain.le_trans D (-b') ab (ab * c')) |> apply
                        Deduce translate                                 |> apply_right ]
        // A least element x, with its quotient k.
        let least = least_element th x y P e
        let Rng = pred_of x ((leq 0 (tm x)) * P.[tm x])
        let minimal (v: Term<int>) = qall y ((leq 0 (tm y)) * (lt (tm y) v)) (!!P.[tm y])
        let Min = pred_of x (minimal (tm x))
        let LE = exists(x, Rng, Min)
        let E = X == b' - tv k * c'
        let N = zero <=. X
        let M = minimal (tm x)
        // x < c: else y₀ = x − c is in S (quotient k + 1), with 0 ≤ y₀ < x — and x is least.
        let y0 = X - c'
        let f1 = lemma th ((c' <=. X) ==> (zero <=. y0)) [
                    OrderedDomain.le_translate D c' X (-c')              |> at_left
                    Integers.ring th (c' + -c') zero                     |> at [left_branch; left_branch]
                    Integers.ring th (X + -c') y0                        |> at [left_branch; right_branch]
                    Deduce (reflex_implies (zero <=. y0))                |> apply_right ]
        let f2 = lemma th (C0 ==> (y0 <. X)) [
                    Integers.ring th y0 (zero + y0)                      |> at [right_branch; left_branch]
                    Integers.ring th X (c' + y0)                         |> at [right_branch; right_branch]
                    OrderedDomain.lt_translate D zero c' y0 |> Commute   |> at_right
                    Deduce (reflex_implies C0)                           |> apply_right ]
        let Py0 = P.[y0]
        let ey = lemma th (E ==> (y0 == b' - (tv k + one) * c')) [
                    Deduce' (reflex_implies E)                           |> at [right_branch; left_branch; left_branch]   // x → b − k·c
                    Integers.ring th ((b' - tv k * c') - c') (b' - (tv k + one) * c') |> at [right_branch; left_branch]
                    IntegralDomain.refl_true th (b' - (tv k + one) * c') |> apply_right ]
        let f3 = lemma th (E ==> Py0) [
                    Establish ey                                         |> apply
                    Deduce (exists_intro k (Pk y0) (tv k + one))         |> apply_right ]                                 // (9.28)
        let Rm = pred_of y ((leq 0 (tm y)) * (lt (tm y) (tm x)))
        let NP = pred_of y (!!P.[tm y])
        let inst = lemma th (M ==> (Rm.[y0] ==> NP.[y0])) [ trade_forall_implies y Rm NP |> at_left ]               // (9.13)
        let f4 = lemma th ((M * Rm.[y0]) ==> NP.[y0]) [ shunt' M Rm.[y0] NP.[y0] |> apply; Taut inst |> apply ]
        let A3 = (E * M) * C0
        let lt_c = lemma th (A3 ==> (X <. c')) [
                    double_negation (X <. c') |> Commute                 |> at_right
                    OrderedDomain.not_lt D X c'                          |> at [right_branch; apply_unary]                // ¬(x < c) → c ≤ x
                    ident_implies_false_not (c' <=. X) |> Commute        |> at_right
                    shunt' A3 (c' <=. X) F |> Commute                    |> apply
                    Establish f1                                         |> apply
                    Establish f2                                         |> apply
                    Establish f3                                         |> apply
                    Establish f4                                         |> apply
                    Deduce (lemma th ((Py0 * !!Py0) ==> F) [ contr Py0 |> at_left ]) |> apply_right ]
        // q, r := k, x.
        let eb = lemma th (E ==> (b' == tv k * c' + X)) [
                    Deduce' (reflex_implies E)                           |> at [right_branch; right_branch; right_branch]  // x → b − k·c
                    Integers.ring th (tv k * c' + (b' - tv k * c')) b'   |> at [right_branch; right_branch]
                    IntegralDomain.refl_true th b'                       |> apply_right ]
        let Bk = pred_of r (b' == tv k * c' + tv r)
        let introR = exists_intro r (Rr * Bk) (tm x)
        let introQ = exists_intro q (pred_of q (ex1 r (Rr * Bq))) (tm k)
        let Rest = (N * M) * C0
        let g = lemma th ((E * Rest) ==> G) [
                    trade_exists_and r Rr Bq                             |> at [right_branch; select_body]                 // (9.19)
                    Establish lt_c                                       |> apply
                    Establish eb                                         |> apply
                    Establish introR                                     |> apply
                    Deduce introQ                                        |> apply_right ]
        // Eliminate the quotient k, then the least element x.
        let Qk = Rest ==> G
        let tk = lemma th ((T * E) ==> Qk) [
                    commute_and T E                                      |> at_left
                    ident_and E                                          |> at_left
                    shunt' E Rest G |> Commute                           |> apply
                    Taut g                                               |> apply ]
        let elimK = exists_elim th k truepred (Pk X) Qk tk
        let PX = Pof X
        let elimK' = lemma th ((PX * Rest) ==> G) [ shunt' PX Rest G |> apply; Taut elimK |> apply ]
        let xbody = lemma th ((Rng.[tm x] * Min.[tm x]) ==> (C0 ==> G)) [
                    shunt' (Rng.[tm x] * Min.[tm x]) C0 G |> Commute     |> apply
                    Deduce elimK'                                        |> apply_right ]
        let elimX = exists_elim th x Rng Min (C0 ==> G) xbody
        let elimX' = lemma th ((LE * C0) ==> G) [ shunt' LE C0 G |> apply; Taut elimX |> apply ]
        lemma th (C0 ==> G) [
            Establish nonneg                                             |> apply
            Establish (wrap Pe)                                          |> apply
            Establish least                                              |> apply
            Deduce elimX'                                                |> apply_right
        ]

    (* Module information members *)

    type private IModuleTypeLocator = interface end

    let Type = match typeof<IModuleTypeLocator>.DeclaringType with | NonNull m -> m | _ -> failwith "Failed to locate module type."
