namespace Sylvia
// A bare rule as a proof step implicitly means "apply to the whole expression" (Rule -> RuleApplication).
#nowarn "3391"

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open FsExpr
open Formula
open Patterns
open Descriptions

open PropCalculus
open DomainTerms
open IntegralDomain

/// Theory of ordered domains — Gries & Schneider ch.15, §15.1 (pp. 307–309).
///
/// An integral domain with a predicate `pos` satisfying (15.30)–(15.33) — the positives are closed
/// under `+` and `·`, 0 is not positive, and of a non-zero `b` and `−b` exactly one is positive — is an
/// ORDERED domain. The relations `<`, `>`, `≤`, `≥` are then DEFINED from `pos` by (15.36)–(15.39), and
/// the order laws (15.40)–(15.46) are theorems. Gries' own proof of (15.41a) is three steps: Less,
/// Addition (15.30), arithmetic.
///
/// GENERIC over the carrier, as `IntegralDomain<'t>` is; `Integers` is the instance at `int`. What an
/// ordered domain does NOT give is discreteness — the rationals are one — so Discreteness
/// `(x ≤ y+1) = (x ≤ y ∨ x = y+1)` is a theorem of `Induction` theories, derived from (12.8).
///
/// Proofs are written over `DomainTerm`s: `<.` `<=.` `>.` `>=.` are `<` `≤` `>` `≥`, and `pos_of`,
/// `min_of` (↓), `max_of` (↑), `abs_of` build the rest.
module OrderedDomain =

    /// The positivity predicate `pos` of an ordered domain. Uninterpreted: its meaning is
    /// (15.30)–(15.33), and it is never evaluated.
    let pos (x: 't) : bool = invalidOp "pos is a symbolic predicate of an ordered domain."

    /// The order operators of an ordered domain: `pos`, the four relations it defines, minimum `↓`
    /// and maximum `↑` (§15.2), and absolute value (§15.3).
    type OrderOps<'t when 't: equality> = DomainTerms.OrderOps<'t>

    (* Axioms *)

    /// Gries (15.30)–(15.33), and the definitions (15.36)–(15.39) of `<`, `>`, `≤`, `≥`.
    let ordered_domain_axioms (d: DomainOps<'t>) (o: OrderOps<'t>) : Axioms =
        let eq : Expr<'t -> 't -> bool> = <@ (=) @>
        let desc = axiom_desc d.Name
        let named n = Some (desc (pattern_name n))
        function
        // (15.30) Addition: pos.a ∧ pos.b ⇒ pos(a + b)
        | Implies(And(Unary o.Pos a, Unary o.Pos b), Unary o.Pos (Binary d.Add (a', b')))
            when sequal a a' && sequal b b' -> named "Addition"
        // (15.31) Multiplication: pos.a ∧ pos.b ⇒ pos(a · b)
        | Implies(And(Unary o.Pos a, Unary o.Pos b), Unary o.Pos (Binary d.Mul (a', b')))
            when sequal a a' && sequal b b' -> named "Multiplication"
        // (15.32) ¬pos.0 — also accepted as pos.0 = false
        | Not(Unary o.Pos z)
        | Equals(Unary o.Pos z, False) when sequal z d.Zero -> named "Zero is not positive"
        // (15.33) b ≠ 0 ⇒ (pos.b = ¬pos(−b))
        | Implies(NonZero d.Zero b, Equals(Unary o.Pos b', Not(Unary o.Pos (Unary d.Neg b''))))
            when sequal b b' && sequal b' b'' -> named "Positivity of negation"
        // (15.36) Less: a < b = pos(b − a)
        | Equals(Binary o.Lt (a, b), Unary o.Pos (Binary d.Sub (b', a')))
            when sequal a a' && sequal b b' -> named "Less"
        // (15.37) Greater: a > b = pos(a − b)
        | Equals(Binary o.Gt (a, b), Unary o.Pos (Binary d.Sub (a', b')))
            when sequal a a' && sequal b b' -> named "Greater"
        // (15.38) At most: a ≤ b = a < b ∨ a = b
        | Equals(Binary o.Le (a, b), Or(Binary o.Lt (a', b'), Binary eq (a'', b'')))
            when sequal a a' && sequal a' a'' && sequal b b' && sequal b' b'' -> named "At most"
        // (15.39) At least: a ≥ b = a > b ∨ a = b
        | Equals(Binary o.Ge (a, b), Or(Binary o.Gt (a', b'), Binary eq (a'', b'')))
            when sequal a a' && sequal a' a'' && sequal b b' && sequal b' b'' -> named "At least"
        // (15.53) Definition of ↓ and ↑. Gries writes them under (∀z |: …); an axiom SCHEME over `z` is
        // the same statement, since a scheme holds at every instance.
        | Equals(Binary o.Le (z, Binary o.Min (x, y)), And(Binary o.Le (z', x'), Binary o.Le (z'', y')))
            when sequal z z' && sequal z' z'' && sequal x x' && sequal y y' -> named "Definition of ↓"
        | Equals(Binary o.Ge (z, Binary o.Max (x, y)), And(Binary o.Ge (z', x'), Binary o.Ge (z'', y')))
            when sequal z z' && sequal z' z'' && sequal x x' && sequal y y' -> named "Definition of ↑"
        // (15.71) abs.x = x ↑ −x
        | Equals(Unary o.Abs x, Binary o.Max (x', Unary d.Neg x''))
            when sequal x x' && sequal x' x'' -> named "Definition of abs"
        | _ -> None

    (* Theory *)

    /// An ordered domain over `'t`: an integral domain with `pos`, and a caller's axioms and rules on top.
    type OrderedDomain<'t when 't: equality>(ops: DomainOps<'t>, order: OrderOps<'t>, ?axioms: Axioms, ?rules: Rules) =
        inherit IntegralDomain<'t>(ops,
                                   BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None)) (ordered_domain_axioms ops order),
                                   defaultArg rules [])
        member val Order = order
        override _.Ordering = Some order

    (* Equality *)

    /// `(a = b) ⇒ (b = a)` — symmetry of `=` at any carrier, as an implication. The logic's symmetry
    /// is that of ≡, so nothing flips `a = b` at a non-bool type directly; this derives it from
    /// Substitution (3.84a) and reflexivity: `a = b ∧ a = a` is `a = b ∧ b = a`. With `Deduce'` it
    /// rewrites `b` to `a` under a hypothesis `a = b`; with `mp` it flips a proved identity.
    let eq_sym (th: Theory) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let ab, aa, ba = eq_t a.Expr b.Expr, eq_t a.Expr a.Expr, eq_t b.Expr a.Expr
        lemma th (ab ==> ba) [
            ident_and ab |> Commute                  |> at_left                         // a = b → a = b ∧ T
            Taut (axiom th aa) |> Commute            |> at [left_branch; right_branch]  // T → a = a
            ax_ident th ((ab * aa) == (ab * ba))     |> at_left                         // (3.84a)
            commute_and ab ba                        |> at_left
            Taut (strengthen_and ba ab)              |> apply
        ]

    (* Arithmetic the order proofs need (identities of any integral domain) *)

    /// `(b − a) + (c − b) = c − a` — Gries' Exercise 15.22, the arithmetic step of (15.41a).
    let sub_chain (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Rule =
        let a, b, c = d.Term a, d.Term b, d.Term c
        // (b + c) − (a + b) = c − a, by (15.28): it is (b + c) + a = (a + b) + c
        let inner =
            ident d ((b + c) - (a + b) == c - a) [
                sub_eq_sub d (b + c) (a + b) c a          |> apply
                ax d ((b + c) + a) (b + (c + a))         |> at_left
                ax d (a + b) (b + a)                     |> at [right_branch; left_branch]
                ax d ((b + a) + c) (b + (a + c))         |> at_right
                ax d (a + c) (c + a)                     |> at [right_branch; right_branch]
            ]
        ident d ((b - a) + (c - b) == c - a) [
            sub_add_sub d b a c b                         |> at_left     // (15.25)
            inner                                         |> at_left
        ]

    /// `(b + e) − (a + e) = b − a` — the arithmetic step of Monotonicity (15.42).
    let sub_translate (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (e: Term<'t>) : Rule =
        let a, b, e = d.Term a, d.Term b, d.Term e
        ident d ((b + e) - (a + e) == b - a) [
            sub_eq_sub d (b + e) (a + e) b a              |> apply       // (15.28)
            ax d ((b + e) + a) (b + (e + a))             |> at_left
            ax d ((a + e) + b) (b + (a + e))             |> at_right
            ax d (a + e) (e + a)                         |> at [right_branch; right_branch]
        ]

    /// `a − (a + 1) = −1`.
    let sub_succ (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let zero, one, a = d.Zero, d.One, d.Term a
        ident d (a - (a + one) == -one) [
            ax d (a - (a + one)) (a + -(a + one))        |> at_left                        // (15.14)
            neg_add d a one                               |> at [left_branch; right_branch] // (15.19)
            rev d ((a + -a) + -one) (a + (-a + -one))    |> at_left
            ax d (a + -a) zero                           |> at [left_branch; left_branch]  // (15.13)
            ax d (zero + -one) (-one)                    |> at_left
        ]

    (* Propositional glue *)

    // The case analyses below are three-atom tautologies. Proving one by the ANF search costs ~1.5 s,
    // and an `Induction` theory needs (15.34) for `succ_not_le` — so they are proved ONCE per process,
    // in the logic, as schemas over boolean metavariables (`Tactics.Schema.p3`, sound by uniform
    // substitution), and each instance is re-homed in the caller's theory by one `Taut` step.
    let internal taut3 (name: string) (f: Prop -> Prop -> Prop -> Prop) =
        Tactics.Schema.p3 name (fun x y z -> let s = f x y z in lemma prop_calculus s (autoproof_anf s).Steps)
    /// (nz ⇒ (p = ¬q)) ⇒ ((nz ∧ ¬p) ⇒ q)
    let private glue_neg = taut3 "od_neg" (fun nz p q -> (nz ==> (p == !!q)) ==> ((nz * !!p) ==> q))
    /// (p ⇒ q) ⇒ (((nz ∧ ¬p) ⇒ q) ⇒ (nz ⇒ q))
    let private glue_cases = taut3 "od_cases" (fun p q nz -> (p ==> q) ==> (((nz * !!p) ==> q) ==> (nz ==> q)))
    /// (nz ⇒ p) ⇒ ((nz ⇒ (p = ¬q)) ⇒ (nz ⇒ (q = false)))
    let private glue_false = taut3 "od_false" (fun nz p q -> (nz ==> p) ==> ((nz ==> (p == !!q)) ==> (nz ==> (q == F))))
    /// A logic theorem as a theorem of `th`, so that `mp` can combine it with `th`'s own.
    let internal in_theory (th: Theory) (t: Theorem) : Theorem = lemma th (Prop(Expr.Cast<bool> t.Stmt)) [ Taut t |> apply ]

    (* Theorems for ordered domains (Gries 15.34 – 15.46) *)

    /// **(15.34)**: `b ≠ 0 ⇒ pos(b·b)`. Gries' proof, by cases on `pos.b`: if positive, (15.31); if
    /// not, `−b` is positive by (15.33), and `b·b = (−b)·(−b)` (15.23).
    let sq_pos (d: OrderedDomain<'t>) (b: Term<'t>) : Theorem =
        let b = d.Term b
        let nb = -b
        let nz = b != d.Zero
        let sq (x: DomainTerm<'t>) =
            lemma d ((pos_of x) ==> (pos_of (x * x))) [
                idemp_and (pos_of x) |> Commute |> at_left  // pos.x → pos.x ∧ pos.x
                // and that is Multiplication (15.31).
            ]
        let q = pos_of (b * b)
        let ax33 = axiom d (nz ==> ((pos_of b) == !!(pos_of nb)))
        let neg_pos = mp (in_theory d (glue_neg nz (pos_of b) (pos_of nb))) ax33
        let not_pos =
            lemma d ((nz * !!(pos_of b)) ==> q) [
                flip d (neg_mul_neg d b b)     |> at [right_branch; apply_unary]   // b·b → (−b)·(−b)
                Establish neg_pos               |> apply                            // pos(−b)
                Deduce (sq nb)                  |> apply_right
            ]
        mp (mp (in_theory d (glue_cases (pos_of b) q nz)) (sq b)) not_pos

    /// `1 ≠ 0 ⇒ pos.1` — the corollary of (15.34) Gries draws: `1 = 1·1`. The premise is needed: the
    /// one-element ring satisfies every axiom above, and in it nothing is positive.
    let one_pos (d: OrderedDomain<'t>) : Theorem =
        let one = d.One
        lemma d ((one != d.Zero) ==> (pos_of one)) [
            rev d (one * one) one                  |> at [right_branch; apply_unary]   // 1 → 1·1
            Taut (sq_pos d one)                     |> apply
        ]

    /// `1 ≠ 0 ⇒ (pos(−1) = false)` — so −1 is not positive: (15.33) at `b := 1`.
    let neg_one_not_pos (d: OrderedDomain<'t>) : Theorem =
        let one = d.One
        let nz1, p1, pm1 = (one != d.Zero), pos_of one, pos_of (-one)
        let ax33 = axiom d (nz1 ==> (p1 == !!pm1))
        mp (mp (in_theory d (glue_false nz1 p1 pm1)) (one_pos d)) ax33

    /// **(15.40) Positive elements**: `pos.b = 0 < b`.
    let pos_lt (d: OrderedDomain<'t>) (b: Term<'t>) : Rule =
        let zero, b = d.Zero, d.Term b
        ident d ((pos_of b) == (zero <. b)) [
            ax_ident d ((zero <. b) == (pos_of (b - zero)))    |> at_right   // (15.36)
            sub_zero d b                                        |> at [right_branch; apply_unary]
        ]

    /// **(15.41a) Transitivity**: `a < b ∧ b < c ⇒ a < c` — Gries' proof: Less, Addition, arithmetic.
    let lt_trans (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Theorem =
        let a, b, c = d.Term a, d.Term b, d.Term c
        lemma d (((a <. b) * (b <. c)) ==> (a <. c)) [
            ax_ident d ((a <. b) == (pos_of (b - a)))  |> at [left_branch; left_branch]     // (15.36)
            ax_ident d ((b <. c) == (pos_of (c - b)))  |> at [left_branch; right_branch]
            ax_ident d ((a <. c) == (pos_of (c - a)))  |> at_right
            flip d (sub_chain d a b c)                 |> at [right_branch; apply_unary]    // c − a → (b − a) + (c − b)
            // and that is Addition (15.30).
        ]

    /// **(15.42) Monotonicity**: `(a < b) = (a + e < b + e)`.
    let lt_translate (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (e: Term<'t>) : Rule =
        let a, b, e = d.Term a, d.Term b, d.Term e
        ident d ((a <. b) == (a + e <. b + e)) [
            ax_ident d ((a <. b) == (pos_of (b - a)))                       |> at_left    // (15.36)
            ax_ident d ((a + e <. b + e) == (pos_of ((b + e) - (a + e))))   |> at_right
            sub_translate d a b e                                            |> at [right_branch; apply_unary]
        ]

    /// `(a + e = b + e) = (a = b)` — (15.8) Cancellation on the right.
    let cancel_add_right (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (e: Term<'t>) : Rule =
        let a, b, e = d.Term a, d.Term b, d.Term e
        ident d ((a + e == b + e) == (a == b)) [
            ax d (a + e) (e + a)                    |> at [left_branch; left_branch]
            ax d (b + e) (e + b)                    |> at [left_branch; right_branch]
            cancel_add d e a b                       |> at_left
        ]

    /// **(15.46) Reflexivity**: `(a ≤ a) = true`.
    let le_refl (d: OrderedDomain<'t>) (a: Term<'t>) : Rule =
        let a = d.Term a
        ident d ((a <=. a) == T) [
            ax_ident d ((a <=. a) == ((a <. a) + (a == a)))    |> at_left      // (15.38)
            Taut (axiom d (a == a))                            |> at [left_branch; right_branch]
            zero_or (a <. a)                                    |> at_left
        ]

    /// Monotonicity for `≤`: `(a ≤ b) = (a + e ≤ b + e)` — (15.42) and Cancellation, through (15.38).
    let le_translate (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (e: Term<'t>) : Rule =
        let a, b, e = d.Term a, d.Term b, d.Term e
        let ae, be = a + e, b + e
        ident d ((a <=. b) == (ae <=. be)) [
            ax_ident d ((a <=. b) == ((a <. b) + (a == b)))        |> at_left               // (15.38)
            ax_ident d ((ae <=. be) == ((ae <. be) + (ae == be)))  |> at_right
            lt_translate d a b e                                    |> at [left_branch; left_branch]
            cancel_add_right d a b e                                |> at [right_branch; right_branch]
        ]

    /// `((A ∨ B) ∧ (C ∨ D)) ⇒ R`, from its four cases `C ∧ A ⇒ R`, `C ∧ B ⇒ R`, `D ∧ A ⇒ R`,
    /// `D ∧ B ⇒ R` — ∧ over ∨ twice, then Case analysis (3.78) three times. Two `≤` premises unfolded
    /// by (15.38) have exactly this shape.
    let private by_cases4 (th: Theory) (A: Prop) (B: Prop) (C: Prop) (D: Prop) (R: Prop)
                          (cCA: Theorem) (cCB: Theorem) (cDA: Theorem) (cDB: Theorem) : Theorem =
        lemma th (((A + B) * (C + D)) ==> R) [
            distrib_and_or (A + B) C D                    |> at_left
            commute_and (A + B) C                         |> at [left_branch; left_branch]
            distrib_and_or C A B                          |> at [left_branch; left_branch]
            commute_and (A + B) D                         |> at [left_branch; right_branch]
            distrib_and_or D A B                          |> at [left_branch; right_branch]
            case_analysis_1 ((C * A) + (C * B)) ((D * A) + (D * B)) R |> Commute |> apply   // (3.78)
            case_analysis_1 (C * A) (C * B) R |> Commute  |> at_left
            case_analysis_1 (D * A) (D * B) R |> Commute  |> at_right
            Taut cCA                                      |> at [left_branch; left_branch]
            Taut cCB                                      |> at [left_branch; right_branch]
            Taut cDA                                      |> at [right_branch; left_branch]
            Taut cDB                                      |> at [right_branch; right_branch]
            ident_and T                                   |> at_left
            ident_and T                                   |> at_right
            ident_and T                                   |> apply
        ]

    /// **(15.41d) Transitivity**: `a ≤ b ∧ b ≤ c ⇒ a ≤ c`. By (15.38) each premise is a disjunction,
    /// and the four cases are (15.41a) and three Substitutions.
    let le_trans (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Theorem =
        let a, b, c = d.Term a, d.Term b, d.Term c
        let A, B, C, D = (a <. b), (a == b), (b <. c), (b == c)
        let R = a <=. c
        let ltc, eqc = (a <. c), (a == c)
        let leR = ax_ident d (R == (ltc + eqc))                                          // (15.38)
        let caseCA =  // b < c ∧ a < b
            lemma d ((C * A) ==> R) [
                leR                                       |> at_right
                Establish (lt_trans d a b c)              |> apply                        // a < c
                Deduce (weaken_or ltc eqc)                |> apply_right
            ]
        let caseCB =  // b < c ∧ a = b
            lemma d ((C * B) ==> R) [
                leR                                       |> at_right
                Deduce' (reflex_implies B)                |> at [right_branch; left_branch]   // a < c → b < c
                Deduce (weaken_or C eqc)                  |> apply_right
            ]
        let caseDA =  // b = c ∧ a < b
            lemma d ((D * A) ==> R) [
                leR                                       |> at_right
                // Addressed at c ITSELF: `at [right_branch; left_branch]` rewrote the first `c` in `a < c`,
                // which is inside `a` when `a` contains `c` (le_trans at a := −A, c := A failed).
                Deduce' (eq_sym d b c)                   |> at [right_branch; left_branch; right_branch]   // a < c → a < b
                Deduce (weaken_or A eqc)                  |> apply_right
            ]
        let caseDB =  // b = c ∧ a = b
            lemma d ((D * B) ==> R) [
                leR                                       |> at_right
                Deduce' (reflex_implies B)                |> at [right_branch; right_branch]  // a = c → b = c
                Deduce (reflex_implies D)                 |> at [right_branch; right_branch]
                zero_or ltc                               |> apply_right
            ]
        lemma d (((a <=. b) * (b <=. c)) ==> R) [
            ax_ident d ((a <=. b) == (A + B))            |> at [left_branch; left_branch]
            ax_ident d ((b <=. c) == (C + D))            |> at [left_branch; right_branch]
            Taut (by_cases4 d A B C D R caseCA caseCB caseDA caseDB) |> apply
        ]

    /// `1 ≠ 0 ⇒ ((a + 1 ≤ a) = false)` — a successor is not at or below its predecessor. `a + 1 < a`
    /// is `pos(−1)`, which is false; `a + 1 = a` is `1 = 0` by Cancellation.
    let succ_not_le (d: OrderedDomain<'t>) (a: Term<'t>) : Theorem =
        let zero, one, a = d.Zero, d.One, d.Term a
        let a1 = a + one
        let nz1 = one != zero
        let one0 = one == zero
        lemma d (nz1 ==> ((a1 <=. a) == F)) [
            ax_ident d ((a1 <=. a) == ((a1 <. a) + (a1 == a)))   |> at [right_branch; left_branch]                 // (15.38)
            ax_ident d ((a1 <. a) == (pos_of (a - a1)))          |> at [right_branch; left_branch; left_branch]    // (15.36)
            sub_succ d a                                   |> at [right_branch; left_branch; left_branch; apply_unary]           // a − (a+1) → −1
            Deduce' (neg_one_not_pos d)                    |> apply_right                                                         // pos(−1) → false
            rev d (a + zero) a                            |> at [right_branch; left_branch; right_branch; right_branch]          // a → a + 0
            cancel_add d a one zero                        |> at [right_branch; left_branch; right_branch]                       // → 1 = 0
            commute_or F one0                              |> at [right_branch; left_branch]
            ident_or one0                                  |> at [right_branch; left_branch]
            ident_eq_false one0                            |> apply_right                                                         // (1 = 0) = F → ¬(1 = 0)
            Taut (reflex_implies nz1)                      |> apply
        ]

    /// `(a + 1) − a = 1`.
    let add_one_sub (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let zero, one, a = d.Zero, d.One, d.Term a
        ident d ((a + one) - a == one) [
            ax d ((a + one) - a) ((a + one) + -a)              |> at_left                        // (15.14)
            ax d ((a + one) + -a) (a + (one + -a))             |> at_left
            ax d (one + -a) ((-a) + one)                       |> at [left_branch; right_branch]
            rev d ((a + -a) + one) (a + ((-a) + one))          |> at_left
            ax d (a + -a) zero                                 |> at [left_branch; left_branch]  // (15.13)
            ax d (zero + one) one                              |> at_left
        ]

    /// `1 ≠ 0 ⇒ a ≤ a + 1` — `a < a + 1` is `pos.1`. NOT from Discreteness: this is what lets
    /// `Induction` derive Discreteness without going in a circle.
    let le_succ (d: OrderedDomain<'t>) (a: Term<'t>) : Theorem =
        let one, a = d.One, d.Term a
        let a1 = a + one
        lemma d ((one != d.Zero) ==> (a <=. a1)) [
            ax_ident d ((a <=. a1) == ((a <. a1) + (a == a1)))    |> at_right                                  // (15.38)
            ax_ident d ((a <. a1) == (pos_of (a1 - a)))           |> at [right_branch; left_branch]             // (15.36)
            add_one_sub d a                                        |> at [right_branch; left_branch; apply_unary]
            Establish (one_pos d)                                  |> apply                                      // pos.1
            Deduce (weaken_or (pos_of one) (a == a1))              |> apply_right
        ]

    /// `0 ≤ a ∧ 0 ≤ b ⇒ 0 ≤ a + b` — non-negatives are closed under `+`: Monotonicity puts `b` on
    /// both sides of `0 ≤ a`, and Transitivity chains `0 ≤ b ≤ a + b`.
    let nonneg_add (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let z = d.Zero
        let a, b = d.Term a, d.Term b
        lemma d (((z <=. a) * (z <=. b)) ==> (z <=. a + b)) [
            le_translate d z a b                          |> at [left_branch; left_branch]                 // 0 ≤ a → 0 + b ≤ a + b
            ax d (z + b) b                               |> at [left_branch; left_branch; left_branch]    // 0 + b → b
            commute_and (b <=. a + b) (z <=. b)           |> at_left
            Taut (le_trans d z b (a + b))                 |> apply
        ]

    (* ------------------------------------------------------------------------------------------
       More of §15.1: irreflexivity, antisymmetry (15.45), totality, and ≥ in terms of ≤
       ------------------------------------------------------------------------------------------ *)

    /// `(a = b) = (b = a)` — `eq_sym` both ways, by mutual implication.
    let eq_comm (th: Theory) (a: Term<'t>) (b: Term<'t>) : Rule =
        let ab, ba = eq_t a.Expr b.Expr, eq_t b.Expr a.Expr
        ident th (ab == ba) [
            mutual_implication' ab ba |> Commute                |> apply
            Taut (conj (eq_sym th a b) (eq_sym th b a))         |> apply
        ]

    /// `a − a = 0`.
    let sub_self (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let zero, a = d.Zero, d.Term a
        ident d (a - a == zero) [
            ax d (a - a) (a + -a)                              |> at_left      // (15.14)
            ax d (a + -a) zero                                 |> at_left      // (15.13)
        ]

    /// `−(b − a) = a − b`.
    let neg_sub (d: IntegralDomain<'t>) (b: Term<'t>) (a: Term<'t>) : Rule =
        let b, a = d.Term b, d.Term a
        ident d (-(b - a) == a - b) [
            ax d (b - a) (b + -a)                              |> at [left_branch; apply_unary]
            neg_add d b (-a)                                    |> at_left                        // (15.19)
            neg_neg d a                                         |> at [left_branch; right_branch] // (15.17)
            ax d ((-b) + a) (a + -b)                           |> at_left
            ax d (a - b) (a + -b)                              |> at_right
        ]

    /// `(b − a = 0) = (b = a)` — (15.15) and (15.17).
    let sub_eq_zero (d: IntegralDomain<'t>) (b: Term<'t>) (a: Term<'t>) : Rule =
        let zero = d.Zero
        let b, a = d.Term b, d.Term a
        ident d ((b - a == zero) == (b == a)) [
            ax d (b - a) (b + -a)                              |> at [left_branch; left_branch]
            add_eq_zero d b (-a)                                |> at_left                        // → b = −(−a)
            neg_neg d a                                         |> at [left_branch; right_branch]
        ]

    /// `(a < a) = false` — `pos(a − a)` is `pos.0` (15.32).
    let lt_irrefl (d: OrderedDomain<'t>) (a: Term<'t>) : Rule =
        let zero, a = d.Zero, d.Term a
        ident d ((a <. a) == F) [
            ax_ident d ((a <. a) == (pos_of (a - a)))          |> at_left                        // (15.36)
            sub_self d a                                        |> at [left_branch; apply_unary]
            ax_ident d ((pos_of zero) == F)                    |> at_left                        // (15.32)
        ]

    /// `(a < b ∧ b < a) = false` — (15.41a) would give `a < a`.
    let lt_asym (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let a, b = d.Term a, d.Term b
        let A = (a <. b) * (b <. a)
        let absurd =
            lemma d (A ==> F) [
                lt_irrefl d a |> Commute                        |> at_right                       // F → a < a
                Taut (lt_trans d a b a)                         |> apply
            ]
        ident d (A == F) [
            ident_eq_false A                                    |> apply                          // (A = F) → ¬A
            ident_implies_false_not A |> Commute                |> apply                          // ¬A → (A ⇒ F)
            Taut absurd                                         |> apply
        ]

    /// **(15.45) Antisymmetry**: `a ≤ b ∧ b ≤ a ⇒ a = b`. Of the four cases, `a < b ∧ b < a` is
    /// impossible (`lt_asym`) and the other three contain `a = b` or `b = a`.
    let le_antisym (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let a, b = d.Term a, d.Term b
        let A, B, C, D = (a <. b), (a == b), (b <. a), (b == a)
        let R = a == b
        let cCA = lemma d ((C * A) ==> R) [
                    commute_and C A                             |> at_left
                    lt_asym d a b                               |> at_left                        // → false
                    Taut (conseq_false R)                       |> apply
                  ]
        let cCB = lemma d ((C * B) ==> R) [
                    commute_and C B                             |> at_left
                    Taut (strengthen_and B C)                   |> apply
                  ]
        let cDA = lemma d ((D * A) ==> R) [ Deduce (eq_sym d b a) |> apply_right ]
        let cDB = lemma d ((D * B) ==> R) [ Deduce (reflex_implies B) |> apply_right ]
        lemma d (((a <=. b) * (b <=. a)) ==> R) [
            ax_ident d ((a <=. b) == (A + B))                  |> at [left_branch; left_branch]  // (15.38)
            ax_ident d ((b <=. a) == (C + D))                  |> at [left_branch; right_branch]
            Taut (by_cases4 d A B C D R cCA cCB cDA cDB)       |> apply
        ]

    /// (¬E ⇒ (p = ¬q)) ⇒ ((p ∨ E) ∨ (q ∨ E))
    let private glue_total = taut3 "od_total" (fun e p q -> (!!e ==> (p == !!q)) ==> ((p + e) + (q + e)))

    /// **Totality** `a ≤ b ∨ b ≤ a` — the part of (15.44) Trichotomy that ↓ and ↑ need. With
    /// `e = b − a`, the disjunction is `(pos.e ∨ e = 0) ∨ (pos(−e) ∨ e = 0)`, and (15.33) at `e` says
    /// exactly one of `pos.e`, `pos(−e)` holds when `e ≠ 0`.
    let le_total (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let zero = d.Zero
        let a, b = d.Term a, d.Term b
        let e = b - a
        let E0 = e == zero
        let ax33 = axiom d ((e != zero) ==> ((pos_of e) == !!(pos_of (-e))))
        lemma d ((a <=. b) + (b <=. a)) [
            ax_ident d ((a <=. b) == ((a <. b) + (a == b)))    |> at_left                        // (15.38)
            ax_ident d ((b <=. a) == ((b <. a) + (b == a)))    |> at_right
            ax_ident d ((a <. b) == (pos_of (b - a)))          |> at [left_branch; left_branch]  // (15.36)
            ax_ident d ((b <. a) == (pos_of (a - b)))          |> at [right_branch; left_branch]
            flip d (neg_sub d b a)                             |> at [right_branch; left_branch; apply_unary]  // a − b → −(b − a)
            eq_comm d a b                                      |> at [left_branch; right_branch]
            flip d (sub_eq_zero d b a)                         |> at [left_branch; right_branch]  // b = a → e = 0
            flip d (sub_eq_zero d b a)                         |> at [right_branch; right_branch]
            Taut (mp (in_theory d (glue_total E0 (pos_of e) (pos_of (-e)))) ax33) |> apply
        ]

    /// `(a ≥ b) = (b ≤ a)` — both are `pos(a − b) ∨ a = b`, by (15.36)–(15.39).
    let ge_le (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let a, b = d.Term a, d.Term b
        ident d ((a >=. b) == (b <=. a)) [
            ax_ident d ((a >=. b) == ((a >. b) + (a == b)))    |> at_left              // (15.39)
            ax_ident d ((a >. b) == (pos_of (a - b)))          |> at [left_branch; left_branch]  // (15.37)
            ax_ident d ((b <=. a) == ((b <. a) + (b == a)))    |> at_right             // (15.38)
            ax_ident d ((b <. a) == (pos_of (a - b)))          |> at [right_branch; left_branch] // (15.36)
            eq_comm d a b                                      |> at [left_branch; right_branch]
        ]

    (* ------------------------------------------------------------------------------------------
       The rest of §15.1: Transitivity (15.41b/c), Trichotomy (15.44), Antisymmetry (15.45) as an
       equivalence, and multiplication — (15.35) and Monotonicity (15.43).
       ------------------------------------------------------------------------------------------ *)

    /// `⊢ s` from an identity rule proved for `s`.
    let private thm_of (r: Rule) =
        match r with
        | Derive(_, p, _) -> Theorem(p.Stmt, p)
        | _ -> failwith "not a derived rule"

    /// `((A ∨ B) ∧ C) ⇒ R`, from its two cases `C ∧ A ⇒ R` and `C ∧ B ⇒ R` — the shape of a `≤`
    /// premise unfolded by (15.38) next to one other premise.
    let private by_cases2 (th: Theory) (A: Prop) (B: Prop) (C: Prop) (R: Prop) (cCA: Theorem) (cCB: Theorem) : Theorem =
        lemma th (((A + B) * C) ==> R) [
            commute_and (A + B) C                              |> at_left
            distrib_and_or C A B                               |> at_left
            case_analysis_1 (C * A) (C * B) R |> Commute       |> apply                          // (3.78)
            Taut cCA                                           |> at_left
            Taut cCB                                           |> at_right
            ident_and T                                        |> apply
        ]

    /// `(a > b) = (b < a)` — both are `pos(a − b)`, by (15.37) and (15.36).
    let gt_lt (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let a, b = d.Term a, d.Term b
        ident d ((a >. b) == (b <. a)) [
            ax_ident d ((a >. b) == (pos_of (a - b)))          |> at_left                        // (15.37)
            ax_ident d ((b <. a) == (pos_of (a - b)))          |> at_right                       // (15.36)
        ]

    /// **(15.41b) Transitivity**: `a ≤ b ∧ b < c ⇒ a < c` — (15.41a), or Substitution of `a = b`.
    let le_lt_trans (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Theorem =
        let a, b, c = d.Term a, d.Term b, d.Term c
        let A, B, C, R = (a <. b), (a == b), (b <. c), (a <. c)
        let cCA = lemma d ((C * A) ==> R) [
                    commute_and C A                            |> at_left
                    Taut (lt_trans d a b c)                    |> apply ]
        let cCB = lemma d ((C * B) ==> R) [
                    Deduce' (reflex_implies B)                 |> at [right_branch; left_branch]  // a < c → b < c
                    Deduce (reflex_implies C)                  |> apply_right ]
        lemma d (((a <=. b) * C) ==> R) [
            ax_ident d ((a <=. b) == (A + B))                  |> at [left_branch; left_branch]  // (15.38)
            Taut (by_cases2 d A B C R cCA cCB)                 |> apply
        ]

    /// **(15.41c) Transitivity**: `a < b ∧ b ≤ c ⇒ a < c`.
    let lt_le_trans (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Theorem =
        let a, b, c = d.Term a, d.Term b, d.Term c
        let A, C, D, R = (a <. b), (b <. c), (b == c), (a <. c)
        let cAD = lemma d ((A * D) ==> R) [
                    Deduce' (eq_sym d b c)                     |> at [right_branch; right_branch] // a < c → a < b
                    Deduce (reflex_implies A)                  |> apply_right ]
        lemma d ((A * (b <=. c)) ==> R) [
            ax_ident d ((b <=. c) == (C + D))                  |> at [left_branch; right_branch] // (15.38)
            commute_and A (C + D)                              |> at_left
            Taut (by_cases2 d C D A R (lt_trans d a b c) cAD)  |> apply
        ]

    /// `(a < b ∨ b = a) ∨ (b < a ∨ b = a)` — Totality, unfolded by (15.38): the cases of Trichotomy.
    let private lt_cover (d: OrderedDomain<'t>) (a: DomainTerm<'t>) (b: DomainTerm<'t>) : Theorem =
        let X, Y, W, Y' = (a <. b), (a == b), (b <. a), (b == a)
        lemma d ((X + Y') + (W + Y')) [
            eq_comm d b a                                      |> at [left_branch; right_branch] // b = a → a = b
            ax_ident d ((a <=. b) == (X + Y)) |> Commute       |> at_left                        // (15.38)
            ax_ident d ((b <=. a) == (W + Y')) |> Commute      |> at_right
            Taut (le_total d a b)                              |> apply
        ]

    /// `b = a ⇒ ¬(a < b)` — Substitution, then irreflexivity.
    let private eq_not_lt (d: OrderedDomain<'t>) (a: DomainTerm<'t>) (b: DomainTerm<'t>) : Theorem =
        lemma d ((b == a) ==> !!(a <. b)) [
            Deduce' (eq_sym d b a)                             |> at [right_branch; apply_unary; left_branch]  // a < b → b < b
            lt_irrefl d b                                      |> apply_right
            not_false                                          |> apply_right
        ]

    // The glue is kept to small formulas, each proved once per process by the ANF search: one glue
    // stating the whole of Trichotomy from its four premises took ~24 s to find.
    /// ((x ∨ y) ∨ (w ∨ y)) ⇒ (¬x ⇒ (w ∨ y))
    let private glue_cover = taut3 "od_cover" (fun x w y -> ((x + y) + (w + y)) ==> (!!x ==> (w + y)))
    /// ((x ∧ w) = F) ⇒ ((y ⇒ ¬x) ⇒ ((w ∨ y) ⇒ ¬x))
    let private glue_excl = taut3 "od_excl" (fun x w y -> ((x * w) == F) ==> ((y ==> !!x) ==> ((w + y) ==> !!x)))
    /// (¬x = (w ∨ y)) ⇒ ((y ⇒ ¬w) ⇒ ((x = y) = w)) — exactly one of three, as Gries' ≡ chain.
    let private glue_one = taut3 "od_one" (fun x w y -> (!!x == (w + y)) ==> ((y ==> !!w) ==> ((x == y) == w)))
    /// (y ⇒ ¬x) ⇒ ¬((x ∧ y) ∧ w)
    let private glue_not_all = taut3 "od_not_all" (fun x y w -> (y ==> !!x) ==> !!((x * y) * w))

    /// `¬(a < b) = (b < a ∨ b = a)`.
    let private not_lt_cases (d: OrderedDomain<'t>) (a: DomainTerm<'t>) (b: DomainTerm<'t>) : Theorem =
        let X, W, Y' = (a <. b), (b <. a), (b == a)
        let fwd = mp (in_theory d (glue_cover X W Y')) (lt_cover d a b)
        let bwd = mp (mp (in_theory d (glue_excl X W Y')) (thm_of (lt_asym d a b))) (eq_not_lt d a b)
        lemma d (!!X == (W + Y')) [
            mutual_implication' (!!X) (W + Y') |> Commute       |> apply
            Taut (conj fwd bwd)                                |> apply
        ]

    /// `¬(a < b) = (b ≤ a)`. `⇐`: `b < a` excludes `a < b` (asymmetry), and so does `b = a`
    /// (irreflexivity). `⇒`: Totality leaves `b ≤ a` when `a < b` fails.
    let not_lt (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let a, b = d.Term a, d.Term b
        let X, W, Y' = (a <. b), (b <. a), (b == a)
        ident d (!!X == (b <=. a)) [
            ax_ident d ((b <=. a) == (W + Y'))                 |> at_right                       // (15.38)
            Taut (not_lt_cases d a b)                          |> apply
        ]

    /// **(15.44) Trichotomy**: `(a < b ≡ a = b ≡ a > b) ∧ ¬(a < b ∧ a = b ∧ a > b)` — exactly one of
    /// the three holds. Totality covers them; asymmetry and irreflexivity exclude every pair.
    let trichotomy (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let a, b = d.Term a, d.Term b
        let X, Y, Z = (a <. b), (a == b), (a >. b)
        let W, Y' = (b <. a), (b == a)
        let y_not_w =
            lemma d (Y' ==> !!W) [
                Deduce' (reflex_implies Y')                    |> at [right_branch; apply_unary; left_branch]  // b < a → a < a
                lt_irrefl d a                                  |> apply_right
                not_false                                      |> apply_right
            ]
        let one = mp (mp (in_theory d (glue_one X W Y')) (not_lt_cases d a b)) y_not_w
        let not_all = mp (in_theory d (glue_not_all X Y' W)) (eq_not_lt d a b)
        let t = conj one not_all
        lemma d (((X == Y) == Z) * !!((X * Y) * Z)) [
            eq_comm d a b                                      |> at_left                        // a = b → b = a
            eq_comm d a b                                      |> at_right
            gt_lt d a b                                        |> at_left                        // a > b → b < a
            gt_lt d a b                                        |> at_right
            Taut t                                             |> apply
        ]

    /// **(15.45) Antisymmetry**, as Gries states it: `(a ≤ b ∧ b ≤ a) = (a = b)`. `⇒` is `le_antisym`;
    /// `⇐` is Substitution and Reflexivity (15.46).
    let le_antisym_eq (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let a, b = d.Term a, d.Term b
        let P, E = ((a <=. b) * (b <=. a)), (a == b)
        let bwd =
            lemma d (E ==> P) [
                Deduce' (reflex_implies E)                     |> at [right_branch; left_branch; left_branch]    // a ≤ b → b ≤ b
                Deduce' (reflex_implies E)                     |> at [right_branch; right_branch; right_branch]  // b ≤ a → b ≤ b
                le_refl d b                                    |> apply_right
                le_refl d b                                    |> apply_right
                ident_and T                                    |> apply_right
            ]
        ident d (P == E) [
            mutual_implication' P E |> Commute                 |> apply
            Taut (conj (le_antisym d a b) bwd)                 |> apply
        ]

    /// `0 < c ∧ a < b ⇒ a·c < b·c` — (15.43) left to right. `a < b` is `pos(b − a)` and `0 < c` is
    /// `pos.c`; their product `(b − a)·c` is `b·c − a·c` (15.29); and that is Multiplication (15.31).
    let lt_mul_pos (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Theorem =
        let zero, a, b, c = d.Zero, d.Term a, d.Term b, d.Term c
        lemma d (((zero <. c) * (a <. b)) ==> (a * c <. b * c)) [
            pos_lt d c |> Commute                              |> at [left_branch; left_branch]  // 0 < c → pos.c
            ax_ident d ((a <. b) == (pos_of (b - a)))          |> at [left_branch; right_branch] // (15.36)
            ax_ident d ((a * c <. b * c) == (pos_of (b * c - a * c))) |> at_right
            flip d (sub_mul d b a c)                           |> at [right_branch; apply_unary] // b·c − a·c → (b − a)·c
            commute_and (pos_of c) (pos_of (b - a))            |> at_left
            // and that is Multiplication (15.31).
        ]

    /// `0 < c ∧ a ≤ b ⇒ a·c ≤ b·c` — `lt_mul_pos`, or Substitution of `a = b`.
    let le_mul_pos (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Theorem =
        let zero, a, b, c = d.Zero, d.Term a, d.Term b, d.Term c
        let P, X, Y = (zero <. c), (a <. b), (a == b)
        let ac, bc = a * c, b * c
        let R = ac <=. bc
        let cX = lemma d ((P * X) ==> R) [
                    ax_ident d (R == ((ac <. bc) + (ac == bc)))  |> at_right                     // (15.38)
                    Establish (lt_mul_pos d a b c)               |> apply                        // a·c < b·c
                    Deduce (weaken_or (ac <. bc) (ac == bc))     |> apply_right ]
        let cY = lemma d ((P * Y) ==> R) [
                    Deduce' (reflex_implies Y)                   |> at [right_branch; left_branch; left_branch]  // a·c → b·c
                    le_refl d bc                                 |> apply_right ]
        lemma d ((P * (a <=. b)) ==> R) [
            ax_ident d ((a <=. b) == (X + Y))                  |> at [left_branch; right_branch] // (15.38)
            commute_and P (X + Y)                              |> at_left
            Taut (by_cases2 d X Y P R cX cY)                   |> apply
        ]

    /// `0 < c ∧ a·c < b·c ⇒ a < b` — (15.43) right to left, by contraposition: `¬(a < b)` is
    /// `b ≤ a`, which gives `b·c ≤ a·c`, which is `¬(a·c < b·c)`.
    let lt_mul_cancel (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Theorem =
        let zero, a, b, c = d.Zero, d.Term a, d.Term b, d.Term c
        let P, X = (zero <. c), (a <. b)
        let ac, bc = a * c, b * c
        let M = ac <. bc
        lemma d ((P * M) ==> X) [
            shunt' P M X                                       |> apply                          // P ⇒ (M ⇒ X)
            def_implies_contr M X                              |> at_right                       // P ⇒ (¬X ⇒ ¬M)
            not_lt d a b                                       |> at [right_branch; left_branch] // ¬(a < b) → b ≤ a
            not_lt d ac bc                                     |> at [right_branch; right_branch]
            shunt' P (b <=. a) (bc <=. ac) |> Commute          |> apply
            Taut (le_mul_pos d b a c)                          |> apply
        ]

    /// (p ∧ x ⇒ m) ⇒ ((p ∧ m ⇒ x) ⇒ (p ⇒ (x = m)))
    let private glue_iff = taut3 "od_iff" (fun p x m -> ((p * x) ==> m) ==> (((p * m) ==> x) ==> (p ==> (x == m))))

    /// **(15.43) Monotonicity**: `0 < c ⇒ (a < b ≡ a·c < b·c)`.
    let lt_mul (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Theorem =
        let zero, a', b', c' = d.Zero, d.Term a, d.Term b, d.Term c
        let P, X, M = (zero <. c'), (a' <. b'), (a' * c' <. b' * c')
        mp (mp (in_theory d (glue_iff P X M)) (lt_mul_pos d a b c)) (lt_mul_cancel d a b c)

    /// **(15.35)**: `pos.a ⇒ (pos.b ≡ pos(a·b))` — (15.43) at `a, b, c := 0, b, a`, since `pos.x` is
    /// `0 < x` (15.40) and `0·a = 0` (15.9).
    let pos_mul (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let zero, a, b = d.Zero, d.Term a, d.Term b
        lemma d ((pos_of a) ==> ((pos_of b) == (pos_of (a * b)))) [
            pos_lt d a                                         |> at_left
            pos_lt d b                                         |> at [right_branch; left_branch]
            pos_lt d (a * b)                                   |> at [right_branch; right_branch]
            ax d (a * b) (b * a)                               |> at [right_branch; right_branch; right_branch]
            flip d (zero_mul_left d a)                         |> at [right_branch; right_branch; left_branch]   // 0 → 0·a
            Taut (lt_mul d zero b a)                           |> apply
        ]

    /// `0 ≤ c ∧ a ≤ b ⇒ c·a ≤ c·b` — `le_mul_pos` when `0 < c`; both sides are `0` when `0 = c`.
    let le_mul_nonneg (d: OrderedDomain<'t>) (c: Term<'t>) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let zero, c, a, b = d.Zero, d.Term c, d.Term a, d.Term b
        let L, P, Z = (a <=. b), (zero <. c), (zero == c)
        let R = c * a <=. c * b
        let cP = lemma d ((L * P) ==> R) [
                    ax d (c * a) (a * c)                       |> at [right_branch; left_branch]
                    ax d (c * b) (b * c)                       |> at [right_branch; right_branch]
                    commute_and L P                            |> at_left
                    Taut (le_mul_pos d a b c)                  |> apply ]
        let cZ = lemma d ((L * Z) ==> R) [
                    Deduce' (eq_sym d zero c)                  |> at [right_branch; left_branch; left_branch]    // c → 0
                    Deduce' (eq_sym d zero c)                  |> at [right_branch; right_branch; left_branch]
                    zero_mul_left d a                          |> at [right_branch; left_branch]
                    zero_mul_left d b                          |> at [right_branch; right_branch]
                    le_refl d zero                             |> apply_right ]
        lemma d (((zero <=. c) * L) ==> R) [
            ax_ident d ((zero <=. c) == (P + Z))               |> at [left_branch; left_branch]  // (15.38)
            Taut (by_cases2 d P Z L R cP cZ)                   |> apply
        ]

    /// `(a ≤ b) = (−b ≤ −a)` — Monotonicity (15.42) by `(−a) + (−b)`.
    let le_neg (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let zero, a, b = d.Zero, d.Term a, d.Term b
        let e = (-a) + (-b)
        ident d ((a <=. b) == (-b <=. -a)) [
            le_translate d a b e                               |> at_left                        // a + e ≤ b + e
            rev d ((a + -a) + -b) (a + e)                      |> at [left_branch; left_branch]
            ax d (a + -a) zero                                 |> at [left_branch; left_branch; left_branch]   // (15.13)
            ax d (zero + -b) (-b)                              |> at [left_branch; left_branch]
            ax d e ((-b) + (-a))                               |> at [left_branch; right_branch; right_branch]
            rev d ((b + -b) + -a) (b + ((-b) + (-a)))          |> at [left_branch; right_branch]
            ax d (b + -b) zero                                 |> at [left_branch; right_branch; left_branch]
            ax d (zero + -a) (-a)                              |> at [left_branch; right_branch]
        ]

    /// `0 ≤ a ⇒ −a ≤ 0` — translate by `−a`.
    let neg_le_zero (d: OrderedDomain<'t>) (a: Term<'t>) : Theorem =
        let zero, a = d.Zero, d.Term a
        lemma d ((zero <=. a) ==> (-a <=. zero)) [
            le_translate d zero a (-a)                          |> at_left                         // 0 + −a ≤ a + −a
            ax d (zero + -a) (-a)                              |> at [left_branch; left_branch]
            ax d (a + -a) zero                                 |> at [left_branch; right_branch]  // (15.13)
            Taut (reflex_implies (-a <=. zero))                 |> apply
        ]

    /// `a ≤ 0 ⇒ 0 ≤ −a` — translate by `−a`.
    let zero_le_neg (d: OrderedDomain<'t>) (a: Term<'t>) : Theorem =
        let zero, a = d.Zero, d.Term a
        lemma d ((a <=. zero) ==> (zero <=. -a)) [
            le_translate d a zero (-a)                          |> at_left                         // a + −a ≤ 0 + −a
            ax d (a + -a) zero                                 |> at [left_branch; left_branch]   // (15.13)
            ax d (zero + -a) (-a)                              |> at [left_branch; right_branch]
            Taut (reflex_implies (zero <=. -a))                 |> apply
        ]

    /// `c ≤ 0 ∧ a ≤ b ⇒ c·b ≤ c·a` — multiplying by a non-positive reverses `≤`: `−c` is non-negative,
    /// so `(−c)·a ≤ (−c)·b`, which is `−(c·a) ≤ −(c·b)` (15.22), which is `c·b ≤ c·a` (`le_neg`).
    let le_mul_nonpos (d: OrderedDomain<'t>) (c: Term<'t>) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let zero, c, a, b = d.Zero, d.Term c, d.Term a, d.Term b
        lemma d (((c <=. zero) * (a <=. b)) ==> (c * b <=. c * a)) [
            le_neg d (c * b) (c * a)                            |> at_right                        // → −(c·a) ≤ −(c·b)
            flip d (neg_mul d c a)                              |> at [right_branch; left_branch]  // −(c·a) → (−c)·a
            flip d (neg_mul d c b)                              |> at [right_branch; right_branch]
            Establish (zero_le_neg d c)                         |> apply                           // 0 ≤ −c
            Deduce (le_mul_nonneg d (-c) a b)                   |> apply_right
        ]

    (* ------------------------------------------------------------------------------------------
       (15.47) INDIRECT EQUALITY: `a = b ≡ (∀z |: z ≤ a ≡ z ≤ b)`. Gries' device for proving the
       min/max laws, as a derived rule: the caller proves `(z ≤ a) = (z ≤ b)` for a z it is handed —
       a schema METAVARIABLE — and the rule instantiates that at `z := a` and `z := b`
       (`InstantiateTerms`, sound because z is unconstrained), reflexivity turns the instances into
       `a ≤ b` and `b ≤ a`, and Antisymmetry (15.45) gives `a = b`. No quantifier is needed.
       ------------------------------------------------------------------------------------------ *)

    let private next_mv = ref 0
    let private metavar<'t> (name: string) : Expr<'t> =
        let i = System.Threading.Interlocked.Increment &next_mv.contents
        Expr.Var(Var(sprintf "%s%s_%d" Tactics.metavar_prefix name i, typeof<'t>)) |> Expr.Cast<'t>

    let private indirect (d: OrderedDomain<'t>) (below: bool) (a: Term<'t>) (b: Term<'t>) (body: DomainTerm<'t> -> Rule) : Rule =
        let a, b = d.Term a, d.Term b
        let z = d.Term (metavar<'t> "z")
        let t = thm_of (body z)
        let expected = if below then (z <=. a) == (z <=. b) else (a <=. z) == (b <=. z)
        if not (sequal (expand t.Stmt) (expand expected.Expr)) then
            failwithf "indirect equality: the body proves\n  %s\nbut it must prove\n  %s" (d.PrintFormula t.Stmt) (d.PrintFormula expected.Expr)
        let at (e: DomainTerm<'t>) = Ident (Tactics.InstantiateTerms t [ z.Expr.Raw, e.Expr.Raw ])
        // below: (a ≤ a) = (a ≤ b) and (b ≤ a) = (b ≤ b);  above: (a ≤ a) = (b ≤ a) and (a ≤ b) = (b ≤ b)
        let ab = if below then lemma d (a <=. b) [ at a |> Commute |> apply; le_refl d a |> apply ]
                 else lemma d (a <=. b) [ at b |> apply; le_refl d b |> apply ]
        let ba = if below then lemma d (b <=. a) [ at b |> apply; le_refl d b |> apply ]
                 else lemma d (b <=. a) [ at a |> Commute |> apply; le_refl d a |> apply ]
        Ident (mp (le_antisym d a b) (conj ab ba))

    /// **(15.47)** `a = b`, from a proof of `(z ≤ a) = (z ≤ b)` at a metavariable `z`.
    let below_eq (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (body: DomainTerm<'t> -> Rule) : Rule = indirect d true a b body

    /// (15.47)'s counterpart: `a = b`, from a proof of `(a ≤ z) = (b ≤ z)` at a metavariable `z`.
    let above_eq (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (body: DomainTerm<'t> -> Rule) : Rule = indirect d false a b body

    (* ------------------------------------------------------------------------------------------
       §15.2 Minimum ↓ and maximum ↑ (Gries pp. 311–313), from Definition (15.53) only.
       ------------------------------------------------------------------------------------------ *)

    /// `⊢ p` from `⊢ p ∧ q`, in `th`.
    let private conj_left (th: Theory) (t: Theorem) : Theorem =
        match t.Stmt with
        | And(p, q) ->
            let p, q = Prop(Expr.Cast<bool> p), Prop(Expr.Cast<bool> q)
            mp (lemma th ((p * q) ==> p) [ Taut (strengthen_and p q) |> apply ]) t
        | _ -> failwith "conj_left: not a conjunction"
    /// `⊢ q` from `⊢ p ∧ q`, in `th`.
    let private conj_right (th: Theory) (t: Theorem) : Theorem =
        match t.Stmt with
        | And(p, q) ->
            let p, q = Prop(Expr.Cast<bool> p), Prop(Expr.Cast<bool> q)
            mp (lemma th ((p * q) ==> q) [ commute_and p q |> at_left; Taut (strengthen_and q p) |> apply ]) t
        | _ -> failwith "conj_right: not a conjunction"

    /// (15.53) for ↓, as a rewrite: `(z ≤ x↓y) = (z ≤ x ∧ z ≤ y)`. An axiom instance.
    let min_def (d: OrderedDomain<'t>) (z: Term<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let z, x, y = d.Term z, d.Term x, d.Term y
        ax_ident d ((z <=. min_of x y) == ((z <=. x) * (z <=. y)))

    /// (15.53) for ↑, in the `≤` form: `(x↑y ≤ z) = (x ≤ z ∧ y ≤ z)` — the axiom, through `ge_le`.
    let max_le (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) (z: Term<'t>) : Rule =
        let x, y, z = d.Term x, d.Term y, d.Term z
        let M = max_of x y
        ident d ((M <=. z) == ((x <=. z) * (y <=. z))) [
            ge_le d z M |> Commute                              |> at_left                        // M ≤ z → z ≥ M
            ax_ident d ((z >=. M) == ((z >=. x) * (z >=. y)))  |> at_left                        // (15.53)
            ge_le d z x                                         |> at [left_branch; left_branch]
            ge_le d z y                                         |> at [left_branch; right_branch]
        ]

    /// **(15.54) Symmetry**: `x↓y = y↓x`. Gries' proof: by (15.47), the body is (15.53) and ∧'s symmetry.
    let min_sym (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let x, y = d.Term x, d.Term y
        below_eq d (min_of x y) (min_of y x) (fun z ->
            ident d ((z <=. min_of x y) == (z <=. min_of y x)) [
                min_def d z x y                                 |> at_left
                min_def d z y x                                 |> at_right
                commute_and (z <=. x) (z <=. y)                 |> at_left
            ])

    /// **(15.54) Symmetry**: `x↑y = y↑x`.
    let max_sym (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let x, y = d.Term x, d.Term y
        above_eq d (max_of x y) (max_of y x) (fun z ->
            ident d ((max_of x y <=. z) == (max_of y x <=. z)) [
                max_le d x y z                                  |> at_left
                max_le d y x z                                  |> at_right
                commute_and (x <=. z) (y <=. z)                 |> at_left
            ])

    /// **(15.55) Associativity**: `(x↓y)↓w = x↓(y↓w)`.
    let min_assoc (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) (w: Term<'t>) : Rule =
        let x, y, w = d.Term x, d.Term y, d.Term w
        below_eq d (min_of (min_of x y) w) (min_of x (min_of y w)) (fun z ->
            ident d ((z <=. min_of (min_of x y) w) == (z <=. min_of x (min_of y w))) [
                min_def d z (min_of x y) w                      |> at_left
                min_def d z x y                                 |> at [left_branch; left_branch]
                min_def d z x (min_of y w)                      |> at_right
                min_def d z y w                                 |> at [right_branch; right_branch]
                right_assoc_and (z <=. x) (z <=. y) (z <=. w)   |> at_left
            ])

    /// **(15.55) Associativity**: `(x↑y)↑w = x↑(y↑w)`.
    let max_assoc (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) (w: Term<'t>) : Rule =
        let x, y, w = d.Term x, d.Term y, d.Term w
        above_eq d (max_of (max_of x y) w) (max_of x (max_of y w)) (fun z ->
            ident d ((max_of (max_of x y) w <=. z) == (max_of x (max_of y w) <=. z)) [
                max_le d (max_of x y) w z                       |> at_left
                max_le d x y z                                  |> at [left_branch; left_branch]
                max_le d x (max_of y w) z                       |> at_right
                max_le d y w z                                  |> at [right_branch; right_branch]
                right_assoc_and (x <=. z) (y <=. z) (w <=. z)   |> at_left
            ])

    /// **(15.56) Idempotency**: `x↓x = x`.
    let min_idem (d: OrderedDomain<'t>) (x: Term<'t>) : Rule =
        let x = d.Term x
        below_eq d (min_of x x) x (fun z ->
            ident d ((z <=. min_of x x) == (z <=. x)) [
                min_def d z x x                                 |> at_left
                idemp_and (z <=. x)                             |> at_left
            ])

    /// **(15.56) Idempotency**: `x↑x = x`.
    let max_idem (d: OrderedDomain<'t>) (x: Term<'t>) : Rule =
        let x = d.Term x
        above_eq d (max_of x x) x (fun z ->
            ident d ((max_of x x <=. z) == (x <=. z)) [
                max_le d x x z                                  |> at_left
                idemp_and (x <=. z)                             |> at_left
            ])

    /// **(15.57)**: `x↓y ≤ x ∧ x↓y ≤ y` — (15.53) at `z := x↓y`, and Reflexivity (15.46).
    let min_le (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let x, y = d.Term x, d.Term y
        let m = min_of x y
        lemma d ((m <=. x) * (m <=. y)) [
            min_def d m x y |> Commute                          |> apply                          // → x↓y ≤ x↓y
            le_refl d m                                         |> apply
        ]

    /// **(15.57)**: `x ≤ x↑y ∧ y ≤ x↑y` (Gries: `x↑y ≥ x ∧ x↑y ≥ y`).
    let max_ge (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let x, y = d.Term x, d.Term y
        let M = max_of x y
        lemma d ((x <=. M) * (y <=. M)) [
            max_le d x y M |> Commute                           |> apply                          // → x↑y ≤ x↑y
            le_refl d M                                         |> apply
        ]

    /// **(15.58)**: `(x ≤ y) = (x↓y = x)`. `⇒`: `x ≤ x↓y` by (15.53), `x↓y ≤ x` by (15.57), and
    /// Antisymmetry; `⇐`: `x↓y ≤ y` (15.57) with `x↓y` for `x`.
    let min_eq (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let x, y = d.Term x, d.Term y
        let m = min_of x y
        let H, E = (x <=. y), (m == x)
        let fwd =
            lemma d (H ==> E) [
                Establish (lemma d (H ==> (x <=. m)) [
                    min_def d x x y                             |> at_right                       // → x ≤ x ∧ x ≤ y
                    le_refl d x                                 |> at [right_branch; left_branch]
                    commute_and T H                             |> at_right
                    ident_and H                                 |> at_right
                    Taut (reflex_implies H)                     |> apply
                ])                                              |> apply
                Establish (lemma d (H ==> (m <=. x)) [ Taut (conj_left d (min_le d x y)) |> apply_right ]) |> apply
                Deduce (le_antisym d m x)                       |> apply_right
            ]
        let bwd =
            lemma d (E ==> H) [
                Deduce' (eq_sym d m x)                         |> at [right_branch; left_branch]   // x → x↓y
                Taut (conj_right d (min_le d x y))             |> apply_right
            ]
        ident d (H == E) [
            mutual_implication' H E |> Commute                  |> apply
            Taut (conj fwd bwd)                                 |> apply
        ]

    /// **(15.58)**: `(y ≤ x) = (x↑y = x)` (Gries: `x ≥ y ≡ x↑y = x`).
    let max_eq (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let x, y = d.Term x, d.Term y
        let M = max_of x y
        let H, E = (y <=. x), (M == x)
        let fwd =
            lemma d (H ==> E) [
                Establish (lemma d (H ==> (M <=. x)) [
                    max_le d x y x                              |> at_right                       // → x ≤ x ∧ y ≤ x
                    le_refl d x                                 |> at [right_branch; left_branch]
                    commute_and T H                             |> at_right
                    ident_and H                                 |> at_right
                    Taut (reflex_implies H)                     |> apply
                ])                                              |> apply
                Establish (lemma d (H ==> (x <=. M)) [ Taut (conj_left d (max_ge d x y)) |> apply_right ]) |> apply
                Deduce (le_antisym d M x)                       |> apply_right
            ]
        let bwd =
            lemma d (E ==> H) [
                Deduce' (eq_sym d M x)                         |> at [right_branch; right_branch]  // x → x↑y
                Taut (conj_right d (max_ge d x y))             |> apply_right
            ]
        ident d (H == E) [
            mutual_implication' H E |> Commute                  |> apply
            Taut (conj fwd bwd)                                 |> apply
        ]

    /// **(15.59)**: `x↓y = x ∨ x↓y = y` — (15.58) both ways round, and Totality.
    let min_cases (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let x, y = d.Term x, d.Term y
        let m = min_of x y
        lemma d ((m == x) + (m == y)) [
            min_eq d x y |> Commute                             |> at_left                        // → x ≤ y
            min_sym d x y                                       |> at [right_branch; left_branch] // x↓y → y↓x
            min_eq d y x |> Commute                             |> at_right                       // → y ≤ x
            Taut (le_total d x y)                               |> apply
        ]

    /// **(15.59)**: `x↑y = x ∨ x↑y = y`.
    let max_cases (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let x, y = d.Term x, d.Term y
        let M = max_of x y
        lemma d ((M == x) + (M == y)) [
            max_eq d x y |> Commute                             |> at_left                        // → y ≤ x
            max_sym d x y                                       |> at [right_branch; left_branch] // x↑y → y↑x
            max_eq d y x |> Commute                             |> at_right                       // → x ≤ y
            commute_or (y <=. x) (x <=. y)                      |> apply
            Taut (le_total d x y)                               |> apply
        ]

    /// `(c + m) + (−c) = m`.
    let add_neg_cancel (d: IntegralDomain<'t>) (c: Term<'t>) (m: Term<'t>) : Rule =
        let zero = d.Zero
        let c, m = d.Term c, d.Term m
        ident d ((c + m) + -c == m) [
            ax d (c + m) (m + c)                               |> at [left_branch; left_branch]
            ax d ((m + c) + -c) (m + (c + -c))                 |> at_left
            ax d (c + -c) zero                                 |> at [left_branch; right_branch]  // (15.13)
            ax d (m + zero) m                                  |> at_left
        ]

    /// `(z + (−c)) + c = z`.
    let neg_add_cancel (d: IntegralDomain<'t>) (z: Term<'t>) (c: Term<'t>) : Rule =
        let zero = d.Zero
        let z, c = d.Term z, d.Term c
        ident d ((z + -c) + c == z) [
            ax d ((z + -c) + c) (z + ((-c) + c))               |> at_left
            left_inverse d c                                    |> at [left_branch; right_branch]
            ax d (z + zero) z                                  |> at_left
        ]

    /// **(15.60) Distributivity**: `c + (x↓y) = (c + x)↓(c + y)` — by (15.47): translate `z ≤ c + m`
    /// by `−c`, unfold (15.53), translate each conjunct back by `c`.
    let min_add (d: OrderedDomain<'t>) (c: Term<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let c, x, y = d.Term c, d.Term x, d.Term y
        let m = min_of x y
        below_eq d (c + m) (min_of (c + x) (c + y)) (fun z ->
            let zc = z + -c
            ident d ((z <=. c + m) == (z <=. min_of (c + x) (c + y))) [
                min_def d z (c + x) (c + y)                     |> at_right
                le_translate d z (c + m) (-c)                   |> at_left                         // z + −c ≤ (c + m) + −c
                add_neg_cancel d c m                            |> at [left_branch; right_branch]
                min_def d zc x y                                |> at_left
                le_translate d zc x c                           |> at [left_branch; left_branch]   // (z + −c) + c ≤ x + c
                neg_add_cancel d z c                            |> at [left_branch; left_branch; left_branch]
                ax d (x + c) (c + x)                           |> at [left_branch; left_branch; right_branch]
                le_translate d zc y c                           |> at [left_branch; right_branch]
                neg_add_cancel d z c                            |> at [left_branch; right_branch; left_branch]
                ax d (y + c) (c + y)                           |> at [left_branch; right_branch; right_branch]
            ])

    /// **(15.60) Distributivity**: `c + (x↑y) = (c + x)↑(c + y)`.
    let max_add (d: OrderedDomain<'t>) (c: Term<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let c, x, y = d.Term c, d.Term x, d.Term y
        let M = max_of x y
        above_eq d (c + M) (max_of (c + x) (c + y)) (fun z ->
            let zc = z + -c
            ident d ((c + M <=. z) == (max_of (c + x) (c + y) <=. z)) [
                max_le d (c + x) (c + y) z                      |> at_right
                le_translate d (c + M) z (-c)                   |> at_left                         // (c + M) + −c ≤ z + −c
                add_neg_cancel d c M                            |> at [left_branch; left_branch]
                max_le d x y zc                                 |> at_left
                le_translate d x zc c                           |> at [left_branch; left_branch]   // x + c ≤ (z + −c) + c
                neg_add_cancel d z c                            |> at [left_branch; left_branch; right_branch]
                ax d (x + c) (c + x)                           |> at [left_branch; left_branch; left_branch]
                le_translate d y zc c                           |> at [left_branch; right_branch]
                neg_add_cancel d z c                            |> at [left_branch; right_branch; right_branch]
                ax d (y + c) (c + y)                           |> at [left_branch; right_branch; left_branch]
            ])

    (* (15.58) as conditional rewrites, for Deduce' under a hypothesis. *)

    /// `x ≤ y ⇒ x↓y = x`.
    let private min_left (d: OrderedDomain<'t>) (x: DomainTerm<'t>) (y: DomainTerm<'t>) : Theorem =
        lemma d ((x <=. y) ==> (min_of x y == x)) [
            min_eq d x y                                        |> at_left
            Taut (reflex_implies (min_of x y == x))             |> apply
        ]
    /// `y ≤ x ⇒ x↓y = y`.
    let private min_right (d: OrderedDomain<'t>) (x: DomainTerm<'t>) (y: DomainTerm<'t>) : Theorem =
        lemma d ((y <=. x) ==> (min_of x y == y)) [
            min_sym d x y                                       |> at [right_branch; left_branch]  // x↓y → y↓x
            min_eq d y x |> Commute                             |> at_right
            Taut (reflex_implies (y <=. x))                     |> apply
        ]
    /// `y ≤ x ⇒ x↑y = x`.
    let private max_left (d: OrderedDomain<'t>) (x: DomainTerm<'t>) (y: DomainTerm<'t>) : Theorem =
        lemma d ((y <=. x) ==> (max_of x y == x)) [
            max_eq d x y                                        |> at_left
            Taut (reflex_implies (max_of x y == x))             |> apply
        ]
    /// `x ≤ y ⇒ x↑y = y`.
    let private max_right (d: OrderedDomain<'t>) (x: DomainTerm<'t>) (y: DomainTerm<'t>) : Theorem =
        lemma d ((x <=. y) ==> (max_of x y == y)) [
            max_sym d x y                                       |> at [right_branch; left_branch]  // x↑y → y↑x
            max_eq d y x |> Commute                             |> at_right
            Taut (reflex_implies (x <=. y))                     |> apply
        ]

    /// (p ⇒ r) ⇒ ((q ⇒ r) ⇒ ((p ∨ q) ⇒ r))
    let private glue_or = taut3 "od_or" (fun p q r -> (p ==> r) ==> ((q ==> r) ==> ((p + q) ==> r)))

    /// `⊢ q` from `x ≤ y ⇒ q` and `y ≤ x ⇒ q` — Totality as a case split, Gries' proof of (15.59).
    let private by_totality (d: OrderedDomain<'t>) (x: DomainTerm<'t>) (y: DomainTerm<'t>) (q: Prop) (s1: Theorem) (s2: Theorem) : Theorem =
        mp (mp (mp (in_theory d (glue_or (x <=. y) (y <=. x) q)) s1) s2) (le_total d x y)

    /// `h ⇒ (c ⇒ e)` from `h ∧ c ⇒ e` (shunting, 4.4).
    let private shunted (th: Theory) (h: Prop) (c: Prop) (e: Prop) (t: Theorem) : Theorem =
        lemma th (h ==> (c ==> e)) [
            shunt' h c e |> Commute                             |> apply
            Taut t                                              |> apply
        ]

    /// **(15.61) Distributivity**: `0 ≤ c ⇒ c·(x↓y) = (c·x)↓(c·y)` (Gries writes `c ≥ 0`; `ge_le` is the
    /// bridge, and `≥` statements are given in `≤` form throughout). By cases on Totality: when `x ≤ y`,
    /// `x↓y = x` and `c·x ≤ c·y`, so both sides are `c·x` (15.58); symmetrically when `y ≤ x`.
    let mul_min (d: OrderedDomain<'t>) (c: Term<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let zero, c, x, y = d.Zero, d.Term c, d.Term x, d.Term y
        let cx, cy = c * x, c * y
        let C = zero <=. c
        let E = c * min_of x y == min_of cx cy
        let case1 =
            lemma d (((x <=. y) * C) ==> E) [
                Deduce' (min_left d x y)                        |> at [right_branch; left_branch]   // x↓y → x
                Establish (le_mul_nonneg d c x y)               |> apply                             // c·x ≤ c·y
                Deduce' (min_left d cx cy)                      |> at [right_branch; right_branch]  // (c·x)↓(c·y) → c·x
                refl_true d cx                                  |> apply_right
            ]
        let case2 =
            lemma d (((y <=. x) * C) ==> E) [
                Deduce' (min_right d x y)                       |> at [right_branch; left_branch]   // x↓y → y
                Establish (le_mul_nonneg d c y x)               |> apply                             // c·y ≤ c·x
                Deduce' (min_right d cx cy)                     |> at [right_branch; right_branch]  // (c·x)↓(c·y) → c·y
                refl_true d cy                                  |> apply_right
            ]
        by_totality d x y (C ==> E) (shunted d (x <=. y) C E case1) (shunted d (y <=. x) C E case2)

    /// **(15.61) Distributivity**: `0 ≤ c ⇒ c·(x↑y) = (c·x)↑(c·y)`.
    let mul_max (d: OrderedDomain<'t>) (c: Term<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let zero, c, x, y = d.Zero, d.Term c, d.Term x, d.Term y
        let cx, cy = c * x, c * y
        let C = zero <=. c
        let E = c * max_of x y == max_of cx cy
        let case1 =
            lemma d (((x <=. y) * C) ==> E) [
                Deduce' (max_right d x y)                       |> at [right_branch; left_branch]   // x↑y → y
                Establish (le_mul_nonneg d c x y)               |> apply                             // c·x ≤ c·y
                Deduce' (max_right d cx cy)                     |> at [right_branch; right_branch]  // (c·x)↑(c·y) → c·y
                refl_true d cy                                  |> apply_right
            ]
        let case2 =
            lemma d (((y <=. x) * C) ==> E) [
                Deduce' (max_left d x y)                        |> at [right_branch; left_branch]   // x↑y → x
                Establish (le_mul_nonneg d c y x)               |> apply                             // c·y ≤ c·x
                Deduce' (max_left d cx cy)                      |> at [right_branch; right_branch]  // (c·x)↑(c·y) → c·x
                refl_true d cx                                  |> apply_right
            ]
        by_totality d x y (C ==> E) (shunted d (x <=. y) C E case1) (shunted d (y <=. x) C E case2)

    /// **(15.62) Distributivity**: `c ≤ 0 ⇒ c·(x↑y) = (c·x)↓(c·y)` — a non-positive factor swaps ↑
    /// and ↓, because it reverses `≤` (`le_mul_nonpos`).
    let mul_max_nonpos (d: OrderedDomain<'t>) (c: Term<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let zero, c, x, y = d.Zero, d.Term c, d.Term x, d.Term y
        let cx, cy = c * x, c * y
        let C = c <=. zero
        let E = c * max_of x y == min_of cx cy
        let case1 =
            lemma d (((x <=. y) * C) ==> E) [
                Deduce' (max_right d x y)                       |> at [right_branch; left_branch]   // x↑y → y
                Establish (le_mul_nonpos d c x y)               |> apply                             // c·y ≤ c·x
                Deduce' (min_right d cx cy)                     |> at [right_branch; right_branch]  // (c·x)↓(c·y) → c·y
                refl_true d cy                                  |> apply_right
            ]
        let case2 =
            lemma d (((y <=. x) * C) ==> E) [
                Deduce' (max_left d x y)                        |> at [right_branch; left_branch]   // x↑y → x
                Establish (le_mul_nonpos d c y x)               |> apply                             // c·x ≤ c·y
                Deduce' (min_left d cx cy)                      |> at [right_branch; right_branch]  // (c·x)↓(c·y) → c·x
                refl_true d cx                                  |> apply_right
            ]
        by_totality d x y (C ==> E) (shunted d (x <=. y) C E case1) (shunted d (y <=. x) C E case2)

    /// **(15.62) Distributivity**: `c ≤ 0 ⇒ c·(x↓y) = (c·x)↑(c·y)`.
    let mul_min_nonpos (d: OrderedDomain<'t>) (c: Term<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let zero, c, x, y = d.Zero, d.Term c, d.Term x, d.Term y
        let cx, cy = c * x, c * y
        let C = c <=. zero
        let E = c * min_of x y == max_of cx cy
        let case1 =
            lemma d (((x <=. y) * C) ==> E) [
                Deduce' (min_left d x y)                        |> at [right_branch; left_branch]   // x↓y → x
                Establish (le_mul_nonpos d c x y)               |> apply                             // c·y ≤ c·x
                Deduce' (max_left d cx cy)                      |> at [right_branch; right_branch]  // (c·x)↑(c·y) → c·x
                refl_true d cx                                  |> apply_right
            ]
        let case2 =
            lemma d (((y <=. x) * C) ==> E) [
                Deduce' (min_right d x y)                       |> at [right_branch; left_branch]   // x↓y → y
                Establish (le_mul_nonpos d c y x)               |> apply                             // c·x ≤ c·y
                Deduce' (max_right d cx cy)                     |> at [right_branch; right_branch]  // (c·x)↑(c·y) → c·y
                refl_true d cy                                  |> apply_right
            ]
        by_totality d x y (C ==> E) (shunted d (x <=. y) C E case1) (shunted d (y <=. x) C E case2)

    /// `x ≤ y ⇒ −y ≤ −x` — `le_neg` as an implication.
    let private neg_le (d: OrderedDomain<'t>) (x: DomainTerm<'t>) (y: DomainTerm<'t>) : Theorem =
        lemma d ((x <=. y) ==> (-y <=. -x)) [
            le_neg d x y                                        |> at_left
            Taut (reflex_implies (-y <=. -x))                   |> apply
        ]

    /// `−(x↓y) = (−x)↑(−y)` — (15.62) at `c := −1`, proved directly and unconditionally: by cases on
    /// Totality, with `le_neg` for `le_mul_nonpos`.
    let neg_min (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let x, y = d.Term x, d.Term y
        let nx, ny = -x, -y
        let E = -(min_of x y) == max_of nx ny
        let case1 =
            lemma d ((x <=. y) ==> E) [
                Deduce' (min_left d x y)                        |> at [right_branch; left_branch]   // −(x↓y) → −x
                Establish (neg_le d x y)                        |> apply                             // −y ≤ −x
                Deduce' (max_left d nx ny)                      |> at [right_branch; right_branch]  // (−x)↑(−y) → −x
                refl_true d nx                                  |> apply_right
            ]
        let case2 =
            lemma d ((y <=. x) ==> E) [
                Deduce' (min_right d x y)                       |> at [right_branch; left_branch]   // −(x↓y) → −y
                Establish (neg_le d y x)                        |> apply                             // −x ≤ −y
                Deduce' (max_right d nx ny)                     |> at [right_branch; right_branch]  // (−x)↑(−y) → −y
                refl_true d ny                                  |> apply_right
            ]
        Ident (by_totality d x y E case1 case2)

    /// `−(x↑y) = (−x)↓(−y)` — (15.62) at `c := −1`.
    let neg_max (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let x, y = d.Term x, d.Term y
        let nx, ny = -x, -y
        let E = -(max_of x y) == min_of nx ny
        let case1 =
            lemma d ((x <=. y) ==> E) [
                Deduce' (max_right d x y)                       |> at [right_branch; left_branch]   // −(x↑y) → −y
                Establish (neg_le d x y)                        |> apply                             // −y ≤ −x
                Deduce' (min_right d nx ny)                     |> at [right_branch; right_branch]  // (−x)↓(−y) → −y
                refl_true d ny                                  |> apply_right
            ]
        let case2 =
            lemma d ((y <=. x) ==> E) [
                Deduce' (max_left d x y)                        |> at [right_branch; left_branch]   // −(x↑y) → −x
                Establish (neg_le d y x)                        |> apply                             // −x ≤ −y
                Deduce' (min_left d nx ny)                      |> at [right_branch; right_branch]  // (−x)↓(−y) → −x
                refl_true d nx                                  |> apply_right
            ]
        Ident (by_totality d x y E case1 case2)

    (* ------------------------------------------------------------------------------------------
       §15.3 Absolute value: (15.71) abs.x = x↑−x, and its properties from those of ↑.
       ------------------------------------------------------------------------------------------ *)

    /// (15.71) as a rewrite: `abs.x = x↑−x`. An axiom instance.
    let abs_def (d: OrderedDomain<'t>) (x: Term<'t>) : Rule =
        let x = d.Term x
        ax_ident d (abs_of x == max_of x (-x))

    /// **(15.72)**: `abs.x = abs(−x)` — both are `x↑−x`, by (15.17) and Symmetry (15.54).
    let abs_neg (d: OrderedDomain<'t>) (x: Term<'t>) : Rule =
        let x = d.Term x
        ident d (abs_of x == abs_of (-x)) [
            abs_def d x                                         |> at_left                         // → x↑−x
            abs_def d (-x)                                      |> at_right                        // → (−x)↑−(−x)
            neg_neg d x                                         |> at [right_branch; right_branch]
            max_sym d (-x) x                                    |> at_right
        ]

    /// `x ≤ abs.x ∧ −x ≤ abs.x` — (15.57) for `x↑−x`.
    let abs_ge (d: OrderedDomain<'t>) (x: Term<'t>) : Theorem =
        let x = d.Term x
        lemma d ((x <=. abs_of x) * (-x <=. abs_of x)) [
            abs_def d x                                         |> at [left_branch; right_branch]
            abs_def d x                                         |> at [right_branch; right_branch]
            Taut (max_ge d x (-x))                              |> apply
        ]

    /// `0 ≤ abs.x` — by Totality, `0 ≤ x` or `x ≤ 0`; then `x ≤ abs.x` or `−x ≤ abs.x`, and Transitivity.
    let abs_nonneg (d: OrderedDomain<'t>) (x: Term<'t>) : Theorem =
        let z, x = d.Zero, d.Term x
        let A = abs_of x
        let goal = z <=. A
        let pos_case =
            lemma d ((z <=. x) ==> goal) [
                Establish (lemma d ((z <=. x) ==> (x <=. A)) [ Taut (conj_left d (abs_ge d x)) |> apply_right ]) |> apply
                Deduce (le_trans d z x A)                       |> apply_right
            ]
        let neg_case =
            lemma d ((x <=. z) ==> goal) [
                Establish (zero_le_neg d x)                     |> apply                           // 0 ≤ −x
                Establish (lemma d ((x <=. z) ==> (-x <=. A)) [ Taut (conj_right d (abs_ge d x)) |> apply_right ]) |> apply
                Deduce (le_trans d z (-x) A)                    |> apply_right
            ]
        let both =
            lemma d (((z <=. x) + (x <=. z)) ==> goal) [
                case_analysis_1 (z <=. x) (x <=. z) goal |> Commute |> apply                       // (3.78)
                Taut pos_case                                   |> at_left
                Taut neg_case                                   |> at_right
                ident_and T                                     |> apply
            ]
        mp both (le_total d z x)

    /// **(15.74)**: `abs(abs.x) = abs.x` — `abs(A) = A↑−A`, and `−A ≤ A` since `0 ≤ A` (15.58).
    let abs_abs (d: OrderedDomain<'t>) (x: Term<'t>) : Rule =
        let zero, x = d.Zero, d.Term x
        let A = abs_of x
        let negA_le_A =
            mp (lemma d ((zero <=. A) ==> (-A <=. A)) [
                    Establish (neg_le_zero d A)                 |> apply                           // −A ≤ 0
                    Deduce (le_trans d (-A) zero A)             |> apply_right
                ]) (abs_nonneg d x)
        ident d (abs_of A == A) [
            abs_def d A                                         |> at_left                         // → A↑−A = A
            max_eq d A (-A) |> Commute                          |> apply                           // → −A ≤ A
            Taut negA_le_A                                      |> apply
        ]

    /// `a ≤ b ∧ c ≤ e ⇒ a + c ≤ b + e` — Monotonicity twice, and Transitivity.
    let le_add (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) (e: Term<'t>) : Theorem =
        let a, b, c, e = d.Term a, d.Term b, d.Term c, d.Term e
        lemma d (((a <=. b) * (c <=. e)) ==> (a + c <=. b + e)) [
            Establish (lemma d ((a <=. b) ==> (a + c <=. b + c)) [
                le_translate d a b c                            |> at_left
                Taut (reflex_implies (a + c <=. b + c))         |> apply
            ])                                                  |> apply
            Establish (lemma d ((c <=. e) ==> (b + c <=. b + e)) [
                le_translate d c e b                            |> at_left                         // c + b ≤ e + b
                ax d (c + b) (b + c)                           |> at [left_branch; left_branch]
                ax d (e + b) (b + e)                           |> at [left_branch; right_branch]
                Taut (reflex_implies (b + c <=. b + e))         |> apply
            ])                                                  |> apply
            Deduce (le_trans d (a + c) (b + c) (b + e))         |> apply_right
        ]

    /// **(15.73) Triangle inequality**: `abs(x + y) ≤ abs.x + abs.y`. `abs(x+y) = (x+y)↑−(x+y)`, so by
    /// (15.53) it is `x + y ≤ S ∧ −x + −y ≤ S`, each `le_add` of the halves of `abs_ge`.
    let triangle (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let x, y = d.Term x, d.Term y
        let S = abs_of x + abs_of y
        let gx, gy = abs_ge d x, abs_ge d y
        let plus = mp (le_add d x (abs_of x) y (abs_of y)) (conj (conj_left d gx) (conj_left d gy))
        let minus = mp (le_add d (-x) (abs_of x) (-y) (abs_of y)) (conj (conj_right d gx) (conj_right d gy))
        lemma d (abs_of (x + y) <=. S) [
            abs_def d (x + y)                                   |> at_left
            max_le d (x + y) (-(x + y)) S                       |> apply                           // (15.53)
            neg_add d x y                                       |> at [right_branch; left_branch]  // (15.19)
            Taut plus                                           |> at_left
            Taut minus                                          |> at_right
            ident_and T                                         |> apply
        ]

    /// `0 ≤ x ⇒ abs.x = x` — `−x ≤ 0 ≤ x`, so `x↑−x = x` (15.58).
    let abs_of_nonneg (d: OrderedDomain<'t>) (x: Term<'t>) : Theorem =
        let zero, x = d.Zero, d.Term x
        lemma d ((zero <=. x) ==> (abs_of x == x)) [
            abs_def d x                                         |> at [right_branch; left_branch]  // → x↑−x
            Establish (neg_le_zero d x)                         |> apply                            // −x ≤ 0
            Establish (le_trans d (-x) zero x)                  |> apply                            // −x ≤ x
            Deduce' (max_left d x (-x))                         |> apply_right                      // x↑−x → x
            refl_true d x                                       |> apply_right
        ]

    /// `x ≤ 0 ⇒ abs.x = −x` — `x ≤ 0 ≤ −x`, so `x↑−x = −x`.
    let abs_of_nonpos (d: OrderedDomain<'t>) (x: Term<'t>) : Theorem =
        let zero, x = d.Zero, d.Term x
        lemma d ((x <=. zero) ==> (abs_of x == -x)) [
            abs_def d x                                         |> at [right_branch; left_branch]  // → x↑−x
            Establish (zero_le_neg d x)                         |> apply                            // 0 ≤ −x
            Establish (le_trans d x zero (-x))                  |> apply                            // x ≤ −x
            Deduce' (max_right d x (-x))                        |> apply_right                      // x↑−x → −x
            refl_true d (-x)                                    |> apply_right
        ]

    /// **(15.75)**: `abs(x·y) = abs.x · abs.y`. By cases on the sign of `x`: when `0 ≤ x`, `abs.x·abs.y`
    /// is `x·(y↑−y)`, which is `(x·y)↑(x·(−y))` by Distributivity (15.61); when `x ≤ 0` it is
    /// `(−x)·(y↑−y)`, the same with `−x`, and Symmetry (15.54).
    let abs_mul (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let zero, x, y = d.Zero, d.Term x, d.Term y
        let xy = x * y
        let E = abs_of xy == abs_of x * abs_of y
        let pos_case =
            lemma d ((zero <=. x) ==> E) [
                Deduce' (abs_of_nonneg d x)                     |> at [right_branch; right_branch]                 // abs.x → x
                abs_def d y                                     |> at [right_branch; right_branch; right_branch]   // abs.y → y↑−y
                Deduce' (mul_max d x y (-y))                    |> at [right_branch; right_branch]                 // (15.61)
                mul_neg d x y                                   |> at [right_branch; right_branch; right_branch]   // x·(−y) → −(x·y)
                abs_def d xy                                    |> at [right_branch; left_branch]
                refl_true d (max_of xy (-xy))                   |> apply_right
            ]
        let neg_case =
            lemma d ((x <=. zero) ==> E) [
                Deduce' (abs_of_nonpos d x)                     |> at [right_branch; right_branch]                 // abs.x → −x
                abs_def d y                                     |> at [right_branch; right_branch; right_branch]
                Establish (zero_le_neg d x)                     |> apply                                            // 0 ≤ −x
                Deduce' (mul_max d (-x) y (-y))                 |> at [right_branch; right_branch]                 // (15.61)
                neg_mul d x y                                   |> at [right_branch; right_branch; left_branch]    // (−x)·y → −(x·y)
                neg_mul_neg d x y                               |> at [right_branch; right_branch; right_branch]   // (−x)·(−y) → x·y
                max_sym d (-xy) xy                              |> at [right_branch; right_branch]
                abs_def d xy                                    |> at [right_branch; left_branch]
                refl_true d (max_of xy (-xy))                   |> apply_right
            ]
        Ident (by_totality d zero x E pos_case neg_case)

    /// **(15.76)**: `−(abs.x + abs.y) ≤ x + y ≤ abs.x + abs.y`. The right half is the triangle
    /// inequality's `le_add`; the left half is the same with `−abs.x ≤ x` (`le_neg` of `−x ≤ abs.x`).
    let abs_bounds (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let x, y = d.Term x, d.Term y
        let A, B = abs_of x, abs_of y
        let gx, gy = abs_ge d x, abs_ge d y
        let below (v: DomainTerm<'t>) (V: DomainTerm<'t>) (g: Theorem) =   // −V ≤ v, from −v ≤ V
            lemma d (-V <=. v) [
                flip d (neg_neg d v)                            |> at_right                        // v → −(−v)
                le_neg d (-v) V |> Commute                      |> apply                           // → −v ≤ V
                Taut (conj_right d g)                           |> apply
            ]
        let upper = mp (le_add d x A y B) (conj (conj_left d gx) (conj_left d gy))
        let lower =
            lemma d (-(A + B) <=. x + y) [
                neg_add d A B                                   |> at_left                         // (15.19)
                Taut (mp (le_add d (-A) x (-B) y) (conj (below x A gx) (below y B gy))) |> apply
            ]
        conj lower upper
