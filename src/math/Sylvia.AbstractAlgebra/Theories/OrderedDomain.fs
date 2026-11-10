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
/// ordered domain does NOT give is discreteness — the rationals are one — so `Integers` keeps
/// `(x ≤ y+1) = (x ≤ y ∨ x = y+1)` as an axiom until well-ordering (15.49) is in.
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
        let th = d :> Theory
        let a, b, c = d.Term a, d.Term b, d.Term c
        // (b + c) − (a + b) = c − a, by (15.28): it is (b + c) + a = (a + b) + c
        let inner =
            ident th ((b + c) - (a + b) == c - a) [
                sub_eq_sub d (b + c) (a + b) c a          |> apply
                ax th ((b + c) + a) (b + (c + a))         |> at_left
                ax th (a + b) (b + a)                     |> at [right_branch; left_branch]
                ax th ((b + a) + c) (b + (a + c))         |> at_right
                ax th (a + c) (c + a)                     |> at [right_branch; right_branch]
            ]
        ident th ((b - a) + (c - b) == c - a) [
            sub_add_sub d b a c b                         |> at_left     // (15.25)
            inner                                         |> at_left
        ]

    /// `(b + e) − (a + e) = b − a` — the arithmetic step of Monotonicity (15.42).
    let sub_translate (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (e: Term<'t>) : Rule =
        let th = d :> Theory
        let a, b, e = d.Term a, d.Term b, d.Term e
        ident th ((b + e) - (a + e) == b - a) [
            sub_eq_sub d (b + e) (a + e) b a              |> apply       // (15.28)
            ax th ((b + e) + a) (b + (e + a))             |> at_left
            ax th ((a + e) + b) (b + (a + e))             |> at_right
            ax th (a + e) (e + a)                         |> at [right_branch; right_branch]
        ]

    /// `a − (a + 1) = −1`.
    let sub_succ (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let th, zero, one, a = d :> Theory, d.Zero, d.One, d.Term a
        ident th (a - (a + one) == -one) [
            ax th (a - (a + one)) (a + -(a + one))        |> at_left                        // (15.14)
            neg_add d a one                               |> at [left_branch; right_branch] // (15.19)
            rev th ((a + -a) + -one) (a + (-a + -one))    |> at_left
            ax th (a + -a) zero                           |> at [left_branch; left_branch]  // (15.13)
            ax th (zero + -one) (-one)                    |> at_left
        ]

    (* Propositional glue *)

    // The case analyses below are three-atom tautologies. Proving one by the ANF search costs ~1.5 s,
    // and an `Induction` theory needs (15.34) for `succ_not_le` — so they are proved ONCE per process,
    // in the logic, as schemas over boolean metavariables (`Tactics.Schema.p3`, sound by uniform
    // substitution), and each instance is re-homed in the caller's theory by one `Taut` step.
    let private taut3 (name: string) (f: Prop -> Prop -> Prop -> Prop) =
        Tactics.Schema.p3 name (fun x y z -> let s = f x y z in lemma prop_calculus s (autoproof_anf s).Steps)
    /// (nz ⇒ (p = ¬q)) ⇒ ((nz ∧ ¬p) ⇒ q)
    let private glue_neg = taut3 "od_neg" (fun nz p q -> (nz ==> (p == !!q)) ==> ((nz * !!p) ==> q))
    /// (p ⇒ q) ⇒ (((nz ∧ ¬p) ⇒ q) ⇒ (nz ⇒ q))
    let private glue_cases = taut3 "od_cases" (fun p q nz -> (p ==> q) ==> (((nz * !!p) ==> q) ==> (nz ==> q)))
    /// (nz ⇒ p) ⇒ ((nz ⇒ (p = ¬q)) ⇒ (nz ⇒ (q = false)))
    let private glue_false = taut3 "od_false" (fun nz p q -> (nz ==> p) ==> ((nz ==> (p == !!q)) ==> (nz ==> (q == F))))
    /// A logic theorem as a theorem of `th`, so that `mp` can combine it with `th`'s own.
    let private in_theory (th: Theory) (t: Theorem) : Theorem = lemma th (Prop(Expr.Cast<bool> t.Stmt)) [ Taut t |> apply ]

    (* Theorems for ordered domains (Gries 15.34 – 15.46) *)

    /// **(15.34)**: `b ≠ 0 ⇒ pos(b·b)`. Gries' proof, by cases on `pos.b`: if positive, (15.31); if
    /// not, `−b` is positive by (15.33), and `b·b = (−b)·(−b)` (15.23).
    let sq_pos (d: OrderedDomain<'t>) (b: Term<'t>) : Theorem =
        let th, b = d :> Theory, d.Term b
        let nb = -b
        let nz = b != d.Zero
        let sq (x: DomainTerm<'t>) =
            lemma th ((pos_of x) ==> (pos_of (x * x))) [
                idemp_and (pos_of x) |> Commute |> at_left  // pos.x → pos.x ∧ pos.x
                // and that is Multiplication (15.31).
            ]
        let q = pos_of (b * b)
        let ax33 = axiom th (nz ==> ((pos_of b) == !!(pos_of nb)))
        let neg_pos = mp (in_theory th (glue_neg nz (pos_of b) (pos_of nb))) ax33
        let not_pos =
            lemma th ((nz * !!(pos_of b)) ==> q) [
                flip th (neg_mul_neg d b b)     |> at [right_branch; apply_unary]   // b·b → (−b)·(−b)
                Establish neg_pos               |> apply                            // pos(−b)
                Deduce (sq nb)                  |> apply_right
            ]
        mp (mp (in_theory th (glue_cases (pos_of b) q nz)) (sq b)) not_pos

    /// `1 ≠ 0 ⇒ pos.1` — the corollary of (15.34) Gries draws: `1 = 1·1`. The premise is needed: the
    /// one-element ring satisfies every axiom above, and in it nothing is positive.
    let one_pos (d: OrderedDomain<'t>) : Theorem =
        let th, one = d :> Theory, d.One
        lemma th ((one != d.Zero) ==> (pos_of one)) [
            rev th (one * one) one                  |> at [right_branch; apply_unary]   // 1 → 1·1
            Taut (sq_pos d one)                     |> apply
        ]

    /// `1 ≠ 0 ⇒ (pos(−1) = false)` — so −1 is not positive: (15.33) at `b := 1`.
    let neg_one_not_pos (d: OrderedDomain<'t>) : Theorem =
        let th, one = d :> Theory, d.One
        let nz1, p1, pm1 = (one != d.Zero), pos_of one, pos_of (-one)
        let ax33 = axiom th (nz1 ==> (p1 == !!pm1))
        mp (mp (in_theory th (glue_false nz1 p1 pm1)) (one_pos d)) ax33

    /// **(15.40) Positive elements**: `pos.b = 0 < b`.
    let pos_lt (d: OrderedDomain<'t>) (b: Term<'t>) : Rule =
        let th, zero, b = d :> Theory, d.Zero, d.Term b
        ident th ((pos_of b) == (zero <. b)) [
            ax_ident th ((zero <. b) == (pos_of (b - zero)))    |> at_right   // (15.36)
            sub_zero d b                                        |> at [right_branch; apply_unary]
        ]

    /// **(15.41a) Transitivity**: `a < b ∧ b < c ⇒ a < c` — Gries' proof: Less, Addition, arithmetic.
    let lt_trans (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Theorem =
        let th = d :> Theory
        let a, b, c = d.Term a, d.Term b, d.Term c
        lemma th (((a <. b) * (b <. c)) ==> (a <. c)) [
            ax_ident th ((a <. b) == (pos_of (b - a)))  |> at [left_branch; left_branch]     // (15.36)
            ax_ident th ((b <. c) == (pos_of (c - b)))  |> at [left_branch; right_branch]
            ax_ident th ((a <. c) == (pos_of (c - a)))  |> at_right
            flip th (sub_chain d a b c)                 |> at [right_branch; apply_unary]    // c − a → (b − a) + (c − b)
            // and that is Addition (15.30).
        ]

    /// **(15.42) Monotonicity**: `(a < b) = (a + e < b + e)`.
    let lt_translate (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (e: Term<'t>) : Rule =
        let th = d :> Theory
        let a, b, e = d.Term a, d.Term b, d.Term e
        ident th ((a <. b) == (a + e <. b + e)) [
            ax_ident th ((a <. b) == (pos_of (b - a)))                       |> at_left    // (15.36)
            ax_ident th ((a + e <. b + e) == (pos_of ((b + e) - (a + e))))   |> at_right
            sub_translate d a b e                                            |> at [right_branch; apply_unary]
        ]

    /// `(a + e = b + e) = (a = b)` — (15.8) Cancellation on the right.
    let cancel_add_right (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (e: Term<'t>) : Rule =
        let th = d :> Theory
        let a, b, e = d.Term a, d.Term b, d.Term e
        ident th ((a + e == b + e) == (a == b)) [
            ax th (a + e) (e + a)                    |> at [left_branch; left_branch]
            ax th (b + e) (e + b)                    |> at [left_branch; right_branch]
            cancel_add d e a b                       |> at_left
        ]

    /// **(15.46) Reflexivity**: `(a ≤ a) = true`.
    let le_refl (d: OrderedDomain<'t>) (a: Term<'t>) : Rule =
        let th, a = d :> Theory, d.Term a
        ident th ((a <=. a) == T) [
            ax_ident th ((a <=. a) == ((a <. a) + (a == a)))    |> at_left      // (15.38)
            Taut (axiom th (a == a))                            |> at [left_branch; right_branch]
            zero_or (a <. a)                                    |> at_left
        ]

    /// Monotonicity for `≤`: `(a ≤ b) = (a + e ≤ b + e)` — (15.42) and Cancellation, through (15.38).
    let le_translate (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (e: Term<'t>) : Rule =
        let th = d :> Theory
        let a, b, e = d.Term a, d.Term b, d.Term e
        let ae, be = a + e, b + e
        ident th ((a <=. b) == (ae <=. be)) [
            ax_ident th ((a <=. b) == ((a <. b) + (a == b)))        |> at_left               // (15.38)
            ax_ident th ((ae <=. be) == ((ae <. be) + (ae == be)))  |> at_right
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
        let th = d :> Theory
        let a, b, c = d.Term a, d.Term b, d.Term c
        let A, B, C, D = (a <. b), (a == b), (b <. c), (b == c)
        let R = a <=. c
        let ltc, eqc = (a <. c), (a == c)
        let leR = ax_ident th (R == (ltc + eqc))                                          // (15.38)
        let caseCA =  // b < c ∧ a < b
            lemma th ((C * A) ==> R) [
                leR                                       |> at_right
                Establish (lt_trans d a b c)              |> apply                        // a < c
                Deduce (weaken_or ltc eqc)                |> apply_right
            ]
        let caseCB =  // b < c ∧ a = b
            lemma th ((C * B) ==> R) [
                leR                                       |> at_right
                Deduce' (reflex_implies B)                |> at [right_branch; left_branch]   // a < c → b < c
                Deduce (weaken_or C eqc)                  |> apply_right
            ]
        let caseDA =  // b = c ∧ a < b
            lemma th ((D * A) ==> R) [
                leR                                       |> at_right
                // Addressed at c ITSELF: `at [right_branch; left_branch]` rewrote the first `c` in `a < c`,
                // which is inside `a` when `a` contains `c` (le_trans at a := −A, c := A failed).
                Deduce' (eq_sym th b c)                   |> at [right_branch; left_branch; right_branch]   // a < c → a < b
                Deduce (weaken_or A eqc)                  |> apply_right
            ]
        let caseDB =  // b = c ∧ a = b
            lemma th ((D * B) ==> R) [
                leR                                       |> at_right
                Deduce' (reflex_implies B)                |> at [right_branch; right_branch]  // a = c → b = c
                Deduce (reflex_implies D)                 |> at [right_branch; right_branch]
                zero_or ltc                               |> apply_right
            ]
        lemma th (((a <=. b) * (b <=. c)) ==> R) [
            ax_ident th ((a <=. b) == (A + B))            |> at [left_branch; left_branch]
            ax_ident th ((b <=. c) == (C + D))            |> at [left_branch; right_branch]
            Taut (by_cases4 th A B C D R caseCA caseCB caseDA caseDB) |> apply
        ]

    /// `1 ≠ 0 ⇒ ((a + 1 ≤ a) = false)` — a successor is not at or below its predecessor. `a + 1 < a`
    /// is `pos(−1)`, which is false; `a + 1 = a` is `1 = 0` by Cancellation.
    let succ_not_le (d: OrderedDomain<'t>) (a: Term<'t>) : Theorem =
        let th, zero, one, a = d :> Theory, d.Zero, d.One, d.Term a
        let a1 = a + one
        let nz1 = one != zero
        let one0 = one == zero
        lemma th (nz1 ==> ((a1 <=. a) == F)) [
            ax_ident th ((a1 <=. a) == ((a1 <. a) + (a1 == a)))   |> at [right_branch; left_branch]                 // (15.38)
            ax_ident th ((a1 <. a) == (pos_of (a - a1)))          |> at [right_branch; left_branch; left_branch]    // (15.36)
            sub_succ d a                                   |> at [right_branch; left_branch; left_branch; apply_unary]           // a − (a+1) → −1
            Deduce' (neg_one_not_pos d)                    |> apply_right                                                         // pos(−1) → false
            rev th (a + zero) a                            |> at [right_branch; left_branch; right_branch; right_branch]          // a → a + 0
            cancel_add d a one zero                        |> at [right_branch; left_branch; right_branch]                       // → 1 = 0
            commute_or F one0                              |> at [right_branch; left_branch]
            ident_or one0                                  |> at [right_branch; left_branch]
            ident_eq_false one0                            |> apply_right                                                         // (1 = 0) = F → ¬(1 = 0)
            Taut (reflex_implies nz1)                      |> apply
        ]

    /// `(a + 1) − a = 1`.
    let add_one_sub (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let th, zero, one, a = d :> Theory, d.Zero, d.One, d.Term a
        ident th ((a + one) - a == one) [
            ax th ((a + one) - a) ((a + one) + -a)              |> at_left                        // (15.14)
            ax th ((a + one) + -a) (a + (one + -a))             |> at_left
            ax th (one + -a) ((-a) + one)                       |> at [left_branch; right_branch]
            rev th ((a + -a) + one) (a + ((-a) + one))          |> at_left
            ax th (a + -a) zero                                 |> at [left_branch; left_branch]  // (15.13)
            ax th (zero + one) one                              |> at_left
        ]

    /// `1 ≠ 0 ⇒ a ≤ a + 1` — `a < a + 1` is `pos.1`. NOT from Discreteness: this is what lets
    /// `Induction` derive Discreteness without going in a circle.
    let le_succ (d: OrderedDomain<'t>) (a: Term<'t>) : Theorem =
        let th, one, a = d :> Theory, d.One, d.Term a
        let a1 = a + one
        lemma th ((one != d.Zero) ==> (a <=. a1)) [
            ax_ident th ((a <=. a1) == ((a <. a1) + (a == a1)))    |> at_right                                  // (15.38)
            ax_ident th ((a <. a1) == (pos_of (a1 - a)))           |> at [right_branch; left_branch]             // (15.36)
            add_one_sub d a                                        |> at [right_branch; left_branch; apply_unary]
            Establish (one_pos d)                                  |> apply                                      // pos.1
            Deduce (weaken_or (pos_of one) (a == a1))              |> apply_right
        ]

    /// `0 ≤ a ∧ 0 ≤ b ⇒ 0 ≤ a + b` — non-negatives are closed under `+`: Monotonicity puts `b` on
    /// both sides of `0 ≤ a`, and Transitivity chains `0 ≤ b ≤ a + b`.
    let nonneg_add (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let th, z = d :> Theory, d.Zero
        let a, b = d.Term a, d.Term b
        lemma th (((z <=. a) * (z <=. b)) ==> (z <=. a + b)) [
            le_translate d z a b                          |> at [left_branch; left_branch]                 // 0 ≤ a → 0 + b ≤ a + b
            ax th (z + b) b                               |> at [left_branch; left_branch; left_branch]    // 0 + b → b
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
        let th, zero, a = d :> Theory, d.Zero, d.Term a
        ident th (a - a == zero) [
            ax th (a - a) (a + -a)                              |> at_left      // (15.14)
            ax th (a + -a) zero                                 |> at_left      // (15.13)
        ]

    /// `−(b − a) = a − b`.
    let neg_sub (d: IntegralDomain<'t>) (b: Term<'t>) (a: Term<'t>) : Rule =
        let th = d :> Theory
        let b, a = d.Term b, d.Term a
        ident th (-(b - a) == a - b) [
            ax th (b - a) (b + -a)                              |> at [left_branch; apply_unary]
            neg_add d b (-a)                                    |> at_left                        // (15.19)
            neg_neg d a                                         |> at [left_branch; right_branch] // (15.17)
            ax th ((-b) + a) (a + -b)                           |> at_left
            ax th (a - b) (a + -b)                              |> at_right
        ]

    /// `(b − a = 0) = (b = a)` — (15.15) and (15.17).
    let sub_eq_zero (d: IntegralDomain<'t>) (b: Term<'t>) (a: Term<'t>) : Rule =
        let th, zero = d :> Theory, d.Zero
        let b, a = d.Term b, d.Term a
        ident th ((b - a == zero) == (b == a)) [
            ax th (b - a) (b + -a)                              |> at [left_branch; left_branch]
            add_eq_zero d b (-a)                                |> at_left                        // → b = −(−a)
            neg_neg d a                                         |> at [left_branch; right_branch]
        ]

    /// `(a < a) = false` — `pos(a − a)` is `pos.0` (15.32).
    let lt_irrefl (d: OrderedDomain<'t>) (a: Term<'t>) : Rule =
        let th, zero, a = d :> Theory, d.Zero, d.Term a
        ident th ((a <. a) == F) [
            ax_ident th ((a <. a) == (pos_of (a - a)))          |> at_left                        // (15.36)
            sub_self d a                                        |> at [left_branch; apply_unary]
            ax_ident th ((pos_of zero) == F)                    |> at_left                        // (15.32)
        ]

    /// `(a < b ∧ b < a) = false` — (15.41a) would give `a < a`.
    let lt_asym (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let th = d :> Theory
        let a, b = d.Term a, d.Term b
        let A = (a <. b) * (b <. a)
        let absurd =
            lemma th (A ==> F) [
                lt_irrefl d a |> Commute                        |> at_right                       // F → a < a
                Taut (lt_trans d a b a)                         |> apply
            ]
        ident th (A == F) [
            ident_eq_false A                                    |> apply                          // (A = F) → ¬A
            ident_implies_false_not A |> Commute                |> apply                          // ¬A → (A ⇒ F)
            Taut absurd                                         |> apply
        ]

    /// **(15.45) Antisymmetry**: `a ≤ b ∧ b ≤ a ⇒ a = b`. Of the four cases, `a < b ∧ b < a` is
    /// impossible (`lt_asym`) and the other three contain `a = b` or `b = a`.
    let le_antisym (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let th = d :> Theory
        let a, b = d.Term a, d.Term b
        let A, B, C, D = (a <. b), (a == b), (b <. a), (b == a)
        let R = a == b
        let cCA = lemma th ((C * A) ==> R) [
                    commute_and C A                             |> at_left
                    lt_asym d a b                               |> at_left                        // → false
                    Taut (conseq_false R)                       |> apply
                  ]
        let cCB = lemma th ((C * B) ==> R) [
                    commute_and C B                             |> at_left
                    Taut (strengthen_and B C)                   |> apply
                  ]
        let cDA = lemma th ((D * A) ==> R) [ Deduce (eq_sym th b a) |> apply_right ]
        let cDB = lemma th ((D * B) ==> R) [ Deduce (reflex_implies B) |> apply_right ]
        lemma th (((a <=. b) * (b <=. a)) ==> R) [
            ax_ident th ((a <=. b) == (A + B))                  |> at [left_branch; left_branch]  // (15.38)
            ax_ident th ((b <=. a) == (C + D))                  |> at [left_branch; right_branch]
            Taut (by_cases4 th A B C D R cCA cCB cDA cDB)       |> apply
        ]

    /// (¬E ⇒ (p = ¬q)) ⇒ ((p ∨ E) ∨ (q ∨ E))
    let private glue_total = taut3 "od_total" (fun e p q -> (!!e ==> (p == !!q)) ==> ((p + e) + (q + e)))

    /// **Totality** `a ≤ b ∨ b ≤ a` — the part of (15.44) Trichotomy that ↓ and ↑ need. With
    /// `e = b − a`, the disjunction is `(pos.e ∨ e = 0) ∨ (pos(−e) ∨ e = 0)`, and (15.33) at `e` says
    /// exactly one of `pos.e`, `pos(−e)` holds when `e ≠ 0`.
    let le_total (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Theorem =
        let th, zero = d :> Theory, d.Zero
        let a, b = d.Term a, d.Term b
        let e = b - a
        let E0 = e == zero
        let ax33 = axiom th ((e != zero) ==> ((pos_of e) == !!(pos_of (-e))))
        lemma th ((a <=. b) + (b <=. a)) [
            ax_ident th ((a <=. b) == ((a <. b) + (a == b)))    |> at_left                        // (15.38)
            ax_ident th ((b <=. a) == ((b <. a) + (b == a)))    |> at_right
            ax_ident th ((a <. b) == (pos_of (b - a)))          |> at [left_branch; left_branch]  // (15.36)
            ax_ident th ((b <. a) == (pos_of (a - b)))          |> at [right_branch; left_branch]
            flip th (neg_sub d b a)                             |> at [right_branch; left_branch; apply_unary]  // a − b → −(b − a)
            eq_comm th a b                                      |> at [left_branch; right_branch]
            flip th (sub_eq_zero d b a)                         |> at [left_branch; right_branch]  // b = a → e = 0
            flip th (sub_eq_zero d b a)                         |> at [right_branch; right_branch]
            Taut (mp (in_theory th (glue_total E0 (pos_of e) (pos_of (-e)))) ax33) |> apply
        ]

    /// `(a ≥ b) = (b ≤ a)` — both are `pos(a − b) ∨ a = b`, by (15.36)–(15.39).
    let ge_le (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let th = d :> Theory
        let a, b = d.Term a, d.Term b
        ident th ((a >=. b) == (b <=. a)) [
            ax_ident th ((a >=. b) == ((a >. b) + (a == b)))    |> at_left              // (15.39)
            ax_ident th ((a >. b) == (pos_of (a - b)))          |> at [left_branch; left_branch]  // (15.37)
            ax_ident th ((b <=. a) == ((b <. a) + (b == a)))    |> at_right             // (15.38)
            ax_ident th ((b <. a) == (pos_of (a - b)))          |> at [right_branch; left_branch] // (15.36)
            eq_comm th a b                                      |> at [left_branch; right_branch]
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
    let private thm_of (r: Rule) =
        match r with
        | Derive(_, p, _) -> Theorem(p.Stmt, p)
        | _ -> failwith "not a derived rule"

    let private indirect (d: OrderedDomain<'t>) (below: bool) (a: Term<'t>) (b: Term<'t>) (body: DomainTerm<'t> -> Rule) : Rule =
        let th = d :> Theory
        let a, b = d.Term a, d.Term b
        let z = d.Term (metavar<'t> "z")
        let t = thm_of (body z)
        let expected = if below then (z <=. a) == (z <=. b) else (a <=. z) == (b <=. z)
        if not (sequal (expand t.Stmt) (expand expected.Expr)) then
            failwithf "indirect equality: the body proves\n  %s\nbut it must prove\n  %s" (th.PrintFormula t.Stmt) (th.PrintFormula expected.Expr)
        let at (e: DomainTerm<'t>) = Ident (Tactics.InstantiateTerms t [ z.Expr.Raw, e.Expr.Raw ])
        // below: (a ≤ a) = (a ≤ b) and (b ≤ a) = (b ≤ b);  above: (a ≤ a) = (b ≤ a) and (a ≤ b) = (b ≤ b)
        let ab = if below then lemma th (a <=. b) [ at a |> Commute |> apply; le_refl d a |> apply ]
                 else lemma th (a <=. b) [ at b |> apply; le_refl d b |> apply ]
        let ba = if below then lemma th (b <=. a) [ at b |> apply; le_refl d b |> apply ]
                 else lemma th (b <=. a) [ at a |> Commute |> apply; le_refl d a |> apply ]
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
        let th = d :> Theory
        let x, y, z = d.Term x, d.Term y, d.Term z
        let M = max_of x y
        ident th ((M <=. z) == ((x <=. z) * (y <=. z))) [
            ge_le d z M |> Commute                              |> at_left                        // M ≤ z → z ≥ M
            ax_ident th ((z >=. M) == ((z >=. x) * (z >=. y)))  |> at_left                        // (15.53)
            ge_le d z x                                         |> at [left_branch; left_branch]
            ge_le d z y                                         |> at [left_branch; right_branch]
        ]

    /// **(15.54) Symmetry**: `x↓y = y↓x`. Gries' proof: by (15.47), the body is (15.53) and ∧'s symmetry.
    let min_sym (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let th = d :> Theory
        let x, y = d.Term x, d.Term y
        below_eq d (min_of x y) (min_of y x) (fun z ->
            ident th ((z <=. min_of x y) == (z <=. min_of y x)) [
                min_def d z x y                                 |> at_left
                min_def d z y x                                 |> at_right
                commute_and (z <=. x) (z <=. y)                 |> at_left
            ])

    /// **(15.54) Symmetry**: `x↑y = y↑x`.
    let max_sym (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let th = d :> Theory
        let x, y = d.Term x, d.Term y
        above_eq d (max_of x y) (max_of y x) (fun z ->
            ident th ((max_of x y <=. z) == (max_of y x <=. z)) [
                max_le d x y z                                  |> at_left
                max_le d y x z                                  |> at_right
                commute_and (x <=. z) (y <=. z)                 |> at_left
            ])

    /// **(15.55) Associativity**: `(x↓y)↓w = x↓(y↓w)`.
    let min_assoc (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) (w: Term<'t>) : Rule =
        let th = d :> Theory
        let x, y, w = d.Term x, d.Term y, d.Term w
        below_eq d (min_of (min_of x y) w) (min_of x (min_of y w)) (fun z ->
            ident th ((z <=. min_of (min_of x y) w) == (z <=. min_of x (min_of y w))) [
                min_def d z (min_of x y) w                      |> at_left
                min_def d z x y                                 |> at [left_branch; left_branch]
                min_def d z x (min_of y w)                      |> at_right
                min_def d z y w                                 |> at [right_branch; right_branch]
                right_assoc_and (z <=. x) (z <=. y) (z <=. w)   |> at_left
            ])

    /// **(15.55) Associativity**: `(x↑y)↑w = x↑(y↑w)`.
    let max_assoc (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) (w: Term<'t>) : Rule =
        let th = d :> Theory
        let x, y, w = d.Term x, d.Term y, d.Term w
        above_eq d (max_of (max_of x y) w) (max_of x (max_of y w)) (fun z ->
            ident th ((max_of (max_of x y) w <=. z) == (max_of x (max_of y w) <=. z)) [
                max_le d (max_of x y) w z                       |> at_left
                max_le d x y z                                  |> at [left_branch; left_branch]
                max_le d x (max_of y w) z                       |> at_right
                max_le d y w z                                  |> at [right_branch; right_branch]
                right_assoc_and (x <=. z) (y <=. z) (w <=. z)   |> at_left
            ])

    /// **(15.56) Idempotency**: `x↓x = x`.
    let min_idem (d: OrderedDomain<'t>) (x: Term<'t>) : Rule =
        let th, x = d :> Theory, d.Term x
        below_eq d (min_of x x) x (fun z ->
            ident th ((z <=. min_of x x) == (z <=. x)) [
                min_def d z x x                                 |> at_left
                idemp_and (z <=. x)                             |> at_left
            ])

    /// **(15.56) Idempotency**: `x↑x = x`.
    let max_idem (d: OrderedDomain<'t>) (x: Term<'t>) : Rule =
        let th, x = d :> Theory, d.Term x
        above_eq d (max_of x x) x (fun z ->
            ident th ((max_of x x <=. z) == (x <=. z)) [
                max_le d x x z                                  |> at_left
                idemp_and (x <=. z)                             |> at_left
            ])

    /// **(15.57)**: `x↓y ≤ x ∧ x↓y ≤ y` — (15.53) at `z := x↓y`, and Reflexivity (15.46).
    let min_le (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let th = d :> Theory
        let x, y = d.Term x, d.Term y
        let m = min_of x y
        lemma th ((m <=. x) * (m <=. y)) [
            min_def d m x y |> Commute                          |> apply                          // → x↓y ≤ x↓y
            le_refl d m                                         |> apply
        ]

    /// **(15.57)**: `x ≤ x↑y ∧ y ≤ x↑y` (Gries: `x↑y ≥ x ∧ x↑y ≥ y`).
    let max_ge (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let th = d :> Theory
        let x, y = d.Term x, d.Term y
        let M = max_of x y
        lemma th ((x <=. M) * (y <=. M)) [
            max_le d x y M |> Commute                           |> apply                          // → x↑y ≤ x↑y
            le_refl d M                                         |> apply
        ]

    /// **(15.58)**: `(x ≤ y) = (x↓y = x)`. `⇒`: `x ≤ x↓y` by (15.53), `x↓y ≤ x` by (15.57), and
    /// Antisymmetry; `⇐`: `x↓y ≤ y` (15.57) with `x↓y` for `x`.
    let min_eq (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let th = d :> Theory
        let x, y = d.Term x, d.Term y
        let m = min_of x y
        let H, E = (x <=. y), (m == x)
        let fwd =
            lemma th (H ==> E) [
                Establish (lemma th (H ==> (x <=. m)) [
                    min_def d x x y                             |> at_right                       // → x ≤ x ∧ x ≤ y
                    le_refl d x                                 |> at [right_branch; left_branch]
                    commute_and T H                             |> at_right
                    ident_and H                                 |> at_right
                    Taut (reflex_implies H)                     |> apply
                ])                                              |> apply
                Establish (lemma th (H ==> (m <=. x)) [ Taut (conj_left th (min_le d x y)) |> apply_right ]) |> apply
                Deduce (le_antisym d m x)                       |> apply_right
            ]
        let bwd =
            lemma th (E ==> H) [
                Deduce' (eq_sym th m x)                         |> at [right_branch; left_branch]   // x → x↓y
                Taut (conj_right th (min_le d x y))             |> apply_right
            ]
        ident th (H == E) [
            mutual_implication' H E |> Commute                  |> apply
            Taut (conj fwd bwd)                                 |> apply
        ]

    /// **(15.58)**: `(y ≤ x) = (x↑y = x)` (Gries: `x ≥ y ≡ x↑y = x`).
    let max_eq (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let th = d :> Theory
        let x, y = d.Term x, d.Term y
        let M = max_of x y
        let H, E = (y <=. x), (M == x)
        let fwd =
            lemma th (H ==> E) [
                Establish (lemma th (H ==> (M <=. x)) [
                    max_le d x y x                              |> at_right                       // → x ≤ x ∧ y ≤ x
                    le_refl d x                                 |> at [right_branch; left_branch]
                    commute_and T H                             |> at_right
                    ident_and H                                 |> at_right
                    Taut (reflex_implies H)                     |> apply
                ])                                              |> apply
                Establish (lemma th (H ==> (x <=. M)) [ Taut (conj_left th (max_ge d x y)) |> apply_right ]) |> apply
                Deduce (le_antisym d M x)                       |> apply_right
            ]
        let bwd =
            lemma th (E ==> H) [
                Deduce' (eq_sym th M x)                         |> at [right_branch; right_branch]  // x → x↑y
                Taut (conj_right th (max_ge d x y))             |> apply_right
            ]
        ident th (H == E) [
            mutual_implication' H E |> Commute                  |> apply
            Taut (conj fwd bwd)                                 |> apply
        ]

    /// **(15.59)**: `x↓y = x ∨ x↓y = y` — (15.58) both ways round, and Totality.
    let min_cases (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let th = d :> Theory
        let x, y = d.Term x, d.Term y
        let m = min_of x y
        lemma th ((m == x) + (m == y)) [
            min_eq d x y |> Commute                             |> at_left                        // → x ≤ y
            min_sym d x y                                       |> at [right_branch; left_branch] // x↓y → y↓x
            min_eq d y x |> Commute                             |> at_right                       // → y ≤ x
            Taut (le_total d x y)                               |> apply
        ]

    /// **(15.59)**: `x↑y = x ∨ x↑y = y`.
    let max_cases (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let th = d :> Theory
        let x, y = d.Term x, d.Term y
        let M = max_of x y
        lemma th ((M == x) + (M == y)) [
            max_eq d x y |> Commute                             |> at_left                        // → y ≤ x
            max_sym d x y                                       |> at [right_branch; left_branch] // x↑y → y↑x
            max_eq d y x |> Commute                             |> at_right                       // → x ≤ y
            commute_or (y <=. x) (x <=. y)                      |> apply
            Taut (le_total d x y)                               |> apply
        ]

    /// `(c + m) + (−c) = m`.
    let add_neg_cancel (d: IntegralDomain<'t>) (c: Term<'t>) (m: Term<'t>) : Rule =
        let th, zero = d :> Theory, d.Zero
        let c, m = d.Term c, d.Term m
        ident th ((c + m) + -c == m) [
            ax th (c + m) (m + c)                               |> at [left_branch; left_branch]
            ax th ((m + c) + -c) (m + (c + -c))                 |> at_left
            ax th (c + -c) zero                                 |> at [left_branch; right_branch]  // (15.13)
            ax th (m + zero) m                                  |> at_left
        ]

    /// `(z + (−c)) + c = z`.
    let neg_add_cancel (d: IntegralDomain<'t>) (z: Term<'t>) (c: Term<'t>) : Rule =
        let th, zero = d :> Theory, d.Zero
        let z, c = d.Term z, d.Term c
        ident th ((z + -c) + c == z) [
            ax th ((z + -c) + c) (z + ((-c) + c))               |> at_left
            left_inverse d c                                    |> at [left_branch; right_branch]
            ax th (z + zero) z                                  |> at_left
        ]

    /// **(15.60) Distributivity**: `c + (x↓y) = (c + x)↓(c + y)` — by (15.47): translate `z ≤ c + m`
    /// by `−c`, unfold (15.53), translate each conjunct back by `c`.
    let min_add (d: OrderedDomain<'t>) (c: Term<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let th = d :> Theory
        let c, x, y = d.Term c, d.Term x, d.Term y
        let m = min_of x y
        below_eq d (c + m) (min_of (c + x) (c + y)) (fun z ->
            let zc = z + -c
            ident th ((z <=. c + m) == (z <=. min_of (c + x) (c + y))) [
                min_def d z (c + x) (c + y)                     |> at_right
                le_translate d z (c + m) (-c)                   |> at_left                         // z + −c ≤ (c + m) + −c
                add_neg_cancel d c m                            |> at [left_branch; right_branch]
                min_def d zc x y                                |> at_left
                le_translate d zc x c                           |> at [left_branch; left_branch]   // (z + −c) + c ≤ x + c
                neg_add_cancel d z c                            |> at [left_branch; left_branch; left_branch]
                ax th (x + c) (c + x)                           |> at [left_branch; left_branch; right_branch]
                le_translate d zc y c                           |> at [left_branch; right_branch]
                neg_add_cancel d z c                            |> at [left_branch; right_branch; left_branch]
                ax th (y + c) (c + y)                           |> at [left_branch; right_branch; right_branch]
            ])

    /// **(15.60) Distributivity**: `c + (x↑y) = (c + x)↑(c + y)`.
    let max_add (d: OrderedDomain<'t>) (c: Term<'t>) (x: Term<'t>) (y: Term<'t>) : Rule =
        let th = d :> Theory
        let c, x, y = d.Term c, d.Term x, d.Term y
        let M = max_of x y
        above_eq d (c + M) (max_of (c + x) (c + y)) (fun z ->
            let zc = z + -c
            ident th ((c + M <=. z) == (max_of (c + x) (c + y) <=. z)) [
                max_le d (c + x) (c + y) z                      |> at_right
                le_translate d (c + M) z (-c)                   |> at_left                         // (c + M) + −c ≤ z + −c
                add_neg_cancel d c M                            |> at [left_branch; left_branch]
                max_le d x y zc                                 |> at_left
                le_translate d x zc c                           |> at [left_branch; left_branch]   // x + c ≤ (z + −c) + c
                neg_add_cancel d z c                            |> at [left_branch; left_branch; right_branch]
                ax th (x + c) (c + x)                           |> at [left_branch; left_branch; left_branch]
                le_translate d y zc c                           |> at [left_branch; right_branch]
                neg_add_cancel d z c                            |> at [left_branch; right_branch; right_branch]
                ax th (y + c) (c + y)                           |> at [left_branch; right_branch; left_branch]
            ])

    (* ------------------------------------------------------------------------------------------
       §15.3 Absolute value: (15.71) abs.x = x↑−x, and its properties from those of ↑.
       ------------------------------------------------------------------------------------------ *)

    /// (15.71) as a rewrite: `abs.x = x↑−x`. An axiom instance.
    let abs_def (d: OrderedDomain<'t>) (x: Term<'t>) : Rule =
        let x = d.Term x
        ax_ident d (abs_of x == max_of x (-x))

    /// **(15.72)**: `abs.x = abs(−x)` — both are `x↑−x`, by (15.17) and Symmetry (15.54).
    let abs_neg (d: OrderedDomain<'t>) (x: Term<'t>) : Rule =
        let th, x = d :> Theory, d.Term x
        ident th (abs_of x == abs_of (-x)) [
            abs_def d x                                         |> at_left                         // → x↑−x
            abs_def d (-x)                                      |> at_right                        // → (−x)↑−(−x)
            neg_neg d x                                         |> at [right_branch; right_branch]
            max_sym d (-x) x                                    |> at_right
        ]

    /// `x ≤ abs.x ∧ −x ≤ abs.x` — (15.57) for `x↑−x`.
    let abs_ge (d: OrderedDomain<'t>) (x: Term<'t>) : Theorem =
        let th, x = d :> Theory, d.Term x
        lemma th ((x <=. abs_of x) * (-x <=. abs_of x)) [
            abs_def d x                                         |> at [left_branch; right_branch]
            abs_def d x                                         |> at [right_branch; right_branch]
            Taut (max_ge d x (-x))                              |> apply
        ]

    /// `0 ≤ a ⇒ −a ≤ 0` — translate by `−a`.
    let neg_le_zero (d: OrderedDomain<'t>) (a: Term<'t>) : Theorem =
        let th, zero, a = d :> Theory, d.Zero, d.Term a
        lemma th ((zero <=. a) ==> (-a <=. zero)) [
            le_translate d zero a (-a)                          |> at_left                         // 0 + −a ≤ a + −a
            ax th (zero + -a) (-a)                              |> at [left_branch; left_branch]
            ax th (a + -a) zero                                 |> at [left_branch; right_branch]  // (15.13)
            Taut (reflex_implies (-a <=. zero))                 |> apply
        ]

    /// `a ≤ 0 ⇒ 0 ≤ −a` — translate by `−a`.
    let zero_le_neg (d: OrderedDomain<'t>) (a: Term<'t>) : Theorem =
        let th, zero, a = d :> Theory, d.Zero, d.Term a
        lemma th ((a <=. zero) ==> (zero <=. -a)) [
            le_translate d a zero (-a)                          |> at_left                         // a + −a ≤ 0 + −a
            ax th (a + -a) zero                                 |> at [left_branch; left_branch]   // (15.13)
            ax th (zero + -a) (-a)                              |> at [left_branch; right_branch]
            Taut (reflex_implies (zero <=. -a))                 |> apply
        ]

    /// `0 ≤ abs.x` — by Totality, `0 ≤ x` or `x ≤ 0`; then `x ≤ abs.x` or `−x ≤ abs.x`, and Transitivity.
    let abs_nonneg (d: OrderedDomain<'t>) (x: Term<'t>) : Theorem =
        let th, z, x = d :> Theory, d.Zero, d.Term x
        let A = abs_of x
        let goal = z <=. A
        let pos_case =
            lemma th ((z <=. x) ==> goal) [
                Establish (lemma th ((z <=. x) ==> (x <=. A)) [ Taut (conj_left th (abs_ge d x)) |> apply_right ]) |> apply
                Deduce (le_trans d z x A)                       |> apply_right
            ]
        let neg_case =
            lemma th ((x <=. z) ==> goal) [
                Establish (zero_le_neg d x)                     |> apply                           // 0 ≤ −x
                Establish (lemma th ((x <=. z) ==> (-x <=. A)) [ Taut (conj_right th (abs_ge d x)) |> apply_right ]) |> apply
                Deduce (le_trans d z (-x) A)                    |> apply_right
            ]
        let both =
            lemma th (((z <=. x) + (x <=. z)) ==> goal) [
                case_analysis_1 (z <=. x) (x <=. z) goal |> Commute |> apply                       // (3.78)
                Taut pos_case                                   |> at_left
                Taut neg_case                                   |> at_right
                ident_and T                                     |> apply
            ]
        mp both (le_total d z x)

    /// **(15.74)**: `abs(abs.x) = abs.x` — `abs(A) = A↑−A`, and `−A ≤ A` since `0 ≤ A` (15.58).
    let abs_abs (d: OrderedDomain<'t>) (x: Term<'t>) : Rule =
        let th, zero, x = d :> Theory, d.Zero, d.Term x
        let A = abs_of x
        let negA_le_A =
            mp (lemma th ((zero <=. A) ==> (-A <=. A)) [
                    Establish (neg_le_zero d A)                 |> apply                           // −A ≤ 0
                    Deduce (le_trans d (-A) zero A)             |> apply_right
                ]) (abs_nonneg d x)
        ident th (abs_of A == A) [
            abs_def d A                                         |> at_left                         // → A↑−A = A
            max_eq d A (-A) |> Commute                          |> apply                           // → −A ≤ A
            Taut negA_le_A                                      |> apply
        ]

    /// `a ≤ b ∧ c ≤ e ⇒ a + c ≤ b + e` — Monotonicity twice, and Transitivity.
    let le_add (d: OrderedDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) (e: Term<'t>) : Theorem =
        let th = d :> Theory
        let a, b, c, e = d.Term a, d.Term b, d.Term c, d.Term e
        lemma th (((a <=. b) * (c <=. e)) ==> (a + c <=. b + e)) [
            Establish (lemma th ((a <=. b) ==> (a + c <=. b + c)) [
                le_translate d a b c                            |> at_left
                Taut (reflex_implies (a + c <=. b + c))         |> apply
            ])                                                  |> apply
            Establish (lemma th ((c <=. e) ==> (b + c <=. b + e)) [
                le_translate d c e b                            |> at_left                         // c + b ≤ e + b
                ax th (c + b) (b + c)                           |> at [left_branch; left_branch]
                ax th (e + b) (b + e)                           |> at [left_branch; right_branch]
                Taut (reflex_implies (b + c <=. b + e))         |> apply
            ])                                                  |> apply
            Deduce (le_trans d (a + c) (b + c) (b + e))         |> apply_right
        ]

    /// **(15.73) Triangle inequality**: `abs(x + y) ≤ abs.x + abs.y`. `abs(x+y) = (x+y)↑−(x+y)`, so by
    /// (15.53) it is `x + y ≤ S ∧ −x + −y ≤ S`, each `le_add` of the halves of `abs_ge`.
    let triangle (d: OrderedDomain<'t>) (x: Term<'t>) (y: Term<'t>) : Theorem =
        let th = d :> Theory
        let x, y = d.Term x, d.Term y
        let S = abs_of x + abs_of y
        let gx, gy = abs_ge d x, abs_ge d y
        let plus = mp (le_add d x (abs_of x) y (abs_of y)) (conj (conj_left th gx) (conj_left th gy))
        let minus = mp (le_add d (-x) (abs_of x) (-y) (abs_of y)) (conj (conj_right th gx) (conj_right th gy))
        lemma th (abs_of (x + y) <=. S) [
            abs_def d (x + y)                                   |> at_left
            max_le d (x + y) (-(x + y)) S                       |> apply                           // (15.53)
            neg_add d x y                                       |> at [right_branch; left_branch]  // (15.19)
            Taut plus                                           |> at_left
            Taut minus                                          |> at_right
            ident_and T                                         |> apply
        ]
