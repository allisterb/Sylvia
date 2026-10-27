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
module OrderedDomain =

    /// The positivity predicate `pos` of an ordered domain. Uninterpreted: its meaning is
    /// (15.30)–(15.33), and it is never evaluated.
    let pos (x: 't) : bool = invalidOp "pos is a symbolic predicate of an ordered domain."

    /// The order operators of an ordered domain: `pos` and the four relations it defines.
    type OrderOps<'t> =
        { Pos: Expr<'t -> bool>
          Lt: Expr<'t -> 't -> bool>
          Gt: Expr<'t -> 't -> bool>
          Le: Expr<'t -> 't -> bool>
          Ge: Expr<'t -> 't -> bool> }

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
        | _ -> None

    (* Theory *)

    /// An ordered domain over `'t`: an integral domain with `pos`, and a caller's axioms and rules on top.
    type OrderedDomain<'t when 't: equality>(ops: DomainOps<'t>, order: OrderOps<'t>, ?axioms: Axioms, ?rules: Rules) =
        inherit IntegralDomain<'t>(ops,
                                   BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None)) (ordered_domain_axioms ops order),
                                   defaultArg rules [])
        member val Order = order

    (* Building propositions *)

    /// `pos.a`.
    let pos_of (o: OrderOps<'t>) (a: Expr<'t>) : Prop = let p = o.Pos in <@ (%p) %a @> |> expand_as<bool> |> Prop
    /// `a R b`, for one of the four relations.
    let rel (r: Expr<'t -> 't -> bool>) (a: Expr<'t>) (b: Expr<'t>) : Prop = <@ (%r) %a %b @> |> expand_as<bool> |> Prop

    (* Equality *)

    /// `(a = b) ⇒ (b = a)` — symmetry of `=` at any carrier, as an implication. The logic's symmetry
    /// is that of ≡, so nothing flips `a = b` at a non-bool type directly; this derives it from
    /// Substitution (3.84a) and reflexivity: `a = b ∧ a = a` is `a = b ∧ b = a`. With `Deduce'` it
    /// rewrites `b` to `a` under a hypothesis `a = b`; with `mp` it flips a proved identity.
    let eq_sym (th: Theory) (a: Expr<'t>) (b: Expr<'t>) : Theorem =
        let ab, aa, ba = eq_t a b, eq_t a a, eq_t b a
        lemma th (ab ==> ba) [
            ident_and ab |> Commute                  |> at_left                         // a = b → a = b ∧ T
            Taut (axiom th aa) |> Commute            |> at [left_branch; right_branch]  // T → a = a
            ax_ident th ((ab * aa) == (ab * ba))     |> at_left                         // (3.84a)
            commute_and ab ba                        |> at_left
            Taut (strengthen_and ba ab)              |> apply
        ]

    (* Arithmetic the order proofs need (identities of any integral domain) *)

    /// `(b − a) + (c − b) = c − a` — Gries' Exercise 15.22, the arithmetic step of (15.41a).
    let sub_chain (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (c: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let s, sub = bin o.Add, bin o.Sub
        // (b + c) − (a + b) = c − a, by (15.28): it is (b + c) + a = (a + b) + c
        let inner =
            ident th (eq_t (sub (s b c) (s a b)) (sub c a)) [
                sub_eq_sub d (s b c) (s a b) c a          |> apply
                ax th (s (s b c) a) (s b (s c a))         |> at_left
                ax th (s a b) (s b a)                     |> at [right_branch; left_branch]
                ax th (s (s b a) c) (s b (s a c))         |> at_right
                ax th (s a c) (s c a)                     |> at [right_branch; right_branch]
            ]
        ident th (eq_t (s (sub b a) (sub c b)) (sub c a)) [
            sub_add_sub d b a c b                         |> at_left     // (15.25)
            inner                                         |> at_left
        ]

    /// `(b + e) − (a + e) = b − a` — the arithmetic step of Monotonicity (15.42).
    let sub_translate (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (e: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let s, sub = bin o.Add, bin o.Sub
        ident th (eq_t (sub (s b e) (s a e)) (sub b a)) [
            sub_eq_sub d (s b e) (s a e) b a              |> apply       // (15.28)
            ax th (s (s b e) a) (s b (s e a))             |> at_left
            ax th (s (s a e) b) (s b (s a e))             |> at_right
            ax th (s a e) (s e a)                         |> at [right_branch; right_branch]
        ]

    /// `a − (a + 1) = −1`.
    let sub_succ (d: IntegralDomain<'t>) (a: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let s, sub, n = bin o.Add, bin o.Sub, un o.Neg
        ident th (eq_t (sub a (s a o.One)) (n o.One)) [
            ax th (sub a (s a o.One)) (s a (n (s a o.One)))           |> at_left                       // (15.14)
            neg_add d a o.One                                         |> at [left_branch; right_branch] // (15.19)
            rev th (s (s a (n a)) (n o.One)) (s a (s (n a) (n o.One))) |> at_left
            ax th (s a (n a)) o.Zero                                  |> at [left_branch; left_branch]  // (15.13)
            ax th (s o.Zero (n o.One)) (n o.One)                      |> at_left
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
    let sq_pos (d: OrderedDomain<'t>) (b: Expr<'t>) : Theorem =
        let o, r, th = d.Ops, d.Order, d :> Theory
        let m, nb = bin o.Mul, un o.Neg b
        let P = pos_of r
        let nz = nonzero o b
        let sq (x: Expr<'t>) =
            lemma th ((P x) ==> (P (m x x))) [
                idemp_and (P x) |> Commute |> at_left       // pos.x → pos.x ∧ pos.x
                // and that is Multiplication (15.31).
            ]
        let q = P (m b b)
        let ax33 = axiom th (nz ==> ((P b) == !!(P nb)))
        let neg_pos = mp (in_theory th (glue_neg nz (P b) (P nb))) ax33
        let not_pos =
            lemma th ((nz * !!(P b)) ==> q) [
                flip th (neg_mul_neg d b b)     |> at [right_branch; apply_unary]   // b·b → (−b)·(−b)
                Establish neg_pos               |> apply                            // pos(−b)
                Deduce (sq nb)                  |> apply_right
            ]
        mp (mp (in_theory th (glue_cases (P b) q nz)) (sq b)) not_pos

    /// `1 ≠ 0 ⇒ pos.1` — the corollary of (15.34) Gries draws: `1 = 1·1`. The premise is needed: the
    /// one-element ring satisfies every axiom above, and in it nothing is positive.
    let one_pos (d: OrderedDomain<'t>) : Theorem =
        let o, r, th = d.Ops, d.Order, d :> Theory
        lemma th ((nonzero o o.One) ==> (pos_of r o.One)) [
            rev th (bin o.Mul o.One o.One) o.One    |> at [right_branch; apply_unary]   // 1 → 1·1
            Taut (sq_pos d o.One)                   |> apply
        ]

    /// `1 ≠ 0 ⇒ (pos(−1) = false)` — so −1 is not positive: (15.33) at `b := 1`.
    let neg_one_not_pos (d: OrderedDomain<'t>) : Theorem =
        let o, r, th = d.Ops, d.Order, d :> Theory
        let nz1, p1, pm1 = nonzero o o.One, pos_of r o.One, pos_of r (un o.Neg o.One)
        let ax33 = axiom th (nz1 ==> (p1 == !!pm1))
        mp (mp (in_theory th (glue_false nz1 p1 pm1)) (one_pos d)) ax33

    /// **(15.40) Positive elements**: `pos.b = 0 < b`.
    let pos_lt (d: OrderedDomain<'t>) (b: Expr<'t>) : Rule =
        let o, r, th = d.Ops, d.Order, d :> Theory
        ident th ((pos_of r b) == (rel r.Lt o.Zero b)) [
            ax_ident th ((rel r.Lt o.Zero b) == (pos_of r (bin o.Sub b o.Zero))) |> at_right   // (15.36)
            sub_zero d b                                                          |> at [right_branch; apply_unary]
        ]

    /// **(15.41a) Transitivity**: `a < b ∧ b < c ⇒ a < c` — Gries' proof: Less, Addition, arithmetic.
    let lt_trans (d: OrderedDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (c: Expr<'t>) : Theorem =
        let o, r, th = d.Ops, d.Order, d :> Theory
        let lt, P, sub = rel r.Lt, pos_of r, bin o.Sub
        lemma th (((lt a b) * (lt b c)) ==> (lt a c)) [
            ax_ident th ((lt a b) == (P (sub b a)))  |> at [left_branch; left_branch]     // (15.36)
            ax_ident th ((lt b c) == (P (sub c b)))  |> at [left_branch; right_branch]
            ax_ident th ((lt a c) == (P (sub c a)))  |> at_right
            flip th (sub_chain d a b c)              |> at [right_branch; apply_unary]    // c − a → (b − a) + (c − b)
            // and that is Addition (15.30).
        ]

    /// **(15.42) Monotonicity**: `(a < b) = (a + e < b + e)`.
    let lt_translate (d: OrderedDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (e: Expr<'t>) : Rule =
        let o, r, th = d.Ops, d.Order, d :> Theory
        let lt, P, s, sub = rel r.Lt, pos_of r, bin o.Add, bin o.Sub
        ident th ((lt a b) == (lt (s a e) (s b e))) [
            ax_ident th ((lt a b) == (P (sub b a)))                          |> at_left    // (15.36)
            ax_ident th ((lt (s a e) (s b e)) == (P (sub (s b e) (s a e))))  |> at_right
            sub_translate d a b e                                            |> at [right_branch; apply_unary]
        ]

    /// `(a + e = b + e) = (a = b)` — (15.8) Cancellation on the right.
    let cancel_add_right (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (e: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let s = bin o.Add
        ident th ((eq_t (s a e) (s b e)) == (eq_t a b)) [
            ax th (s a e) (s e a)                    |> at [left_branch; left_branch]
            ax th (s b e) (s e b)                    |> at [left_branch; right_branch]
            cancel_add d e a b                       |> at_left
        ]

    /// **(15.46) Reflexivity**: `(a ≤ a) = true`.
    let le_refl (d: OrderedDomain<'t>) (a: Expr<'t>) : Rule =
        let r, th = d.Order, d :> Theory
        ident th ((rel r.Le a a) == T) [
            ax_ident th ((rel r.Le a a) == ((rel r.Lt a a) + (eq_t a a)))  |> at_left      // (15.38)
            Taut (axiom th (eq_t a a))                                      |> at [left_branch; right_branch]
            zero_or (rel r.Lt a a)                                          |> at_left
        ]

    /// Monotonicity for `≤`: `(a ≤ b) = (a + e ≤ b + e)` — (15.42) and Cancellation, through (15.38).
    let le_translate (d: OrderedDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (e: Expr<'t>) : Rule =
        let o, r, th = d.Ops, d.Order, d :> Theory
        let le, lt, s = rel r.Le, rel r.Lt, bin o.Add
        let ae, be = s a e, s b e
        ident th ((le a b) == (le ae be)) [
            ax_ident th ((le a b) == ((lt a b) + (eq_t a b)))       |> at_left               // (15.38)
            ax_ident th ((le ae be) == ((lt ae be) + (eq_t ae be))) |> at_right
            lt_translate d a b e                                    |> at [left_branch; left_branch]
            cancel_add_right d a b e                                |> at [right_branch; right_branch]
        ]

    /// **(15.41d) Transitivity**: `a ≤ b ∧ b ≤ c ⇒ a ≤ c`. By (15.38) each premise is a disjunction,
    /// and the four cases are (15.41a) and three Substitutions.
    let le_trans (d: OrderedDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (c: Expr<'t>) : Theorem =
        let r, th = d.Order, d :> Theory
        let le, lt = rel r.Le, rel r.Lt
        let A, B, C, D = lt a b, eq_t a b, lt b c, eq_t b c
        let R = le a c
        let ltc, eqc = lt a c, eq_t a c
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
                Deduce' (eq_sym th b c)                   |> at [right_branch; left_branch]   // a < c → a < b
                Deduce (weaken_or A eqc)                  |> apply_right
            ]
        let caseDB =  // b = c ∧ a = b
            lemma th ((D * B) ==> R) [
                leR                                       |> at_right
                Deduce' (reflex_implies B)                |> at [right_branch; right_branch]  // a = c → b = c
                Deduce (reflex_implies D)                 |> at [right_branch; right_branch]
                zero_or ltc                               |> apply_right
            ]
        lemma th (((le a b) * (le b c)) ==> R) [
            ax_ident th ((le a b) == (A + B))             |> at [left_branch; left_branch]
            ax_ident th ((le b c) == (C + D))             |> at [left_branch; right_branch]
            distrib_and_or (A + B) C D                    |> at_left
            commute_and (A + B) C                         |> at [left_branch; left_branch]
            distrib_and_or C A B                          |> at [left_branch; left_branch]
            commute_and (A + B) D                         |> at [left_branch; right_branch]
            distrib_and_or D A B                          |> at [left_branch; right_branch]
            case_analysis_1 ((C * A) + (C * B)) ((D * A) + (D * B)) R |> Commute |> apply   // (3.78)
            case_analysis_1 (C * A) (C * B) R |> Commute  |> at_left
            case_analysis_1 (D * A) (D * B) R |> Commute  |> at_right
            Taut caseCA                                   |> at [left_branch; left_branch]
            Taut caseCB                                   |> at [left_branch; right_branch]
            Taut caseDA                                   |> at [right_branch; left_branch]
            Taut caseDB                                   |> at [right_branch; right_branch]
            ident_and T                                   |> at_left
            ident_and T                                   |> at_right
            ident_and T                                   |> apply
        ]

    /// `1 ≠ 0 ⇒ ((a + 1 ≤ a) = false)` — a successor is not at or below its predecessor. `a + 1 < a`
    /// is `pos(−1)`, which is false; `a + 1 = a` is `1 = 0` by Cancellation.
    let succ_not_le (d: OrderedDomain<'t>) (a: Expr<'t>) : Theorem =
        let o, r, th = d.Ops, d.Order, d :> Theory
        let s, sub = bin o.Add, bin o.Sub
        let a1 = s a o.One
        let nz1 = nonzero o o.One
        let one0 = eq_t o.One o.Zero
        lemma th (nz1 ==> ((rel r.Le a1 a) == F)) [
            ax_ident th ((rel r.Le a1 a) == ((rel r.Lt a1 a) + (eq_t a1 a)))  |> at [right_branch; left_branch]                 // (15.38)
            ax_ident th ((rel r.Lt a1 a) == (pos_of r (sub a a1)))            |> at [right_branch; left_branch; left_branch]    // (15.36)
            sub_succ d a                                   |> at [right_branch; left_branch; left_branch; apply_unary]           // a − (a+1) → −1
            Deduce' (neg_one_not_pos d)                    |> apply_right                                                         // pos(−1) → false
            rev th (s a o.Zero) a                          |> at [right_branch; left_branch; right_branch; right_branch]          // a → a + 0
            cancel_add d a o.One o.Zero                    |> at [right_branch; left_branch; right_branch]                       // → 1 = 0
            commute_or F one0                              |> at [right_branch; left_branch]
            ident_or one0                                  |> at [right_branch; left_branch]
            ident_eq_false one0                            |> apply_right                                                         // (1 = 0) = F → ¬(1 = 0)
            Taut (reflex_implies nz1)                      |> apply
        ]

    /// `0 ≤ a ∧ 0 ≤ b ⇒ 0 ≤ a + b` — non-negatives are closed under `+`: Monotonicity puts `b` on
    /// both sides of `0 ≤ a`, and Transitivity chains `0 ≤ b ≤ a + b`.
    let nonneg_add (d: OrderedDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) : Theorem =
        let o, r, th = d.Ops, d.Order, d :> Theory
        let le, s, z = rel r.Le, bin o.Add, o.Zero
        lemma th (((le z a) * (le z b)) ==> (le z (s a b))) [
            le_translate d z a b                          |> at [left_branch; left_branch]                 // 0 ≤ a → 0 + b ≤ a + b
            ax th (s z b) b                               |> at [left_branch; left_branch; left_branch]    // 0 + b → b
            commute_and (le b (s a b)) (le z b)           |> at_left
            Taut (le_trans d z b (s a b))                 |> apply
        ]
