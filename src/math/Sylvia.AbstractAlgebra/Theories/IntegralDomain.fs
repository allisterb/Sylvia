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

/// Theory of integral domains — Gries & Schneider ch.15, §15.1.
///
/// A set D with elements 0 and 1 and binary operators + and · satisfying (15.1)–(15.7): associativity,
/// symmetry, additive and multiplicative identities, distributivity, an additive inverse, and
/// CANCELLATION for · at a non-zero factor — the axiom that makes a ring an integral domain. With the
/// inverse unique (15.12), Gries makes unary minus (15.13) and subtraction (15.14) axioms too.
///
/// GENERIC over the carrier, the way `BooleanAlgebra<'t>` is: the operators are parameters. `Integers`
/// is the instance at `int`, so every theorem here, proved once in an `IntegralDomain<int>`, is usable
/// in `Integers` and `Induction` proofs (a theorem may be used in any theory whose type derives from
/// the one it was proved in).
module IntegralDomain =

    (* The operators *)

    /// The operators of an integral domain: `+`, `·`, `0`, `1`, unary `−` and binary `−`.
    type DomainOps<'t when 't: equality> =
        { Name: string
          Add: Expr<'t -> 't -> 't>
          Mul: Expr<'t -> 't -> 't>
          Zero: Expr<'t>
          One: Expr<'t>
          Neg: Expr<'t -> 't>
          Sub: Expr<'t -> 't -> 't> }

    (* Axiom patterns not already in `Patterns` *)

    /// `0 + a = a` — the LEFT identity (`Patterns.Identity` is the right one).
    let (|LeftIdentity|_|) (eq: Expr<'t -> 't -> bool>) (op: Expr<'t -> 't -> 't>) (unit: Expr<'t>) =
        function
        | Binary eq (Binary op (u, a1), a2) when sequal u unit && sequal a1 a2 -> pattern_name "Identity" |> Some
        | _ -> None

    /// `(b + c)·a = b·a + c·a` — RIGHT distributivity (`Patterns.Distrib` is the left one).
    let (|RightDistrib|_|) (eq: Expr<'t -> 't -> bool>) (mul: Expr<'t -> 't -> 't>) (add: Expr<'t -> 't -> 't>) =
        function
        | Binary eq (Binary mul (Binary add (b, c), a), Binary add (Binary mul (b', a'), Binary mul (c', a'')))
            when sequal b b' && sequal c c' && sequal a a' && sequal a a'' -> pattern_name "Distributivity" |> Some
        | _ -> None

    /// `(∃x |: x + a = 0)` and `(∃x |: a + x = 0)` — (15.6), the additive inverse EXISTS. `a` must not
    /// mention the dummy.
    let (|AdditiveInverseExists|_|) (add: Expr<'t -> 't -> 't>) (zero: Expr<'t>) =
        function
        | Exists(_, [x], True, Equals(Binary add (Var x', a), z))
        | Exists(_, [x], True, Equals(Binary add (a, Var x'), z))
            when vequal x x' && sequal z zero && not (occurs [x] a) -> pattern_name "Additive inverse" |> Some
        | _ -> None

    /// A non-zero premise, in either spelling: `c ≠ 0` (Gries) or `¬(c = 0)`. The logic knows `≠` is
    /// `¬=` only at bool, so at another type the two are different terms; both are accepted here.
    let private (|NonZero|_|) (zero: Expr<'t>) =
        function
        | NotEquals(c, z) | Not(Equals(c, z)) when sequal z zero -> Some c
        | _ -> None

    /// `c ≠ 0 ⇒ ((c·a = c·b) = (a = b))` and `c ≠ 0 ⇒ ((a·c = b·c) = (a = b))` — (15.7) Cancellation:
    /// a non-zero factor cancels. This is the integral-domain axiom proper (no zero divisors).
    let (|MulCancellation|_|) (mul: Expr<'t -> 't -> 't>) (zero: Expr<'t>) =
        function
        | Implies(NonZero zero c, Equals(Equals(Binary mul (c1, a), Binary mul (c2, b)), Equals(a', b')))
            when sequal c c1 && sequal c c2 && sequal a a' && sequal b b' -> pattern_name "Cancellation" |> Some
        | Implies(NonZero zero c, Equals(Equals(Binary mul (a, c1), Binary mul (b, c2)), Equals(a', b')))
            when sequal c c1 && sequal c c2 && sequal a a' && sequal b b' -> pattern_name "Cancellation" |> Some
        | _ -> None

    (* Axioms *)

    /// Gries (15.1)–(15.7), plus Unary minus (15.13) and Subtraction (15.14).
    let integral_domain_axioms (d: DomainOps<'t>) : Axioms =
        let eq : Expr<'t -> 't -> bool> = <@ (=) @>
        let desc = axiom_desc d.Name
        function
        | Assoc eq d.Add x | Assoc eq d.Mul x                                  // (15.1)
        | Commute eq d.Add x | Commute eq d.Mul x                              // (15.2)
        | Identity eq d.Add d.Zero x | LeftIdentity eq d.Add d.Zero x          // (15.3)
        | Identity eq d.Mul d.One x | LeftIdentity eq d.Mul d.One x            // (15.4)
        | Distrib eq d.Mul d.Add x | RightDistrib eq d.Mul d.Add x             // (15.5)
        | AdditiveInverseExists d.Add d.Zero x                                 // (15.6)
        | MulCancellation d.Mul d.Zero x                                       // (15.7)
        | Inverse eq d.Add d.Neg d.Zero x                                      // (15.13)
        | BinaryOpDefR eq d.Sub d.Add d.Neg x -> Some (desc x)                 // (15.14)
        | _ -> None

    (* Theory *)

    /// An integral domain over `'t`, with a caller's extra axioms and rules on top.
    type IntegralDomain<'t when 't: equality>(ops: DomainOps<'t>, ?axioms: Axioms, ?rules: Rules) =
        inherit Theory(BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None)) (integral_domain_axioms ops),
                       defaultArg rules [])
        member val Ops = ops

    (* Building terms *)

    let private bin (op: Expr<'t -> 't -> 't>) (a: Expr<'t>) (b: Expr<'t>) : Expr<'t> = expand_as<'t> <@ (%op) %a %b @>
    let private un (op: Expr<'t -> 't>) (a: Expr<'t>) : Expr<'t> = expand_as<'t> <@ (%op) %a @>

    /// `a = b`, as a proposition.
    let eq_t (a: Expr<'t>) (b: Expr<'t>) : Prop = <@ %a = %b @> |> expand_as<bool> |> Prop

    (* Local devices *)

    // An axiom instance `lhs = rhs` as a rewrite lhs → rhs, and right-to-left as rhs → lhs. Nothing in
    // the logic flips an equation of a non-boolean type, so the reverse is a lemma proved by rewriting
    // the other side of its own goal.
    let private ax (th: Theory) (lhs: Expr<'t>) (rhs: Expr<'t>) : Rule = ax_ident th (eq_t lhs rhs)
    let private rev (th: Theory) (lhs: Expr<'t>) (rhs: Expr<'t>) : Rule = ident th (eq_t rhs lhs) [ ax_ident th (eq_t lhs rhs) |> at_right ]
    /// A derived identity `l = r` used right-to-left, as `r → l`: the same `=` call with its operands
    /// swapped, proved by rewriting its right side with the identity itself.
    let private flip (th: Theory) (rule: Rule) : Rule =
        match rule with
        | Derive(_, p, _) ->
            match p.Stmt with
            | Call(None, mi, [l; r]) when mi.Name = "op_Equality" ->
                ident th (Prop(Expr.Cast<bool> (Expr.Call(mi, [r; l])))) [ rule |> at_right ]
            | _ -> failwith "flip: not an identity"
        | _ -> failwith "flip: not a derived rule"
    /// `(x = x) → T`, at any type.
    let private refl_true (th: Theory) (x: Expr<'t>) = ax_ident th (T == (eq_t x x)) |> Commute

    (* Theorems for integral domains (Gries 15.8 – 15.24) *)

    /// **(15.14) Subtraction**, as a rewrite: `a − b → a + (−b)`. An axiom instance.
    let sub_def (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) : Rule =
        let o = d.Ops
        ax d (bin o.Sub a b) (bin o.Add a (un o.Neg b))

    /// `(−a) + a = 0` — the LEFT inverse, from (15.13) and symmetry.
    let left_inverse (d: IntegralDomain<'t>) (a: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let na = un o.Neg a
        ident th (eq_t (bin o.Add na a) o.Zero) [
            ax th (bin o.Add na a) (bin o.Add a na) |> at_left
        ]

    /// **(15.8) Cancellation**: `(a + b = a + c) = (b = c)`. By mutual implication: `⇐` is Leibniz,
    /// `⇒` is Gries' chain `b = 0 + b = ((−a) + a) + b = (−a) + (a + b) = (−a) + (a + c) = … = c`, with
    /// the antecedent used to replace `a + b` by `a + c`.
    let cancel_add (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (c: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let na = un o.Neg a
        let H = eq_t (bin o.Add a b) (bin o.Add a c)
        let Q = eq_t b c
        let fwd =
            lemma th (H ==> Q) [
                rev th (bin o.Add o.Zero b) b                       |> at [right_branch; left_branch]                // b → 0 + b
                flip th (left_inverse d a)                          |> at [right_branch; left_branch; left_branch]  // 0 → (−a) + a
                ax th (bin o.Add (bin o.Add na a) b) (bin o.Add na (bin o.Add a b)) |> at [right_branch; left_branch]
                Deduce' (reflex_implies H)                          |> at [right_branch; left_branch; right_branch] // a + b → a + c
                rev th (bin o.Add (bin o.Add na a) c) (bin o.Add na (bin o.Add a c)) |> at [right_branch; left_branch]
                left_inverse d a                                    |> at [right_branch; left_branch; left_branch]  // (−a) + a → 0
                ax th (bin o.Add o.Zero c) c                        |> at [right_branch; left_branch]                // 0 + c → c
                refl_true th c                                      |> apply_right
            ]
        let bwd =
            lemma th (Q ==> H) [
                Deduce' (reflex_implies Q)                          |> at [right_branch; left_branch; right_branch] // b → c
                refl_true th (bin o.Add a c)                        |> apply_right
            ]
        ident th (H == Q) [
            mutual_implication' H Q |> Commute |> apply
            Taut (conj fwd bwd) |> apply
        ]

    /// Prove `x = y` by cancellation: rewrite the goal to `c + x = c + y` (15.8, right-to-left), run
    /// `steps` on that, and expect a reflexivity. Gries' own device for (15.9): "add something to both
    /// sides of the equation so that Distributivity can be used".
    let private by_cancel (d: IntegralDomain<'t>) (c: Expr<'t>) (x: Expr<'t>) (y: Expr<'t>) (steps: RuleApplication list) : Rule =
        let th = d :> Theory
        ident th (eq_t x y) ((cancel_add d c x y |> Commute |> apply) :: steps)

    /// **(15.9) Zero**: `a·0 = 0`. Gries' proof, verbatim: cancel `a·a`, then Distributivity and
    /// Identity.
    let zero_mul (d: IntegralDomain<'t>) (a: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let aa = bin o.Mul a a
        by_cancel d aa (bin o.Mul a o.Zero) o.Zero [
            rev th (bin o.Mul a (bin o.Add a o.Zero)) (bin o.Add aa (bin o.Mul a o.Zero)) |> at_left  // a·a + a·0 → a·(a + 0)
            ax th (bin o.Add a o.Zero) a                     |> at [left_branch; right_branch]          // a + 0 → a
            ax th (bin o.Add aa o.Zero) aa                   |> at_right                                // a·a + 0 → a·a
        ]

    /// `0·a = 0` — (15.9) on the left, by symmetry of ·.
    let zero_mul_left (d: IntegralDomain<'t>) (a: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        ident th (eq_t (bin o.Mul o.Zero a) o.Zero) [
            ax th (bin o.Mul o.Zero a) (bin o.Mul a o.Zero) |> at_left
            zero_mul d a                                    |> at_left
        ]

    /// **(15.10a) Unique identity**: `(a + z = a) = (z = 0)`.
    let unique_zero (d: IntegralDomain<'t>) (a: Expr<'t>) (z: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        ident th ((eq_t (bin o.Add a z) a) == (eq_t z o.Zero)) [
            rev th (bin o.Add a o.Zero) a        |> at [left_branch; right_branch]   // a → a + 0
            cancel_add d a z o.Zero              |> at_left
        ]

    /// **(15.15)**: `(x + a = 0) = (x = −a)`.
    let add_eq_zero (d: IntegralDomain<'t>) (x: Expr<'t>) (a: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let na = un o.Neg a
        ident th ((eq_t (bin o.Add x a) o.Zero) == (eq_t x na)) [
            flip th (left_inverse d a)                    |> at [left_branch; right_branch]   // 0 → (−a) + a
            ax th (bin o.Add x a) (bin o.Add a x)         |> at [left_branch; left_branch]
            ax th (bin o.Add na a) (bin o.Add a na)       |> at [left_branch; right_branch]
            cancel_add d a x na                           |> at_left
        ]

    /// **(15.17)**: `−(−a) = a`. Cancel `−a`: both sides become 0.
    let neg_neg (d: IntegralDomain<'t>) (a: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let na = un o.Neg a
        by_cancel d na (un o.Neg na) a [
            ax th (bin o.Add na (un o.Neg na)) o.Zero     |> at_left        // (−a) + −(−a) → 0   (15.13)
            left_inverse d a                              |> at_right       // (−a) + a → 0
        ]

    /// **(15.18)**: `−0 = 0`.
    let neg_zero (d: IntegralDomain<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        by_cancel d o.Zero (un o.Neg o.Zero) o.Zero [
            ax th (bin o.Add o.Zero (un o.Neg o.Zero)) o.Zero   |> at_left  // 0 + −0 → 0   (15.13)
            ax th (bin o.Add o.Zero o.Zero) o.Zero              |> at_right // 0 + 0 → 0
        ]

    /// **(15.20)**: `−a = (−1)·a`. Cancel `a`: `a + (−1)·a` is `(1 + (−1))·a`, which is `0·a`.
    let neg_as_mul (d: IntegralDomain<'t>) (a: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let m1 = un o.Neg o.One
        by_cancel d a (un o.Neg a) (bin o.Mul m1 a) [
            ax th (bin o.Add a (un o.Neg a)) o.Zero                          |> at_left                         // (15.13)
            rev th (bin o.Mul o.One a) a                                     |> at [right_branch; left_branch]  // a → 1·a
            rev th (bin o.Mul (bin o.Add o.One m1) a) (bin o.Add (bin o.Mul o.One a) (bin o.Mul m1 a)) |> at_right
            ax th (bin o.Add o.One m1) o.Zero                                |> at [right_branch; left_branch]  // 1 + (−1) → 0
            zero_mul_left d a                                                |> at_right
        ]

    /// **(15.22)**: `a·(−b) = −(a·b)`. Cancel `a·b`: `a·b + a·(−b)` is `a·(b + (−b))`, which is `a·0`.
    let mul_neg (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let ab = bin o.Mul a b
        let nb = un o.Neg b
        by_cancel d ab (bin o.Mul a nb) (un o.Neg ab) [
            rev th (bin o.Mul a (bin o.Add b nb)) (bin o.Add ab (bin o.Mul a nb)) |> at_left
            ax th (bin o.Add b nb) o.Zero                                    |> at [left_branch; right_branch]  // (15.13)
            zero_mul d a                                                     |> at_left
            ax th (bin o.Add ab (un o.Neg ab)) o.Zero                        |> at_right                        // (15.13)
        ]

    /// `(−a)·b = −(a·b)` — (15.22) on the left.
    let neg_mul (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let na = un o.Neg a
        ident th (eq_t (bin o.Mul na b) (un o.Neg (bin o.Mul a b))) [
            ax th (bin o.Mul na b) (bin o.Mul b na)          |> at_left
            mul_neg d b a                                    |> at_left
            ax th (bin o.Mul b a) (bin o.Mul a b)            |> at [left_branch; apply_unary]
        ]

    /// **(15.21)**: `(−a)·b = a·(−b)` — both are `−(a·b)`.
    let neg_mul_comm (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        ident th (eq_t (bin o.Mul (un o.Neg a) b) (bin o.Mul a (un o.Neg b))) [
            neg_mul d a b                                    |> at_left
            mul_neg d a b                                    |> at_right
        ]

    /// **(15.23)**: `(−a)·(−b) = a·b`.
    let neg_mul_neg (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let nb = un o.Neg b
        ident th (eq_t (bin o.Mul (un o.Neg a) nb) (bin o.Mul a b)) [
            neg_mul_comm d a nb                              |> at_left        // (−a)·(−b) → a·(−(−b))
            neg_neg d b                                      |> at [left_branch; right_branch]
        ]

    /// **(15.24)**: `a − 0 = a`.
    let sub_zero (d: IntegralDomain<'t>) (a: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        ident th (eq_t (bin o.Sub a o.Zero) a) [
            ax th (bin o.Sub a o.Zero) (bin o.Add a (un o.Neg o.Zero)) |> at_left   // (15.14)
            neg_zero d                                                 |> at [left_branch; right_branch]
            ax th (bin o.Add a o.Zero) a                               |> at_left
        ]

    (* Cancellation by a non-zero factor, and no zero divisors *)

    /// `¬(a = 0)`.
    let private nonzero (o: DomainOps<'t>) (a: Expr<'t>) : Prop = !!(eq_t a o.Zero)

    /// A propositional step, proved by the COMPLETE ANF prover (`autoproof`'s search is incomplete and
    /// missed (15.11)'s glue) and replayed in `th` — so that a
    /// theorem of `th` can be combined with it (a theorem may only be used in a theory deriving
    /// from the one it was proved in, and the search's own proof lives in the logic).
    let private tautology (th: Theory) (stmt: Prop) : Theorem =
        lemma th stmt (PropCalculus.autoproof_anf stmt).Steps

    /// **(15.10b) Unique identity**: `a ≠ 0 ⇒ ((a·z = a) = (z = 1))`, by (15.7) Cancellation.
    let unique_one (d: IntegralDomain<'t>) (a: Expr<'t>) (z: Expr<'t>) : Theorem =
        let o, th = d.Ops, d :> Theory
        let nz = nonzero o a
        let cancel = axiom th (nz ==> ((eq_t (bin o.Mul a z) (bin o.Mul a o.One)) == (eq_t z o.One)))
        lemma th (nz ==> ((eq_t (bin o.Mul a z) a) == (eq_t z o.One))) [
            rev th (bin o.Mul a o.One) a        |> at [right_branch; left_branch; right_branch]   // a → a·1
            Deduce' cancel                      |> apply_right                                    // (a·z = a·1) → (z = 1)
            def_true (eq_t z o.One) |> Commute  |> apply_right
        ]

    /// **(15.11)**: `(a·b = 0) = (a = 0 ∨ b = 0)` — an integral domain has no zero divisors.
    ///
    /// `⇐`: either factor zero makes the product zero (15.9). `⇒`: if `a ≠ 0` then `a·b = 0` is
    /// `a·b = a·0`, which by (15.7) Cancellation is `b = 0`. The propositional glue between those facts
    /// is a three-atom tautology, found by search.
    let zero_divisors (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let P, A, B = eq_t (bin o.Mul a b) o.Zero, eq_t a o.Zero, eq_t b o.Zero
        // ¬(a = 0) ⇒ ((a·b = 0) = (b = 0))
        let cancel = axiom th ((nonzero o a) ==> ((eq_t (bin o.Mul a b) (bin o.Mul a o.Zero)) == B))
        let L = lemma th ((nonzero o a) ==> (P == B)) [
                    flip th (zero_mul d a)      |> at [right_branch; left_branch; right_branch]       // 0 → a·0
                    Deduce' cancel              |> apply_right
                    def_true B |> Commute       |> apply_right
                ]
        // a = 0 ⇒ a·b = 0   and   b = 0 ⇒ a·b = 0
        let ta = lemma th (A ==> P) [
                    Deduce' (reflex_implies A)  |> at [right_branch; left_branch; left_branch]        // a → 0
                    zero_mul_left d b           |> at [right_branch; left_branch]
                    refl_true th o.Zero         |> apply_right
                ]
        let tb = lemma th (B ==> P) [
                    Deduce' (reflex_implies B)  |> at [right_branch; left_branch; right_branch]       // b → 0
                    zero_mul d a                |> at [right_branch; left_branch]
                    refl_true th o.Zero         |> apply_right
                ]
        let fwd = mp (tautology th ((!!A ==> (P == B)) ==> (P ==> (A + B)))) L
        let bwd = mp (mp (tautology th ((A ==> P) ==> ((B ==> P) ==> ((A + B) ==> P)))) ta) tb
        ident th (P == (A + B)) [
            mutual_implication' P (A + B) |> Commute |> apply
            Taut (conj fwd bwd) |> apply
        ]

    (* More of subtraction (Gries 15.12 – 15.29) *)

    /// **(15.12) Unique additive inverse**: `(x + a = 0 ∧ y + a = 0) ⇒ x = y`. Each conjunct is
    /// `_ = −a` by (15.15).
    let unique_inverse (d: IntegralDomain<'t>) (x: Expr<'t>) (y: Expr<'t>) (a: Expr<'t>) : Theorem =
        let o, th = d.Ops, d :> Theory
        let na = un o.Neg a
        let hx, hy = eq_t x na, eq_t y na
        lemma th (((eq_t (bin o.Add x a) o.Zero) * (eq_t (bin o.Add y a) o.Zero)) ==> (eq_t x y)) [
            add_eq_zero d x a                   |> at [left_branch; left_branch]      // x + a = 0 → x = −a
            add_eq_zero d y a                   |> at [left_branch; right_branch]
            Deduce' (reflex_implies hx)         |> at [right_branch; left_branch]     // x → −a
            Deduce' (reflex_implies hy)         |> at [right_branch; right_branch]    // y → −a
            refl_true th na                     |> apply_right
        ]

    /// **(15.16)**: `(−a = −b) = (a = b)`.
    let neg_eq (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let na, nb = un o.Neg a, un o.Neg b
        let H, Q = eq_t na nb, eq_t a b
        let fwd =
            lemma th (H ==> Q) [
                flip th (neg_neg d a)           |> at [right_branch; left_branch]                     // a → −(−a)
                flip th (neg_neg d b)           |> at [right_branch; right_branch]                    // b → −(−b)
                Deduce' (reflex_implies H)      |> at [right_branch; left_branch; apply_unary]        // −a → −b
                refl_true th (un o.Neg nb)      |> apply_right
            ]
        let bwd =
            lemma th (Q ==> H) [
                Deduce' (reflex_implies Q)      |> at [right_branch; left_branch; apply_unary]        // a → b
                refl_true th nb                 |> apply_right
            ]
        ident th (H == Q) [
            mutual_implication' H Q |> Commute |> apply
            Taut (conj fwd bwd) |> apply
        ]

    /// **(15.19)**: `−(a + b) = (−a) + (−b)`. Cancel `a + b`: both sides become 0.
    let neg_add (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let na, nb = un o.Neg a, un o.Neg b
        let ab = bin o.Add a b
        let R = bin o.Add na nb
        by_cancel d ab (un o.Neg ab) R [
            ax th (bin o.Add ab (un o.Neg ab)) o.Zero                          |> at_left                          // (15.13)
            ax th (bin o.Add ab R) (bin o.Add a (bin o.Add b R))               |> at_right
            rev th (bin o.Add (bin o.Add b na) nb) (bin o.Add b R)             |> at [right_branch; right_branch]
            ax th (bin o.Add b na) (bin o.Add na b)                            |> at [right_branch; right_branch; left_branch]
            ax th (bin o.Add (bin o.Add na b) nb) (bin o.Add na (bin o.Add b nb)) |> at [right_branch; right_branch]
            ax th (bin o.Add b nb) o.Zero                                      |> at [right_branch; right_branch; right_branch]
            ax th (bin o.Add na o.Zero) na                                     |> at [right_branch; right_branch]
            ax th (bin o.Add a na) o.Zero                                      |> at_right
        ]

    /// `(w + x) + (y + z) = (w + y) + (x + z)` — exchanging the middle terms of a sum of four.
    let interchange (d: IntegralDomain<'t>) (w: Expr<'t>) (x: Expr<'t>) (y: Expr<'t>) (z: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let s = bin o.Add
        ident th (eq_t (s (s w x) (s y z)) (s (s w y) (s x z))) [
            ax th (s (s w x) (s y z)) (s w (s x (s y z)))            |> at_left
            rev th (s (s x y) z) (s x (s y z))                       |> at [left_branch; right_branch]
            ax th (s x y) (s y x)                                    |> at [left_branch; right_branch; left_branch]
            ax th (s (s y x) z) (s y (s x z))                        |> at [left_branch; right_branch]
            rev th (s (s w y) (s x z)) (s w (s y (s x z)))           |> at_left
        ]

    /// **(15.25)**: `(a − b) + (c − d) = (a + c) − (b + d)`.
    let sub_add_sub (dom: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (c: Expr<'t>) (d: Expr<'t>) : Rule =
        let o, th = dom.Ops, dom :> Theory
        let s, n = bin o.Add, un o.Neg
        ident th (eq_t (s (bin o.Sub a b) (bin o.Sub c d)) (bin o.Sub (s a c) (s b d))) [
            ax th (bin o.Sub a b) (s a (n b))                        |> at [left_branch; left_branch]     // (15.14)
            ax th (bin o.Sub c d) (s c (n d))                        |> at [left_branch; right_branch]
            interchange dom a (n b) c (n d)                          |> at_left
            ax th (bin o.Sub (s a c) (s b d)) (s (s a c) (n (s b d))) |> at_right
            neg_add dom b d                                          |> at [right_branch; right_branch]   // (15.19)
        ]

    /// **(15.26)**: `(a − b) − (c − d) = (a + d) − (b + c)`.
    let sub_sub_sub (dom: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (c: Expr<'t>) (d: Expr<'t>) : Rule =
        let o, th = dom.Ops, dom :> Theory
        let s, n = bin o.Add, un o.Neg
        let amb, cmd = bin o.Sub a b, bin o.Sub c d
        ident th (eq_t (bin o.Sub amb cmd) (bin o.Sub (s a d) (s b c))) [
            ax th (bin o.Sub amb cmd) (s amb (n cmd))                |> at_left
            ax th amb (s a (n b))                                    |> at [left_branch; left_branch]
            ax th cmd (s c (n d))                                    |> at [left_branch; right_branch; apply_unary]
            neg_add dom c (n d)                                      |> at [left_branch; right_branch]   // −(c + −d) → −c + −(−d)
            neg_neg dom d                                            |> at [left_branch; right_branch; right_branch]
            ax th (s (n c) d) (s d (n c))                            |> at [left_branch; right_branch]
            interchange dom a (n b) d (n c)                          |> at_left
            ax th (bin o.Sub (s a d) (s b c)) (s (s a d) (n (s b c))) |> at_right
            neg_add dom b c                                          |> at [right_branch; right_branch]
        ]

    /// **(15.27)**: `(a − b)·(c − d) = (a·c + b·d) − (a·d + b·c)`.
    let sub_mul_sub (dom: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (c: Expr<'t>) (d: Expr<'t>) : Rule =
        let o, th = dom.Ops, dom :> Theory
        let s, m, n = bin o.Add, bin o.Mul, un o.Neg
        let cd' = s c (n d)
        ident th (eq_t (m (bin o.Sub a b) (bin o.Sub c d)) (bin o.Sub (s (m a c) (m b d)) (s (m a d) (m b c)))) [
            ax th (bin o.Sub a b) (s a (n b))                        |> at [left_branch; left_branch]
            ax th (bin o.Sub c d) cd'                                |> at [left_branch; right_branch]
            ax th (m (s a (n b)) cd') (s (m a cd') (m (n b) cd'))    |> at_left                          // right distributivity
            ax th (m a cd') (s (m a c) (m a (n d)))                  |> at [left_branch; left_branch]
            ax th (m (n b) cd') (s (m (n b) c) (m (n b) (n d)))      |> at [left_branch; right_branch]
            mul_neg dom a d                                          |> at [left_branch; left_branch; right_branch]   // a·(−d) → −(a·d)
            neg_mul dom b c                                          |> at [left_branch; right_branch; left_branch]   // (−b)·c → −(b·c)
            neg_mul_neg dom b d                                      |> at [left_branch; right_branch; right_branch]  // (−b)·(−d) → b·d
            ax th (s (n (m b c)) (m b d)) (s (m b d) (n (m b c)))    |> at [left_branch; right_branch]
            interchange dom (m a c) (n (m a d)) (m b d) (n (m b c))  |> at_left
            ax th (bin o.Sub (s (m a c) (m b d)) (s (m a d) (m b c))) (s (s (m a c) (m b d)) (n (s (m a d) (m b c)))) |> at_right
            neg_add dom (m a d) (m b c)                              |> at [right_branch; right_branch]
        ]

    /// **(15.28)**: `(a − b = c − d) = (a + d = b + c)`. Cancel `b + d` on the left equation.
    let sub_eq_sub (dom: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (c: Expr<'t>) (d: Expr<'t>) : Rule =
        let o, th = dom.Ops, dom :> Theory
        let s, n = bin o.Add, un o.Neg
        let bd = s b d
        ident th ((eq_t (bin o.Sub a b) (bin o.Sub c d)) == (eq_t (s a d) (s b c))) [
            cancel_add dom bd (bin o.Sub a b) (bin o.Sub c d) |> Commute |> at_left    // (b+d) + (a−b) = (b+d) + (c−d)
            // left: (b + d) + (a − b) → a + d
            ax th (bin o.Sub a b) (s a (n b))                        |> at [left_branch; left_branch; right_branch]
            ax th (s a (n b)) (s (n b) a)                            |> at [left_branch; left_branch; right_branch]
            interchange dom b d (n b) a                              |> at [left_branch; left_branch]
            ax th (s b (n b)) o.Zero                                 |> at [left_branch; left_branch; left_branch]
            ax th (s o.Zero (s d a)) (s d a)                         |> at [left_branch; left_branch]
            ax th (s d a) (s a d)                                    |> at [left_branch; left_branch]
            // right: (b + d) + (c − d) → b + c
            ax th (bin o.Sub c d) (s c (n d))                        |> at [left_branch; right_branch; right_branch]
            interchange dom b d c (n d)                              |> at [left_branch; right_branch]
            ax th (s d (n d)) o.Zero                                 |> at [left_branch; right_branch; right_branch]
            ax th (s (s b c) o.Zero) (s b c)                         |> at [left_branch; right_branch]
        ]

    /// **(15.29)**: `(a − b)·c = a·c − b·c`.
    let sub_mul (d: IntegralDomain<'t>) (a: Expr<'t>) (b: Expr<'t>) (c: Expr<'t>) : Rule =
        let o, th = d.Ops, d :> Theory
        let s, m, n = bin o.Add, bin o.Mul, un o.Neg
        ident th (eq_t (m (bin o.Sub a b) c) (bin o.Sub (m a c) (m b c))) [
            ax th (bin o.Sub a b) (s a (n b))                        |> at [left_branch; left_branch]
            ax th (m (s a (n b)) c) (s (m a c) (m (n b) c))          |> at_left
            neg_mul d b c                                            |> at [left_branch; right_branch]
            ax th (bin o.Sub (m a c) (m b c)) (s (m a c) (n (m b c))) |> at_right
        ]