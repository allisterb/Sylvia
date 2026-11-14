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
///
/// Proofs are written over `DomainTerm`s (`DomainTerm.fs`), whose `+ · −` are the domain's own
/// operators: `d.Term e` / `d.Var "a"` / `d.Zero` / `d.One` make them. The theorems take any
/// `Term<'t>` — an `IntTerm`, an `IntVar`, a `DomainTerm` — and read it as a term of `d`.
module IntegralDomain =

    (* The operators *)

    /// The operators of an integral domain: `+`, `·`, `0`, `1`, unary `−` and binary `−`.
    type DomainOps<'t when 't: equality> = DomainTerms.DomainOps<'t>

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
    let internal (|NonZero|_|) (zero: Expr<'t>) =
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
    /// `extends`: theory instances whose theorems this one accepts (see `Theory.Includes`).
    type IntegralDomain<'t when 't: equality>(ops: DomainOps<'t>, ?axioms: Axioms, ?rules: Rules, ?extends: Theory list) =
        inherit Theory(BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None)) (integral_domain_axioms ops),
                       defaultArg rules [], extends = defaultArg extends [])
        member val Ops = ops
        /// The order operators, when the domain is ordered (`OrderedDomain` overrides this).
        abstract Ordering: OrderOps<'t> option
        default _.Ordering = None
        /// An expression as a term of this domain.
        member x.Term (e: Expr<'t>) = DomainTerm<'t>(e, ops, x.Ordering)
        /// Any term — an `IntTerm`, an `IntVar`, a term of this domain — as a term of this domain.
        member x.Term (t: Term<'t>) = DomainTerm<'t>(t.Expr, ops, x.Ordering)
        /// A variable of this domain: both a quantifier dummy and a term.
        member x.Var (name: string) = DomainVar<'t>(name, ops, x.Ordering)
        /// `0`, as a term.
        member x.Zero = x.Term ops.Zero
        /// `1`, as a term.
        member x.One = x.Term ops.One

    (* Building terms *)

    /// `a = b`, as a proposition, from raw expressions.
    let eq_t (a: Expr<'t>) (b: Expr<'t>) : Prop = eq_e a b

    (* Local devices *)

    // An axiom instance `lhs = rhs` as a rewrite lhs → rhs, and right-to-left as rhs → lhs. Nothing in
    // the logic flips an equation of a non-boolean type, so the reverse is a lemma proved by rewriting
    // the other side of its own goal.
    let internal ax (th: Theory) (lhs: Term<'t>) (rhs: Term<'t>) : Rule = ax_ident th (eq_e lhs.Expr rhs.Expr)
    let internal rev (th: Theory) (lhs: Term<'t>) (rhs: Term<'t>) : Rule = ident th (eq_e rhs.Expr lhs.Expr) [ ax th lhs rhs |> at_right ]
    /// A derived identity `l = r` used right-to-left, as `r → l`: the same `=` call with its operands
    /// swapped, proved by rewriting its right side with the identity itself.
    let internal flip (th: Theory) (rule: Rule) : Rule =
        match rule with
        | Derive(_, p, _) ->
            match p.Stmt with
            | Call(None, mi, [l; r]) when mi.Name = "op_Equality" ->
                ident th (Prop(Expr.Cast<bool> (Expr.Call(mi, [r; l])))) [ rule |> at_right ]
            | _ -> failwith "flip: not an identity"
        | _ -> failwith "flip: not a derived rule"
    /// `(x = x) → T`, at any type.
    let internal refl_true (th: Theory) (x: Term<'t>) = ax_ident th (T == (eq_e x.Expr x.Expr)) |> Commute

    (* Theorems for integral domains (Gries 15.8 – 15.24) *)

    /// **(15.14) Subtraction**, as a rewrite: `a − b → a + (−b)`. An axiom instance.
    let sub_def (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let a, b = d.Term a, d.Term b
        ax d (a - b) (a + -b)

    /// `(−a) + a = 0` — the LEFT inverse, from (15.13) and symmetry.
    let left_inverse (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let a = d.Term a
        ident d ((-a) + a == d.Zero) [
            ax d ((-a) + a) (a + -a) |> at_left
        ]

    /// **(15.8) Cancellation**: `(a + b = a + c) = (b = c)`. By mutual implication: `⇐` is Leibniz,
    /// `⇒` is Gries' chain `b = 0 + b = ((−a) + a) + b = (−a) + (a + b) = (−a) + (a + c) = … = c`, with
    /// the antecedent used to replace `a + b` by `a + c`.
    let cancel_add (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Rule =
        let zero = d.Zero
        let a, b, c = d.Term a, d.Term b, d.Term c
        let na = -a
        let H = a + b == a + c
        let Q = b == c
        let fwd =
            lemma d (H ==> Q) [
                rev d (zero + b) b                         |> at [right_branch; left_branch]                // b → 0 + b
                flip d (left_inverse d a)                  |> at [right_branch; left_branch; left_branch]  // 0 → (−a) + a
                ax d ((na + a) + b) (na + (a + b))         |> at [right_branch; left_branch]
                Deduce' (reflex_implies H)                  |> at [right_branch; left_branch; right_branch] // a + b → a + c
                rev d ((na + a) + c) (na + (a + c))        |> at [right_branch; left_branch]
                left_inverse d a                            |> at [right_branch; left_branch; left_branch]  // (−a) + a → 0
                ax d (zero + c) c                          |> at [right_branch; left_branch]                // 0 + c → c
                refl_true d c                              |> apply_right
            ]
        let bwd =
            lemma d (Q ==> H) [
                Deduce' (reflex_implies Q)                  |> at [right_branch; left_branch; right_branch] // b → c
                refl_true d (a + c)                        |> apply_right
            ]
        ident d (H == Q) [
            mutual_implication' H Q |> Commute |> apply
            Taut (conj fwd bwd) |> apply
        ]

    /// Prove `x = y` by cancellation: rewrite the goal to `c + x = c + y` (15.8, right-to-left), run
    /// `steps` on that, and expect a reflexivity. Gries' own device for (15.9): "add something to both
    /// sides of the equation so that Distributivity can be used".
    let internal by_cancel (d: IntegralDomain<'t>) (c: DomainTerm<'t>) (x: DomainTerm<'t>) (y: DomainTerm<'t>) (steps: RuleApplication list) : Rule =
        ident d (x == y) ((cancel_add d c x y |> Commute |> apply) :: steps)

    /// **(15.9) Zero**: `a·0 = 0`. Gries' proof, verbatim: cancel `a·a`, then Distributivity and
    /// Identity.
    let zero_mul (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let zero, a = d.Zero, d.Term a
        let aa = a * a
        by_cancel d aa (a * zero) zero [
            rev d (a * (a + zero)) (aa + a * zero)     |> at_left                      // a·a + a·0 → a·(a + 0)
            ax d (a + zero) a                          |> at [left_branch; right_branch] // a + 0 → a
            ax d (aa + zero) aa                        |> at_right                     // a·a + 0 → a·a
        ]

    /// `0·a = 0` — (15.9) on the left, by symmetry of ·.
    let zero_mul_left (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let zero, a = d.Zero, d.Term a
        ident d (zero * a == zero) [
            ax d (zero * a) (a * zero)                 |> at_left
            zero_mul d a                                |> at_left
        ]

    /// **(15.10a) Unique identity**: `(a + z = a) = (z = 0)`.
    let unique_zero (d: IntegralDomain<'t>) (a: Term<'t>) (z: Term<'t>) : Rule =
        let zero = d.Zero
        let a, z = d.Term a, d.Term z
        ident d ((a + z == a) == (z == zero)) [
            rev d (a + zero) a                         |> at [left_branch; right_branch]   // a → a + 0
            cancel_add d a z zero                       |> at_left
        ]

    /// **(15.15)**: `(x + a = 0) = (x = −a)`.
    let add_eq_zero (d: IntegralDomain<'t>) (x: Term<'t>) (a: Term<'t>) : Rule =
        let zero = d.Zero
        let x, a = d.Term x, d.Term a
        let na = -a
        ident d ((x + a == zero) == (x == na)) [
            flip d (left_inverse d a)                  |> at [left_branch; right_branch]   // 0 → (−a) + a
            ax d (x + a) (a + x)                       |> at [left_branch; left_branch]
            ax d (na + a) (a + na)                     |> at [left_branch; right_branch]
            cancel_add d a x na                         |> at_left
        ]

    /// **(15.17)**: `−(−a) = a`. Cancel `−a`: both sides become 0.
    let neg_neg (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let zero, a = d.Zero, d.Term a
        let na = -a
        by_cancel d na (-na) a [
            ax d (na + -na) zero                       |> at_left        // (−a) + −(−a) → 0   (15.13)
            left_inverse d a                            |> at_right       // (−a) + a → 0
        ]

    /// **(15.18)**: `−0 = 0`.
    let neg_zero (d: IntegralDomain<'t>) : Rule =
        let zero = d.Zero
        by_cancel d zero (-zero) zero [
            ax d (zero + -zero) zero                   |> at_left  // 0 + −0 → 0   (15.13)
            ax d (zero + zero) zero                    |> at_right // 0 + 0 → 0
        ]

    /// **(15.20)**: `−a = (−1)·a`. Cancel `a`: `a + (−1)·a` is `(1 + (−1))·a`, which is `0·a`.
    let neg_as_mul (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let zero, one, a = d.Zero, d.One, d.Term a
        let m1 = -one
        by_cancel d a (-a) (m1 * a) [
            ax d (a + -a) zero                         |> at_left                         // (15.13)
            rev d (one * a) a                          |> at [right_branch; left_branch]  // a → 1·a
            rev d ((one + m1) * a) (one * a + m1 * a)  |> at_right
            ax d (one + m1) zero                       |> at [right_branch; left_branch]  // 1 + (−1) → 0
            zero_mul_left d a                           |> at_right
        ]

    /// **(15.22)**: `a·(−b) = −(a·b)`. Cancel `a·b`: `a·b + a·(−b)` is `a·(b + (−b))`, which is `a·0`.
    let mul_neg (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let zero = d.Zero
        let a, b = d.Term a, d.Term b
        let ab, nb = a * b, -b
        by_cancel d ab (a * nb) (-ab) [
            rev d (a * (b + nb)) (ab + a * nb)         |> at_left
            ax d (b + nb) zero                         |> at [left_branch; right_branch]  // (15.13)
            zero_mul d a                                |> at_left
            ax d (ab + -ab) zero                       |> at_right                        // (15.13)
        ]

    /// `(−a)·b = −(a·b)` — (15.22) on the left.
    let neg_mul (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let a, b = d.Term a, d.Term b
        ident d ((-a) * b == -(a * b)) [
            ax d ((-a) * b) (b * -a)                   |> at_left
            mul_neg d b a                               |> at_left
            ax d (b * a) (a * b)                       |> at [left_branch; apply_unary]
        ]

    /// **(15.21)**: `(−a)·b = a·(−b)` — both are `−(a·b)`.
    let neg_mul_comm (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let a, b = d.Term a, d.Term b
        ident d ((-a) * b == a * -b) [
            neg_mul d a b                               |> at_left
            mul_neg d a b                               |> at_right
        ]

    /// **(15.23)**: `(−a)·(−b) = a·b`.
    let neg_mul_neg (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let a, b = d.Term a, d.Term b
        let nb = -b
        ident d ((-a) * nb == a * b) [
            neg_mul_comm d a nb                         |> at_left        // (−a)·(−b) → a·(−(−b))
            neg_neg d b                                 |> at [left_branch; right_branch]
        ]

    /// **(15.24)**: `a − 0 = a`.
    let sub_zero (d: IntegralDomain<'t>) (a: Term<'t>) : Rule =
        let zero, a = d.Zero, d.Term a
        ident d (a - zero == a) [
            ax d (a - zero) (a + -zero)                |> at_left   // (15.14)
            neg_zero d                                  |> at [left_branch; right_branch]
            ax d (a + zero) a                          |> at_left
        ]

    (* Cancellation by a non-zero factor, and no zero divisors *)

    /// A propositional step, proved by the COMPLETE ANF prover (`autoproof`'s search is incomplete and
    /// missed (15.11)'s glue) and replayed in `th` — so that a
    /// theorem of `th` can be combined with it (a theorem may only be used in a theory deriving
    /// from the one it was proved in, and the search's own proof lives in the logic).
    let internal tautology (th: Theory) (stmt: Prop) : Theorem =
        lemma th stmt (PropCalculus.autoproof_anf stmt).Steps

    /// **(15.10b) Unique identity**: `a ≠ 0 ⇒ ((a·z = a) = (z = 1))`, by (15.7) Cancellation.
    let unique_one (d: IntegralDomain<'t>) (a: Term<'t>) (z: Term<'t>) : Theorem =
        let one = d.One
        let a, z = d.Term a, d.Term z
        let nz = a != d.Zero
        let cancel = axiom d (nz ==> ((a * z == a * one) == (z == one)))
        lemma d (nz ==> ((a * z == a) == (z == one))) [
            rev d (a * one) a                          |> at [right_branch; left_branch; right_branch]   // a → a·1
            Deduce' cancel                              |> apply_right                                    // (a·z = a·1) → (z = 1)
            def_true (z == one) |> Commute              |> apply_right
        ]

    /// **(15.11)**: `(a·b = 0) = (a = 0 ∨ b = 0)` — an integral domain has no zero divisors.
    ///
    /// `⇐`: either factor zero makes the product zero (15.9). `⇒`: if `a ≠ 0` then `a·b = 0` is
    /// `a·b = a·0`, which by (15.7) Cancellation is `b = 0`. The propositional glue between those facts
    /// is a three-atom tautology, found by search.
    let zero_divisors (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let zero = d.Zero
        let a, b = d.Term a, d.Term b
        let P, A, B = (a * b == zero), (a == zero), (b == zero)
        // ¬(a = 0) ⇒ ((a·b = 0) = (b = 0))
        let cancel = axiom d ((a != zero) ==> ((a * b == a * zero) == B))
        let L = lemma d ((a != zero) ==> (P == B)) [
                    flip d (zero_mul d a)      |> at [right_branch; left_branch; right_branch]       // 0 → a·0
                    Deduce' cancel              |> apply_right
                    def_true B |> Commute       |> apply_right
                ]
        // a = 0 ⇒ a·b = 0   and   b = 0 ⇒ a·b = 0
        let ta = lemma d (A ==> P) [
                    Deduce' (reflex_implies A)  |> at [right_branch; left_branch; left_branch]        // a → 0
                    zero_mul_left d b           |> at [right_branch; left_branch]
                    refl_true d zero           |> apply_right
                ]
        let tb = lemma d (B ==> P) [
                    Deduce' (reflex_implies B)  |> at [right_branch; left_branch; right_branch]       // b → 0
                    zero_mul d a                |> at [right_branch; left_branch]
                    refl_true d zero           |> apply_right
                ]
        let fwd = mp (tautology d ((!!A ==> (P == B)) ==> (P ==> (A + B)))) L
        let bwd = mp (mp (tautology d ((A ==> P) ==> ((B ==> P) ==> ((A + B) ==> P)))) ta) tb
        ident d (P == (A + B)) [
            mutual_implication' P (A + B) |> Commute |> apply
            Taut (conj fwd bwd) |> apply
        ]

    (* More of subtraction (Gries 15.12 – 15.29) *)

    /// **(15.12) Unique additive inverse**: `(x + a = 0 ∧ y + a = 0) ⇒ x = y`. Each conjunct is
    /// `_ = −a` by (15.15).
    let unique_inverse (d: IntegralDomain<'t>) (x: Term<'t>) (y: Term<'t>) (a: Term<'t>) : Theorem =
        let zero = d.Zero
        let x, y, a = d.Term x, d.Term y, d.Term a
        let na = -a
        let hx, hy = (x == na), (y == na)
        lemma d (((x + a == zero) * (y + a == zero)) ==> (x == y)) [
            add_eq_zero d x a                   |> at [left_branch; left_branch]      // x + a = 0 → x = −a
            add_eq_zero d y a                   |> at [left_branch; right_branch]
            Deduce' (reflex_implies hx)         |> at [right_branch; left_branch]     // x → −a
            Deduce' (reflex_implies hy)         |> at [right_branch; right_branch]    // y → −a
            refl_true d na                     |> apply_right
        ]

    /// **(15.16)**: `(−a = −b) = (a = b)`.
    let neg_eq (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let a, b = d.Term a, d.Term b
        let na, nb = -a, -b
        let H, Q = (na == nb), (a == b)
        let fwd =
            lemma d (H ==> Q) [
                flip d (neg_neg d a)           |> at [right_branch; left_branch]                     // a → −(−a)
                flip d (neg_neg d b)           |> at [right_branch; right_branch]                    // b → −(−b)
                Deduce' (reflex_implies H)      |> at [right_branch; left_branch; apply_unary]        // −a → −b
                refl_true d (-nb)              |> apply_right
            ]
        let bwd =
            lemma d (Q ==> H) [
                Deduce' (reflex_implies Q)      |> at [right_branch; left_branch; apply_unary]        // a → b
                refl_true d nb                 |> apply_right
            ]
        ident d (H == Q) [
            mutual_implication' H Q |> Commute |> apply
            Taut (conj fwd bwd) |> apply
        ]

    /// **(15.19)**: `−(a + b) = (−a) + (−b)`. Cancel `a + b`: both sides become 0.
    let neg_add (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) : Rule =
        let zero = d.Zero
        let a, b = d.Term a, d.Term b
        let na, nb = -a, -b
        let ab = a + b
        let R = na + nb
        by_cancel d ab (-ab) R [
            ax d (ab + -ab) zero                       |> at_left                          // (15.13)
            ax d (ab + R) (a + (b + R))                |> at_right
            rev d ((b + na) + nb) (b + R)              |> at [right_branch; right_branch]
            ax d (b + na) (na + b)                     |> at [right_branch; right_branch; left_branch]
            ax d ((na + b) + nb) (na + (b + nb))       |> at [right_branch; right_branch]
            ax d (b + nb) zero                         |> at [right_branch; right_branch; right_branch]
            ax d (na + zero) na                        |> at [right_branch; right_branch]
            ax d (a + na) zero                         |> at_right
        ]

    /// `(w + x) + (y + z) = (w + y) + (x + z)` — exchanging the middle terms of a sum of four.
    let interchange (d: IntegralDomain<'t>) (w: Term<'t>) (x: Term<'t>) (y: Term<'t>) (z: Term<'t>) : Rule =
        let w, x, y, z = d.Term w, d.Term x, d.Term y, d.Term z
        ident d ((w + x) + (y + z) == (w + y) + (x + z)) [
            ax d ((w + x) + (y + z)) (w + (x + (y + z)))       |> at_left
            rev d ((x + y) + z) (x + (y + z))                  |> at [left_branch; right_branch]
            ax d (x + y) (y + x)                               |> at [left_branch; right_branch; left_branch]
            ax d ((y + x) + z) (y + (x + z))                   |> at [left_branch; right_branch]
            rev d ((w + y) + (x + z)) (w + (y + (x + z)))      |> at_left
        ]

    /// **(15.25)**: `(a − b) + (c − d) = (a + c) − (b + d)`.
    let sub_add_sub (dom: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) (d: Term<'t>) : Rule =
        let a, b, c, d = dom.Term a, dom.Term b, dom.Term c, dom.Term d
        ident dom ((a - b) + (c - d) == (a + c) - (b + d)) [
            ax dom (a - b) (a + -b)                              |> at [left_branch; left_branch]     // (15.14)
            ax dom (c - d) (c + -d)                              |> at [left_branch; right_branch]
            interchange dom a (-b) c (-d)                       |> at_left
            ax dom ((a + c) - (b + d)) ((a + c) + -(b + d))      |> at_right
            neg_add dom b d                                     |> at [right_branch; right_branch]   // (15.19)
        ]

    /// **(15.26)**: `(a − b) − (c − d) = (a + d) − (b + c)`.
    let sub_sub_sub (dom: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) (d: Term<'t>) : Rule =
        let a, b, c, d = dom.Term a, dom.Term b, dom.Term c, dom.Term d
        let amb, cmd = a - b, c - d
        ident dom (amb - cmd == (a + d) - (b + c)) [
            ax dom (amb - cmd) (amb + -cmd)                      |> at_left
            ax dom amb (a + -b)                                  |> at [left_branch; left_branch]
            ax dom cmd (c + -d)                                  |> at [left_branch; right_branch; apply_unary]
            neg_add dom c (-d)                                  |> at [left_branch; right_branch]   // −(c + −d) → −c + −(−d)
            neg_neg dom d                                       |> at [left_branch; right_branch; right_branch]
            ax dom ((-c) + d) (d + -c)                           |> at [left_branch; right_branch]
            interchange dom a (-b) d (-c)                       |> at_left
            ax dom ((a + d) - (b + c)) ((a + d) + -(b + c))      |> at_right
            neg_add dom b c                                     |> at [right_branch; right_branch]
        ]

    /// **(15.27)**: `(a − b)·(c − d) = (a·c + b·d) − (a·d + b·c)`.
    let sub_mul_sub (dom: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) (d: Term<'t>) : Rule =
        let a, b, c, d = dom.Term a, dom.Term b, dom.Term c, dom.Term d
        let cd' = c + -d
        let lhs, rhs = a * d + b * c, a * c + b * d
        ident dom ((a - b) * (c - d) == rhs - lhs) [
            ax dom (a - b) (a + -b)                              |> at [left_branch; left_branch]
            ax dom (c - d) cd'                                   |> at [left_branch; right_branch]
            ax dom ((a + -b) * cd') (a * cd' + (-b) * cd')       |> at_left                          // right distributivity
            ax dom (a * cd') (a * c + a * -d)                    |> at [left_branch; left_branch]
            ax dom ((-b) * cd') ((-b) * c + (-b) * -d)           |> at [left_branch; right_branch]
            mul_neg dom a d                                     |> at [left_branch; left_branch; right_branch]   // a·(−d) → −(a·d)
            neg_mul dom b c                                     |> at [left_branch; right_branch; left_branch]   // (−b)·c → −(b·c)
            neg_mul_neg dom b d                                 |> at [left_branch; right_branch; right_branch]  // (−b)·(−d) → b·d
            ax dom (-(b * c) + b * d) (b * d + -(b * c))         |> at [left_branch; right_branch]
            interchange dom (a * c) (-(a * d)) (b * d) (-(b * c)) |> at_left
            ax dom (rhs - lhs) (rhs + -lhs)                      |> at_right
            neg_add dom (a * d) (b * c)                         |> at [right_branch; right_branch]
        ]

    /// **(15.28)**: `(a − b = c − d) = (a + d = b + c)`. Cancel `b + d` on the left equation.
    let sub_eq_sub (dom: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) (d: Term<'t>) : Rule =
        let zero = dom.Zero
        let a, b, c, d = dom.Term a, dom.Term b, dom.Term c, dom.Term d
        let bd = b + d
        ident dom ((a - b == c - d) == (a + d == b + c)) [
            cancel_add dom bd (a - b) (c - d) |> Commute        |> at_left    // (b+d) + (a−b) = (b+d) + (c−d)
            // left: (b + d) + (a − b) → a + d
            ax dom (a - b) (a + -b)                              |> at [left_branch; left_branch; right_branch]
            ax dom (a + -b) ((-b) + a)                           |> at [left_branch; left_branch; right_branch]
            interchange dom b d (-b) a                          |> at [left_branch; left_branch]
            ax dom (b + -b) zero                                 |> at [left_branch; left_branch; left_branch]
            ax dom (zero + (d + a)) (d + a)                      |> at [left_branch; left_branch]
            ax dom (d + a) (a + d)                               |> at [left_branch; left_branch]
            // right: (b + d) + (c − d) → b + c
            ax dom (c - d) (c + -d)                              |> at [left_branch; right_branch; right_branch]
            interchange dom b d c (-d)                          |> at [left_branch; right_branch]
            ax dom (d + -d) zero                                 |> at [left_branch; right_branch; right_branch]
            ax dom ((b + c) + zero) (b + c)                      |> at [left_branch; right_branch]
        ]

    /// **(15.29)**: `(a − b)·c = a·c − b·c`.
    let sub_mul (d: IntegralDomain<'t>) (a: Term<'t>) (b: Term<'t>) (c: Term<'t>) : Rule =
        let a, b, c = d.Term a, d.Term b, d.Term c
        ident d ((a - b) * c == a * c - b * c) [
            ax d (a - b) (a + -b)                              |> at [left_branch; left_branch]
            ax d ((a + -b) * c) (a * c + (-b) * c)             |> at_left
            neg_mul d b c                                       |> at [left_branch; right_branch]
            ax d (a * c - b * c) (a * c + -(b * c))            |> at_right
        ]
