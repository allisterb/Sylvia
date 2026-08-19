namespace Sylvia

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open FsExpr
open Formula
open Patterns
open Descriptions

/// Σ — the generalized SUM quantifier of Gries §8.2 at ★ := integer `+`, with its axioms.
///
/// §8.2 asks of a quantified operator ★ only that it be symmetric, associative and have a unit; `+`
/// is, with unit 0. Those three properties and Σ's two own axioms — Empty range (8.13) and Range
/// split (8.18) — are the whole content of this module. The three axioms that are already GENERIC
/// over the quantified operator (One-Point 8.14, Nesting 8.20, Dummy renaming 8.21, all keyed on
/// `Quantifier`, which covers `Sum`) apply to a Σ term as they stand and are not restated here.
///
/// Where this is used: Gries defines Size (11.12) as `#S = (Σx | x∈S : 1)`, so every size law is a
/// Σ law with a membership range — see `SetTheory`.
module Sums =

    let private desc = axiom_desc "Sums"

    (* Formula *)

    /// The quantified operator. A NAMED function, so `sum`'s eta-expansion of it is
    /// `fun l r -> int_add l r` and the axioms below can key on the name — the device
    /// `SetAlgebra.union`/`intersect` use, and what keeps a Σ from matching a family-union axiom
    /// (and vice versa) even though both are `Formula.sum` terms.
    let int_add (l: int) (r: int) : int = l + r

    /// (Σx | R : E) — §8.2's `(★x | R : E)` at ★ := `int_add`.
    [<Formula>]
    let sigma<'u> (bound: 'u) (range: bool) (body: int) : int = sum int_add "Σ" bound range body

    (* Patterns *)

    /// A Σ term: a `Formula.sum` whose operator is integer addition.
    let (|Sigma|_|) =
        function
        | SumTerm(Lambda(_, Lambda(_, Call(None, mi, _))), _, bound, range, body) when mi.Name = "int_add" ->
            Some(bound, range, body)
        | _ -> None

    (* Axioms *)

    // Hoisted operator templates — a quotation literal in a match arm re-deserializes its pickled
    // template on every probe (see docs/expressions-perf.md).
    let private eq_op : Expr<int -> int -> bool> = <@ (=) @>
    let private add_op : Expr<int -> int -> int> = <@ (+) @>
    let private zero_int : Expr<int> = <@ 0 @>

    /// (Σx | false : E) = 0   — Empty range (8.13) at ★ := +, whose unit is 0.
    ///
    /// This is the axiom that is NOT generic over the quantified operator: it names the unit, and
    /// `EquationalLogic`'s (8.13) is stated for ∀/∃ (unit `true`/`false`) only.
    let (|SumEmptyRange|_|) =
        function
        | Equals(Sigma(_, False, _), Int32 0) -> pattern_name "Empty Range" |> Some
        | _ -> None

    /// (Σx | R∨Q : E) + (Σx | R∧Q : E) = (Σx | R : E) + (Σx | Q : E)   — Range split (8.18).
    ///
    /// The UNCONDITIONAL form, which is why it is the one worth having: (8.16) needs the ranges
    /// disjoint and (8.17) needs ★ idempotent, and `+` is not idempotent. The disjoint case follows
    /// from this one plus Empty range, since a `false` split range contributes 0.
    let (|SumRangeSplit|_|) =
        function
        | Equals(Add(Sigma(b1, Or(R1, Q1), E1), Sigma(b2, And(R2, Q2), E2)),
                 Add(Sigma(b3, R3, E3), Sigma(b4, Q4, E4)))
            when vequal' b1 b2 && vequal' b2 b3 && vequal' b3 b4
                 && sequal R1 R2 && sequal R2 R3 && sequal Q1 Q2 && sequal Q2 Q4
                 && sequal E1 E2 && sequal E2 E3 && sequal E3 E4 -> pattern_name "Range Split" |> Some
        | _ -> None

    /// (Σx | R : E) + (Σx | R : F) = (Σx | R : E + F)   — Distributivity (8.15) at ★ := +.
    ///
    /// Unconditional for `+`: at an empty range both sides are `0 + 0` and `0`. (Gries states 8.15
    /// with the proviso that the range be non-empty or ★ be idempotent, which is what a ★ WITHOUT a
    /// unit would need; `+` has one.)
    let (|SumDistrib|_|) =
        function
        | Equals(Add(Sigma(b1, R1, E), Sigma(b2, R2, F)), Sigma(b3, R3, Add(E', F')))
            when vequal' b1 b2 && vequal' b2 b3 && sequal R1 R2 && sequal R2 R3
                 && sequal E E' && sequal F F' -> pattern_name "Distributivity" |> Some
        | _ -> None

    /// What §8.2 requires of the quantified operator, for ★ := `+`: symmetry, associativity, and a
    /// unit. Stated at `int` (`Binary` carries the operand type), and only these three — the rest of
    /// integer arithmetic is `Integers`, which is a separate theory built on top of this one.
    let sum_axioms =
        function
        | Assoc eq_op add_op x
        | Commute eq_op add_op x
        | Identity eq_op add_op zero_int x
        | SumEmptyRange x
        | SumRangeSplit x
        | SumDistrib x -> Some (desc x)
        | _ -> None

    (* Builders *)

    /// (Σx | R : E), symbolically. Range and body are given as a proposition / integer term in `x`,
    /// mirroring `PredCalculus.qall`/`qex` and `SetTheory.qunion`.
    let qsum (x: #ISymbolicVar<'u>) (R: Prop) (E: Term<int>) : IntTerm =
        IntTerm(<@ sigma %x.Expr %R.Expr %E.Expr @>)

    /// An integer literal as a term (the body of a size sum is the constant `1`).
    let intv (n: int) : IntTerm = IntTerm(n)
