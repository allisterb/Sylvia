namespace Sylvia

open FSharp.Quotations
open FSharp.Quotations.Patterns

open FsExpr
open Formula

/// The LANGUAGE of integral and ordered domains (Gries & Schneider ch.15): the operator signatures
/// `DomainOps` / `OrderOps`, and `DomainTerm<'t>`, a symbolic term whose `+ · −` and `< ≤ > ≥` build
/// calls to the operators of ITS signature rather than to F#'s.
///
/// The theories `IntegralDomain<'t>` and `OrderedDomain<'t>` take their operators as parameters, so
/// that one proof serves `int`, `int64`, or a carrier whose `+` is not F#'s. No other term type of the
/// DSL can be built from operators given as data: `IntTerm` is hard-wired to F#'s `int` operators,
/// `Scalar<'t>` runs every operator through the CAS, and `Term<'t>` / `ElemVar<'t>` have no algebra.
/// A `DomainTerm` carries its signature, and its operators build EXACTLY the trees the theories'
/// axiom patterns match (`expand_as <@ (%op) %a %b @>`), so a proof written with them is checked by
/// the same kernel steps as one written with raw quotations.
///
/// A term carries the SIGNATURE, not a theory instance: a term is a sentence of the language, and
/// several theories share a language — `Integers` and every `Induction` theory derived from it build
/// the same `int` terms, and a theorem proved in one is used in the other.
module DomainTerms =

    /// The operators of an integral domain: `+`, `·`, `0`, `1`, unary `−` and binary `−`.
    type DomainOps<'t when 't: equality> =
        { Name: string
          Add: Expr<'t -> 't -> 't>
          Mul: Expr<'t -> 't -> 't>
          Zero: Expr<'t>
          One: Expr<'t>
          Neg: Expr<'t -> 't>
          Sub: Expr<'t -> 't -> 't> }

    /// The order operators of an ordered domain: `pos`, the four relations it defines, minimum `↓`
    /// and maximum `↑` (§15.2), and absolute value (§15.3).
    type OrderOps<'t when 't: equality> =
        { Pos: Expr<'t -> bool>
          Lt: Expr<'t -> 't -> bool>
          Gt: Expr<'t -> 't -> bool>
          Le: Expr<'t -> 't -> bool>
          Ge: Expr<'t -> 't -> bool>
          Min: Expr<'t -> 't -> 't>
          Max: Expr<'t -> 't -> 't>
          Abs: Expr<'t -> 't> }

    (* Raw tree builders — the one place the shapes are decided *)

    let internal bin (op: Expr<'t -> 't -> 't>) (a: Expr<'t>) (b: Expr<'t>) : Expr<'t> = expand_as<'t> <@ (%op) %a %b @>
    let internal un (op: Expr<'t -> 't>) (a: Expr<'t>) : Expr<'t> = expand_as<'t> <@ (%op) %a @>
    let internal eq_e (a: Expr<'t>) (b: Expr<'t>) : Prop = <@ %a = %b @> |> expand_as<bool> |> Prop
    let internal rel_e (r: Expr<'t -> 't -> bool>) (a: Expr<'t>) (b: Expr<'t>) : Prop = <@ (%r) %a %b @> |> expand_as<bool> |> Prop
    let internal pred_e (p: Expr<'t -> bool>) (a: Expr<'t>) : Prop = <@ (%p) %a @> |> expand_as<bool> |> Prop

    /// A term of an integral (or ordered) domain over `'t`.
    ///
    /// `+`, `*`, binary and unary `-` build the signature's operators; `==` and `!=` build `=` and
    /// `¬(=)`; `<.`, `<=.`, `>.`, `>=.` build the four order relations (an ordered signature only).
    /// F#'s own `<`/`<=` cannot be used: they must return `bool`, and a relation here is a `Prop`.
    /// The relational operators share the precedence of `==`, below `+` and `*` — so `a + b <=. c` is
    /// `(a + b) <=. c`, and a CONJUNCTION of relations needs parentheses: `(a <=. b) * (b <=. c)`.
    [<StructuredFormatDisplay("{Display}")>]
    type DomainTerm<'t when 't: equality>(expr: Expr<'t>, ops: DomainOps<'t>, order: OrderOps<'t> option) =
        inherit Term<'t>(expr)
        let display = lazy (sprinte expr)
        member val Ops = ops
        member val Order = order
        override x.Display = display.Value

        /// The order operators, or a clear failure for a term of an unordered signature.
        member x.OrderOps =
            match order with
            | Some o -> o
            | None -> invalidOp (sprintf "%s is a term of the integral domain %s, which has no order." x.Display ops.Name)

        /// A term of the same signature.
        member x.With (e: Expr<'t>) = DomainTerm<'t>(e, ops, order)

        /// Both operands must be sentences of ONE language: a sum of an `int` term and a term of some
        /// other integral domain over `int` is meaningless, and would silently pick one `+`.
        static member internal Join (l: DomainTerm<'t>, r: DomainTerm<'t>) =
            if not (obj.ReferenceEquals(l.Ops, r.Ops) || l.Ops = r.Ops) then
                invalidOp (sprintf "The terms %s and %s belong to different domains (%s, %s)." l.Display r.Display l.Ops.Name r.Ops.Name)
            match l.Order with
            | Some _ -> l
            | None -> r

        static member (+) (l: DomainTerm<'t>, r: DomainTerm<'t>) = (DomainTerm<'t>.Join(l, r)).With(bin l.Ops.Add l.Expr r.Expr)
        static member ( * ) (l: DomainTerm<'t>, r: DomainTerm<'t>) = (DomainTerm<'t>.Join(l, r)).With(bin l.Ops.Mul l.Expr r.Expr)
        static member (-) (l: DomainTerm<'t>, r: DomainTerm<'t>) = (DomainTerm<'t>.Join(l, r)).With(bin l.Ops.Sub l.Expr r.Expr)
        /// Unary minus. F#'s `~-` is typed `^a -> ^a`, so on a `DomainVar` write `-(x :> DomainTerm<_>)`,
        /// as with `IntVar`.
        static member (~-) (l: DomainTerm<'t>) = l.With(un l.Ops.Neg l.Expr)

        /// `a = b`, as a proposition.
        static member (==) (l: DomainTerm<'t>, r: DomainTerm<'t>) : Prop = DomainTerm<'t>.Join(l, r) |> ignore; eq_e l.Expr r.Expr
        /// `¬(a = b)` — the spelling of `≠` the axiom patterns accept at a non-bool carrier.
        static member (!=) (l: DomainTerm<'t>, r: DomainTerm<'t>) : Prop = !!(l == r)

        /// `a < b`.
        static member (<.) (l: DomainTerm<'t>, r: DomainTerm<'t>) : Prop = rel_e (DomainTerm<'t>.Join(l, r)).OrderOps.Lt l.Expr r.Expr
        /// `a ≤ b`.
        static member (<=.) (l: DomainTerm<'t>, r: DomainTerm<'t>) : Prop = rel_e (DomainTerm<'t>.Join(l, r)).OrderOps.Le l.Expr r.Expr
        /// `a > b`.
        static member (>.) (l: DomainTerm<'t>, r: DomainTerm<'t>) : Prop = rel_e (DomainTerm<'t>.Join(l, r)).OrderOps.Gt l.Expr r.Expr
        /// `a ≥ b`.
        static member (>=.) (l: DomainTerm<'t>, r: DomainTerm<'t>) : Prop = rel_e (DomainTerm<'t>.Join(l, r)).OrderOps.Ge l.Expr r.Expr

    /// A domain VARIABLE that is also a domain TERM — the `IntVar` design, for any signature. It is an
    /// `ISymbolicVar<'t>` (a quantifier dummy) and an `IFreshVar` (a fresh witness keeps its algebra).
    type DomainVar<'t when 't: equality>(n: string, ops: DomainOps<'t>, order: OrderOps<'t> option) =
        inherit DomainTerm<'t>(Expr.Var(Var(n, typeof<'t>)) |> expand_as<'t>, ops, order)
        member x.Name = n
        member x.Var = match x.Expr with | Var v -> v | _ -> failwith "A DomainVar's expression is always a variable."
        interface ISymbolicVar<'t> with
            member x.Name = x.Name
            member x.Symbol = x.Name
            member x.Expr = x.Expr
            member x.Var = x.Var
        interface IFreshVar<DomainVar<'t>> with member x.WithName n = DomainVar<'t>(n, ops, order)

    (* Order builders that are not infix *)

    /// `pos.a` — the positivity predicate of an ordered domain.
    let pos_of (a: DomainTerm<'t>) : Prop = pred_e a.OrderOps.Pos a.Expr
    /// `a ↓ b` — minimum.
    let min_of (a: DomainTerm<'t>) (b: DomainTerm<'t>) : DomainTerm<'t> = let j = DomainTerm<'t>.Join(a, b) in j.With(bin j.OrderOps.Min a.Expr b.Expr)
    /// `a ↑ b` — maximum.
    let max_of (a: DomainTerm<'t>) (b: DomainTerm<'t>) : DomainTerm<'t> = let j = DomainTerm<'t>.Join(a, b) in j.With(bin j.OrderOps.Max a.Expr b.Expr)
    /// `abs.a` — absolute value.
    let abs_of (a: DomainTerm<'t>) : DomainTerm<'t> = a.With(un a.OrderOps.Abs a.Expr)
