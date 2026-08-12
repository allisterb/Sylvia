namespace Sylvia

open FSharp.Quotations
open Formula
open BooleanAlgebra

/// Theory of set algebra based on a generic Boolean algebra.
module SetAlgebra =
        
    (* Symbols *)
    // The set notation (`\u2205 \ud835\udd4c \u222a \u2229 \u2212 \u2208 \u2286`) is registered with `Symbols.BulitIn` by `SetTerm`'s static
    // initializer in `Definitions/Set.fs` \u2014 see the comment there for why it cannot live in a module
    // `do` binding like this one.
    
    (* Formulas *)

    (* Union and intersection of families of sets (Gries \u00a711.4). \u222a and \u2229 are symmetric, associative
       and idempotent and have identities, so each is an operator to which \u00a78.2's `(\u2605x | R : E)`
       quantification notation applies \u2014 that is all (11.74)/(11.75) are. There is no separate
       "big union of a set of sets" operator in the chapter: a family `S : Set<Set<'t>>` is handled
       as the instance `(\u222au | u \u2208 S : u)`, which is how (11.76) Partition is stated.

       Built on `Formula.sum`/`product`, so `(|Quantifier|_|)` recognizes them and the three GENERIC
       quantifier axioms \u2014 One-Point (8.14), Nesting (8.20), Renaming (8.21) \u2014 already apply. Empty
       range (8.13), Distributivity (8.15), Range split (8.18) and Interchange (8.19) are keyed on
       \u2200/\u2203 and do NOT; those come via the membership axioms in `SetTheory`, which reduce \u222a/\u2229 to \u2203/\u2200. *)

    /// n-ary union of sets \u2014 Gries (11.74), `(\u222ax | R : E)`.
    [<Formula>]
    let union<'t, 'u when 't : equality> (bound:'u) (range:bool) (body:Set<'t>) = sum Set.set_union "\u22c3" bound range body

    /// n-ary intersection of sets \u2014 Gries (11.75), `(\u2229x | R : E)`.
    [<Formula>]
    let intersect<'t, 'u when 't : equality> (bound:'u) (range:bool) (body:Set<'t>) = product Set.set_intersection "\u22c2" bound range body

    /// Symbolic set comprehension {x | R : E} (Gries 11.1): dummy `bound`:'t, range `R`, body `E`:'t,
    /// yielding a set of 't. A quantifier-shaped placeholder mirroring `forall_expr`/`exists_expr`
    /// (Formula.fs); the theory's Set-Membership axiom (Gries 11.3) keys on the method name `set_comp`.
    let set_comp<'t when 't : equality> (bound:'t) (range:bool) (body:'t) : Set<'t> = formula<Set<'t>>
    
    (* Theory *)

    type SetAlgebra<'t when 't: equality>(?axioms:Axioms, ?rules:Rules) =
        // Join/meet are the ∪ / ∩ SetTerm operators (op_BarPlusBar / op_BarMultiplyBar), so a set
        // expression written `S |+| T` / `S |*| T` matches BOTH these Boolean-algebra laws AND the
        // membership operator axioms in SetTheory. (`<@ (|+|) @>` is a direct method reference that
        // SpecificCall accepts — unlike a `fun a b -> a |+| b` lambda, which it rejects.)
        inherit BooleanAlgebra<Set<'t>>("Set Algebra",
            <@ (|+|) : Set<'t> -> Set<'t> -> Set<'t> @>, <@ (|*|) : Set<'t> -> Set<'t> -> Set<'t> @>,
            <@ Set.Empty @>, <@ Set.U @>, <@ Set.(~-) @>,
            ?axioms = axioms, ?rules = rules)
    
    let set_algebra<'t when 't: equality> = SetAlgebra<'t>()

    (* Admissible Rules *)
    
    let left_assoc<'t when 't : equality> = set_algebra<'t>.Rules.[0]

    let right_assoc<'t when 't : equality> = set_algebra<'t>.Rules.[1]

    let commute<'t when 't : equality> = set_algebra<'t>.Rules.[2]

    let idemp<'t when 't : equality> = set_algebra<'t>.Rules.[3]

    let ident_set<'t when 't : equality> = set_algebra<'t>.Rules.[4]

    let comp<'t when 't : equality> = set_algebra<'t>.Rules.[5]

    let distrib<'t when 't : equality> = set_algebra<'t>.Rules.[6]