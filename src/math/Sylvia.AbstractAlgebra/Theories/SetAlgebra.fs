namespace Sylvia

open FSharp.Quotations
open Formula
open BooleanAlgebra

/// Theory of set algebra based on a generic Boolean algebra.
module SetAlgebra =
        
    (* Symbols *)
    do 
        Symbols.BulitIn.Add(src <@ Empty @>, "\u2205")
        Symbols.BulitIn.Add(src <@ Set.U @>, "\U0001D54C")
    
    (* Formulas *)

    /// n-ary union of sets
    [<Formula>]
    let union<'t when 't : equality> (bound:int) (range:bool) (body:Set<'t>) = formula<Set<'t>>

    /// n-ary intersection of sets
    [<Formula>]
    let intersect<'t when 't : equality> (bound:int) (range:bool) (body:Set<'t>) = product Set.set_intersection "\u22c2" bound range body

    /// Symbolic set comprehension {x | R : E} (Gries 11.1): dummy `bound`:'t, range `R`, body `E`:'t,
    /// yielding a set of 't. A quantifier-shaped placeholder mirroring `forall_expr`/`exists_expr`
    /// (Formula.fs); the theory's Set-Membership axiom (Gries 11.3) keys on the method name `set_comp`.
    let set_comp<'t when 't : equality> (bound:'t) (range:bool) (body:'t) : Set<'t> = formula<Set<'t>>
    
    (* Theory *)

    type SetAlgebra<'t when 't: equality>(?axioms:Axioms, ?rules:Rules) =
        inherit BooleanAlgebra<Set<'t>>("Set Algebra", <@ Set.set_union @>, <@ Set.set_intersection @>, <@ Set.Empty @>, <@ Set.U @>, <@ Set.(~-) @>,
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