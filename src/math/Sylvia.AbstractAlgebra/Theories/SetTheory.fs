namespace Sylvia

open FSharp.Quotations
open FSharp.Quotations.Patterns

open Formula
open Descriptions

open SetAlgebra

/// Theory of sets and set algebra.
module SetTheory =
    let desc = Some << axiom_name "Set Theory"
    
    (* Patterns *)
    
    let (|SetEmpty|_|) =
        function
        | NewUnionCase(uc, e) when uc.Name = "Empty" -> e |> List.map expand |> Some
        | _ -> None

    let (|SetSeq|_|) =
        function
        | NewUnionCase(uc, Sequence e::[]) when uc.Name = "Seq" -> Some e
        | Call(None, mi, l) when mi.Name = "infinite_seq" || mi.Name = "finite_seq" || mi.Name = "sseq" -> l |> List.map expand |> Some
        | _ -> None

    let (|SetComp|_|) =
        function
        | Call(None, mi, (BoundVars(_)::s as c)) when mi.Name = "set_comp" || mi.Name = "finite_set" || mi.Name = "infinite_set_0" || mi.Name = "infinite_set_1" || mi.Name = "set" || mi.Name = "set'" -> c |> List.map expand |> Some
        | _ -> None

    let (|Set|_|) =
        function
        | SetEmpty e
        | SetSeq e
        | SetComp e -> Some e
        | _ -> None

    /// e ∈ S  →  (e, S). The set operand S is returned RAW — it may be a set variable (SetVar), a
    /// comprehension, or any set-typed expression. Callers destructure it (e.g. via `SetComp`) as
    /// needed; membership on a bare set variable (as in Extensionality) must not require a literal set.
    let (|ElementOf|_|) =
        function
        | Call(None, mi, l::r::[]) when mi.Name = "op_BarQmarkBar" -> Some(expand l, expand r)
        | _ -> None

    (* Axioms *)

    let private desfc = axiom_desc "Set Theory"

    /// Set membership (Gries 11.3):  F ∈ {x | R : E}  =  (∃x | R : F = E).
    /// The set operand must be a comprehension (SetComp); F is x-free (not checked here).
    let (|Membership|_|) =
        function
        | Equals(ElementOf(F, SetComp(BoundVars(bound)::range::body::_)), Exists(_, bound', range', Equals(F', body')))
            when vequal' bound bound' && sequal3 F range body F' range' body' -> desc "Set Membership"
        | _ -> None

    /// Set extensionality (Gries 11.4):  S = T  =  (∀x |: x∈S = x∈T).
    /// S and T are arbitrary set expressions (usually set variables); the ∀ has a true range.
    let (|Extensionality|_|) =
        function
        | Equals(Equals(s, t), ForAll(_, xv, True, Equals(ElementOf(xe1, s1), ElementOf(xe2, t1))))
            when sequal s s1 && sequal t t1 && vequal' xv (get_vars xe1) && vequal' xv (get_vars xe2) -> desc "Set Extensionality"
        | _ -> None

    let set_theory_axioms = 
        function
        | Membership x 
        | Extensionality x -> Some x
        | _ -> None
    (* Theory *)

    // The theory of sets sits over TWO foundations (Gries ch. 11):
    //   1. Predicate calculus — supplied automatically as the ambient logical theory (`Proof.Logic`,
    //      i.e. `Theory.S`). Every proof consults both the theory's own axioms/rules AND the logic's,
    //      so ∀/∃, Trading (9.19), the One-point rule (8.14), etc. are already available here without
    //      re-inheritance. This is what lets set membership (11.3) reduce ∈ to ∃ during a proof.
    //   2. The Boolean algebra of set operators (∪/∩/~/∅/U) — inherited from SetAlgebra : BooleanAlgebra,
    //      the object-level payoff of Metatheorem (11.25).
    // Set-specific axioms (Membership 11.3, Extensionality 11.4, the operator definitions 11.12-11.23)
    // are injected through `?axioms`; the plumbing now composes them over the Boolean-algebra axioms
    // instead of discarding them. Wiring those axioms in is the next step (see docs/prover-set-theory.md).
    type SetTheory<'t when 't : equality>(?axioms:Axioms, ?rules:Rules) =
        inherit SetAlgebra<'t>(BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None)) set_theory_axioms, ?rules = rules)

    let set_theory<'t when 't: equality> = SetTheory<'t>()