namespace Sylvia

open System

[<AttributeUsage(AttributeTargets.All)>]
type AdmissibleRuleAttribute(description:string) =
    inherit Attribute()
    member val Description = description


[<AttributeUsage(AttributeTargets.All)>]
type DerivedRuleAttribute(description:string) =
    inherit Attribute()
    member val Description = description

[<AttributeUsage(AttributeTargets.All)>]
type TheoremAttribute(description:string) =
    inherit Attribute()
    member val Description = description

[<AttributeUsage(AttributeTargets.All)>]
type TacticAttribute(description:string) =
    inherit Attribute()
    member val Description = description

module Descriptions =
    /// Text description of a formula pattern. The description TEXT is lazy: pattern/axiom
    /// descriptions are allocated on every successful axiom/pattern match (equational_logic_axioms
    /// probes run on every proof step), the text is a decompiled example formula, and it is
    /// almost never read — decompiling it eagerly dominated allocation/CPU profiles
    /// (see docs/expressions-perf.md). Only `Name` is read on the hot path.
    type PatternDescription = PatternDescription of string * Lazy<string> with
        member x.Name = let (PatternDescription(n, _)) = x in n
        member x.Description = let (PatternDescription(_, d)) = x in d.Value

    /// Create a pattern description from a name and an example formula. The example is
    /// decompiled only if the description text is actually read.
    let pattern_desc name example  = PatternDescription(name, lazy (example |> src))

    /// Create a pattern description from a name only.
    let pattern_name name = PatternDescription(name, lazy "")

    /// Text description of an axiom based on a theory name and formula pattern.
    type AxiomDescription = AxiomDescription of string * PatternDescription with
        member x.TheoryName = let (AxiomDescription(n, d)) = x in n
        member x.Name = let (AxiomDescription(n, d)) = x in d.Name
        member x.Description = let (AxiomDescription(n, d)) = x in d.Description

    /// Create a axiom description for a theory from a name and an example.
    /// (Passes the pattern description through as-is — rebuilding it via `.Description`
    /// forced the lazy text.)
    let axiom_desc theoryName (patternDesc:PatternDescription)  =
        AxiomDescription(theoryName, patternDesc)

    /// Create a axiom description for a theory from a name only.
    let axiom_name theoryName axiomName =
        AxiomDescription(theoryName, PatternDescription(axiomName, lazy ""))

    /// Set the theory name for an existing axiom description.
    let set_axiom_desc_theory theoryName (a:AxiomDescription)  =
        let (AxiomDescription(_, pd)) = a in AxiomDescription(theoryName, pd)
    