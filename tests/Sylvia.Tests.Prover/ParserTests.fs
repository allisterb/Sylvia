namespace Sylvia.Tests.Prover

open FSharp.Quotations

open Xunit

module private Helper =
    type Result<'TOk,'Error> with
        member x.Val =
            match x with
            | Ok v -> v
            | Error e -> failwithf "Expected Ok but got Error: %A" e  

open Sylvia
open TermParsers

module TestRules =    
    open PropCalculus
    [<DerivedRule "p ∧ q ∧ (r ∧ s) = p ∧ r ∧ (q ∧ s)">]
    let commute_and_and (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p * q) * (r * s)) == ((p * r) * (q * s))) [
        right_assoc_and p q ( r * s ) |> apply_left
        left_assoc_and q r s |>  apply_left
        commute_and q r |> apply_left
        right_assoc_and r q s |> apply_left
        left_assoc_and p r ( q * s ) |> apply_left
    ]

    [<AdmissibleRule("Test Admissible Rule")>]
    let rule1 = Admit("rule1", id)

    [<DerivedRule("Test Derived Rule")>]
    let rule2 (p:Prop) = Admit(sprintf "rule2(%s)" (src p.Expr), fun _ -> p.Expr)

    [<Theorem("Test Derived Rule")>]
    let modus_ponens p q = PropCalculus.modus_ponens p q
    (* Module information members *)

    type private IModuleTypeLocator = interface end
    
    let Type = match typeof<IModuleTypeLocator>.DeclaringType with | NonNull m -> m | _ -> failwith "Failed to locate module type."

open Helper

type ParserTests() =
    inherit TestsRuntime()

    let admissible = ProofModules.getModuleAdmissibleRules TestRules.Type
    let derived = ProofModules.getModuleDerivedRules TestRules.Type
    let theorems = ProofModules.getModuleTheorems TestRules.Type
    
    [<Fact>]
    member this.``Can parse simple prop`` () =
        let p = parseProp<bool> "p ==> q"
        Assert.Contains("===>", p.Val.Display)

    [<Fact>]
    member this.``Can parse complex prop`` () =
        let p = parseProp<bool> "((p ==> q) ==> ((p && r) ==> (q && r)))"
        Assert.NotNull(p)

    [<Fact>]
    member this.``Can parse rule application with admissible rule`` () =
        let admissible = ProofModules.getModuleAdmissibleRules TestRules.Type
        let derived = [||]
        
        let ra = ProofParsers.parseRuleApp<bool> admissible derived [||] [||] "rule1 |> apply_left |> branch_right"
        match ra with
        | Ok(BranchRight(ApplyLeft(r))) when r.Name = "rule1" -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

    [<Fact>]
    member this.``Can parse rule application with derived rule and arguments`` () =
        let admissible = [||]
        let derived = ProofModules.getModuleDerivedRules TestRules.Type
        
        let ra = ProofParsers.parseRuleApp<int> admissible derived [||] [||] "rule2 (p || q) |> apply_right"
        match ra with
        | Ok(ApplyRight(r)) when r.Name.Contains("rule2") && r.Name.Contains("p") && r.Name.Contains("q") -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

    [<Fact>]
    member this.``Can parse theorem use`` () =

        let ra = ProofParsers.parseRuleApp<bool> admissible derived theorems [||] "modus_ponens p q |> apply_right"
        match ra with
        | Ok(ApplyRight(r)) when r.Name.Contains("Substitute") && r.Name.Contains("q") -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

    [<Fact>]
    member this.``Can parse symbolic predicate expression`` () =
        let p = parseProp<bool> "P(x)"
        Assert.NotNull(p)
        // Check that the display string contains the predicate and argument
        Assert.Contains("P", p.Val.Display)
        Assert.Contains("x", p.Val.Display)

    [<Fact>]
    member this.``Can parse complex symbolic predicate expression`` () =
        let p = parseProp<bool> "P(x) ==> Q(y)"
        Assert.Contains("===>", p.Val.Display)
        Assert.Contains("P", p.Val.Display)
        Assert.Contains("x", p.Val.Display)
        Assert.Contains("Q", p.Val.Display)
        Assert.Contains("y", p.Val.Display)

    [<Fact>]
    member this.``Can parse arithmetic comparison`` () =
        let p = parseProp<int> "a + b + 5 < 7"
        Assert.NotNull(p)
        Assert.Contains("+", p.Val.Display)
        Assert.Contains("<", p.Val.Display)

    [<Fact>]
    member this.``Can parse arithmetic equality and multiplication`` () =
        let p = parseProp<int> "x * y = 10"
        Assert.NotNull(p)
        Assert.Contains("*", p.Val.Display)
        Assert.Contains("=", p.Val.Display)

    [<Fact>]
    member this.``Can parse complex logic with arithmetic`` () =
        let p = parseProp<int> "not (x < 5) ==> x >= 5"
        Assert.NotNull(p)
        Assert.Contains("===>", p.Val.Display)
        Assert.Contains("<", p.Val.Display)
        Assert.Contains(">=", p.Val.Display)

    [<Fact>]
    member this.``Can parse unary minus in arithmetic`` () =
        let p = parseProp<int> "-x + y > 0"
        Assert.NotNull(p)
        Assert.Contains("-", p.Val.Display)
        Assert.Contains(">", p.Val.Display)

    [<Fact>]
    member __.``Can parse rull params``() =
        let p = ProofParsers.parseRuleApp<bool> admissible derived theorems [||] "commute_and_and p q p (q || r) |> apply_left"
        Assert.True p.IsOk

