namespace Sylvia.Tests.Giant

open FSharp.Quotations

open Xunit

open Sylvia
open TermParsers
module TestRules =
    [<AdmissibleRule("Test Admissible Rule")>]
    let rule1 = Admit("rule1", id)

    [<DerivedRule("Test Derived Rule")>]
    let rule2 (e:Expr) = Admit(sprintf "rule2(%s)" (src e), fun _ -> e)

    (* Module information members *)

    type private IModuleTypeLocator = interface end
    
    let Type = match typeof<IModuleTypeLocator>.DeclaringType with | NonNull m -> m | _ -> failwith "Failed to locate module type."

type ParserTests() =
    inherit TestsRuntime()

    [<Fact>]
    member this.``Can parse simple prop`` () =
        let p = parseProp<bool> "p ==> q"
        Assert.Contains("===>", p.Display)

    [<Fact>]
    member this.``Can parse complex prop`` () =
        let p = parseProp<bool> "((p ==> q) ==> ((p && r) ==> (q && r)))"
        Assert.NotNull(p)

    [<Fact>]
    member this.``Can parse rule application with admissible rule`` () =
        let admissible = ProofModules.getModuleAdmissibleRules TestRules.Type
        let derived = [||]
        
        let ra = ProofParsers.parseRuleApp<bool> admissible derived "rule1 |> apply_left |> branch_right"
        match ra with
        | BranchRight(ApplyLeft(r)) when r.Name = "rule1" -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

    [<Fact>]
    member this.``Can parse rule application with derived rule and arguments`` () =
        let admissible = [||]
        let derived = ProofModules.getModuleDerivedRules TestRules.Type
        
        let ra = ProofParsers.parseRuleApp<int> admissible derived "rule2 (p || q) |> apply_right"
        match ra with
        | ApplyRight(r) when r.Name.Contains("rule2") && r.Name.Contains("p") && r.Name.Contains("q") -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

    [<Fact>]
    member this.``Can parse symbolic predicate expression`` () =
        let p = parseProp<bool> "P(x)"
        Assert.NotNull(p)
        // Check that the display string contains the predicate and argument
        Assert.Contains("P", p.Display)
        Assert.Contains("x", p.Display)

    [<Fact>]
    member this.``Can parse complex symbolic predicate expression`` () =
        let p = parseProp<bool> "P(x) ==> Q(y)"
        Assert.Contains("===>", p.Display)
        Assert.Contains("P", p.Display)
        Assert.Contains("x", p.Display)
        Assert.Contains("Q", p.Display)
        Assert.Contains("y", p.Display)

    [<Fact>]
    member this.``Can parse arithmetic comparison`` () =
        let p = parseProp<int> "a + b + 5 < 7"
        Assert.NotNull(p)
        Assert.Contains("+", p.Display)
        Assert.Contains("<", p.Display)

    [<Fact>]
    member this.``Can parse arithmetic equality and multiplication`` () =
        let p = parseProp<int> "x * y = 10"
        Assert.NotNull(p)
        Assert.Contains("*", p.Display)
        Assert.Contains("=", p.Display)

    [<Fact>]
    member this.``Can parse complex logic with arithmetic`` () =
        let p = parseProp<int> "not (x < 5) ==> x >= 5"
        Assert.NotNull(p)
        Assert.Contains("===>", p.Display)
        Assert.Contains("<", p.Display)
        Assert.Contains(">=", p.Display)

    [<Fact>]
    member this.``Can parse unary minus in arithmetic`` () =
        let p = parseProp<int> "-x + y > 0"
        Assert.NotNull(p)
        Assert.Contains("-", p.Display)
        Assert.Contains(">", p.Display)

