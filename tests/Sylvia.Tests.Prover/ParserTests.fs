namespace Sylvia.Tests.Prover

open FSharp.Quotations

open Xunit

open Sylvia

open PropCalculus
open TermParsers
open ProofParsers

module TestRules =    

    [<DerivedRule "p ∧ q ∧ (r ∧ s) = p ∧ r ∧ (q ∧ s)">]
    let commute_and_and (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p * q) * (r * s)) == ((p * r) * (q * s))) [
        right_assoc_and p q ( r * s ) |> apply_left
        left_assoc_and q r s |>  apply_left
        commute_and q r |> apply_left
        right_assoc_and r q s |> apply_left
        left_assoc_and p r ( q * s ) |> apply_left
    ]

    [<DerivedRule "p ∨ q = q ∨ p">]
    let commute_or (p:Prop) (q:Prop) = ident prop_calculus ((p + q) == (q + p)) [
        commute_or p q |> apply_left
    ]

    [<DerivedRule "p ∧ (q ∨ r) = (p ∧ q) ∨ (p ∧ r)">]
    let distribute_and_or (p:Prop) (q:Prop) (r:Prop) = PropCalculus.distrib_and_or p q r
       
    [<DerivedRule "Test Derived Rule with Prop and Int parameters">]
    let ruleWithMultipleTypes (p:Prop) (i:int) = Admit(sprintf "ruleWithMultipleTypes(%s, %d)" (src p.Expr) i, fun _ -> p.Expr)

    [<AdmissibleRule("Test Admissible Rule")>]
    let rule1 = Admit("rule1", id)

    [<DerivedRule("Test Derived Rule with 1 parameter")>]
    let rule2 (p:Prop) = Admit(sprintf "rule2(%s)" (src p.Expr), fun _ -> p.Expr)

    [<Theorem("Test theorem with 2 parameters")>]
    let theorem2 (p:Prop) (q:Prop) = theorem prop_calculus T []

    //[<Theorem("Forall x. P[x] implies Exists x. P[x]")>]
    //let quantifierTheorem (x:Prop) (P:Pred<bool>)= theorem prop_calculus (forall'(x, P[x] ==> exists(x, x)) [] // Placeholder logic
    
    (* Module information members *)

    type private IModuleTypeLocator = interface end
    
    let Type = match typeof<IModuleTypeLocator>.DeclaringType with | NonNull m -> m | _ -> failwith "Failed to locate module type."

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
    member this.``Can parse rule application with derived rule and argument`` () =
        let admissible = [||]
        let derived = ProofModules.getModuleDerivedRules TestRules.Type
        
        let ra = ProofParsers.parseRuleApp<int> admissible derived [||] [||] "rule2 (p || q) |> apply_right"
        match ra with
        | Ok(ApplyRight(r)) when r.Name.Contains("rule2") && r.Name.Contains("p") && r.Name.Contains("q") -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

     [<Fact>]
     member this.``Can parse rule application with derived rule and multiple arguments`` () =
        let admissible = [||]
        let derived = ProofModules.getModuleDerivedRules TestRules.Type
        
        let ra = ProofParsers.parseRuleApp<int> admissible derived [||] [||] "commute_and_and p q p (p || q) |> apply_right"
        match ra with
        | Ok(ApplyRight(r)) when r.Name.Contains("p ∨ ") && r.Name.Contains("q") -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

    [<Fact>]
    member this.``Can parse theorem use`` () =

        let ra = ProofParsers.parseRuleApp<bool> admissible derived theorems [||] "theorem2 p q |> apply_right"
        match ra with
        | Ok(ApplyRight(r)) when r.Name.Contains("Substitute") && r.Name.Contains("") -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

    [<Fact>]
    member this.``Can parse symbolic predicate expression`` () =
        let p = parseProp<bool> "P[x]"
        Assert.NotNull(p)
        // Check that the display string contains the predicate and argument
        Assert.Contains("P", p.Val.Display)
        Assert.Contains("x", p.Val.Display)

    [<Fact>]
    member this.``Can parse complex symbolic predicate expression`` () =
        let p = parseProp<bool> "P[x] ==> Q[y]"
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
    member __.``Can parse rule parameters``() =
        let p = ProofParsers.parseRuleApp<bool> admissible derived theorems [||] "commute_and_and (p&&q) q p p |> apply_left"
        printfn "%A" p
        Assert.True p.IsOk
        
        let p = ProofParsers.parseRuleApp<bool> admissible derived theorems [||] "commute_and_and p q p (p) |> apply_left"
        printfn "%A" p
        Assert.True p.IsOk
  

    [<Fact>]
    member this.``Can parse complex nested propositional expression`` () =
        let p = parseProp<bool> "((a && b) || (c && d)) ==> !(e || f)"
        Assert.NotNull(p)
        Assert.Contains("&&", p.Val.Display)
        Assert.Contains("||", p.Val.Display)
        Assert.Contains("===>", p.Val.Display)
        Assert.Contains("!", p.Val.Display)

    [<Fact>]
    member this.``Can parse combined arithmetic and logical expression`` () =
        let p = parseProp<int> "(x + y = 10) && (z > 5 || w < 2)"
        Assert.NotNull(p)
        Assert.Contains("=", p.Val.Display)
        Assert.Contains("&&", p.Val.Display)
        Assert.Contains(">", p.Val.Display)
        Assert.Contains("||", p.Val.Display)

    [<Fact>]
    member this.``Can parse arithmetic expression with multiple parentheses`` () =
        let p = parseProp<int> "(a + b > (4 * v))"
        Assert.NotNull(p)
        Assert.Contains("+", p.Val.Display)
        Assert.Contains("*", p.Val.Display)    
    [<Fact>]
    member this.``Can parse predicate logic with quantifiers and functions`` () =
        let p = parseProp<bool> "forall x. (P[x] ==> exists y. Q[x, f(y)])"
        Assert.NotNull(p)
        Assert.Contains("forall", p.Val.Display)
        Assert.Contains("exists", p.Val.Display)
        Assert.Contains("P", p.Val.Display)
        Assert.Contains("Q", p.Val.Display)
        Assert.Contains("f(y)", p.Val.Display)

    [<Fact>]
    member this.``Can parse rule application with commute_or`` () =
        let admissible = [||]
        let derived = ProofModules.getModuleDerivedRules TestRules.Type
        let ra = ProofParsers.parseRuleApp<bool> admissible derived [||] [||] "commute_or p q |> apply_left"
        match ra with
        | Ok(ApplyLeft(r)) when r.Name.Contains("q ∨ p") -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

    [<Fact>]
    member this.``Can parse rule application with distribute_and_or`` () =
        let admissible = [||]
        let derived = ProofModules.getModuleDerivedRules TestRules.Type
        let ra = ProofParsers.parseRuleApp<bool> admissible derived [||] [||] "distribute_and_or a b c |> apply_left"
        match ra with
        | Ok(ApplyLeft(r)) when r.Name.Contains("a ∧ b ∨ a ∧ c") -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

    [<Fact>]
    member this.``Can parse rule application with mixed types`` () =
        let admissible = [||]
        let derived = ProofModules.getModuleDerivedRules TestRules.Type
        let ra = ProofParsers.parseRuleApp<int> admissible derived [||] [||] "ruleWithMultipleTypes (x || y) 42 |> apply_right"
        match ra with
        | Ok(ApplyRight(r)) when r.Name.Contains("ruleWithMultipleTypes") && r.Name.Contains("x ∨ y") && r.Name.Contains("42") -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra

    [<Fact>]
    member this.``Can parse theorem application with quantifierTheorem`` () =
        let ra = ProofParsers.parseRuleApp<bool> admissible derived theorems [||] "quantifierTheorem P[x] |> apply_left"
        match ra with
        | Ok(ApplyLeft(r)) when r.Name.Contains("Substitute") && r.Name.Contains("T") -> ()
        | _ -> failwithf "Unexpected RuleApplication structure: %A" ra