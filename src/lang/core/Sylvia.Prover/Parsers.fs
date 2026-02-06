namespace Sylvia

open System
open System.Reflection

open FSharp.Quotations
open FParsec

open TermParsers

module ProofParsers =
    
    // -------------------------------------------------------------------------
    // Shared Parser Utilities
    // -------------------------------------------------------------------------

    let private ws = spaces
    let private str_ws s = pstring s .>> ws
    let private parens p = between (str_ws "(") (str_ws ")") p
    
    let private isMathChar = function | '\u03C0' | '\u221E' | '\u29DD' -> true | _ -> false
    let private isIdentifierFirstChar c = isLetter c || isMathChar c
    let private isIdentifierChar c = isLetter c || isDigit c || isMathChar c || c = '_'

    // -------------------------------------------------------------------------
    // RuleApplication Parser
    // -------------------------------------------------------------------------

    let ruleApplicationParser (admissible: ModuleAdmissibleRule[]) (derived: ModuleDerivedRule[]) : Parser<RuleApplication, unit> =
        
        // Identifier for rule names
        let ruleIdentifier = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "rule identifier" .>> ws
        
        // Parse arguments for a rule (0 or more expressions)
        let parseArgs (paramCount: int) =
             parray paramCount boolExprParser
        
        // Lookup rule and parse args if necessary
        let parseRuleStart = 
            ruleIdentifier >>= fun name ->
                match admissible |> Array.tryFind (fun r -> r.Name = name) with
                | Some rule -> 
                    // Admissible rules (Properties)
                    match rule.Property.GetValue(null) with
                    | :? Rule as r -> preturn r
                    | _ -> fail "Admissible rule property did not return a Rule."
                | None ->
                    match derived |> Array.tryFind (fun r -> r.Name = name) with
                    | Some rule ->
                        // Derived rules (Methods)
                        let paramCount = rule.Method.GetParameters().Length
                        parseArgs paramCount |>> fun args ->
                             let argsArray = args |> Array.map (fun e -> e :> obj)
                             match rule.Method.Invoke(null, argsArray) with
                             | :? Rule as r -> r
                             | _ -> failwith "Derived rule method did not return a Rule."
                    | None -> fail (sprintf "Unknown rule name: %s" name)

        // Pipeline operations
        
        // Stage 1: Rule -> RuleApplication
        let opRuleToApp = 
            choice [
                pstring "apply_left" >>. preturn (function | r -> RuleApplication.ApplyLeft r)
                pstring "apply_right" >>. preturn (function | r -> RuleApplication.ApplyRight r)
                pstring "apply_range" >>. preturn (function | r -> RuleApplication.ApplyRange r)
                pstring "apply_body" >>. preturn (function | r -> RuleApplication.ApplyBody r)
                pstring "apply" >>. preturn (function | r -> RuleApplication.Apply r)
            ] .>> ws

        // Stage 2: RuleApplication -> RuleApplication
        let opAppToApp = 
            choice [
                pstring "branch_left" >>. preturn (function | ra -> RuleApplication.BranchLeft ra)
                pstring "branch_right" >>. preturn (function | ra -> RuleApplication.BranchRight ra)
                pstring "apply_unary" >>. preturn (function | ra -> RuleApplication.ApplyUnary ra)
                pstring "select_range" >>. preturn (function | ra -> RuleApplication.SelectRange ra)
                pstring "select_body" >>. preturn (function | ra -> RuleApplication.SelectBody ra)
            ] .>> ws
            
        let pipe = str_ws "|>"
        
        // Full pipeline: Rule [ |> op1 [ |> op2 ... ] ]
        parseRuleStart >>= fun rule ->
             (pipe >>. opRuleToApp >>= fun firstOp ->
                let firstApp = firstOp rule
                let rec restApp currentApp =
                    (pipe >>. opAppToApp >>= fun nextOp -> restApp (nextOp currentApp))
                    <|> preturn currentApp
                restApp firstApp
             )
             <|> preturn (RuleApplication.Apply rule) // Implicit Apply if no pipeline

    let parseRuleApp (admissible: ModuleAdmissibleRule[]) (derived: ModuleDerivedRule[]) text =
        match run (ruleApplicationParser admissible derived) text with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwithf "Failed to parse RuleApplication: %s" errorMsg