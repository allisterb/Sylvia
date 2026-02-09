namespace Sylvia

open System.Collections.Generic

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

    let ruleApplicationParser<'t when 't: equality and 't:comparison> (admissible: ModuleAdmissibleRule[]) (derived: ModuleDerivedRule[]) : Parser<RuleApplication, unit> =
        
        // Identifier for rule names
        let ruleIdentifier = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "rule identifier" .>> ws
        
        // Parse arguments for a rule (0 or more expressions)
        let parseArgs (paramCount: int) =
             parray paramCount boolExprParser<'t>
        
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
                             let argsArray = args |> Array.map (fun e -> e |> expand_as<bool> |> Prop :> obj)
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

    let parseRuleApp<'t when 't: equality and 't:comparison> (admissible: ModuleAdmissibleRule[]) (derived: ModuleDerivedRule[]) text =
        match run (ruleApplicationParser<'t> admissible derived) text with
        | Success(result, _, _) -> Result.Ok result
        | Failure(errorMsg, _, _) -> sprintf "Failed to parse RuleApplication: %s" errorMsg |> Result.Error 

    let parseProof (theories:Dictionary<string, Theory>) (admissibleRules: Dictionary<string, ModuleAdmissibleRule array>) (derivedRules: Dictionary<string, ModuleDerivedRule array>) 
        (theory:string) (theorem:string) (ruleApplications: string array) =
        
        let parse theory theorem =
            match theory with
            | "prop_calculus"
            | "pred_calculus" -> parseProp<bool> theorem
            | _ -> failwith "not implemented"

        let parseRuleApp theory ra =
            match theory with
            | "prop_calculus"
            | "pred_calculus" -> parseRuleApp<bool> admissibleRules[theory] derivedRules[theory] ra
            | _ -> failwith "not implemented"

        match parse theory theorem with
        | Result.Ok t ->
            if not (theories.ContainsKey theory) then
                sprintf "Theory %A does not exist" theory |> Result.Error
            else
                let ra = ruleApplications |> Array.map (parseRuleApp theory)
                if ra |> Array.exists(fun r -> r.IsError) then            
                    ra 
                    |> Array.choose (function Result.Error e -> Some e | _ -> None) 
                    |> Array.insertAt 0 "Could not parse one more of the constraints:\n" 
                    |> String.concat "\n"  
                    |> Result.Error
                else
                    let p = proof theories[theory] t (ra |> Array.choose (function Result.Ok r -> Some r | _ -> None) |> Array.toList) 
                    Result.Ok p                
        | Result.Error error -> sprintf "%s" error |> Result.Error