namespace Sylvia

open System.Collections.Generic
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
    let private isIdentifierChar c = isLetter c || isDigit c || isMathChar c || c = '_' || c = '\''
        
    let private getArgTerm (e:Expr) : obj=
         if e.Type = typeof<int> then e |> expand_as<int> :> obj
         elif e.Type = typeof<real> then e |> expand_as<real> :> obj
         elif e.Type = typeof<bool> then e |> expand_as<bool> :> obj
         else failwithf "type %A not supported" e.Type

    let private getArgTerm' (e:Expr) : obj=
         if e.Type = typeof<int> then e |> expand_as<int> |> Scalar<int> :> obj
         elif e.Type = typeof<real> then e |> expand_as<real> |> Scalar<real> :> obj
         elif e.Type = typeof<bool> then e |> expand_as<bool> |> Prop :> obj
         else failwithf "type %A not supported" e.Type


    // -------------------------------------------------------------------------
    // RuleApplication Parser
    // -------------------------------------------------------------------------

    let ruleApplicationParser<'t when 't: equality and 't:comparison> (admissible: ModuleAdmissibleRule[]) (derived: ModuleDerivedRule[]) (theorems: ModuleTheorem[]) (tactics: ModuleTactic[]) : Parser<RuleApplication, unit> =
        // Identifier for rule/theorem names (lowercase)
        let lowerIdentifier = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws

        // Identifier for tactic names (starting with uppercase)
        let upperIdentifier = many1Satisfy2L (fun c -> isLetter c && System.Char.IsUpper c) isIdentifierChar "tactic" .>> ws

        // Parse arguments for a rule/theorem (0 or more expressions)
        let parseArgs (paramCount: int) =
             parray paramCount boolExprParser<'t>
             <|>  parray paramCount (parens boolExprParser<'t>)

        let taut :Theorem->Rule=  
            let ieq p = 
                let stmt = <@@ ((%%p) = true) = (%%p) @@> in Theorem(stmt, Proof (stmt, Theory.S, [apply_left Theory.S.Rules[3]; apply Theory.S.Rules[2]], true)) |> Ident  
            Tactics.Taut ieq
        // Lookup and create a rule from admissible/derived rules
        let parseRuleStart = 
            lowerIdentifier >>= fun name ->
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
                             let argsArray = args |> Array.map getArgTerm'
                             match rule.Method.Invoke(null, argsArray) with
                             | :? Rule as r -> r
                             | _ -> failwith "Derived rule method did not return a Rule."
                    | None ->
                        match theorems |> Array.tryFind (fun t -> t.Name = name) with
                        | Some theorem ->
                            // Theorems (Methods that return Theorem)
                            let paramCount = theorem.Method.GetParameters().Length
                            parseArgs paramCount |>> fun args ->
                                 let argsArray = args |> Array.map getArgTerm'
                                 match theorem.Method.Invoke(null, argsArray) with
                                 | :? Theorem as th -> taut th
                                 | _ -> failwith "Theorem method did not return a Theorem."
                        | None -> fail (sprintf "Unknown rule or theorem name: %s" name)

        // Parse a tactic application
        let parseRuleTactic : Parser<Rule->Rule, unit> =
            upperIdentifier >>= fun tacticName ->
                match tactics |> Array.tryFind (fun t -> t.Name = tacticName) with
                | Some tactic ->
                    // Tactics are functions that take a rule and return a rule
                    // We need to invoke them with appropriate arguments
                    preturn (fun rule -> match tactic.Method.Invoke(null, [|rule|]) with | NonNull r -> r :?> Rule | Null -> failwith "Could not retrieve tactic method")                        
                | None -> fail (sprintf "Unknown tactic: %s" tacticName)
        
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

        let rec parseTactics currentRule = (pipe >>. parseRuleTactic >>= fun tacticOp -> parseTactics (tacticOp currentRule)) <|> preturn currentRule
        // Full pipeline: Rule | Theorem [ |> tactic [ |> tactic ... ] ] [ |> op1 [ |> op2 ... ] ]
        parseRuleStart >>= fun rule ->     
            (
                pipe >>. opRuleToApp >>= fun firstOp ->
                let firstApp = firstOp rule
                let rec restApp currentApp =
                    (pipe >>. opAppToApp >>= fun nextOp -> restApp (nextOp currentApp))
                    <|> preturn currentApp
                restApp firstApp
            )
            <|>
            (
                // Try to parse tactics first                          
                parseTactics rule >>= fun ruleAfterTactics ->
                    // Then try to parse rule operations
                    (pipe >>. opRuleToApp >>= fun firstOp ->
                       let firstApp = firstOp ruleAfterTactics
                       let rec restApp currentApp =
                           (pipe >>. opAppToApp >>= fun nextOp -> restApp (nextOp currentApp))
                           <|> preturn currentApp
                       restApp firstApp
                    )                
                    <|> preturn (RuleApplication.Apply ruleAfterTactics)
            )
             
    let parseRuleApp<'t when 't: equality and 't:comparison> (admissible: ModuleAdmissibleRule[]) (derived: ModuleDerivedRule[]) (theorems: ModuleTheorem[]) (tactics: ModuleTactic[]) text =
        match run (ruleApplicationParser<'t> admissible derived theorems tactics) text with
        | Success(result, _, _) -> Result.Ok result
        | Failure(errorMsg, _, _) -> sprintf "Failed to parse rule application: %s" errorMsg |> Result.Error

    let parseProof<'t when 't: equality and 't:comparison> (theories:Dictionary<string, Theory>) (admissibleRules: Dictionary<string, ModuleAdmissibleRule array>) (derivedRules: Dictionary<string, ModuleDerivedRule array>) 
        (theorems: Dictionary<string, ModuleTheorem array>) (tactics: Dictionary<string, ModuleTactic array>)
        (theory:string) (theorem:string) (ruleApplications: string array) =      
                
        match parseProp<'t> theorem with
        | Result.Ok t ->
            if not (theories.ContainsKey theory) then
                sprintf "Theory %A does not exist" theory |> Result.Error
            else
                let ra = ruleApplications |> Array.map (parseRuleApp<'t> admissibleRules[theory] derivedRules[theory] theorems[theory] tactics[theory])
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