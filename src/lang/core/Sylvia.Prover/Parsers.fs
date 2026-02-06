namespace Sylvia

open System
open System.Reflection

open FSharp.Quotations
open FParsec

module ProofParsers =
    
    // -------------------------------------------------------------------------
    // Shared Parser Utilities
    // -------------------------------------------------------------------------

    let ws = spaces
    let str_ws s = pstring s .>> ws
    let parens p = between (str_ws "(") (str_ws ")") p
    
    let isMathChar = function | '\u03C0' | '\u221E' | '\u29DD' -> true | _ -> false
    let isIdentifierFirstChar c = isLetter c || isMathChar c
    let isIdentifierChar c = isLetter c || isDigit c || isMathChar c || c = '_'

    // -------------------------------------------------------------------------
    // Integer Expression Parser
    // -------------------------------------------------------------------------

    let private intExprParser : Parser<Expr, unit> =
        let identifierStr = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws

        let operand : Parser<Expr, unit> =
            (pint32 .>> ws |>> (fun n -> Expr.Value n))
            <|>
            (identifierStr .>>. opt (parens identifierStr)
            >>= fun (id, argOpt) ->
                match id with
                | "true" | "false" -> fail "reserved"
                | _ ->
                    match argOpt with
                    | Some arg ->
                        let argVar = Var(arg, typeof<obj>)
                        let funcType = FSharp.Reflection.FSharpType.MakeFunctionType(typeof<obj>, typeof<int>)
                        let funcVar = Var(id, funcType)
                        preturn (Expr.Application(Expr.Var(funcVar), Expr.Var(argVar)))
                    | None ->
                        preturn (Expr.Var(Var(id, typeof<int>))))

        let opp = OperatorPrecedenceParser<Expr,unit,unit>()
        let expr = opp.ExpressionParser
        let term = parens expr <|> operand

        // Helper expressions for arithmetic
        let _add l r = <@ (%l:int) + (%r:int) @>
        let _sub l r = <@ (%l:int) - (%r:int) @>
        let _mul l r = <@ (%l:int) * (%r:int) @>
        let _div l r = <@ (%l:int) / (%r:int) @>
        let _neg l = <@ -(%l:int) @>

        opp.TermParser <- term
        
        // Operator precedence (Standard Arithmetic)
        // 3: Unary -
        // 2: * /
        // 1: + -
        
        opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun l r -> _add (expand_as<int> l) (expand_as<int> r)))
        opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun l r -> _sub (expand_as<int> l) (expand_as<int> r)))
        opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun l r -> _mul (expand_as<int> l) (expand_as<int> r)))
        opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun l r -> _div (expand_as<int> l) (expand_as<int> r)))
        opp.AddOperator(PrefixOperator("-", ws, 3, true, fun l -> _neg (expand_as<int> l)))
        
        expr

    // -------------------------------------------------------------------------
    // Boolean Expression Parser (Prop)
    // -------------------------------------------------------------------------
    // Used for both Prop parsing and parsing arguments to derived rules.

    let private expressionParser : Parser<Expr, unit> =
        let identifierStr = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws

        // Comparison parser: intExpr op intExpr
        let comparison : Parser<Expr, unit> =
            let _eq l r = <@ (%l:int) = (%r:int) @>
            let _lt l r = <@ (%l:int) < (%r:int) @>
            let _gt l r = <@ (%l:int) > (%r:int) @>
            let _lte l r = <@ (%l:int) <= (%r:int) @>
            let _gte l r = <@ (%l:int) >= (%r:int) @>

            attempt (
                intExprParser .>>. 
                choice [
                    str_ws "=" >>. preturn _eq
                    str_ws "<" >>. preturn _lt
                    str_ws ">" >>. preturn _gt
                    str_ws "<=" >>. preturn _lte
                    str_ws ">=" >>. preturn _gte
                ] .>>. intExprParser
                |>> fun ((l, op), r) -> op (expand_as<int> l) (expand_as<int> r)
            )

        let operand : Parser<Expr, unit> =
            comparison
            <|>
            (identifierStr .>>. opt (parens identifierStr)
            |>> fun (id, argOpt) ->
                match argOpt with
                | Some arg ->
                    let argVar = Var(arg, typeof<obj>)
                    let funcType = FSharp.Reflection.FSharpType.MakeFunctionType(typeof<obj>, typeof<bool>)
                    let funcVar = Var(id, funcType)
                    Expr.Application(Expr.Var(funcVar), Expr.Var(argVar))
                | None ->
                    match id with
                    | "true" -> Expr.Value true
                    | "false" -> Expr.Value false
                    | _ -> Expr.Var(Var(id, typeof<bool>)))

        let opp = OperatorPrecedenceParser<Expr,unit,unit>()
        let expr = opp.ExpressionParser
        let term = parens expr <|> operand

        // Helper expressions for operators, creating Expr<bool>
        let _equal l r = <@ (%l:bool) = (%r:bool) @>
        let _implies l r = <@ (%l:bool) ===> (%r:bool) @>
        let _and l r = <@ (%l:bool) && (%r:bool) @>
        let _or l r = <@ (%l:bool) || (%r:bool) @>
        let _not l = <@ not (%l:bool) @>

        opp.TermParser <- term
        
        // Operator precedence (Standard Logic)
        // 5: NOT (Prefix)
        // 4: AND (*)
        // 3: OR (+)
        // 2: IMPLIES (==>)
        // 1: EQUALS (=)
        
        opp.AddOperator(InfixOperator("=", ws, 1, Associativity.Left, fun l r -> _equal (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(InfixOperator("==>", ws, 2, Associativity.Right, fun l r -> _implies (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(InfixOperator("+", ws, 3, Associativity.Left, fun l r -> _or (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(InfixOperator("*", ws, 4, Associativity.Left, fun l r -> _and (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(PrefixOperator("not", ws, 5, true, fun l -> _not (expand_as<bool> l)))
        
        expr

    // Public Prop Parser
    let propExprParser : Parser<Prop, unit> =
        expressionParser .>> eof |>> (expand_as<bool> >> Prop)

    let parseProp text = 
        match run propExprParser text with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwithf "Failed to parse Prop: %s" errorMsg

    // -------------------------------------------------------------------------
    // RuleApplication Parser
    // -------------------------------------------------------------------------

    let ruleApplicationParser (admissible: ModuleAdmissibleRule[]) (derived: ModuleDerivedRule[]) : Parser<RuleApplication, unit> =
        
        // Identifier for rule names
        let ruleIdentifier = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "rule identifier" .>> ws
        
        // Parse arguments for a rule (0 or more expressions)
        let parseArgs (paramCount: int) =
             parray paramCount expressionParser
        
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