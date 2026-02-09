namespace Sylvia

open System
open System.IO
open System.Reflection
open System.Text

open FSharp.Quotations

open FParsec
open Antlr4.Runtime

open Formula
open TPTPParser

module TermParsers =
    
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
        let _mod l r = <@ (%l:int) % (%r:int) @>
        let _neg l = <@ -(%l:int) @>

        opp.TermParser <- term
        
        // Operator precedence (Standard Arithmetic)
        // 3: Unary -
        // 2: * /
        // 1: + - %
        
        opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun l r -> _add (expand_as<int> l) (expand_as<int> r)))
        opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun l r -> _sub (expand_as<int> l) (expand_as<int> r)))
        opp.AddOperator(InfixOperator("mod", ws, 1, Associativity.Left, fun l r -> _mod (expand_as<int> l) (expand_as<int> r)))
        opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun l r -> _mul (expand_as<int> l) (expand_as<int> r)))
        opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun l r -> _div (expand_as<int> l) (expand_as<int> r)))
        opp.AddOperator(PrefixOperator("-", ws, 3, true, fun l -> _neg (expand_as<int> l)))
        
        expr

     // -------------------------------------------------------------------------
    // Real Expression Parser
    // -------------------------------------------------------------------------

    let private realExprParser : Parser<Expr, unit> =
        let identifierStr = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws

        let operand : Parser<Expr, unit> =
            (pfloat .>> ws |>> (fun n -> Expr.Value n))
            <|>
            (identifierStr .>>. opt (parens identifierStr)
            >>= fun (id, argOpt) ->
                match id with
                | "true" | "false" -> fail "reserved"
                | _ ->
                    match argOpt with
                    | Some arg ->
                        let argVar = Var(arg, typeof<obj>)
                        let funcType = FSharp.Reflection.FSharpType.MakeFunctionType(typeof<obj>, typeof<real>)
                        let funcVar = Var(id, funcType)
                        preturn (Expr.Application(Expr.Var(funcVar), Expr.Var(argVar)))
                    | None ->
                        preturn (Expr.Var(Var(id, typeof<real>))))

        let opp = OperatorPrecedenceParser<Expr,unit,unit>()
        let expr = opp.ExpressionParser
        let term = parens expr <|> operand

        // Helper expressions for arithmetic
        let _add l r = <@ (%l:real) + (%r:real) @>
        let _sub l r = <@ (%l:real) - (%r:real) @>
        let _mul l r = <@ (%l:real) * (%r:real) @>
        let _div l r = <@ (%l:real) / (%r:real) @>
        let _pow l r = <@ (%l:real) ** (%r:real) @>
        let _neg l = <@ -(%l:real) @>

        opp.TermParser <- term
        
        // Operator precedence (Standard Arithmetic)
        // 4: Unary -
        // 3: ^
        // 2: * /
        // 1: + -
        
       
        opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun l r -> _add (expand_as<real> l) (expand_as<real> r)))
        opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun l r -> _sub (expand_as<real> l) (expand_as<real> r)))
        opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun l r -> _mul (expand_as<real> l) (expand_as<real> r)))
        opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun l r -> _div (expand_as<real> l) (expand_as<real> r)))
        opp.AddOperator(InfixOperator("^", ws, 3, Associativity.Left, fun l r -> _pow (expand_as<real> l) (expand_as<real> r)))
        opp.AddOperator(PrefixOperator("-", ws, 4, true, fun l -> _neg (expand_as<real> l)))
        
        expr
    // -------------------------------------------------------------------------
    // Boolean Expression Parser (Prop)
    // -------------------------------------------------------------------------
    // Used for both Prop parsing and parsing arguments to derived rules.

    let boolExprParser<'t when 't: equality and 't : comparison> : Parser<Expr, unit> =
        let t = typeof<'t>
        let identifierStr = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
        
        let expr, exprRef = createParserForwardedToRef<Expr, unit>()

        let comparisonParser() : Parser<Expr, unit> =
            if t = typeof<int> || t = typeof<real> then
                let _eq l r = <@ (%l:'t) = (%r:'t) @>
                let _noteq l r = <@ (%l:'t) <> (%r:'t) @>
                let _lt l r = <@ (%l:'t) < (%r:'t) @>
                let _gt l r = <@ (%l:'t) > (%r:'t) @>
                let _lte l r = <@ (%l:'t) <= (%r:'t) @>
                let _gte l r = <@ (%l:'t) >= (%r:'t) @>

                let parser = 
                    if t = typeof<int> then intExprParser 
                    elif t  = typeof<real> then realExprParser                
                    else  failwithf "%A expression parser is not implemented." t
                            
                attempt (
                    parser .>>. 
                    choice [
                        str_ws "<=" >>. preturn _lte
                        str_ws ">=" >>. preturn _gte
                        str_ws "=" >>. preturn _eq
                        str_ws "<>" >>. preturn _noteq
                        str_ws "<" >>. preturn _lt
                        str_ws ">" >>. preturn _gt                    
                    ] .>>. parser
                    |>> fun ((l, op), r) -> op (expand_as<'t> l) (expand_as<'t> r)
                )
            else failwithf "%A expression parser is not implemented." t
            
        let operand : Parser<Expr, unit> =       
            let idp = 
                (identifierStr .>>. opt (parens identifierStr)
                |>> fun (id, argOpt) ->
                    match argOpt with
                    | Some arg ->
                        let argVar = Var(arg, typeof<'t>)
                        let funcType = FSharp.Reflection.FSharpType.MakeFunctionType(typeof<'t>, typeof<bool>)
                        let funcVar = Var(id, funcType)
                        Expr.Application(Expr.Var(funcVar), Expr.Var(argVar))
                    | None ->
                        match id with
                        | "true" -> T.Expr 
                        | "false" -> F.Expr
                        | _ -> Expr.Var(Var(id, typeof<bool>)))

            let quantifierParser op =
                 parens (
                    identifierStr .>> str_ws "," .>>. expr .>>. opt (str_ws "," >>. expr)
                 )
                 |>> fun ((boundName, e1), e2Opt) ->
                    let bound = Var(boundName, typeof<'t>)
                    let range, body = 
                        match e2Opt with
                        | None -> (T.Expr, expand_as<bool> e1)
                        | Some e2 -> (expand_as<bool> e1, expand_as<bool> e2)
                    op (Expr.Var(bound)) (expand_as<bool> range) (expand_as<bool> body)

            if t  = typeof<bool> || t = typeof<obj> then 
                     choice [
                        str_ws "forall" >>. quantifierParser (fun b r bod -> <@@ forall_expr (%%b:'t) %r %bod @@>)
                        str_ws "exists" >>. quantifierParser (fun b r bod -> <@@ exists_expr (%%b:'t) %r %bod @@>)
                        idp
                     ]
                elif t = typeof<int> || t = typeof<real> then 
                     choice [
                        str_ws "forall" >>. quantifierParser (fun b r bod -> <@@ forall_expr (%%b:'t) %r %bod @@>)
                        str_ws "exists" >>. quantifierParser (fun b r bod -> <@@ exists_expr (%%b:'t) %r %bod @@>)
                        comparisonParser()
                        idp
                     ]                    
                else failwithf "%A expression parser is not implemented." t

        let opp = OperatorPrecedenceParser<Expr,unit,unit>()
       
        let term = parens expr <|> operand

        // Helper expressions for operators, creating Expr<bool>
        let _equal l r = <@ (%l:bool) = (%r:bool) @>
        let _notequal l r = <@ (%l:bool) <> (%r:bool) @>
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
        opp.AddOperator(InfixOperator("==", ws, 1, Associativity.Left, fun l r -> _equal (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(InfixOperator("<>", ws, 1, Associativity.Left, fun l r -> _notequal (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(InfixOperator("!=", ws, 1, Associativity.Left, fun l r -> _notequal (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(InfixOperator("==>", ws, 2, Associativity.Right, fun l r -> _implies (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(InfixOperator("||", ws, 3, Associativity.Left, fun l r -> _or (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(InfixOperator("|||", ws, 3, Associativity.Left, fun l r -> _or (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(InfixOperator("&&", ws, 4, Associativity.Left, fun l r -> _and (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(InfixOperator("&&&", ws, 4, Associativity.Left, fun l r -> _and (expand_as<bool> l) (expand_as<bool> r)))
        opp.AddOperator(PrefixOperator("not", ws, 5, true, fun l -> _not (expand_as<bool> l)))
        opp.AddOperator(PrefixOperator("-", ws, 5, true, fun l -> _not (expand_as<bool> l)))
        exprRef := opp.ExpressionParser
        expr

    // Public Prop Parser
    let propExprParser<'t when 't: equality and 't : comparison> : Parser<Prop, unit> =
        boolExprParser<'t> .>> eof |>> (expand_as<bool> >> Prop)

    let parseProp<'t when 't: equality and 't : comparison> text = 
        match run propExprParser<'t> text with
        | Success(result, _, _) -> Result.Ok result
        | Failure(errorMsg, _, _) -> sprintf "Failed to parse Prop: %s" errorMsg |> Result.Error

    let parseBoolExpr<'t when 't: equality and 't : comparison> text = 
        match run boolExprParser<'t> text with
        | Success(result, _, _) -> Result.Ok result
        | Failure(errorMsg, _, _) -> sprintf "Failed to parse Prop: %s" errorMsg |> Result.Error

    let parseIntExpr text = 
        match run intExprParser text with
        | Success(result, _, _) -> result |> expand_as<int> |> Result.Ok 
        | Failure(errorMsg, _, _) -> sprintf "Failed to parse integer expression: %s" errorMsg |> Result.Error

    let parseRealExpr text = 
        match run realExprParser text with
        | Success(result, _, _) -> result |> expand_as<real> |> Result.Ok 
        | Failure(errorMsg, _, _) -> sprintf "Failed to parse real expression: %s" errorMsg |> Result.Error

module TPTPParser =
    type Parser(text:string) = 
        inherit tptp_v7_0_0_0Parser(new CommonTokenStream(new tptp_v7_0_0_0Lexer(new AntlrInputStream(text), new StringWriter(new StringBuilder()), new StringWriter(new StringBuilder()))))
        member x.GetOutput() = let sw = x.Output :?> StringWriter in sw.GetStringBuilder().ToString()
        member x.GetErrorOutput() = let sw = (x.ErrorOutput :?> StringWriter) in sw.GetStringBuilder().ToString()
        member x.FileContext = x.tptp_file()
    
    type Visitor() = 
        inherit TPTPVisitor<Expr>()
        let mutable expr = null

    
    let parse text = 
        let p = new Parser(text)    
        let v = new Visitor()
        p.tptp_input().Accept(v)

    let parse_file path = 
        let text = File.ReadAllText(path) in parse text
        
      