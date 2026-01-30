namespace Sylvia.GenAI.Giant

open System.Collections.Generic

open FSharp.Quotations

open Microsoft.SemanticKernel

open Sylvia
open Sylvia.CAS

type VariableType =
    | Bool = 0
    | Int = 1
    | Real = 2

type LLMPlugin(name:string, sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) =
    do 
        sharedState.Add(name, new Dictionary<string, obj>())   

    member val Name = name
    
    member val Id = defaultArg id (System.Guid.NewGuid().ToString())   
    
    member val SharedState:Dictionary<string, Dictionary<string, obj>>  = sharedState 
        
    member val State = sharedState[name]

    member x.Vars with get() = x.SharedState["Symbols"]["Vars"] :?> List<Var>

    member internal x.GetVar(n:string) : Var =          
        match x.Vars.Find(fun v -> v.Name = n) with
        | NonNull v -> v
        | Null _ -> failwithf "The variable %s is not declared. You must declare this variable and its type first before you use it." n

    member internal x.Parse<'t>(expression: string) : Expr<'t> =
        match MathNetExprParser.parse_to_expr<'t> x.Vars expression with
            | Ok expr -> expr
            | Error error -> failwithf "Could not parse expression %s: %s." expression error
            
    interface IPlugin with
        member x.Name with get() = name
        member x.SharedState with get() = x.SharedState
        
        
