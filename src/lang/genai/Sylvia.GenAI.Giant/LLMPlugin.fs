namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic

open FSharp.Quotations

open Microsoft.SemanticKernel
open Microsoft.Extensions.Logging
open Sylvia
open Sylvia.CAS

type SymbolException(message:string) =
    inherit Exception(message)

[<AutoOpen>]
module private GiantUtils =
    let log_kernel_func_info (logger: ILogger option) m = if logger.IsSome then logger.Value.LogInformation m

    let log_kernel_func_ret (logger: ILogger option) m = 
        do if logger.IsSome then logger.Value.LogInformation m
        m

    let symbol_failure m = raise <| SymbolException m
    
type VariableType =
    | Bool = 0
    | Int = 1
    | Real = 2

type LLMPlugin(name:string, sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) =
    inherit Runtime()

    do 
        sharedState.Add(name, new Dictionary<string, obj>())   

    member val Name = name
    
    member val Id = defaultArg id (System.Guid.NewGuid().ToString())   
    
    member val SharedState:Dictionary<string, Dictionary<string, obj>>  = sharedState 
        
    member val State = sharedState[name]

    member x.Vars with get() = x.SharedState["Symbols"]["Vars"] :?> List<Var>

    member x.Functions with get() = x.SharedState["Symbols"]["Functions"] :?> Dictionary<string, Expr>

    member internal x.GetVar(n:string) : Var =          
        match x.Vars.Find(fun v -> v.Name = n) with
        | NonNull v -> v
        | Null _ -> sprintf "The variable %s is not declared. You must declare this variable and its type first before you use it." n |> symbol_failure
    
    member internal x.Parse<'t>(expression: string) : Expr<'t> =
        match MathNetExprParser.parse_to_expr<'t> x.Vars expression with
            | Ok expr -> expr
            | Error error -> sprintf "Could not parse expression %s: %s." expression error |> symbol_failure
            
    interface IPlugin with
        member x.Name with get() = name
        member x.SharedState with get() = x.SharedState
        
        

