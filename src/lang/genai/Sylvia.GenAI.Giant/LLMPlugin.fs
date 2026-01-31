namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic

open FSharp.Quotations

open Microsoft.Extensions.Logging

open Sylvia
open Sylvia.CAS

type SymbolException(message:string) =
    inherit Exception(message)

[<AutoOpen>]
module private LLMUtils =
    let log_kernel_func_info (logger: ILogger option) m = if logger.IsSome then logger.Value.LogInformation m

    let log_kernel_func_error (logger: ILogger option) m = if logger.IsSome then logger.Value.LogError m

    let log_kernel_func_ret (logger: ILogger option) m = 
        do if logger.IsSome then logger.Value.LogInformation m
        m
   
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

    member internal x.GetVar<'t>(n:string) : Var =          
        let t = typeof<'t>
        match x.Vars.Find(fun v -> v.Name = n) with
        | NonNull v when v.Type = t -> v
        | NonNull v ->
            x.Vars.Remove v |> ignore
            let _v = new Var(n, t)
            x.Vars.Add(_v)
            _v
        | Null _ -> 
            let _v = new Var(n, t)
            x.Vars.Add(_v)
            _v
    
    member internal x.GetFunc<'t>(n:string) : Result<Expr<'t>, string> =
         let t = typeof<'t>
         if x.Functions.ContainsKey(n) && x.Functions[n].Type = t then x.Functions[n] :?> Expr<'t> |> Ok
         else if x.Functions.ContainsKey(n) && x.Functions[n].Type <> t then Error <| sprintf "The defined function %s = %A has type %A not %A." n (Maxima.sprint x.Functions[n]) x.Functions[n].Type typeof<'t>
         else Error <| sprintf "The function %s is not defined."  n

    member internal x.Parse<'t>(expression: string) = MathNetExprParser.parse_to_expr<'t> x.Vars expression
                        
    interface IPlugin with
        member x.Name with get() = name
        member x.SharedState with get() = x.SharedState
        
        

