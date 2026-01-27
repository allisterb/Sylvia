namespace Sylvia.GenAI

open System.Collections.Generic
open System.ComponentModel

open FSharp.Quotations

open Microsoft.SemanticKernel

open Sylvia

type GiantPlugin(name:string, ?id:string) =
    member val Name = name
    member val Id = defaultArg id (System.Guid.NewGuid().ToString())   
    member val SharedState = new Dictionary<string, Dictionary<string, obj>>() with get, set
    
    member val Variables: List<Var> = new List<Var>()

    member x.GetVar(n:string) : Var =  
        let var = x.Variables.Find(fun v -> v.Name = n) in
        match var with
        | NonNull v -> v
        | Null _ -> failwith ""


    interface IPlugin with
        member x.Name with get () = name
        member x.SharedState with get() = x.SharedState and set(value) = x.SharedState <- value
        

type CASPlugin(?id:string) =
    inherit GiantPlugin("CAS", ?id=id)

    
    [<KernelFunction("diff")>]
    [<Description("Differentiate an expression wrt a variable")>]
    member x.diff(variable: string, expression:string) =
        let expr = 
            match MathNetExprParser.parse_to_expr x.Variables expression with
            | Ok(expr) -> expr
