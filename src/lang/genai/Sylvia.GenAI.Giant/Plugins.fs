namespace Sylvia.GenAI

open System.Collections.Generic
open System.ComponentModel

open FSharp.Quotations

open Microsoft.SemanticKernel

open Sylvia
open Sylvia.CAS

type VariableType =
    | Bool = 0
    | Int = 1
    | Real = 2

type GiantPlugin(name:string, ?id:string) =
    member val Name = name
    
    member val Id = defaultArg id (System.Guid.NewGuid().ToString())   
    
    member val SharedState = new Dictionary<string, Dictionary<string, obj>>() with get, set
    
    member val Variables: List<Var> = new List<Var>()

    member internal x.Parse<'t>(expression: string) : Expr<'t> =
        match MathNetExprParser.parse_to_expr<'t> x.Variables expression with
            | Ok expr -> expr
            | Error error -> failwithf "Could not parse expression %s: %s." expression error
        
    member internal x.DeclareVar(name:string, variableType:VariableType) = 
        let t = 
            match variableType with
            | VariableType.Bool -> typeof<bool>
            | VariableType.Int -> typeof<int>
            | VariableType.Real -> typeof<real>
            | _ -> failwithf "Variable type %A is not supported." variableType
        x.Variables.Add(Var(name, t))
        sprintf "Variable %s declared." name

    member internal x.GetVar(n:string) : Var =  
        let var = x.Variables.Find(fun v -> v.Name = n) in
        match var with
        | NonNull v -> v
        | Null _ -> failwithf "Variable %s not declared." n

    
    [<KernelFunction("boolvar")>]
    [<Description("Declare a boolean variable")>]
    member  x.BoolVar(name:string) = x.DeclareVar(name, VariableType.Bool)

    interface IPlugin with
        member x.Name with get () = name
        member x.SharedState with get() = x.SharedState and set(value) = x.SharedState <- value
        
type CASPlugin(?id:string) =
    inherit GiantPlugin("CAS", ?id=id)
    
    [<KernelFunction("diff")>]
    [<Description("Differentiate an expression wrt a variable")>]
    member x.Diff(variable: string, expression:string) =
        let v = variable |> x.GetVar |> Expr.Var
        let expr = x.Parse<real> expression
        let deriv = Maxima.sprint <| Analysis.diff v expr in 
        sprintf "The derivative of %s wrt %s is %s." expression variable deriv
        
        
