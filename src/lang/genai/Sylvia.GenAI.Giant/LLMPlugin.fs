namespace Sylvia.GenAI.Giant

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

type LLMPlugin(name:string, ?id:string) =
    member val Name = name
    
    member val Id = defaultArg id (System.Guid.NewGuid().ToString())   
    
    member val SharedState = new Dictionary<string, Dictionary<string, obj>>() with get, set
    
    member val Variables: List<Var> = new List<Var>()

    member internal x.Parse<'t>(expression: string) : Expr<'t> =
        match MathNetExprParser.parse_to_expr<'t> x.Variables expression with
            | Ok expr -> expr
            | Error error -> failwithf "Could not parse expression %s: %s." expression error
        
    member internal x.IntroduceVar(name:string, variableType:VariableType) = 
        let t = 
            match variableType with
            | VariableType.Bool -> typeof<bool>
            | VariableType.Int -> typeof<int>
            | VariableType.Real -> typeof<real>
            | _ -> failwithf "Variable type %A is not supported." variableType
        x.Variables.Add(Var(name, t))
        sprintf "%s variable %s introduced." t.Name name

    member internal x.GetVar(n:string) : Var =          
        match x.Variables.Find(fun v -> v.Name = n) with
        | NonNull v -> v
        | Null _ -> failwithf "The variable %s is not declared. You must declare this variable and its type first before you use it." n
    
    [<KernelFunction("boolvar")>]
    [<Description("Introduce a boolean variable")>]
    member x.BoolVar(name:string) = x.IntroduceVar(name, VariableType.Bool)
    
    [<KernelFunction("intvar")>]
    [<Description("Introduce an integer variable")>]
    member x.IntVar(name:string) = x.IntroduceVar(name, VariableType.Int)

    [<KernelFunction("realvar")>]
    [<Description("Introduce a real variable")>]
    member x.RealVar(name:string) = x.IntroduceVar(name, VariableType.Real)

    interface IPlugin with
        member x.Name with get () = name
        member x.SharedState with get() = x.SharedState and set(value) = x.SharedState <- value
        
type CASPlugin(?id:string) =
    inherit LLMPlugin("CAS", ?id=id)
    
    [<KernelFunction("diff")>]
    [<Description("Differentiate an expression wrt a variable")>]
    member x.Diff(variable: string, expression:string) =
        let v = variable |> x.GetVar |> Expr.Var
        let expr = x.Parse<real> expression
        let deriv = Maxima.sprint <| Analysis.diff v expr in 
        sprintf "The derivative of %s wrt %s is %s." expression variable deriv
        
        
