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

type SymbolsPlugin(sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) as this =
    inherit LLMPlugin("Symbols", sharedState, ?id=id)    
    do 
        this.State["Vars"] <- new List<Var>()

    member internal x.IntroduceVar(name:string, variableType:VariableType) = 
        let t = 
            match variableType with
            | VariableType.Bool -> typeof<bool>
            | VariableType.Int -> typeof<int>
            | VariableType.Real -> typeof<real>
            | _ -> failwithf "Variable type %A is not supported." variableType
        x.Vars.Add(Var(name, t))
        sprintf "%s variable %s introduced." t.Name name
      
    [<KernelFunction("boolvar")>]
    [<Description("Introduce a boolean variable")>]
    member x.BoolVar(name:string) = x.IntroduceVar(name, VariableType.Bool)
    
    [<KernelFunction("intvar")>]
    [<Description("Introduce an integer variable")>]
    member x.IntVar(name:string) = x.IntroduceVar(name, VariableType.Int)

    [<KernelFunction("realvar")>]
    [<Description("Introduce a real variable")>]
    member x.RealVar(name:string) = x.IntroduceVar(name, VariableType.Real)
    
type CASPlugin(sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) =
    inherit LLMPlugin("CAS", sharedState, ?id=id)
    
    [<KernelFunction("diff")>]
    [<Description("Differentiate an expression wrt a variable")>]
    member x.Diff(variable: string, expression:string) =
        let v = variable |> x.GetVar |> Expr.Var
        let expr = x.Parse<real> expression
        let deriv = Maxima.sprint <| Analysis.diff v expr in 
        sprintf "The derivative of %s wrt %s is %s." expression variable deriv
        
        
