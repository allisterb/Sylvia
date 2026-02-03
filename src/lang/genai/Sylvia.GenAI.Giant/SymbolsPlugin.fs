namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic
open System.ComponentModel

open FSharp.Quotations

open Microsoft.SemanticKernel

open Sylvia

type VariableType =
    | Bool = 0
    | Int = 1
    | Real = 2

type SymbolsPlugin(sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) as this =
    inherit LLMPlugin("Symbols", sharedState, ?id=id)            

    do 
        this.State["Vars"] <- new List<Var>()
        this.State["Functions"] <- new Dictionary<string, Expr>()

    member internal x.GetVarType = 
         function
        | VariableType.Bool -> typeof<bool>
        | VariableType.Int -> typeof<int>
        | VariableType.Real -> typeof<real>
        | t -> failwithf "Variable type %A is not supported." t 

    member internal x.IntroduceVar(name:string, variableType:VariableType) = 
        let t = x.GetVarType variableType    
        match x.Vars.Find(fun _v -> _v.Name = name) with
        | Null ->
            x.Vars.Add(Var(name, t))
            sprintf "Introduced %s variable %s." t.Name name
        | NonNull v -> (sprintf "Variable %s with type %s has already been introduced." name v.Type.Name)
            
    member internal x.DefineFunc<'t>(name:string, expression:string) = 
        match x.Parse<'t> expression with
        | Ok expr ->
            let v = get_vars expr
            x.Functions[name] <- expr
            sprintf "Defined function %s(%A) = %A" name v (sprinte expr)
        | Error error -> sprintf "Could not parse expression %s: %s. Make sure all variables in the expression have been introduced." expression error 

    [<KernelFunction("boolvar")>]
    [<Description("Introduce a boolean variable")>]
    member x.BoolVar(name:string) = x.IntroduceVar(name, VariableType.Bool)
    
    [<KernelFunction("intvar")>]
    [<Description("Introduce an integer variable")>]
    member x.IntVar(name:string) = x.IntroduceVar(name, VariableType.Int)

    [<KernelFunction("realvar")>]
    [<Description("Introduce a real variable")>]
    member x.RealVar(name:string) = x.IntroduceVar(name, VariableType.Real)
    
    [<KernelFunction("realfun")>]
    [<Description("Define a function of one or more real variables")>]
    member x.RealFun(name:string, expression:string) = x.DefineFunc<real>(name, expression)


