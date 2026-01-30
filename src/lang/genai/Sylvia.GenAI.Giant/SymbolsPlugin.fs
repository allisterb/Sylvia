namespace Sylvia.GenAI.Giant

open System.Collections.Generic
open System.ComponentModel

open FSharp.Quotations

open Microsoft.SemanticKernel

open Sylvia

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
    

