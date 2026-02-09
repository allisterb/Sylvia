namespace Sylvia.GenAI.Giant

open System.Collections.Generic
open System.ComponentModel

open FSharp.Quotations

open Microsoft.SemanticKernel
open Microsoft.Extensions.Logging

open Sylvia
open Sylvia.CAS

type CASPlugin(sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) =
    inherit LLMPlugin("CAS", sharedState, ?id=id)
    
    [<KernelFunction("diff")>]
    [<Description("Differentiate an expression wrt a variable")>]
    member x.Diff(variable: string, expression:string, logger:ILogger | null) =
        let v = variable |> x.GetVar<real> |> Expr.Var
        match x.Parse<real> expression with
        | Ok expr -> 
            let deriv = Maxima.sprint <| Analysis.diff v expr in 
            sprintf "The derivative of %s wrt %s is %s." expression variable deriv |> log_kernel_func_ret logger
        | Error error -> sprintf "Could not parse expression %s: %s. Make sure all variables have been introduced." expression error |> log_kernel_func_ret logger
        
