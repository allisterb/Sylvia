namespace Sylvia.GenAI.Giant

open System.Collections.Generic
open System.ComponentModel

open FSharp.Quotations

open Microsoft.SemanticKernel

open Sylvia
open Sylvia.CAS

type CASPlugin(sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) =
    inherit LLMPlugin("CAS", sharedState, ?id=id)
    
    do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

    [<KernelFunction("diff")>]
    [<Description("Differentiate an expression wrt a variable")>]
    member x.Diff(variable: string, expression:string) =
        let v = variable |> x.GetVar |> Expr.Var
        let expr = x.Parse<real> expression
        let deriv = Maxima.sprint <| Analysis.diff v expr in 
        sprintf "The derivative of %s wrt %s is %s." expression variable deriv
