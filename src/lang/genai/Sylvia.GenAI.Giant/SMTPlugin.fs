namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic
open System.ComponentModel

open Microsoft.SemanticKernel
open Microsoft.Extensions.Logging
open Microsoft.Z3

open Sylvia
open Sylvia.Z3

type SMTPlugin(sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) as this =
    inherit LLMPlugin("SMT", sharedState, ?id=id)
    let models = new Stack<Microsoft.Z3.Model>()
    do
        this.State.Add("Models", models)

    member x.Models = models

    [<KernelFunction("check_bool_sat")>]
    [<Description("Check if a set of boolean formulas is satisfiable.")>]
    member x.CheckBoolSat([<ParamArray>] formulas:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        match check_bool_sat s formulas with
        | Ok status -> status |> sprintf "%A" |> log_kernel_func_ret logger
        | Error error -> log_kernel_func_ret logger error
        
    [<KernelFunction("check_int_sat")>]
    [<Description("Check if a set of integer constraints is satisfiable.")>]
    member x.CheckIntSat([<ParamArray>] constraints:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        match check_int_sat s constraints with
        | Ok status -> status |> sprintf "%A" |> log_kernel_func_ret logger
        | Error error -> log_kernel_func_ret logger error
            
    [<KernelFunction("get_int_model")>]
    [<Description("Get a model or interpretation that satisifies a set of integer constraints.")>]
    member x.GetIntModel([<ParamArray>] constraints:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        get_int_model s constraints 
        |> Seq.map (fun (v,e) -> sprintf "%A=%A\n" v e) 
        |> Seq.reduce (+)
        |> log_kernel_func_ret logger
            
            
            