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
    let models = new List<Microsoft.Z3.Model>()
    let proofs = new List<string>()
    do
        this.State.Add("Models", models)

    member x.Models = models

    member x.Proofs = proofs

    [<KernelFunction("check_bool_sat")>]
    [<Description("Check if a set of boolean formulas is satisfiable.")>]
    member x.CheckBoolSat([<ParamArray>] formulas:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        match check_bool_sat s formulas with
        | Ok Status.UNSATISFIABLE -> 
              proofs.Add(s.Solver.Proof.ToString())
              log_kernel_func_ret logger "UNSATISFIABLE"
        | Ok status -> 
            x.Models.Add(s.Model())
            status |> sprintf "%A" |> log_kernel_func_ret logger
        | Error error -> log_kernel_func_ret logger error
        
    [<KernelFunction("check_int_sat")>]
    [<Description("Check if a set of integer constraints is satisfiable.")>]
    member x.CheckIntSat([<ParamArray>] constraints:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        match check_int_sat s constraints with
        | Ok Status.UNSATISFIABLE -> 
              proofs.Add(s.Solver.Proof.ToString())
              log_kernel_func_ret logger "UNSATISFIABLE"
        | Ok status -> status |> sprintf "%A" |> log_kernel_func_ret logger
        | Error error -> log_kernel_func_ret logger error
            
    [<KernelFunction("check_real_sat")>]
    [<Description("Check if a set of real constraints is satisfiable.")>]
    member x.CheckRealSat([<ParamArray>] constraints:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        match check_real_sat s constraints with
        | Ok Status.UNSATISFIABLE -> 
              proofs.Add(s.Solver.Proof.ToString())
              log_kernel_func_ret logger "UNSATISFIABLE"
        | Ok status -> status |> sprintf "%A" |> log_kernel_func_ret logger
        | Error error -> log_kernel_func_ret logger error

    [<KernelFunction("get_bool_model")>]
    [<Description("Get a model or interpretation that satisifies a boolean formula.")>]
    member x.GetBoolModel([<ParamArray>] constraints:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        match get_bool_model s constraints with
        | Ok model -> 
            models.Add(s.Model())         
            model 
            |> Seq.map (fun (v,e) -> sprintf "%A=%A\n" v e) 
            |> Seq.reduce (+)
            |> log_kernel_func_ret logger
        | Error "UNSAT" -> 
              proofs.Add(s.Solver.Proof.ToString())
              log_kernel_func_ret logger "UNSAT"
        | Error error -> log_kernel_func_ret logger error

    [<KernelFunction("get_int_model")>]
    [<Description("Get a model or interpretation that satisifies a set of integer constraints.")>]
    member x.GetIntModel([<ParamArray>] constraints:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        s.SolverParams.Add("proof", "true") |> ignore
        match get_int_model s constraints with
        | Ok model -> 
            models.Add(s.Model())                        
            model 
            |> Seq.map (fun (v,e) -> sprintf "%A=%A\n" v e) 
            |> Seq.reduce (+)
            |> log_kernel_func_ret logger
        | Error "UNSAT" -> 
              proofs.Add(s.Solver.Proof.ToString())
              log_kernel_func_ret logger "UNSAT"
        | Error error -> log_kernel_func_ret logger error

    [<KernelFunction("get_real_model")>]
    [<Description("Get a model or interpretation that satisifies a set of real constraints.")>]
    member x.GetRealModel([<ParamArray>] constraints:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        match get_real_model s constraints with
        | Ok model -> 
            models.Add(s.Model())          
            model 
            |> Seq.map (fun (v,e) -> sprintf "%A=%A\n" v e) 
            |> Seq.reduce (+)
            |> log_kernel_func_ret logger
        | Error "UNSAT" -> 
              proofs.Add(s.Solver.Proof.ToString())
              log_kernel_func_ret logger "UNSAT"
        | Error error -> log_kernel_func_ret logger error


        
        
            
            
            