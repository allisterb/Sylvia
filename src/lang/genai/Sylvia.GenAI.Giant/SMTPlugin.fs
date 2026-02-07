namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic
open System.ComponentModel

open Microsoft.SemanticKernel
open Microsoft.Extensions.Logging
open Microsoft.Z3

open Sylvia
open Sylvia.Z3

type SMTPlugin(sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) =
    inherit LLMPlugin("SMT", sharedState, ?id=id)

    [<KernelFunction("check_bool_sat")>]
    [<Description("Check if a set of boolean formulas is satisfiable.")>]
    member x.CheckBoolSat([<ParamArray>] formulas:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        let c = formulas |> Array.map (parse_bool_expr s)
        if c |> Array.exists(fun r -> r.IsError) then            
                c 
                |> Array.choose (function Error e -> Some e | _ -> None) 
                |> Array.insertAt 0 "Could not parse one more of the constraints:\n" 
                |> String.concat "\n" 
                |> log_kernel_func_info_ret logger
        else
            c 
            |> Array.choose (function Ok _e -> Some _e | _ -> None) 
            |> s.Check 
            |> sprintf "%A" 
            |> log_kernel_func_info_ret logger       

    [<KernelFunction("check_int_sat")>]
    [<Description("Check if a set of integer constraints is satisfiable.")>]
    member x.CheckIntSat([<ParamArray>] constraints:string array, logger:ILogger | null) : string =
        let s = new Z3Solver()
        let c = constraints |> Array.map (parse_bool_expr s)
        if c |> Array.exists(fun r -> r.IsError) then            
                c 
                |> Array.choose (function Error e -> Some e | _ -> None) 
                |> Array.insertAt 0 "Could not parse one more of the constraints:\n" 
                |> String.concat "\n" 
                |> log_kernel_func_info_ret logger
        else
            c 
            |> Array.choose (function Ok _e -> Some _e | _ -> None) 
            |> s.Check 
            |> sprintf "%A" 
            |> log_kernel_func_info_ret logger         
            
            