namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic
open System.ComponentModel

open Microsoft.SemanticKernel
open Microsoft.Extensions.Logging

open Sylvia
open Sylvia.TermParsers

type ProverPlugin(sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) as this =
    inherit LLMPlugin("Prover", sharedState, ?id=id)
    let proofs = new Dictionary<string, Proof>()
    let theories = new Dictionary<string, Theory>()
    let admissibleRules = new Dictionary<string, ModuleAdmissibleRule array>()
    let derivedRules = new Dictionary<string, ModuleDerivedRule array>()

    let parse theory theorem =
        match theory with
        | "prop_calculus"
        | "pred_calculus" -> parseProp<bool> theorem
        | _ -> failwith "not implemented"

    do
        theories.Add("prop_calculus", PropCalculus.prop_calculus)
        theories.Add("pred_calculus", PredCalculus.pred_calculus)
        admissibleRules.Add("prop_calculus", ProofModules.getModuleAdmissibleRules(PropCalculus.Type))
        admissibleRules.Add("pred_calculus", ProofModules.getModuleAdmissibleRules(PredCalculus.Type))
        derivedRules.Add("prop_calculus", ProofModules.getModuleDerivedRules(PropCalculus.Type))
        derivedRules.Add("pred_calculus", ProofModules.getModuleDerivedRules(PredCalculus.Type))
        this.State.Add("Proofs", proofs)

    member x.Proofs = proofs

    [<KernelFunction("proof")>]
    [<Description("Start a proof of a theorem using the specified theory.")>]
    member x.BoolProof(id:string, theory:string, theorem:string, logger:ILogger | null) : string =
        match parse theory theorem with
        | Ok t ->
            if not (theories.ContainsKey theory) then
                sprintf "Theory %A does not exist" theory |> log_kernel_func_ret logger
            else
                let p = proof theories[theory] t []
                do proofs.Add(id, p)
                sprintf "Started proof %s of %s using theory %s." id theorem theory |> log_kernel_func_ret logger
        | Error error -> sprintf "%s" error |> log_kernel_func_ret logger
            
            