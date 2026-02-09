namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic
open System.ComponentModel

open Microsoft.SemanticKernel
open Microsoft.Extensions.Logging

open Sylvia
open Sylvia.ProofParsers

type ProverPlugin(sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) as this =
    inherit LLMPlugin("Prover", sharedState, ?id=id)
    let proofs = new Dictionary<string, Proof>()
    let theories = new Dictionary<string, Theory>()
    let admissibleRules = new Dictionary<string, ModuleAdmissibleRule array>()
    let derivedRules = new Dictionary<string, ModuleDerivedRule array>()    
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
    [<Description("Create a proof of a theorem using the specified theory.")>]
    member x.Proof(theory:string, theorem:string, ruleApplications: string array, id:string, logger:ILogger | null) : string =
       match parseProof theories admissibleRules derivedRules theory theorem ruleApplications with
       | Ok proof -> 
              proofs.Add(id, proof)
              sprintf "Proof created with ID: %s" id |> log_kernel_func_ret logger
       | Error error -> log_kernel_func_ret logger error
            
            