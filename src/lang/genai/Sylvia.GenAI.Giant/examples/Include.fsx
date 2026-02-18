#r "nuget: Microsoft.Extensions.Logging.Abstractions, 10.0.0"
#r "nuget: Microsoft.Extensions.Configuration.Abstractions, 10.0.1"
#r "nuget: Microsoft.Extensions.Configuration.Json, 10.0.1"
#r "nuget: Microsoft.SemanticKernel.Abstractions, 1.70.0"
#r "nuget: Microsoft.SemanticKernel.Connectors.Google, 1.70.0-alpha"
#r "nuget: FParsec, 1.0.3"
#r "nuget: MathNet.Numerics.FSharp, 4.15.0"
#r "nuget: Microsoft.Z3, 4.11.2"
#r "nuget: FSharp.Quotations.Evaluator, 2.1.0"
#r "nuget: CsvHelper, 12.1.2"
#r "nuget: Google.GenAI, 0.11.0"
#r "nuget: Microsoft.DotNet.Interactive.Formatting, 1.0.0-beta.24568.1"
#r "nuget: Unquote, 7.0.1"
#r "nuget: Sylvia.Arithmetic, 0.2.8"

#r "..\\..\\..\\..\\..\\src\\lang\\core\\Sylvia.Expressions\\bin\\Debug\\net10.0\\Expect.NETStandard.dll"
#r "..\\..\\..\\..\\..\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.dll"
#r "..\\..\\..\\..\\..\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.Interop.NETStandard.dll"
#r "..\\..\\..\\..\\..\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.Util.dll"
#r "..\\..\\..\\..\\..\\ext\\FunScript\\src\\extra\\FunScript.Bindings.JSXGraph\\bin\\Debug\\netstandard2.0\\FunScript.Bindings.Base.dll"
#r "..\\..\\..\\..\\..\\ext\\FunScript\\src\\extra\\FunScript.Bindings.JSXGraph\\bin\\Debug\\netstandard2.0\\FunScript.Bindings.JSXGraph.dll"
#r "..\\..\\..\\..\\..\\ext\\mathnet-symbolics\\src\\Symbolics\\bin\\Debug\\netstandard2.0\\MathNet.Symbolics.dll"

#r "..\\..\\..\\..\\..\\src\\lang\\CAS\\Sylvia.CAS.Maxima\\bin\\Debug\\net10.0\\Sylvia.Runtime.dll"
#r "..\\..\\..\\..\\..\\src\\base\\Sylvia.Collections\\bin\\Debug\\netstandard2.0\\Sylvia.Collections.dll"
#r "..\\..\\..\\..\\..\\src\\lang\\core\\Sylvia.Expressions\\bin\\Debug\\net10.0\\Sylvia.Expressions.dll"
#r "..\\..\\..\\..\\..\\src\\lang\\core\\Sylvia.Prover\\bin\\Debug\\net10.0\\Sylvia.Prover.dll"
#r "..\\..\\..\\..\\..\\src\\lang\\CAS\\Sylvia.CAS.Maxima\\bin\\Debug\\net10.0\\MathNet.Symbolics.dll"
#r "..\\..\\..\\..\\..\\src\\lang\\CAS\\Sylvia.CAS.Maxima\\bin\\Debug\\net10.0\\Sylvia.CAS.Maxima.dll"
#r "..\\..\\..\\..\\..\\src\\lang\\solvers\\Sylvia.Solver.Z3\\bin\\Debug\\net10.0\\Sylvia.Solver.Z3.dll"
#r "..\\..\\..\\..\\..\\src\\data\\Sylvia.Data\\bin\\Debug\\netstandard2.0\\Sylvia.Data.dll"
#r "..\\..\\..\\..\\..\\src\\lang\\visualization\\Sylvia.Visualization.Html\\bin\\Debug\\net10.0\\Sylvia.Visualization.Html.dll"
#r "..\\..\\..\\..\\..\\src\\lang\\genai\\Sylvia.GenAI.Gemini\\bin\\Debug\\net10.0\\Sylvia.GenAI.Gemini.dll"
#r "..\\..\\..\\..\\..\\src\\lang\\genai\\Sylvia.GenAI.Giant\\bin\\Debug\\net10.0\\Sylvia.GenAI.Giant.dll"

#r "..\\..\\..\\..\\..\\src\\Math\\Sylvia.AbstractAlgebra\\bin\\Debug\\net10.0\\Sylvia.AbstractAlgebra.dll"
#r "..\\..\\..\\..\\..\\src\\Math\\Sylvia.LinearAlgebra\\bin\\Debug\\net10.0\\Sylvia.LinearAlgebra.dll"
#r "..\\..\\..\\..\\..\\src\\Math\\Sylvia.RealAnalysis\\bin\\Debug\\net10.0\\Sylvia.RealAnalysis.dll"
#r "..\\..\\..\\..\\..\\src\\Math\\Sylvia.Statistics\\bin\\Debug\\net10.0\\Sylvia.Statistics.dll"

#nowarn "3391"

namespace Sylvia.GenAI.Giant

open System.Collections.Generic
open Sylvia
open Sylvia.Z3
open Sylvia.ProofParsers
module Examples =

    let check_bool_sat (constraints: string list) =
        let s = new Z3Solver()
        Sylvia.Z3.check_bool_sat s constraints  

    let check_int_sat (constraints: string list) =
        let s = new Z3Solver()
        Sylvia.Z3.check_int_sat s constraints  

    let check_real_sat (constraints: string list) =
        let s = new Z3Solver()
        Sylvia.Z3.check_real_sat s constraints  

    let get_bool_model (constraints: string list) =
        let s = new Z3Solver()
        Sylvia.Z3.get_bool_model s constraints
        
    let get_int_model (constraints: string list) =
        let s = new Z3Solver()
        Sylvia.Z3.get_int_model s constraints

    let theories = new Dictionary<string, Theory>()
    let admissibleRules = new Dictionary<string, ModuleAdmissibleRule array>()
    let derivedRules = new Dictionary<string, ModuleDerivedRule array>()    
    let theorems = new Dictionary<string, ModuleTheorem array>()    
    let tactics = new Dictionary<string, ModuleTactic array>()    
    do
        theories.Add("prop_calculus", PropCalculus.prop_calculus)
        theories.Add("pred_calculus", PredCalculus.pred_calculus)
        admissibleRules.Add("prop_calculus", ProofModules.getModuleAdmissibleRules(PropCalculus.Type))
        admissibleRules.Add("pred_calculus", ProofModules.getModuleAdmissibleRules(PredCalculus.Type))
        derivedRules.Add("prop_calculus", ProofModules.getModuleDerivedRules(PropCalculus.Type))
        derivedRules.Add("pred_calculus", ProofModules.getModuleDerivedRules(PredCalculus.Type))
        theorems.Add("prop_calculus", ProofModules.getModuleTheorems(PropCalculus.Type))
        theorems.Add("pred_calculus", ProofModules.getModuleTheorems(PredCalculus.Type))
        tactics.Add("prop_calculus", ProofModules.getModuleTactics(PropCalculus.Type))
        tactics.Add("pred_calculus", ProofModules.getModuleTactics(PredCalculus.Type))

    let proof (theory:string) (theorem:string) (ruleApplications: string list) =
        let proofParser theory= 
            match theory with        
            | "prop_calculus" 
            | "pred_calculus" -> parseProof<bool>
            | _ -> failwithf "%s theory is not supported by the proof parser" theory
        let parse = proofParser theory
        match parse theories admissibleRules derivedRules theorems tactics theory theorem (List.toArray ruleApplications) with
        | Ok proof -> proof 
        | Error error -> failwith error
