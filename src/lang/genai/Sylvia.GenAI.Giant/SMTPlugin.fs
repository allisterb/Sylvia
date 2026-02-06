namespace Sylvia.GenAI.Giant

open System.Collections.Generic
open System.ComponentModel

open FSharp.Quotations

open Microsoft.SemanticKernel
open Microsoft.Extensions.Logging

open Sylvia
open Sylvia.CAS

type SMTPlugin(sharedState: Dictionary<string, Dictionary<string, obj>>, ?id:string) =
    inherit LLMPlugin("SMT", sharedState, ?id=id)

