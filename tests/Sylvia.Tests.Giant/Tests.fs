namespace Sylvia.Tests.Giant

open System
open System.Collections.Generic
open System.Threading.Tasks

open Microsoft.SemanticKernel
open Xunit

open Sylvia.GenAI.Gemini
open Sylvia.GenAI.Giant

type PluginTests() =
    inherit TestsRuntime()

    [<Fact>]
    member this.``Can create LLM session`` ()  =
        let llm = new LLMSession()                
        async {
            let! r = llm.Prompt("Introduce the bool variable x") |> Async.AwaitTask
            Assert.NotNull r
        } |> Async.RunSynchronously
        Assert.NotEmpty llm.PluginState
        
        
        
       

