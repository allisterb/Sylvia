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
            let! r1 = llm.Prompt("Introduce the real variable x") |> Async.AwaitTask
            Assert.NotNull r1
            let! r2 = llm.Prompt("diffrentiate x^2 wrt x") |> Async.AwaitTask
            Assert.NotNull r2
        } |> Async.RunSynchronously
        Assert.NotEmpty llm.SharedState
        
    [<Fact>]
    member this.``Can define function`` ()  =
        let llm = new LLMSession()  
        async {
            let! r1 = llm.Prompt("Introduce the real variable x") |> Async.AwaitTask
            Assert.NotNull r1
            let! r2 = llm.Prompt("Let f(x) = x^2. Find f'(x)") |> Async.AwaitTask
            Assert.NotNull r2
        } |> Async.RunSynchronously
        Assert.NotEmpty llm.SharedState
        
        
       

