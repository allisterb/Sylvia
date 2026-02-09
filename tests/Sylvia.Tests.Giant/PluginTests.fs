namespace Sylvia.Tests.Giant

open System
open System.Collections.Generic
open System.Threading.Tasks

open Xunit

open Sylvia
open Sylvia.GenAI.Gemini
open Sylvia.GenAI.Giant

type PluginTests() =
    inherit Sylvia.Tests.Giant.TestsRuntime()

    [<Fact>]
    member this.``Can create LLM session`` ()  =
        let llm = new LLMSession()  
        let r2 = llm.Prompt("diffrentiate x^2 wrt x") 
        Assert.NotEmpty llm.SharedState
        
    [<Fact>]
    member this.``Can define function`` ()  =
        let llm = new LLMSession()  
        let r2 = llm.Prompt("Let f(x, y) = x^2 + y^3. Find f'(x)")
        Assert.NotEmpty llm.SharedState
        
    [<Fact>]
    member this.``Can solve integer constraints`` ()  =
        let llm = new LLMSession()  
        let r1 = llm.Prompt("Is the set of integer constraints x > 5 and x < 9 satisfiable?")
        Assert.NotEmpty llm.SharedState 

    [<Fact>]
    member this.``Can formalize constraints`` ()  =
        let llm = new LLMSession()  
        let r1 = llm.Prompt("Is the set of integer constraints x > 5 and x < 9 satisfiable?")
        Assert.NotEmpty llm.SharedState 

    
    [<Fact>]
    member this.``Can start proof`` ()  =
        let llm = new LLMSession()  
        let r1 = llm.Prompt("")
        Assert.NotEmpty llm.SharedState 
       

