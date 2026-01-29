namespace Sylvia.GenAI.Giant

open System

open Sylvia
open Sylvia.GenAI.Gemini

type LLMSession([<ParamArray>] plugins: IPlugin array) =
    inherit ModelConversation(ModelIds.Gemma3, systemPrompts=LLMSession.SystemPrompts, plugins=[|
        new CASPlugin()
    |])
    
    static member SystemPrompts = [|
        "You are a Neurosymbolic Transition System for the Gemini LLM using the F# DSL Sylvia." 
    |]




