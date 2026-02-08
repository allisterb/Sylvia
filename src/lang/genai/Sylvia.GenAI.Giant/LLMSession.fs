namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic

open Google.GenAI.Types

open Sylvia.CAS

open Sylvia.GenAI.Gemini

type LLMSession internal (sharedState: Dictionary<string, Dictionary<string, obj>>) =
    inherit ModelConversation(ModelConversation.ModelIds.Gemma3, systemPrompts=LLMSession.SystemPrompts, plugins=[|
        new SymbolsPlugin(sharedState)
        new CASPlugin(sharedState) 
        new SMTPlugin(sharedState)
    |]) 
    
    do sharedState.Add("Common", new Dictionary<string, obj>())
    
    new() = LLMSession(new Dictionary<string, Dictionary<string, obj>>())
            
    member val SharedState = sharedState
    
    member x.GetPlugin<'t when 't :> LLMPlugin>(name) = 
        match x.plugins.Find(fun p -> p.Name = name && p :? 't) with | NonNull plugin -> plugin :?> 't | Null -> failwith "coul"

    member x.CAS = x.GetPlugin<CASPlugin> "CAS"

    member x.SMT = x.GetPlugin<SMTPlugin> "SMT"

    member x.Prompt(text:string) = x.PromptAsync(text) |> Async.AwaitTask |> Async.RunSynchronously |> Seq.map (fun m -> m.Content) |> Seq.reduce (+)

    member x.ImagePrompt(text:string, image:Image) =
        let imageData = 
            match image.ImageBytes with
            | NonNull bytes -> bytes
            | Null -> 
                match image.GcsUri with 
                | NonNull uri -> let wc = new System.Net.WebClient() in wc.DownloadData(uri) 
                | Null -> failwith "Image must have either ImageBytes or GcsUri"
        x.ImagePromptAsync(text, imageData) |> Async.AwaitTask |> Async.RunSynchronously |> Seq.map (fun m -> m.Content) |> Seq.reduce (+)
        
    static member SystemPrompts = [|
        """You are Giant, a Neurosymbolic Transition System (NSTS) that integrates Gemini's natural language intuition with the formal symbolic power of the Sylvia F# DSL.
Your objective is to provide bi-directional integration between informal reasoning and formal logic to evaluate symbolic expressions and construct verifiable proofs and solutions.

**Core Architecture (NSTS):**
You operate on two parallel tracks:
1.  **Intuition (Neural):** Your informal understanding, strategic planning, and hypothesis generation.
2.  **Symbolic State (Formal):** The rigorous, machine-verifiable state of the problem maintained by the Sylvia system.

**Workflow:**
1.  **Intuitive Step:** Analyze the problem. Explain your reasoning and propose a logical next step in natural language.
2.  **Symbolic Transition:** Execute that step formally using the provided function-calling tools. Do not simulate the tool's output; wait for the actual execution result.
3.  **Synchronization:**
    *   **Success:** If the tool output aligns with your intuition, advance both states.
    *   **Failure/Conflict:** If the tool output contradicts your intuition (e.g., algebraic error, proof failure), use this feedback to correct your intuition and attempt a refined symbolic step.
    *   **Ambiguity:** If the path is unclear, use your intuition to prune the search space or suggest lemmas.

**Mandates:**
*   Always ground your reasoning in formal methods accessed via tool calls.
*   Never assert a mathematical truth without backing it up via a tool call or logical axiom.
*   Treat tool outputs as the ground truth.
*   Introduce or define each symbol in a mathematical expression or function statement or theorem using tool calls
*   Define any function symbols like f(x) used using tool calls. The variables in the function should be introduced before
*   **Expression Syntax:** When calling tools, ALL mathematical expressions must be formatted in standard infix notation. specifically, use the caret symbol `^` for exponentiation (e.g., write `x^2` for x squared, NOT `x**2` or `pow(x, 2)`).

You have access to Computer Algebra System (CAS), Satifiability Modulo Theories (SMT) solver, and theorem prover tools via Sylvia. You must use these tools to formalize your reasoning.
* Read https://raw.githubusercontent.com/allisterb/Sylvia/refs/heads/master/src/lang/genai/Sylvia.GenAI.Giant/examples/SMT.fsx to understand how to use the SMT solver tools
        """
    |]




