namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic

open Microsoft.Z3
open Google.GenAI.Types

open Sylvia
open Sylvia.GenAI.Gemini

type LLMSession internal (sharedState: Dictionary<string, Dictionary<string, obj>>) =
    inherit ModelConversation(ModelConversation.ModelIds.Gemma3, systemPrompts=LLMSession.SystemPrompts, plugins=[|
        new SymbolsPlugin(sharedState)
        new CASPlugin(sharedState) 
        new SMTPlugin(sharedState)
        new ProverPlugin(sharedState)
    |]) 
    
    let mutable lastProofIndex = 0

    let mutable lastModelIndex = 0
    
    let mutable lastExprIndex = 0

    new() = LLMSession(new Dictionary<string, Dictionary<string, obj>>())
            
    member val SharedState = sharedState
            
    member x.GetPlugin<'t when 't :> LLMPlugin>(name) = 
        x.plugins.Find(fun p -> p.Name = name && p :? 't) |> function | NonNull plugin -> plugin :?> 't | Null -> failwithf "Could not find plugin %s." name

    member x.CAS = x.GetPlugin<CASPlugin> "CAS"

    member x.SMT = x.GetPlugin<SMTPlugin> "SMT"

    member x.Prover = x.GetPlugin<ProverPlugin> "Prover"

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
        
    member x.Prove(prompt: string) =
        let r = x.Prompt(prompt)    
        let proof = 
            if x.Prover.Proofs.Count > lastProofIndex then
                let proofId = x.Prover.Proofs.Keys |> Seq.skip lastProofIndex |> Seq.head
                lastProofIndex <- lastProofIndex + 1
                Some x.Prover.Proofs.[proofId]
            else None
        {Text = r; Proof = proof}

    member x.Solve(prompt: string) =
        let r = x.Prompt(prompt)    
        let model = 
            if x.SMT.Models.Count > lastModelIndex then
                let m = x.SMT.Models.[lastModelIndex]
                lastModelIndex <- lastModelIndex + 1
                Some m
            else None
        {Text = r; Model = model}

    static member SystemPrompts = [|
        """You are GIANT, a Neurosymbolic Transition System (NSTS) that integrates Gemini's natural language intuition with the formal symbolic power of the Sylvia F# DSL.
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
*   Never assert a mathematical or logical truth without backing it up via a tool call or logical axiom.
*   Treat tool outputs as the ground truth.
*   Define any function symbols like f(x) using tool calls. The variables in the function should be introduced before function definition.
*   **Expression Syntax:** When calling tools, ALL mathematical expressions must be formatted in standard infix notation. specifically, use the caret symbol `^` for exponentiation (e.g., write `x^2` for x squared, NOT `x**2` or `pow(x, 2)`). All boolean expressions should use the following logical operators: `&&&` for AND, `|||` for OR, `-` for NOT, `==` for equality, `!=` for inequality, and `==>` for implication.

You have access to Computer Algebra System (CAS), Satifiability Modulo Theories (SMT) solver, and theorem prover tools via Sylvia. You must use these tools to formalize your reasoning.
* Read https://raw.githubusercontent.com/allisterb/Sylvia/refs/heads/master/src/lang/genai/Sylvia.GenAI.Giant/examples/SMT.fsx to understand how to use the SMT solver tools.
* Read https://raw.githubusercontent.com/allisterb/Sylvia/refs/heads/master/src/lang/genai/Sylvia.GenAI.Giant/examples/Prover.fsx to understand how to use the Prover tools.
        """
    |]

and LLMProof = {
    Text: string | null
    Proof: Proof option     
}  

and LLMModel = {
    Text: string | null
    Model: Microsoft.Z3.Model option     
}
