namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
  
open Google.GenAI.Types

open Sylvia
open Sylvia.GenAI.Gemini

module LLMSessionHelpers =
    
    let defaultMaxDocChars = 30000
    
    let ingestPromptText (path:string) (maxChars:int) : string =
        if File.Exists(path) then
            let text = File.ReadAllText(path)
            if text.Length > maxChars then text.Substring(0, maxChars) else text
        else failwithf "File not found: %s" path

    let doc0 = ingestPromptText (Path.Combine(Runtime.AssemblyLocation, "docs", "eqlogic.txt")) defaultMaxDocChars
    let doc1 = ingestPromptText (Path.Combine(Runtime.AssemblyLocation, "examples", "prompts", "SMT.txt")) defaultMaxDocChars
    let doc2 = ingestPromptText (Path.Combine(Runtime.AssemblyLocation, "examples", "prompts", "Prover.txt")) defaultMaxDocChars

type LLMSession internal (sharedState: Dictionary<string, Dictionary<string, obj>>) =
    inherit ModelConversation(ModelConversation.ModelIds.Gemma3, 
    systemPrompts=Array.append LLMSession.SystemPrompts [|LLMSessionHelpers.doc0; LLMSessionHelpers.doc1; LLMSessionHelpers.doc2|], 
    plugins=[|
        new SymbolsPlugin(sharedState)
        new CASPlugin(sharedState) 
        new SMTPlugin(sharedState)
        new ProverPlugin(sharedState)
    |]) 
    
    let mutable lastProofIndex = 0

    let mutable lastModelIndex = 0
    
    let mutable lastModelProofIndex = 0

    let mutable lastExprIndex = 0
                                 
    new() = new LLMSession(new Dictionary<string, Dictionary<string, obj>>())
                   
    member val SharedState = sharedState
            
    member x.GetPlugin<'t when 't :> LLMPlugin>(name) = 
        x.plugins.Find(fun p -> p.Name = name && p :? 't) |> function | NonNull plugin -> plugin :?> 't | Null -> failwithf "Could not find plugin %s." name

    member x.CAS = x.GetPlugin<CASPlugin> "CAS"

    member x.SMT = x.GetPlugin<SMTPlugin> "SMT"

    member x.Prover = x.GetPlugin<ProverPlugin> "Prover"

    member x.Prompt(text:string, [<ParamArray>] content: obj array) = x.PromptAsync(text, content) |> Async.AwaitTask |> Async.RunSynchronously |> Seq.map (fun m -> m.Content) |> Seq.reduce (+)

    member x.ImagePrompt(text:string, image:Image) =
        let imageData = 
            match image.ImageBytes with
            | NonNull bytes -> bytes
            | Null -> 
                match image.GcsUri with 
                | NonNull uri -> let wc = new System.Net.WebClient() in wc.DownloadData(uri) 
                | Null -> failwith "Image must have either ImageBytes or GcsUri"
        x.ImagePromptAsync(text, imageData) |> Async.AwaitTask |> Async.RunSynchronously |> Seq.map (fun m -> m.Content) |> Seq.reduce (+)
        
    member x.Prove(prompt: string,  [<ParamArray>] content: obj array) =
        let r = x.Prompt(prompt, content)    
        let proof = 
            if x.Prover.Proofs.Count > lastProofIndex then
                let proofId = x.Prover.Proofs.Keys |> Seq.skip lastProofIndex |> Seq.head
                lastProofIndex <-  x.Prover.Proofs.Count
                Some x.Prover.Proofs.[proofId]
            else None
        {Text = r; Proof = proof}

    member x.Solve(prompt: string,  [<ParamArray>] content: obj array) =
        let r = x.Prompt(prompt, content)    
        let m = 
            if x.SMT.Models.Count > lastModelIndex then
                let m = x.SMT.Models.[lastModelIndex]           
                lastModelIndex <- x.SMT.Models.Count
                Some m
            else None
        let up = 
            if x.SMT.Proofs.Count > lastModelProofIndex then
                let p = x.SMT.Proofs.[lastModelProofIndex]           
                lastModelProofIndex <- x.SMT.Proofs.Count
                Some p
            else None
        {Text = r; Model = m; ModelProof=up}
    
    static member val SystemPrompts = [|
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
*   **Ouput** Always return mathematical and logical expressions to the user as LaTex, using standard operator symbols.
You have access to Computer Algebra System (CAS), Satifiability Modulo Theories (SMT) solver, and theorem prover tools via Sylvia. You must use these tools to formalize your reasoning.
Read the provided examples to understand how to use the different tools.
**Tools**

***Solver***
The Solver plugin uses Microsoft's Z3 SMT solver to check the satisfiability of logical formulas and constraints, and to find models that satisfy them.
When you want to check if a set of constraints is satisfiable, use the `check_int_sat` function for integer constraints, `check_real_sat` for real constraints, and `check_bool_sat` for boolean formulas. If you want to find a model that satisfies a set of integer constraints, use the `get_int_model` function.
Read the provided examples to understand the syntax for expressing constraints and queries to the SMT solver. 


***Prover***
The Sylvia prover is an equational logic theorem prover. Read the provided document to understand the logic and the examples to understand the syntax for expressing theorems and proof steps to the prover. 

Do not spend too long attempting to construct a formal proof. If you make 4 attempts and are stuck, return the incomplete proof along with your thinking and intuition about how to proceed and ask the user for hints.


        """ |] with get, set

and LLMProof = {
    Text: string | null
    Proof: Proof option     
}  

and LLMModel = {
    Text: string | null
    Model: Microsoft.Z3.Model option   
    ModelProof: string option
}
