# Giant Project Analysis: Neurosymbolic Transition System

## 1. Executive Summary

**Giant** (implemented in `Sylvia.GenAI.Giant`) is a Neurosymbolic Transition System (NSTS) designed to bridge the gap between the informal, intuitive reasoning of Large Language Models (specifically Google's Gemini) and the rigorous, formal verification of symbolic mathematics tools.

The primary objective of Giant is to enable Gemini to:
1.  **Reason Symbolically**: Move beyond statistical text generation to perform mathematically verified operations.
2.  **Construct Formal Proofs**: Iteratively build verifiable proofs for theorems using equational logic.
3.  **Verify Safety**: Provide traceable and logically sound decision-making paths in safety-critical scenarios.

By integrating the **Sylvia** F# DSL (which wraps CAS, SMT solvers, and Theorem Provers) with Gemini, Giant allows the LLM to use "tools" to verify its own "thoughts."

## 2. Architecture: Neurosymbolic Transition System (NSTS)

The core architectural pattern of Giant is the **NSTS**, which operates on two parallel tracks:

1.  **Intuition Track (Neural)**:
    *   **Agent**: Gemini LLM.
    *   **Role**: Strategic planning, hypothesis generation, natural language explanation, and pruning the search space for proofs.
    *   **Nature**: Probabilistic, creative, but prone to hallucination.

2.  **Symbolic Track (Formal)**:
    *   **Agent**: Sylvia DSL (F#).
    *   **Role**: Execution of mathematical operations, maintaining strict state (variable types, axioms), and verifying logical steps.
    *   **Nature**: Deterministic, rigorous, but lacks "insight."

### The NSTS Workflow
The system employs a cyclic feedback loop defined in the `LLMSession` system prompts:
1.  **Intuitive Step**: The LLM analyzes the problem and proposes a logical next step in natural language.
2.  **Symbolic Transition**: The LLM executes this step formally by calling a specific function (Tool).
3.  **Synchronization & Feedback**:
    *   **Success**: If the tool returns a result that aligns with intuition, the state advances.
    *   **Failure**: If the tool returns an error or a contradiction (e.g., "UNSAT"), the LLM uses this feedback to correct its hypothesis.
    *   **Ambiguity**: The LLM uses intuition to suggest lemmas or alternative paths.

## 3. Implementation Details

The implementation is centered around the **Microsoft Semantic Kernel** SDK, bridging F# code with the Gemini API.

### 3.1 Core Session (`LLMSession.fs`)
The `LLMSession` class is the heart of the application.
*   **Inheritance**: Inherits from `ModelConversation` (Sylvia.GenAI.Gemini), utilizing the Gemini 1.5/2.0 models.
*   **Context Management**: Maintains a `SharedState` dictionary that persists across tool calls, allowing different plugins to access common symbols (variables, constants).
*   **System Prompts**: Injects a rigorous persona definition ("You are GIANT...") that mandates all mathematical assertions be backed by tool calls.
*   **Ingestion**: Automatically loads documentation (`eqlogic.txt`) and examples (`SMT.txt`, `Prover.txt`) into the context window to "teach" the model the specific syntax of the Sylvia DSL in-context.

### 3.2 The Plugin System
Giant exposes its capabilities through modular plugins, decorated with `[<KernelFunction>]` attributes for Semantic Kernel discovery.

#### **A. SymbolsPlugin (`SymbolsPlugin.fs`)**
Acts as the type system enforcer.
*   **Responsibilities**: Introduces variables (`BoolVar`, `IntVar`, `RealVar`) and constants into the shared state.
*   **Why it's needed**: Prevents the LLM from using undefined symbols or mixing incompatible types (e.g., adding a Boolean to a Real).

#### **B. CASPlugin (`CASPlugin.fs`)**
Provides Computer Algebra System capabilities via **MathNet.Symbolics** (and optionally Maxima).
*   **Key Functions**:
    *   `Diff`: Symbolic differentiation.
    *   `Parse`: Verifies that natural language math expressions are valid Sylvia expressions.
*   **Format**: Enforces standard infix notation (e.g., `^` for power) to ensure parsing reliability.

#### **C. SMTPlugin (`SMTPlugin.fs`)**
Interfaces with **Microsoft Z3** for satisfiability checking.
*   **Key Functions**:
    *   `CheckBoolSat` / `CheckIntSat` / `CheckRealSat`: Validates if a set of constraints holds.
    *   `GetModel`: Returns a concrete example (counter-example or solution) satisfying the constraints.
*   **Usage**: Used for logic puzzles, constraint solving, and verifying logical consistency.

#### **D. ProverPlugin (`ProverPlugin.fs`)**
Interfaces with **Sylvia.Prover**, an equational logic theorem prover.
*   **Key Functions**:
    *   `ListTheories` / `ListRules`: Allows the LLM to explore available axioms.
    *   `Proof`: Attempts to construct a formal proof object given a theorem and a sequence of rule applications.
*   **Logic**: Supports Propositional and Predicate Calculus.

## 4. Integration with Gemini

Giant leverages specific features of the Gemini models to achieve reliability:

1.  **Function Calling (Tools)**:
    *   Giant does not rely on Gemini to *calculate* answers. Instead, it forces Gemini to write function calls.
    *   Example: Instead of saying "The derivative of x^2 is 2x", Gemini calls `CASPlugin.Diff("x", "x^2")`.

2.  **Strict Syntax Mandates**:
    *   The system prompt explicitly restricts the mathematical syntax (e.g., `==>` for implication, `&&&` for AND) to match F# Quotations and Sylvia's parsers, reducing parsing errors.

3.  **Iterative Reasoning**:
    *   The system allows the model "4 attempts" (heuristic mentioned in prompt) to construct a proof. If it gets stuck, it is instructed to return the partial proof and ask for human hints, effectively implementing a human-in-the-loop fallback.

## 5. Conclusion

Sylvia.GenAI.Giant represents a significant step towards **Neurosymbolic AI**. By treating the LLM not as an oracle but as a creative operator of formal tools, it mitigates the "hallucination" problem common in mathematical LLM tasks. The architecture cleanly separates the "Thinking" (Gemini) from the "Doing" (Sylvia/F#), mediated by a shared state and strict function calling protocols.
