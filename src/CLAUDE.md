## About this project
Sylvia is an F# DSL for mathematical and scientific computing that provides a common language for representing the symbolic, logical, and visual aspects of mathematics. Sylvia provides a single functional language and interface for tools like theorem provers, computer algebra systems, SMT and other solvers, and visualization libraries.
Sylvia uses the Sylvia.CAS.Maxima project at @src/lang/cas/Sylvia.CAS.Maxima as its computer algebra system and the Sylvia.Solver.Z3 project at @src/lang/solvers/Sylvia.Solver.Z3
as its SMT solver and the Sylvia.Prover project at @src/lang/core/Sylvia.Prover as its equational logic theorem prover. 

Read the PDFS in the @src/lang/core/Sylvia.Prover/docs to understand the concepts behind the equational logic prover. Read the code in @src/lang/core/Sylvia.Prover to understand the prover implementation and the core propositional and predicate logic theories.

The Sylvia.GenAI.Giant project at @src/lang/genai/Sylvia.GenAI/Giant is an implementation of an Neurosymbolic Transition System for LLMs using the F# DSL Sylvia. The objective is to provide bi-directional integration between the informal natural language thinking and understanding of LLMs with
the formal logic and methods and symbolic manipulations of tools like theorem provers and computer algebra systems and solvers. This will allow LLMs to both provide verifiable formal proofs for mathematical theorems
as well as reason correctly and provide verifiable traces of their decision-making in safety critical scenarios.

Read the PDFs in the @src/lang/genai/Sylvia.GenAI.Giant/docs folder to understand the concept of a Neurosymbolic Transition System.

Giant uses LLMs function calling capabilities to provide it with the ability to reason symbolically and iteratively construct formal proofs of statements and theorem in parallel with its informal natural language thinking 
and reasoning. 

## Using interactive tools
The main way of interactively testing proofs and theorems and other aspects of Sylvia is by creating and running F# interactive scripts (.fsx) using the F# interactive shell (fsi). 
The scripts should reference the Sylvia libraries and any other libraries they need to use. The scripts can be run from the command line using the `dotnet fsi` command.
See the example scripts in @docs\examples\math and @docs\examples\proofs for examples of how to use the Sylvia libraries and tools in scripts.

## Interactive tools
```bash
dotnet fsi .\docs\examples\proofs\PropCalculus.fsx # Run the Propositional Calculus example script
```