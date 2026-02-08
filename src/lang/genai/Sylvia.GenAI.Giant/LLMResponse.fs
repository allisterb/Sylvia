namespace Sylvia.GenAI.Giant

open System
open System.Collections.Generic

open Microsoft.Z3

open Google.GenAI.Types

open Sylvia.CAS

open Sylvia.GenAI.Gemini

type LLMResponse = {
    Text: string
    Expr: Expr option 
    Model: Model option
    Image: Image option
}   
