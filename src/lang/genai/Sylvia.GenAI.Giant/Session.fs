namespace Sylvia.GenAI.Giant

open Sylvia.GenAI.Gemini


type Session() =
    inherit ModelConversation(ModelIds.Gemma3, systemPrompts=Session.SystemPrompts)

    static member SystemPrompts = [|"gg"; "ll"|]




