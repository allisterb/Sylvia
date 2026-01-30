namespace Sylvia.Tests.Giant

open Sylvia
open Sylvia.GenAI.Gemini

type TestsRuntime() =
    inherit Sylvia.TestsRuntime()
    do
        ModelConversation.config <- Sylvia.TestsRuntime.config;
