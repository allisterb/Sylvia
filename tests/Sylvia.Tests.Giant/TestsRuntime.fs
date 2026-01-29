namespace Sylvia.Tests.Giant

open Sylvia
open Sylvia.GenAI.Gemini

type TestsRuntime() =
    inherit Sylvia.TestsRuntime()
    do
        TestsRuntime.Initialize("OnlyHumans", "Tests", true);
        ModelConversation.config <- Sylvia.TestsRuntime.config;
