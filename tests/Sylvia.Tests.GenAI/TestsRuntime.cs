namespace Sylvia.Tests.GenAI;

using Sylvia.GenAI.Gemini;

public class TestsRuntime : Sylvia.TestsRuntime
{
    static TestsRuntime()
    {
        Initialize("OnlyHumans", "Tests", true);
        ModelConversation.config = config;
        ImageGenerator.config = config; 
    }

}

