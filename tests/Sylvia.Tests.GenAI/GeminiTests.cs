namespace Sylvia.Tests.GenAI;

using System;
using System.Threading.Tasks;

using Sylvia.GenAI.Gemini;


public class GeminiTests : TestsRuntime
{
    [Fact]
    public async Task CanStartGemma3ProConversation()
    {
        var mc = new ModelConversation(ModelIds.Gemma3);
        var m = mc.PromptAsync("Hello who are you");
        await foreach (var message in m)
        {
            Console.WriteLine(message);
        }
        Assert.NotEmpty(mc.messages);

    }
}
