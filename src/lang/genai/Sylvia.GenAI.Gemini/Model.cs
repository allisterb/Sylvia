namespace Sylvia.GenAI.Gemini;

using Google.GenAI;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.VectorData;
using Microsoft.KernelMemory;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.ChatCompletion;
using Microsoft.SemanticKernel.Connectors.Google;
using Microsoft.SemanticKernel.Connectors.InMemory;
using Microsoft.SemanticKernel.Embeddings;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;

public class Model : Runtime
{
    public Model(string modelId, string? embeddingModelId = null, string[]? systemPrompts = null, params IPlugin[]? plugins) : base()
    {
        this.modelId = modelId;
        this.embeddingModelId = embeddingModelId;
        IKernelBuilder builder = Kernel.CreateBuilder();
        builder.Services.AddLogging(builder =>
            builder
                .SetMinimumLevel(LogLevel.Trace)
                .AddProvider(loggerProvider)
            );
        var apiKey = config?["Model:ApiKey"] ?? throw new Exception();
        chat = new GoogleAIGeminiChatCompletionService(this.modelId, apiKey, loggerFactory: loggerFactory);
        chatClient = chat.AsChatClient();
        // Source - https://stackoverflow.com/a
        // Posted by Suriya, modified by community. See post 'Timeline' for change history
                 // Retrieved 2026-01-27, License - CC BY-SA 3.0
        client   = (Client) typeof(GoogleAIGeminiChatCompletionService).GetField("client", BindingFlags.NonPublic | BindingFlags.Instance)?.GetValue(chatClient) ?? throw new Exception();

        if (this.embeddingModelId is not null)
        {
            builder.AddGoogleAIEmbeddingGenerator(this.embeddingModelId, apiKey);
        }
        promptExecutionSettings = new GeminiPromptExecutionSettings()
        {
            ModelId = this.modelId,
            FunctionChoiceBehavior = FunctionChoiceBehavior.Auto(autoInvoke: true),
            ToolCallBehavior = GeminiToolCallBehavior.AutoInvokeKernelFunctions,
        };
        Info("Using Google Gemini model {0}.", this.modelId);
        builder.Services
            .AddChatClient(chatClient)
            .UseFunctionInvocation(loggerFactory)
            .UseKernelFunctionInvocation(loggerFactory);
        kernel = builder.Build();

        if (systemPrompts is not null)
        {
            foreach (var systemPrompt in systemPrompts)
            {
                messages.AddSystemMessage(systemPrompt);
            }
        }

        if (plugins is not null)
        {
            foreach (var plugin in plugins)
            {
                kernel.Plugins.AddFromObject(plugin, plugin.Name);
            }
        }
    }

    public readonly string modelId;

    public readonly string? embeddingModelId;

    public readonly Kernel kernel = new Kernel();

    public readonly Client client;

    public readonly IChatClient chatClient;

    public readonly IChatCompletionService chat;

    public readonly ChatHistory messages = new ChatHistory();

    public readonly GeminiPromptExecutionSettings promptExecutionSettings;

    public static IConfigurationRoot? config = null;
}
