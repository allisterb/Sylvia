namespace Sylvia.GenAI.Gemini;

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.ChatCompletion;
using Microsoft.SemanticKernel.Connectors.Google;

public record ModelIds
{
    public const string Gemma3 = "gemini-3-pro-preview";
}

public class ModelConversation : Runtime
{
    public ModelConversation(string modelId, string? embeddingModelId = null, string[]? systemPrompts = null, params IPlugin[]? plugins) : base()
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

    #region Methods

    public ModelConversation AddPlugin<T>(string pluginName)
    {
        kernel.Plugins.AddFromType<T>(pluginName);
        return this;
    }

    public ModelConversation AddPlugin<T>(T obj, string pluginName)
    {
#pragma warning disable SKEXP0120 // Type is for evaluation purposes only and is subject to change or removal in future updates. Suppress this diagnostic to proceed.
        kernel.Plugins.AddFromObject<T>(obj, jsonSerializerOptions: new System.Text.Json.JsonSerializerOptions(), pluginName: pluginName);
#pragma warning restore SKEXP0120 // Type is for evaluation purposes only and is subject to change or removal in future updates. Suppress this diagnostic to proceed.
        return this;
    }

    public async IAsyncEnumerable<StreamingChatMessageContent> PromptAsync(string prompt, params object[] args)
    {
        var messageItems = new ChatMessageContentItemCollection()
        {
            new Microsoft.SemanticKernel.TextContent(string.Format(prompt, args))
        };
        messages.AddUserMessage(messageItems);
        StringBuilder sb = new StringBuilder();
        await foreach (var m in chat.GetStreamingChatMessageContentsAsync(messages, promptExecutionSettings, kernel))
        {
            if (m.Content is not null && !string.IsNullOrEmpty(m.Content))
            {
                sb.Append(m.Content);
                yield return m;
            }
        }
        messages.AddAssistantMessage(sb.ToString());
    }

    public async IAsyncEnumerable<StreamingChatMessageContent> PromptAsync(string prompt, byte[]? imageData, string imageMimeType = "image/png")
    {
        messages.AddUserMessage([
            new Microsoft.SemanticKernel.TextContent(prompt),
            new ImageContent(imageData, imageMimeType),

        ]);
        StringBuilder sb = new StringBuilder();
        await foreach (var m in chat.GetStreamingChatMessageContentsAsync(messages, promptExecutionSettings, kernel))
        {
            if (m.Content is not null && !string.IsNullOrEmpty(m.Content))
            {
                sb.Append(m.Content);
                yield return m;
            }
        }
        messages.AddAssistantMessage(sb.ToString());
    }

    public async Task<List<ChatMessageContent>> Prompt(string prompt, params object[] args)
    {
        var messageItems = new ChatMessageContentItemCollection()
        {
            new Microsoft.SemanticKernel.TextContent(string.Format(prompt, args))
        };
        messages.AddUserMessage(messageItems);
        List<ChatMessageContent> response = new List<ChatMessageContent>();
                
        foreach(var m in await chat.GetChatMessageContentsAsync(messages, promptExecutionSettings, kernel))
        {
            response.Add(m);
            messages.Add(m);
        }
        return response;
    }

    #endregion

    #region Fields
    public readonly string modelId;

    public readonly string? embeddingModelId;

    public readonly Kernel kernel = new Kernel();

    //public readonly Client client;

    public readonly IChatClient chatClient;

    public readonly IChatCompletionService chat;

    public readonly ChatHistory messages = new ChatHistory();

    public readonly GeminiPromptExecutionSettings promptExecutionSettings;

    public static IConfigurationRoot? config = null;
    #endregion
}
