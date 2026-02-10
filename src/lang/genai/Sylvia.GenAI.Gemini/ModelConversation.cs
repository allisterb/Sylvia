namespace Sylvia.GenAI.Gemini;

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;
using Google.GenAI.Types;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.ChatCompletion;
using Microsoft.SemanticKernel.Connectors.Google;

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
        chat = new GoogleAIGeminiChatCompletionService(this.modelId, apiKey, loggerFactory: loggerFactory)
            .UsingChatHistoryReducer(new ChatHistoryTruncationReducer(16, 24));            
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
                this.plugins.Add(plugin);
            }
        }
    }

    #region Properties
    public IReadOnlyList<IPlugin> Plugins => plugins;
    #endregion

    #region Methods

    public ModelConversation AddPlugin<T>(string pluginName)
    {
        kernel.Plugins.AddFromType<T>(pluginName);
        return this;
    }

    public ModelConversation AddPlugin<T>(T obj, string pluginName)
    {
        kernel.Plugins.AddFromObject<T>(obj, jsonSerializerOptions: new System.Text.Json.JsonSerializerOptions(), pluginName: pluginName);
        return this;
    }

    public async IAsyncEnumerable<StreamingChatMessageContent> StreamingPromptAsync(string prompt)
    {
        var messageItems = new ChatMessageContentItemCollection()
        {
            new Microsoft.SemanticKernel.TextContent(prompt)
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

    public async Task<List<ChatMessageContent>> PromptAsync(string prompt, params object[] content)
    {
        var messageItems = new ChatMessageContentItemCollection()
        {
            new Microsoft.SemanticKernel.TextContent(prompt)
        };
        if (content is not null)
        {
            foreach (var item in content)
            {
                if (item is string s)
                {
                    messageItems.Add(new Microsoft.SemanticKernel.TextContent(s));
                }
                else if (item is byte[] b)
                {
                    messageItems.Add(new ImageContent(b, "image/png"));
                }
                else if (item is Image image)
                {
                    if (image.ImageBytes is not null)
                    {
                        messageItems.Add(new ImageContent(image.ImageBytes, image.MimeType ?? "image/png"));
                    }
                    else if (image.GcsUri is not null)
                    {
#pragma warning disable SYSLIB0014 // Type or member is obsolete
                        var wc = new System.Net.WebClient();
#pragma warning restore SYSLIB0014 // Type or member is obsolete
                        var data = wc.DownloadData(image.GcsUri);
                        {
                            messageItems.Add(new ImageContent(image.ImageBytes, image.MimeType ?? "image/png"));
                        }
                    }
                    else
                    {
                        throw new ArgumentException("Image content must have either ImageBytes or GcsUri.");
                    }
                }
                else
                {
                    throw new ArgumentException($"Unsupported content type {item.GetType()}");
                }
            }
        }
        messages.AddUserMessage(messageItems);
        List<ChatMessageContent> response = new List<ChatMessageContent>();      
        foreach(var m in await chat.GetChatMessageContentsAsync(messages, promptExecutionSettings, kernel))
        {
            response.Add(m);
            messages.Add(m);
        }
        return response;
    }

    public async Task<List<ChatMessageContent>> ImagePromptAsync(string prompt, byte[] imageData, string imageMimeType = "image/png") 
    {
        messages.AddUserMessage([
            new Microsoft.SemanticKernel.TextContent(prompt),
            new ImageContent(imageData, imageMimeType),
        ]);
        List<ChatMessageContent> response = new List<ChatMessageContent>();
        foreach (var m in await chat.GetChatMessageContentsAsync(messages, promptExecutionSettings, kernel))
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

    public readonly IChatClient chatClient;

    public readonly IChatCompletionService chat;

    public readonly ChatHistory messages = new ChatHistory();

    public readonly GeminiPromptExecutionSettings promptExecutionSettings;

    protected List<IPlugin> plugins = new List<IPlugin>();

    public static IConfigurationRoot? config = null;
    #endregion

    #region Types
    public record ModelIds
    {
        public const string Gemma3 = "gemini-3-pro-preview";
    }
    #endregion
}
