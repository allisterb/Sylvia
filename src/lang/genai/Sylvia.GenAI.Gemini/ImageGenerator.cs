namespace Sylvia.GenAI.Gemini;

using System;
using System.Collections.Generic;
using System.Text;

using Microsoft.Extensions.Configuration;

using Google.GenAI;
using Google.GenAI.Types;
using System.Threading.Tasks;
using System.Linq;

public class ImageGenerator : Runtime
{
    #region Constructors
    public ImageGenerator(string apiKey, GenerateImagesConfig config)
    {
        client = new Client(apiKey: apiKey);
        imagesConfig = config;
    }

    public ImageGenerator(GenerateImagesConfig config) : this(
         apiKey: ImageGenerator.config?["Model:ApiKey"] ?? throw new Exception("ApiKey not present in configuration file."),
         config: config
        )
    {}

    public ImageGenerator() : this(
         config: new GenerateImagesConfig()
         { 
             NumberOfImages = 1,
             ImageSize = "1K",
             OutputMimeType = "image/png",
         })
    {}
    #endregion

    #region Methods
    public async Task<Image?> PromptAsync(string prompt, string? outputFile = null)
    {      
        GenerateImagesResponse response = await client.Models.GenerateImagesAsync(ModelIds.Imagen4, prompt, imagesConfig);
        if (response.GeneratedImages is not null && response.GeneratedImages.Count > 0)
        {
            var image = response.GeneratedImages.First().Image;
            if (image is not null && image.ImageBytes is not null)
            {
                if (outputFile is not null)
                {
                    await System.IO.File.WriteAllBytesAsync(outputFile, image.ImageBytes);
                }
                return image;
            }
            else
            {
                return null;
            }
            
        }
        else
        {
            return null;
        }
    }

    public Image? Prompt(string prompt, string? outputFile = null) => PromptAsync(prompt, outputFile).GetAwaiter().GetResult();
    
    public async Task<List<string>> GetImageModelsAsync()
    {
        var _models = await client.Models.ListAsync();
        List<string> imageModels = new List<string>();  
        await foreach (var model in _models)
        {
                        
            if (model is not null && model.Name is not null && model.SupportedActions is not null && model.SupportedActions.Contains("predict") && model.DisplayName is not null && model.DisplayName.ToLower().Contains("image"))
            {
                imageModels.Add(model.Name);
            }
            
        }
        return imageModels;
    }
    #endregion

    #region Fields
    public readonly Client client;

    public readonly GenerateImagesConfig imagesConfig;

    public static IConfigurationRoot? config = null;
    #endregion

    public record ModelIds
    {
        public const string Imagen3 = "imagen-3.0-generate-002";
        public const string NanoBanana = "nano-banana-pro-preview";
        public const string Imagen4 = "imagen-4.0-generate-001";
    }
}
