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
             NumberOfImages = 1
             
         })
    {}
    #endregion

    #region Methods
    public async Task<byte[]> Prompt(string prompt, string? outputFile = null)
    {      
        GenerateImagesResponse response = await client.Models.GenerateImagesAsync("ll", prompt, imagesConfig);
        //response.GeneratedImages.First().Image.
        //return response;
    }
    #endregion

    #region Fields
    public readonly Client client;

    public readonly GenerateImagesConfig imagesConfig;

    public readonly static IConfigurationRoot? config = null;
    #endregion
}
