namespace Sylvia.GenAI.Gemini;

using System;
using System.Collections.Generic;
using System.Text;
using Google.GenAI;
using Google.GenAI.Types;
using Microsoft.Extensions.Configuration;

public class ImageGenerator : Runtime
{
    public ImageGenerator(string apiKey, GenerateImagesConfig config)
    {
        client = new Client(apiKey: apiKey);
        imagesConfig = config;
    }

    #region Fields
    public readonly Client client;

    public readonly GenerateImagesConfig imagesConfig;

    public readonly static IConfigurationRoot? config = null;
    #endregion
}
