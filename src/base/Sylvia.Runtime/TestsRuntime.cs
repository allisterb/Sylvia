namespace Sylvia;

using Microsoft.Extensions.Configuration;   

public class TestsRuntime : Runtime
{
    static TestsRuntime()
    {
        Initialize("Sylvia", "Tests", true);
        config = LoadConfigFile("testappsettings.json");
    }    
    static protected IConfigurationRoot config;
}

