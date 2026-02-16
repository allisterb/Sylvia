namespace Sylvia;

using Microsoft.Extensions.Configuration;

using Serilog;
using Serilog.Extensions.Logging;

public class TestsRuntime : Runtime
{
    static TestsRuntime()
    {
        Runtime.WithFileAndConsoleLogging("Sylvia", "Tests", true);
        config = LoadConfigFile("testappsettings.json");        
    }    
    static protected IConfigurationRoot config;
}

