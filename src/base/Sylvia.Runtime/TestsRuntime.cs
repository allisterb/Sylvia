namespace Sylvia;

using Microsoft.Extensions.Configuration;

using Serilog;
using Serilog.Extensions.Logging;

public class TestsRuntime : Runtime
{
    static TestsRuntime()
    {
        config = LoadConfigFile("testappsettings.json");
        Runtime.WithFileAndConsoleLogging("Sylvia", "Tests", true);
          
    }    
    static protected IConfigurationRoot config;
}

