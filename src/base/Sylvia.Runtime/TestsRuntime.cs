namespace Sylvia;

using Microsoft.Extensions.Configuration;

using Serilog;
using Serilog.Extensions.Logging;

public class TestsRuntime : Runtime
{
    static TestsRuntime()
    {
        config = LoadConfigFile("testappsettings.json");
        var logger = new LoggerConfiguration()
             .Enrich.FromLogContext()
             .WriteTo.Console()
             .WriteTo.File(Path.Combine(Runtime.AssemblyLocation, "Sylvia.log"))
             .CreateLogger();
        var lf = new SerilogLoggerFactory(logger);
        var lp = new SerilogLoggerProvider(logger, false);
        Runtime.Initialize("Sylvia", "Tests", true, lf, lp);       
    }    
    static protected IConfigurationRoot config;
}

