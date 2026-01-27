namespace Sylvia;

public interface IHtmlDisplay
{
    string Html();
}

public interface IPlugin
{
    string Name { get; }

    Dictionary<string, Dictionary<string, object>> SharedState { get; set; }
}

