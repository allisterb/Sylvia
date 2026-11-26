/// The Sylvia Jupyter kernel.
///
/// A Jupyter frontend (JupyterLab, Notebook, VS Code) starts this process with the path of a
/// connection file; the process serves the Jupyter messaging protocol on the five ZeroMQ sockets
/// that file names, and runs each cell in an F# Interactive session.
///
/// The pieces are .NET Interactive's (our fork, ext/interactive): `FSharpKernel` runs the code,
/// and `Shell` + `Heartbeat` from Microsoft.DotNet.Interactive.Jupyter speak the protocol. This
/// file is the composition that Microsoft's `dotnet interactive jupyter` command performed
/// (`CommandLineParser.Jupyter` + `KernelBuilder.CreateKernel` in src/dotnet-interactive), cut
/// down to the F# kernel: no C#/PowerShell/HTTP subkernels, no ASP.NET web host, no telemetry.
module Sylvia.Interactive.Jupyter.Program

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Threading.Tasks

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

open Microsoft.DotNet.Interactive
open Microsoft.DotNet.Interactive.Formatting
open Microsoft.DotNet.Interactive.FSharp
open Microsoft.DotNet.Interactive.Jupyter
open Microsoft.DotNet.Interactive.PackageManagement

/// `#r "nuget: …"` for the F# kernel: the shared PackageManagement directive restores the
/// packages, and the resolved assemblies and package roots are handed to F# Interactive as `#r`
/// and `#I`. This is the wrapper dotnet-interactive's app defines for F#
/// (`KernelExtensions.UseNugetDirective(FSharpKernel)`), which lives in a project we don't build.
let private useNuget (kernel: FSharpKernel) : FSharpKernel =
    let onResolved =
        Func<FSharpKernel, IReadOnlyList<ResolvedPackageReference>, Task>(fun k resolved ->
            k.AddAssemblyReferencesAndPackageRoots(
                resolved |> Seq.collect (fun r -> r.AssemblyPaths),
                resolved |> Seq.map (fun r -> r.PackageRoot))
            |> ignore
            Task.CompletedTask)
    kernel.UseNugetDirective(onResolved, false)

/// F# Interactive options for every notebook session. A `#nowarn` in a cell only covers that
/// cell (each cell is its own submission), so session-wide settings belong here.
let fsiOptions = [
    // FS3391: implicit conversion at a method argument. Sylvia's bare-rule proof style
    // (`ident ... [ rule |> at ...; rule ]`) converts `Rule` to `RuleApplication` this way.
    "--nowarn:3391"
]

/// The kernel tree: one F# subkernel, set up as dotnet-interactive sets up its own, inside a
/// composite that carries the notebook magics (`#!lsmagic`, `#!markdown`, `#!time`, `#!import`).
let createKernel () : CompositeKernel =
    let fsharp = (new FSharpKernel(fsiOptions)).UseDefaultFormatting() |> useNuget
    let fsharp = fsharp.UseKernelHelpers().UseWho().UseValueSharing()

    let kernel = new CompositeKernel()
    kernel.Add(fsharp, [ "f#"; "F#" ])
    kernel.UseDefaultMagicCommands().UseImportMagicCommand() |> ignore
    kernel.DefaultKernelName <- fsharp.Name

    // As dotnet-interactive does for a browser-based frontend: values render as HTML by default,
    // but a string is shown as plain text rather than as an HTML fragment.
    Formatter.DefaultMimeType <- HtmlFormatter.MimeType
    Formatter.SetPreferredMimeTypesFor(typeof<string>, PlainTextFormatter.MimeType)
    kernel

/// Serve the Jupyter protocol for `kernel` on the sockets named in `connectionFile`, until the
/// frontend sends `shutdown_request` (Shell then stops the host).
let run (kernel: Kernel) (connectionFile: FileInfo) =
    use host =
        Host.CreateDefaultBuilder()
            // The generic host logs to the console by default; a kernel's stdout is the
            // frontend's log, so keep it to what the kernel itself writes.
            .ConfigureLogging(fun logging -> logging.ClearProviders() |> ignore)
            .ConfigureServices(fun services ->
                services
                    .AddSingleton<Kernel>(kernel)
                    .AddSingleton(ConnectionInformation.Load connectionFile)
                    .AddSingleton(new JupyterRequestContextHandler(kernel))
                    .AddSingleton<JupyterRequestContextScheduler>(fun (sp: IServiceProvider) ->
                        JupyterRequestContextScheduler(fun delivery ->
                            sp.GetRequiredService<JupyterRequestContextHandler>().Handle delivery))
                    .AddSingleton<IHostedService, Shell>()
                    .AddSingleton<IHostedService, Heartbeat>()
                |> ignore)
            .Build()
    host.Run()

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Encoding.UTF8
    match argv with
    | [| path |] when File.Exists path ->
        use kernel = createKernel ()
        run kernel (FileInfo path)
        0
    | _ ->
        eprintfn "usage: Sylvia.Interactive.Jupyter <connection-file>"
        eprintfn "  Started by a Jupyter frontend; register it with install-kernel.ps1."
        2
