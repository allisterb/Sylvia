#r "nuget: Microsoft.Extensions.Logging.Abstractions, 10.0.0"
#r "nuget: Microsoft.Extensions.Configuration.Abstractions, 10.0.1"
#r "nuget: Microsoft.Extensions.Configuration.Json, 10.0.1"
#r "nuget: Microsoft.SemanticKernel.Abstractions, 1.70.0"
#r "nuget: Microsoft.SemanticKernel.Connectors.Google, 1.70.0-alpha"
#r "nuget: FParsec, 1.0.3"
#r "nuget: MathNet.Numerics.FSharp, 4.15.0"
#r "nuget: Microsoft.Z3, 4.12.2"
#r "nuget: FSharp.Quotations.Evaluator, 2.1.0"
#r "nuget: CsvHelper, 12.1.2"
#r "nuget: Google.GenAI, 0.11.0"
#r "nuget: Microsoft.DotNet.Interactive.Formatting, 1.0.0-beta.24568.1"
#r "nuget: Unquote, 7.0.1"
#r "nuget: sylvia.arithmetic, 0.2.8"

#r "..\\src\\lang\\core\\Sylvia.Expressions\\bin\\Debug\\net10.0\\Expect.NETStandard.dll"
#r "..\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.dll"
#r "..\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.Interop.NETStandard.dll"
#r "..\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.Util.dll"
#r "..\\ext\\FunScript\\src\\extra\\FunScript.Bindings.JSXGraph\\bin\\Debug\\netstandard2.0\\FunScript.Bindings.Base.dll"
#r "..\\ext\\FunScript\\src\\extra\\FunScript.Bindings.JSXGraph\\bin\\Debug\\netstandard2.0\\FunScript.Bindings.JSXGraph.dll"
#r "..\\ext\\mathnet-symbolics\\src\\Symbolics\\bin\\Debug\\netstandard2.0\\MathNet.Symbolics.dll"

#r "..\\src\\lang\\CAS\\Sylvia.CAS.Maxima\\bin\\Debug\\net10.0\\Sylvia.Runtime.dll"
#r "..\\src\\base\\Sylvia.Collections\\bin\\Debug\\netstandard2.0\\Sylvia.Collections.dll"
#r "..\\src\\lang\\core\\Sylvia.Expressions\\bin\\Debug\\net10.0\\Sylvia.Expressions.dll"
#r "..\\src\\lang\\core\\Sylvia.Prover\\bin\\Debug\\net10.0\\Sylvia.Prover.dll"
#r "..\\src\\lang\\CAS\\Sylvia.CAS.Maxima\\bin\\Debug\\net10.0\\MathNet.Symbolics.dll"
#r "..\\src\\lang\\CAS\\Sylvia.CAS.Maxima\\bin\\Debug\\net10.0\\Sylvia.CAS.Maxima.dll"
#r "..\\src\\lang\\solvers\\Sylvia.Solver.Z3\\bin\\Debug\\net10.0\\Sylvia.Solver.Z3.dll"
#r "..\\src\\data\\Sylvia.Data\\bin\\Debug\\netstandard2.0\\Sylvia.Data.dll"
#r "..\\src\\lang\\visualization\\Sylvia.Visualization.Html\\bin\\Debug\\net10.0\\Sylvia.Visualization.Html.dll"
#r "..\\src\\lang\\genai\\Sylvia.GenAI.Gemini\\bin\\Debug\\net10.0\\Sylvia.GenAI.Gemini.dll"
#r "..\\src\\lang\\genai\\Sylvia.GenAI.Giant\\bin\\Debug\\net10.0\\Sylvia.GenAI.Giant.dll"

#r "..\\src\\Math\\Sylvia.AbstractAlgebra\\bin\\Debug\\net10.0\\Sylvia.AbstractAlgebra.dll"
#r "..\\src\\Math\\Sylvia.LinearAlgebra\\bin\\Debug\\net10.0\\Sylvia.LinearAlgebra.dll"
#r "..\\src\\Math\\Sylvia.RealAnalysis\\bin\\Debug\\net10.0\\Sylvia.RealAnalysis.dll"
#r "..\\src\\Math\\Sylvia.Statistics\\bin\\Debug\\net10.0\\Sylvia.Statistics.dll"

#nowarn "3391"

open Google.GenAI.Types
open Microsoft.DotNet.Interactive.Formatting

open Sylvia
open Sylvia.GenAI.Gemini    

ModelConversation.config <- Runtime.LoadConfigFile("testappsettings.json")
ImageGenerator.config <- Runtime.LoadConfigFile("testappsettings.json")

Formatter.Register<Image>(
        (fun (image: Image) (writer: System.IO.TextWriter) ->
            let html =
                if image.ImageBytes <> null && image.ImageBytes.Length > 0 then
                    let base64 = System.Convert.ToBase64String(image.ImageBytes)
                    $"<img src=\"data:image/png;base64,{base64}\" />"
                elif not (System.String.IsNullOrEmpty(image.GcsUri)) then
                    $"<img src=\"{image.GcsUri}\" />"
                else
                    "<!-- No image data available -->"
            writer.Write(html)
        ),
        HtmlFormatter.MimeType
    )