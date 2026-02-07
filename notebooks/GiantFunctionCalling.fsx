#r "C:\\Users\\Allister\\.nuget\\packages\\microsoft.extensions.logging.abstractions\\10.0.1\\lib\\net10.0\\Microsoft.Extensions.Logging.Abstractions.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\microsoft.extensions.configuration.json\\10.0.1\\lib\\net10.0\\Microsoft.Extensions.Configuration.Json.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\fparsec\\1.0.3\\lib\\netstandard1.6\FParsec.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\fparsec\\1.0.3\\lib\\netstandard1.6\FParsecCS.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\mathnet.numerics\\4.15.0\\lib\\netstandard2.0\MathNet.Numerics.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\mathnet.numerics.fsharp\\4.15.0\\lib\\netstandard2.0\\MathNet.Numerics.FSharp.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\microsoft.z3\\4.12.2\\lib\\netstandard2.0\\Microsoft.Z3.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\fsharp.quotations.evaluator\\2.1.0\\lib\\netstandard2.0\\FSharp.Quotations.Evaluator.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\csvhelper\\12.1.2\\lib\\netstandard2.0\\CsvHelper.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\google.genai\0.11.0\\lib\\netstandard2.0\\Google.GenAI.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\microsoft.dotnet.interactive.formatting\\1.0.0-beta.24568.1\\lib\\netstandard2.0\\Microsoft.DotNet.Interactive.Formatting.dll"

#r "C:\\Users\\Allister\\.nuget\\packages\\unquote\\7.0.1\\lib\\netstandard2.0\\Unquote.dll"
#r "C:\\Projects\\Sylvia\\src\\lang\\core\\Sylvia.Expressions\\bin\\Debug\\net10.0\\Expect.NETStandard.dll"
#r "C:\\Projects\\Sylvia\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.dll"
#r "C:\\Projects\\Sylvia\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.Interop.NETStandard.dll"
#r "C:\\Projects\\Sylvia\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.Util.dll"
#r "C:\\Projects\\Sylvia\\ext\\FunScript\\src\\extra\\FunScript.Bindings.JSXGraph\\bin\\Debug\\netstandard2.0\\FunScript.Bindings.Base.dll"
#r "C:\\Projects\\Sylvia\\ext\\FunScript\\src\\extra\\FunScript.Bindings.JSXGraph\\bin\\Debug\\netstandard2.0\\FunScript.Bindings.JSXGraph.dll"
#r "C:\\Projects\\Sylvia\\ext\\mathnet-symbolics\\src\\Symbolics\\bin\\Debug\\netstandard2.0\\MathNet.Symbolics.dll"

#r "C:\\Projects\\Sylvia\\src\\lang\\CAS\\Sylvia.CAS.Maxima\\bin\\Debug\\net10.0\\Sylvia.Runtime.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\sylvia.arithmetic\\0.2.8\\lib\\netstandard2.0\\Sylvia.Provider.Arithmetic.Runtime.dll"
#r "C:\\Projects\\Sylvia\\src\\base\\Sylvia.Collections\\bin\\Debug\\netstandard2.0\\Sylvia.Collections.dll"
#r "C:\\Projects\\Sylvia\\src\\lang\\core\\Sylvia.Expressions\\bin\\Debug\\net10.0\\Sylvia.Expressions.dll"
#r "C:\\Projects\\Sylvia\\src\\lang\\core\\Sylvia.Prover\\bin\\Debug\\net10.0\\Sylvia.Prover.dll"
#r "C:\\Projects\\Sylvia\\src\\lang\\CAS\\Sylvia.CAS.Maxima\\bin\\Debug\\net10.0\\MathNet.Symbolics.dll"
#r "C:\\Projects\\Sylvia\\src\\lang\\CAS\\Sylvia.CAS.Maxima\\bin\\Debug\\net10.0\\Sylvia.CAS.Maxima.dll"
#r "C:\\Projects\\Sylvia\\src\\lang\\solvers\\Sylvia.Solver.Z3\\bin\\Debug\\net10.0\\Sylvia.Solver.Z3.dll"
#r "C:\\Projects\\Sylvia\\src\\data\\Sylvia.Data\\bin\\Debug\\netstandard2.0\\Sylvia.Data.dll"
#r "C:\\Projects\\Sylvia\\src\\lang\\visualization\\Sylvia.Visualization.Html\\bin\\Debug\\net10.0\\Sylvia.Visualization.Html.dll"


#r "C:\\Projects\\Sylvia\\src\\Math\\Sylvia.AbstractAlgebra\\bin\\Debug\\net10.0\\Sylvia.AbstractAlgebra.dll"
#r "C:\\Projects\\Sylvia\\src\\Math\\Sylvia.LinearAlgebra\\bin\\Debug\\net10.0\\Sylvia.LinearAlgebra.dll"
#r "C:\\Projects\\Sylvia\\src\\Math\\Sylvia.RealAnalysis\\bin\\Debug\\net10.0\\Sylvia.RealAnalysis.dll"
#r "C:\\Projects\\Sylvia\\src\\Math\\Sylvia.Statistics\\bin\\Debug\\net10.0\\Sylvia.Statistics.dll"

#nowarn "3391"

namespace Sylvia.GenAI.Giant

open Sylvia

module Examples =

    let check_bool_sat (constraints: string list) =
        let s = new Z3Solver()
        Sylvia.Z3.check_bool_sat s constraints  

    let check_int_sat (constraints: string list) =
        let s = new Z3Solver()
        Sylvia.Z3.check_int_sat s constraints  

    let check_real_sat (constraints: string list) =
        let s = new Z3Solver()
        Sylvia.Z3.check_real_sat s constraints  