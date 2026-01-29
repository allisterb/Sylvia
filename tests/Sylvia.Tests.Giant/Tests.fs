namespace Sylvia.Tests.Giant

open System
open Xunit

open Sylvia.GenAI.Giant

type PluginTests() =
    inherit TestsRuntime()

    [<Fact>]
    member this.``Can create Plugin`` () =
        let p = new CASPlugin()
        let desc = p.BoolVar "X"
        Assert.NotNull(desc)
