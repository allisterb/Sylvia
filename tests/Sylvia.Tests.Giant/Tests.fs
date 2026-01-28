namespace Sylvia.Tests.Giant

open System
open Xunit

open Sylvia.GenAI
module PluginTests =

    [<Fact>]
    let ``Can create Plugin`` () =
        let p = new CASPlugin()
        let desc = p.BoolVar "X"
        Assert.NotNull(desc)
