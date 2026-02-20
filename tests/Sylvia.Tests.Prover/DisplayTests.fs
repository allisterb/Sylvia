namespace Sylvia.Tests.Prover

open Xunit

open Sylvia

type DisplayTests() =
    inherit Sylvia.Tests.Prover.TestsRuntime()

    let p,q,r = boolvar3 "p" "q" "r"

    [<Fact>]
    let ``Can display and``() =
        let d = p * (q + r) ==> r |> sexpr |> Display.print_formula 
        Assert.NotNull d