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
    [<Fact>]
    let ``PropConst constructs and carries its name and value``() =
        // It never did: `Expr.ValueWithName` was handed the optional parameter rather than the
        // value, so the quotation had type `bool option` and `expand_as<bool>` threw on EVERY
        // construction. Nothing in the repo referenced it, which is how that survived.
        let a = PropConst "alpha"
        Assert.Equal("alpha", a.Name)
        Assert.False a.Val
        Assert.True (PropConst("t", true)).Val

    [<Fact>]
    let ``rendering follows TransliterateGreek, and the display cache invalidates itself``() =
        // A named constant whose name is Greek is the one case where `Display.print_formula`'s
        // output depends on `Symbols.TransliterateGreek` — plain variables render their raw name.
        // That makes it the test for `Display`'s memo: it must not serve a string computed under
        // the other setting. The check lives in the renderer rather than in a setter hook because
        // `Symbols` is a module (no property setter to hook) and direct assignment would bypass one.
        let e = sexpr (PropConst "alpha" :> Prop)
        let saved = Symbols.TransliterateGreek
        try
            Symbols.TransliterateGreek <- true
            Display.clear_caches ()
            let greek = Display.print_formula e
            // NO clear_caches here — the point is that flipping the setting is enough.
            Symbols.TransliterateGreek <- false
            let plain = Display.print_formula e
            Symbols.TransliterateGreek <- true
            let back = Display.print_formula e
            Assert.Equal("\u03b1", greek)          // α
            Assert.Equal("alpha", plain)
            Assert.Equal(greek, back)              // and it comes back, so neither value got stuck
        finally
            Symbols.TransliterateGreek <- saved
            Display.clear_caches ()
