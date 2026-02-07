namespace Sylvia.Tests.Expressions

module Parsers = 

    open Xunit
    open FSharp.Quotations

    open Sylvia
    open MathNet.Symbolics

    [<Fact>]
    let ``Can parse function def``() =
        let R = Infix.parse("BAR(x) + 1")
        // Assert.Equal (R, Infix.parse("BAR()")) 
        // The original assertion seems wrong or placeholder, I'll leave it as is but commented out if it fails, 
        // but since I'm overwriting, I should keep original intent or fix it. 
        // The original read showed: Assert.Equal (R, Infix.parse("BAR()"))
        // I'll keep it exactly as it was.
        Assert.Equal (R, Infix.parse("BAR()"))

    [<Fact>]
    let ``Can parse forall``() =
        let text = "forall(x, P(x))"
        let result = TermParsers.parseProp<bool> text
        // Verify it is a ForAll expression
        match result.Expr with
        | Formula.ForAll(op, bound, range, body) -> 
            Assert.Single(bound)
            Assert.Equal("x", bound.Head.Name)
        | _ -> Assert.Fail(sprintf "Expected ForAll expression but got: %A" result.Expr)

    [<Fact>]
    let ``Can parse forall with range``() =
        let text = "forall(x, Q(x), P(x))"
        let result = TermParsers.parseProp<bool> text
        match result.Expr with
        | Formula.ForAll(op, bound, range, body) -> 
            Assert.Single(bound)
            Assert.Equal("x", bound.Head.Name)
            // Check range is not True
            match range with
            | Formula.True -> Assert.Fail("Range should not be True")
            | _ -> ()
        | _ -> Assert.Fail(sprintf "Expected ForAll expression but got: %A" result.Expr)

    [<Fact>]
    let ``Can parse exists``() =
        let text = "exists(y, P(y))"
        let result = TermParsers.parseProp<bool> text
        match result.Expr with
        | Formula.Exists(op, bound, range, body) -> 
            Assert.Single(bound)
            Assert.Equal("y", bound.Head.Name)
        | _ -> Assert.Fail(sprintf "Expected Exists expression but got: %A" result.Expr)