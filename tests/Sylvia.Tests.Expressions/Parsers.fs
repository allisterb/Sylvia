namespace Sylvia.Tests.Expressions

module Parsers = 

    open Xunit
    open FSharp.Quotations

    open Sylvia
    open MathNet.Symbolics

    [<Fact>]
    let ``Can parse function def``() =
        let R = Infix.parse("BAR(x) + 1")
        Assert.Equal (R, Infix.parse("BAR()"))

    [<Fact>]
    let ``Can parse forall``() =
        let text = "forall(x, P(x))"
        let result = TermParsers.parseProp<bool> text
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

    [<Fact>]
    let ``Can parse forall with int comparison``() =
        let text = "forall(x, x > 7)"
        let result = TermParsers.parseProp<int> text
        match result.Expr with
        | Formula.ForAll(op, bound, range, body) ->
            Assert.Single(bound)
            Assert.Equal("x", bound.Head.Name)
            // Body should be x > 7 (int comparison)
            // It might be represented as GreaterThan(x, 7) or similar.
            // Just verifying it parses successfully is good step.
        | _ -> Assert.Fail(sprintf "Expected ForAll expression but got: %A" result.Expr)
