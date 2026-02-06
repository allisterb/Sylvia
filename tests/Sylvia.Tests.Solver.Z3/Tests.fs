namespace Sylvia.Tests.Solver

open Xunit

open Sylvia 
open Z3
open TermParsers

module Z3Tests = 
    
    [<Fact>]
    let ``Can create arithmetic expr``() =
        let x = intvar "x"
        let x' = realvar "xx"
        let y' = realvar "y"
        let s = new Z3Solver()
        
        let e  =  create_arith_expr s ((x + 2).Expr )
        Assert.NotNull e

    [<Fact>]
    let ``Can parse arithmetic bool expr``() =
        let s = new Z3Solver()
        let e = "x + 2 > y - 3" |> parse_bool_expr s      
        Assert.True e.IsOk