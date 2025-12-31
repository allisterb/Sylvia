namespace Sylvia.Tests.CAS

module Algebra = 

    open System
    open Xunit

    open Sylvia
    open Sylvia.CAS

    do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
    
    [<Fact>]
    let ``Can start maxima process`` () =
        let m = Maxima.start "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
        Assert.True m.Initialized
        let g = 
            match Maxima.send m "partfrac ( 1/(x^2*(x^2 + 1)), x);" with
            | Ok r -> true
            | Error _ -> false
        Assert.True g

    [<ReflectedDefinition>]
    let fo x = x ** 2. + 2. * x

    [<Fact>]
    let ``Can get part frac``() =
        let a = intvar "a"
        let b = intvar "b"
        let f = Algebra.partfrac_of a.Expr <@ (2 * %a.Expr)/ (%a.Expr + 3) @>
        Assert.NotNull f
        let f' = Algebra.partfrac_of b.Expr <@ (1 + 2)/ %b.Expr @>
        Assert.NotNull f'
        let c = realvar "c"
        let f'' = Algebra.partfrac_of c.Expr <@ (2.5 + 1.) / %c.Expr @>
        Assert.NotNull f''

        let x = realvar "x"
        let a = realvar "a"
        
        let f''' = Analysis.limit a.Expr <@ 0. @> <@ (fo(%x.Expr + %a.Expr) - fo %x.Expr) / %a.Expr @>
        Assert.NotNull f'''


    [<Fact>]
    let ``Can divide``() =
        let gg = rat 0.01
        Assert.True ((1 / 5N) = 0Q)

    [<Fact>]
    let ``Can solve``() =
        let x, y = realvar2 "x" "y"
        let soln:Quotations.Expr<bool> list = Algebra.solve_for {||} [y.Expr] [ (3. * x + 5. * y == 120.).Expr ]
        let xx = MathNet.Symbolics.Infix.parse "(3*x-120)/5"
        
        let r = xx |> function | Ok e -> Some <| MathNetExpr.toQuotation<int> (get_vars x.Expr) e | _ -> None
        Assert.True <| Option.isSome r