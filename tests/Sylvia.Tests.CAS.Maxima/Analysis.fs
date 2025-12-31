namespace Sylvia.Tests.CAS

module Analysis = 

    open System
    open Xunit

    open Sylvia
    open Sylvia.CAS

    do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

    [<Fact>]
    let ``Can differentiate``() =
        let x = realvar "x"
        let y = realvar "y"
        let a = realvar "a"
        let d0 = Analysis.diff x.Expr <@ %x.Expr ** 2.@>  
        Assert.NotNull d0
        let d1 = Analysis.diff x.Expr  <@ (%a.Expr * %x.Expr - %x.Expr ** 2. - %x.Expr ** 3.) @> 
        Assert.NotNull d1