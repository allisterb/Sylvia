namespace Sylvia.Tests.Expressions

module FsExpr =

    open Xunit

    open Sylvia
    open FSharp.Quotations

    [<Fact>]
    let ``Can get vars``() =
        let f = <@fun x -> (2.*x**3. + 1.) @>
        let v = get_vars <| body' f
        Assert.NotEmpty v

    (* Corpus for sequal tests: pairs built independently so they are structurally
       equal but never reference-equal. *)

    let private p, q, r = boolvar3 "p" "q" "r"
    let private pnot (x:Prop) : Prop = Prop <@ not %x.Expr @>

    let private mkSmall () = (p * q).Expr.Raw
    let private mkMedium () = ((p * q ==> r) == (pnot p + pnot q + r)).Expr.Raw
    let private mkClause i =
        let pi, qi, ri = boolvar3 (sprintf "p%d" i) (sprintf "q%d" i) (sprintf "r%d" i)
        pi + qi + pnot ri
    let private mkLarge n = ([1..n] |> List.map mkClause |> List.reduce (*)).Expr.Raw

    let private xvar = Expr.Var(Var("x", typeof<double>)) |> Expr.Cast<double>
    let private mkArith () = (<@ 2. * %xvar ** 3. + 1. @>).Raw

    [<Fact>]
    let ``sequal: reference-equal expression is equal``() =
        let e = mkMedium ()
        Assert.True(sequal e e)

    [<Fact>]
    let ``sequal: independently built equal expressions are equal``() =
        Assert.True(sequal (mkSmall ()) (mkSmall ()))
        Assert.True(sequal (mkMedium ()) (mkMedium ()))
        Assert.True(sequal (mkLarge 8) (mkLarge 8))
        Assert.True(sequal (mkArith ()) (mkArith ()))

    [<Fact>]
    let ``sequal: different expressions are not equal``() =
        Assert.False(sequal (mkSmall ()) (mkMedium ()))
        Assert.False(sequal ((p * q).Expr.Raw) ((p * r).Expr.Raw))
        Assert.False(sequal ((p * q).Expr.Raw) ((p + q).Expr.Raw))
        // differs only in the last clause
        let large' = (([1..7] |> List.map mkClause) @ [mkClause 9] |> List.reduce (*)).Expr.Raw
        Assert.False(sequal (mkLarge 8) large')

    [<Fact>]
    let ``sequal: named values compare by name and payload``() =
        // Historical (string-rendering) semantics: the rendered form of a named value
        // includes both the payload and the name, so both participate in equality.
        let namedTrue = Expr.ValueWithName(true, "c")
        let namedTrue2 = Expr.ValueWithName(true, "c")
        let namedFalse = Expr.ValueWithName(false, "c")
        let namedOther = Expr.ValueWithName(true, "d")
        Assert.True(sequal namedTrue namedTrue2)
        Assert.False(sequal namedTrue namedFalse)
        Assert.False(sequal namedTrue namedOther)
        // named vs unnamed value never equal
        Assert.False(sequal namedTrue (Expr.Value true))
        // T/F truth constants
        Assert.True(sequal T.Expr.Raw (Expr.ValueWithName(true, "True")))
        Assert.False(sequal T.Expr.Raw F.Expr.Raw)

    [<Fact>]
    let ``sequal: plain values compare by value``() =
        Assert.True(sequal (Expr.Value 42) (Expr.Value 42))
        Assert.False(sequal (Expr.Value 42) (Expr.Value 43))
        Assert.False(sequal (Expr.Value 42) (Expr.Value 42L))

    [<Fact>]
    let ``sequal: lambdas compare binders by name``() =
        Assert.True(sequal (<@ fun (x:int) -> x + 1 @>).Raw (<@ fun (x:int) -> x + 1 @>).Raw)
        Assert.False(sequal (<@ fun (x:int) -> x + 1 @>).Raw (<@ fun (y:int) -> y + 1 @>).Raw)

    [<Fact>]
    let ``sequal agrees with string-based sequal_str over the corpus``() =
        let corpus : Expr list = [
            mkSmall (); mkSmall (); mkMedium (); mkMedium (); mkLarge 4; mkLarge 4
            mkArith (); (p * q ==> r).Expr.Raw; (pnot p).Expr.Raw; T.Expr.Raw; F.Expr.Raw
            Expr.Value 42; Expr.Value 43; Expr.ValueWithName(true, "c")
            (<@ fun (x:int) -> x + 1 @>).Raw ]
        for l in corpus do
            for r in corpus do
                Assert.Equal(sequal_str l r, sequal l r)

    [<Fact>]
    let ``sequal_check dual-run flag validates without failing on corpus``() =
        let saved = sequal_check
        try
            sequal_check <- true
            Assert.True(sequal (mkMedium ()) (mkMedium ()))
            Assert.False(sequal (mkMedium ()) (mkSmall ()))
        finally
            sequal_check <- saved

    (* Safety net for the traversal/rewriting functions ahead of the allocation work. *)

    [<Fact>]
    let ``get_vars: returns distinct vars including binders``() =
        let e = (<@ %xvar * %xvar + 3. @>).Raw
        let vs = get_vars e
        Assert.Equal(1, vs.Length)
        Assert.Equal("x", vs.Head.Name)
        let lam = (<@ fun (y:double) -> y + %xvar @>).Raw
        let vs2 = get_vars lam |> List.map (fun v -> v.Name) |> List.sort
        Assert.Equal<string list>(["x"; "y"], vs2)

    [<Fact>]
    let ``replace_expr: replaces all matching subterms``() =
        let target = (p * q + p * q).Expr.Raw
        let result = replace_expr ((p * q).Expr.Raw) ((p * r).Expr.Raw) target
        Assert.True(sequal result ((p * r + p * r).Expr.Raw))

    [<Fact>]
    let ``replace_first_expr: replaces only the first matching subterm``() =
        let target = (p * q + p * q).Expr.Raw
        let result = replace_first_expr ((p * q).Expr.Raw) ((p * r).Expr.Raw) target
        Assert.True(sequal result ((p * r + p * q).Expr.Raw))

    [<Fact>]
    let ``replace_var_expr: replaces variable occurrences``() =
        let v = Var("p", typeof<bool>)
        let result = replace_var_expr v (r.Expr.Raw) ((p * q).Expr.Raw)
        Assert.True(sequal result ((Prop <@ %r.Expr && %q.Expr @>).Expr.Raw))

    [<Fact>]
    let ``is_inst_expr: recognizes instantiation``() =
        let x = Var("x", typeof<bool>)
        let xe = Expr.Var x |> Expr.Cast<bool>
        let l = (<@ %xe && %q.Expr @>).Raw
        let r' = (<@ (%p.Expr || %r.Expr) && %q.Expr @>).Raw
        Assert.True(is_inst_expr x l r')
        // inconsistent instantiation across two occurrences must be rejected
        let l2 = (<@ %xe && %xe @>).Raw
        let r2 = (<@ %p.Expr && %q.Expr @>).Raw
        Assert.False(is_inst_expr x l2 r2)
