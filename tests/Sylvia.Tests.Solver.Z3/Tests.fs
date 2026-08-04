namespace Sylvia.Tests.Solver

open System

open Xunit

open Microsoft.Z3

open Sylvia
open Z3
open TermParsers

/// Tests for the `Sylvia.Solver.Z3` integration: the Sylvia-quotation → Z3-expression translation,
/// the satisfiability entry points, model read-back, the string-constraint parsers, and the optimizer.
///
/// Z3 is an EXTERNAL, UNTRUSTED tool here, exactly as CaDiCaL and E are — nothing in this assembly
/// produces a `Theorem`. So these tests are about the integration being *faithful*: that what we hand
/// Z3 means what the Sylvia expression meant, and that what comes back is read correctly. They are not
/// about soundness of any proof.
///
/// Where a model is returned, the tests check that the model SATISFIES the constraints rather than
/// asserting a particular witness. Any satisfying assignment is a legitimate answer, and pinning one
/// would make the suite hostage to Z3's search order and version.
module Z3Tests =

    /// Z3 contexts hold native resources; every test disposes its solver.
    let private withSolver (f: Z3Solver -> unit) =
        let s = new Z3Solver()
        try f s finally (s :> IDisposable).Dispose()

    (* ---------------------------------------------------------------- *)
    (* Translation: Sylvia quotation -> Z3 expression                     *)
    (* ---------------------------------------------------------------- *)

    [<Fact>]
    let ``Translates integer arithmetic`` () =
        withSolver (fun s ->
            let x = intvar "x"
            let e = create_arith_expr s ((x + 2).Expr)
            Assert.NotNull e
            Assert.True(e.IsInt, sprintf "expected an integer-sorted expression, got %O" e.Sort))

    [<Fact>]
    let ``Translates real arithmetic`` () =
        withSolver (fun s ->
            let y = realvar "y"
            let e = create_arith_expr s ((y * y).Expr)
            Assert.NotNull e
            Assert.True(e.IsReal, sprintf "expected a real-sorted expression, got %O" e.Sort))

    [<Fact>]
    let ``Translates boolean structure`` () =
        withSolver (fun s ->
            let p, q = boolvar "p", boolvar "q"
            let e = create_bool_expr s ((p * q).Expr)      // p ∧ q
            Assert.NotNull e
            Assert.True(e.IsAnd, sprintf "expected a conjunction, got %O" e))

    [<Fact>]
    let ``Translates a comparison to a boolean expression`` () =
        withSolver (fun s ->
            let x = intvar "x"
            let e = create_bool_expr s (<@ %(x.Expr) > 3 @>)
            Assert.NotNull e
            Assert.True(e.IsBool))

    [<Fact>]
    let ``Translates sorts for the base types`` () =
        withSolver (fun s ->
            Assert.Equal(Z3_sort_kind.Z3_INT_SORT, (create_sort s typeof<int>).SortKind)
            Assert.Equal(Z3_sort_kind.Z3_REAL_SORT, (create_sort s typeof<real>).SortKind)
            Assert.Equal(Z3_sort_kind.Z3_BOOL_SORT, (create_sort s typeof<bool>).SortKind))

    (* ---------------------------------------------------------------- *)
    (* Satisfiability                                                     *)
    (* ---------------------------------------------------------------- *)

    [<Fact>]
    let ``check_sat accepts a satisfiable constraint set`` () =
        withSolver (fun s ->
            let x = intvar "x"
            Assert.True(check_sat s [ <@ %(x.Expr) > 4 @>; <@ %(x.Expr) < 9 @> ]))

    [<Fact>]
    let ``check_sat rejects a contradictory constraint set`` () =
        withSolver (fun s ->
            let x = intvar "x"
            Assert.False(check_sat s [ <@ %(x.Expr) > 9 @>; <@ %(x.Expr) < 4 @> ]))

    [<Fact>]
    let ``check_sat_model returns a model exactly when satisfiable`` () =
        withSolver (fun s ->
            let x = intvar "x"
            Assert.True((check_sat_model s [ <@ %(x.Expr) = 7 @> ]).IsSome)
            reset s
            Assert.True((check_sat_model s [ <@ %(x.Expr) > 1 @>; <@ %(x.Expr) < 1 @> ]).IsNone))

    [<Fact>]
    let ``Unsatisfiability over the reals is decided, not just over the integers`` () =
        withSolver (fun s ->
            let y = realvar "y"
            Assert.False(check_sat s [ <@ %(y.Expr) * %(y.Expr) < 0.0 @> ]))

    (* ---------------------------------------------------------------- *)
    (* Model read-back                                                    *)
    (* ---------------------------------------------------------------- *)

    [<Fact>]
    let ``get_int_var_model returns an assignment that satisfies the constraints`` () =
        withSolver (fun s ->
            let x = intvar "x"
            match get_int_var_model s [ <@ %(x.Expr) > 4 @>; <@ %(x.Expr) < 9 @> ] with
            | None -> Assert.Fail "expected a model"
            | Some vars ->
                match vars |> List.tryFind (fun (n, _) -> n.ToString() = "x") with
                | None -> Assert.Fail(sprintf "no binding for x; got %A" (vars |> List.map (fst >> string)))
                | Some(_, v) -> Assert.True(v > 4 && v < 9, sprintf "x = %d is outside (4,9)" v))

    [<Fact>]
    let ``get_int_var_sol picks out a named variable`` () =
        withSolver (fun s ->
            let x = intvar "x"
            match get_int_var_sol s [ <@ %(x.Expr) = 42 @> ] x.Expr with
            | Some v -> Assert.Equal(42, v)
            | None -> Assert.Fail "expected a solution for x")

    [<Fact>]
    let ``get_bool_var_model reads a boolean assignment back`` () =
        withSolver (fun s ->
            let p = boolvar "p"
            match get_bool_var_model s [ <@ %(p.Expr) @> ] with
            | None -> Assert.Fail "expected a model"
            | Some vars ->
                match vars |> List.tryFind (fun (n, _) -> n.ToString() = "p") with
                | Some(_, v) -> Assert.True(v, "p was asserted, so it must be true in the model")
                | None -> Assert.Fail(sprintf "no binding for p; got %A" (vars |> List.map (fst >> string))))

    [<Fact>]
    let ``get_rat_var_model reads a rational assignment back`` () =
        withSolver (fun s ->
            let y = realvar "y"
            match get_rat_var_model s [ <@ %(y.Expr) > 1.0 @>; <@ %(y.Expr) < 2.0 @> ] with
            | None -> Assert.Fail "expected a model"
            | Some vars -> Assert.NotEmpty vars)

    (* ---------------------------------------------------------------- *)
    (* String constraints                                                 *)
    (* ---------------------------------------------------------------- *)

    [<Fact>]
    let ``parse_bool_expr parses a well-formed arithmetic comparison`` () =
        withSolver (fun s ->
            let e = "x + 2 > y - 3" |> parse_bool_expr<int> s
            Assert.True(e.IsOk, sprintf "%A" e))

    [<Fact>]
    let ``parse_bool_expr reports a parse failure instead of throwing`` () =
        withSolver (fun s ->
            let e = "*" |> parse_bool_expr<int> s
            Assert.True(e.IsError, "a malformed constraint must come back as Error, not an exception"))

    [<Fact>]
    let ``KNOWN DEFECT parse_bool_expr silently accepts trailing garbage`` () =
        // `TermParsers.parseBoolExpr` runs `boolExprParser` WITHOUT the `.>> eof` anchor that
        // `parseProp` (right above it in Parsers.fs) uses. So it parses a prefix and discards the
        // rest: "x +" yields `x`, and "x > 1 garbage" yields `x > 1`. That is a silent wrong answer,
        // not a loud failure — the worst kind — and it is on the path the Giant SMT plugin uses to
        // hand LLM-authored constraint strings to the solver.
        //
        // Pinned as-is rather than fixed here: the parser is shared (`parseIntExpr` and
        // `parseRealExpr` have the same omission) and tightening it would turn currently-accepted
        // inputs into errors for every caller. Flip this test when the anchor is added.
        withSolver (fun s ->
            Assert.True(("x +" |> parse_bool_expr<int> s).IsOk, "expected the prefix parse (the defect)")
            Assert.True(("x > 1 garbage" |> parse_bool_expr<int> s).IsOk, "expected the prefix parse (the defect)"))

    [<Fact>]
    let ``parse_constraints aggregates every failure`` () =
        withSolver (fun s ->
            match parse_constraints<int> s [ "x > 1"; "*" ] with
            | Ok _ -> Assert.Fail "expected an aggregated error"
            | Error msg -> Assert.Contains("Could not parse", msg))

    [<Fact>]
    let ``check_int_sat decides string constraints`` () =
        withSolver (fun s ->
            Assert.Equal(Ok Status.SATISFIABLE, check_int_sat s [ "x > 4"; "x < 9" ])
            reset s
            Assert.Equal(Ok Status.UNSATISFIABLE, check_int_sat s [ "x > 9"; "x < 4" ]))

    [<Fact>]
    let ``get_int_model reports UNSAT as an error rather than an empty model`` () =
        withSolver (fun s ->
            match get_int_model s [ "x > 9"; "x < 4" ] with
            | Error e -> Assert.Equal("UNSAT", e)
            | Ok m -> Assert.Fail(sprintf "expected UNSAT, got a model: %A" m))

    [<Fact>]
    let ``get_int_model returns bindings for a satisfiable set`` () =
        withSolver (fun s ->
            match get_int_model s [ "x > 4"; "x < 9" ] with
            | Ok m -> Assert.NotEmpty m
            | Error e -> Assert.Fail(sprintf "expected a model, got %s" e))

    (* ---------------------------------------------------------------- *)
    (* Solver state                                                       *)
    (* ---------------------------------------------------------------- *)

    [<Fact>]
    let ``Constraints are per-call ASSUMPTIONS, so successive checks do not accumulate`` () =
        // Worth pinning explicitly, because the module also exposes `push`/`pop`/`reset`, which
        // strongly suggests the opposite. `check_sat` routes to `Solver.Check(assumptions)`, so the
        // constraints are scoped to the single call and the solver's own assertion stack stays empty.
        // Two mutually contradictory checks in a row therefore BOTH succeed.
        withSolver (fun s ->
            let x = intvar "x"
            Assert.True(check_sat s [ <@ %(x.Expr) > 9 @> ])
            Assert.True(check_sat s [ <@ %(x.Expr) < 4 @> ])   // no memory of the previous call
            Assert.Equal(0u, s.Solver.NumAssertions))

    [<Fact>]
    let ``Contradictory constraints within a SINGLE call are unsatisfiable`` () =
        // The flip side of the above: assumptions are conjoined within one call, so a contradiction
        // has to be presented together to be detected.
        withSolver (fun s ->
            let x = intvar "x"
            Assert.False(check_sat s [ <@ %(x.Expr) > 9 @>; <@ %(x.Expr) < 4 @> ]))

    [<Fact>]
    let ``push, pop and reset run without disturbing the assumption-based API`` () =
        // Since `check_sat` asserts nothing, these manipulate an empty assertion stack. The test
        // guards against a future change that starts asserting without adding the scoping to match.
        withSolver (fun s ->
            let x = intvar "x"
            push s
            Assert.True(check_sat s [ <@ %(x.Expr) > 0 @> ])
            pop s
            reset s
            Assert.True(check_sat s [ <@ %(x.Expr) = 5 @> ])
            Assert.Equal(0u, s.Solver.NumAssertions))

    (* ---------------------------------------------------------------- *)
    (* Optimization                                                       *)
    (* ---------------------------------------------------------------- *)

    [<Fact>]
    let ``opt_maximize finds the maximum under hard constraints`` () =
        withSolver (fun s ->
            let x = intvar "x"
            opt_assert_hard s [ <@ %(x.Expr) >= 0 @>; <@ %(x.Expr) <= 10 @> ]
            opt_maximize s (x.Expr) |> ignore
            Assert.True(opt_check_sat s)
            match opt_get_int_var_model s with
            | Some vars ->
                match vars |> List.tryFind (fun (n, _) -> n.ToString() = "x") with
                | Some(_, v) -> Assert.Equal(10, v)
                | None -> Assert.Fail "no binding for x"
            | None -> Assert.Fail "expected an optimizer model")

    [<Fact>]
    let ``opt_minimize finds the minimum under hard constraints`` () =
        withSolver (fun s ->
            let x = intvar "x"
            opt_assert_hard s [ <@ %(x.Expr) >= 3 @>; <@ %(x.Expr) <= 10 @> ]
            opt_minimize s (x.Expr) |> ignore
            Assert.True(opt_check_sat s)
            match opt_get_int_var_model s with
            | Some vars ->
                match vars |> List.tryFind (fun (n, _) -> n.ToString() = "x") with
                | Some(_, v) -> Assert.Equal(3, v)
                | None -> Assert.Fail "no binding for x"
            | None -> Assert.Fail "expected an optimizer model")

    (* ---------------------------------------------------------------- *)
    (* Behaviour that is easy to misread                                  *)
    (* ---------------------------------------------------------------- *)

    [<Fact>]
    let ``check_unsat answers whether a COUNTERMODEL exists, not whether the goal is unsatisfiable`` () =
        // Pinning actual behaviour, which is the opposite of what the name suggests. `check_unsat e`
        // is implemented as `check_sat [¬e]`, so `true` means "¬e is satisfiable", i.e. e is NOT
        // valid. A validity check is therefore `not (check_unsat s e)`. See the note in the module.
        withSolver (fun s ->
            let x = intvar "x"
            // Valid: no countermodel exists, so this is false.
            Assert.False(check_unsat s <@ %(x.Expr) > 0 || %(x.Expr) <= 0 @>)
            reset s
            // Satisfiable but not valid: a countermodel exists, so this is true.
            Assert.True(check_unsat s <@ %(x.Expr) > 5 @>))
