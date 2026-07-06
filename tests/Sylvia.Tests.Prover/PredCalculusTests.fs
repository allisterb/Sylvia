namespace Sylvia.Tests.Prover

open Xunit

open Sylvia
open Sylvia.PropCalculus
open Sylvia.PredCalculus

/// Smoke tests for the ported Gries ch. 8/9 predicate-calculus theorems. Each derived rule /
/// theorem self-checks its proof when constructed with concrete arguments (the `ident`/`theorem`
/// builders throw if the proof does not close), so simply instantiating each one is a full
/// verification that its derivation is still valid.
type PredCalculusTests() =
    inherit Sylvia.Tests.Prover.TestsRuntime()

    do Proof.LogLevel <- 0

    let x = intvar "x"
    let N = symbolic_pred<int> "N"
    let P = symbolic_pred<int> "P"
    let Q = symbolic_pred<int> "Q"
    let pp = boolvar "pp"   // an x-free proposition

    [<Fact>]
    member _.``universal quantification theorems (Gries 9.2-9.13) prove`` () =
        trade_forall_implies x N P |> ignore
        trade_forall_and_implies x Q N P |> ignore
        trade_forall_or_not x N pp |> ignore
        distrib_or_forall' x N pp Q |> ignore
        split_range_forall' x N P Q |> ignore
        collect_forall_and' x N P Q |> ignore
        distrib_forall_and' x N P Q |> ignore
        ident_forall_true x N |> ignore
        distrib_forall_body x N P Q |> ignore
        strengthen_forall_range_or x N P Q |> ignore
        strengthen_forall_body_and x N P Q |> ignore
        mono_forall_body x N Q P |> ignore
        forall_implies x N P |> ignore
        ident_forall_true' x |> ignore
        forall_conseq x pp |> ignore
        inst x P (Scalar<int> 3) |> ignore
        inst' x P |> ignore
        // instantiation with a negated body exercises the structural is_inst_expr fix
        inst x (-P) (Scalar<int> 3) |> ignore
        exists_intro x P (Scalar<int> 3) |> ignore

    [<Fact>]
    member _.``existential quantification theorems (Gries 9.17-9.27) prove`` () =
        ident_exists_not_forall x N P |> ignore
        ident_not_exists_forall x N P |> ignore
        ident_not_exists_forall_not x N P |> ignore
        ident_exists_not_forall_not x N P |> ignore
        collect_exists_or' x N P Q |> ignore
        distrib_exists_or' x N P Q |> ignore
        split_range_exists' x N P Q |> ignore
        trade_exists_and x N P |> ignore
        trade_exists_and_and x N P Q |> ignore
        distrib_and_exists_and x N pp Q |> ignore
        distrib_and_exists x N pp |> ignore
        ident_exists_false x N |> ignore
        weaken_exists_range x N P Q |> ignore
        weaken_exists_body x N P Q |> ignore
        mono_exists x N Q P |> ignore
        ident_exists_implies x N P pp |> ignore   // ex.9.27: (∃x|R:P)⇒Q = (∀x|R:P⇒Q)
        trade_exists_or x N pp Q |> ignore         // 9.23 (conditional): (∃x|:R) ⇒ ((∃x|R:pp∨Q) = pp∨(∃x|R:Q))

    [<Fact>]
    member _.``existential/universal interchange (Gries 9.29) proves`` () =
        let y = intvar "y"
        // a proposition depending on both dummies x and y
        let qv = FSharp.Quotations.Var("q2", typeof<int -> int -> bool>)
        let pxy = FSharp.Quotations.Expr.Application(FSharp.Quotations.Expr.Application(FSharp.Quotations.Expr.Var qv, x.Expr), y.Expr) |> expand_as<bool> |> Prop
        exists_forall_interchange x y pxy |> ignore
