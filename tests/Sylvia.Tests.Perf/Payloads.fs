namespace Sylvia.Tests.Perf

open FSharp.Quotations

open Sylvia

/// Benchmark payloads for the Sylvia.Expressions optimization work (docs/expressions-perf.md).
/// Public, parameterless functions so both the Stopwatch harness (Program.fs) and the
/// BenchmarkDotNet project (tests/Sylvia.Benchmarks) can call them.
module Payloads =
    let private pnot (x: Prop) : Prop = Prop <@ not %x.Expr @>

    let p, q, r = boolvar3 "p" "q" "r"

    (* Expression corpus. Pairs are built independently so they are structurally equal
       but never reference-equal. *)

    // small: p ∧ q
    let private mkSmall () = (p * q).Expr.Raw
    let smallA = mkSmall ()
    let smallB = mkSmall ()
    let smallC = (p * r).Expr.Raw

    // medium: (p ∧ q ⇒ r) = (¬p ∨ ¬q ∨ r)
    let private mkMedium () = ((p * q ==> r) == (pnot p + pnot q + r)).Expr.Raw
    let mediumA = mkMedium ()
    let mediumB = mkMedium ()

    // large: conjunction of 64 clauses (pᵢ ∨ qᵢ ∨ ¬xᵢ) — the SAT-reconstruction shape.
    let private clause (prefix: string) i =
        let pi, qi, xi = boolvar3 (sprintf "p%d" i) (sprintf "q%d" i) (sprintf "%s%d" prefix i)
        pi + qi + pnot xi

    let private conj (props: Prop list) = (props |> List.reduce (*)).Expr.Raw

    let largeA = [1..64] |> List.map (clause "r") |> conj
    let largeB = [1..64] |> List.map (clause "r") |> conj
    // Differs from largeA only in the final clause — worst case for a structural walk.
    let largeC = ([1..63] |> List.map (clause "r")) @ [clause "z" 64] |> conj

    // A mid-position clause (occurs in largeA) and a replacement for it.
    let private clause32 = (clause "r" 32).Expr.Raw
    let private clause32' = (clause "z" 32).Expr.Raw

    (* sequal *)

    let sequal_small_eq () = sequal smallA smallB
    let sequal_small_neq () = sequal smallA smallC
    let sequal_medium_eq () = sequal mediumA mediumB
    let sequal_large_eq () = sequal largeA largeB
    let sequal_large_neq_late () = sequal largeA largeC

    (* traversal / rewriting *)

    let get_vars_large () = get_vars largeA
    let replace_expr_large () = replace_expr clause32 clause32' largeA

    (* prover macro benchmark: the profiled scenario from the last session *)

    let trans_implies_run () = PropCalculus.trans_implies p q r
