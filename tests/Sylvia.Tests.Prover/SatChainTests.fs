namespace Sylvia.Tests.Prover

open System
open System.Collections.Generic

open Xunit

open Sylvia
open Sylvia.SAT

/// Tests for `SAT.rupChain` — the unfolding of an LRAT step's unit-propagation hints into an
/// explicit chain of BINARY resolutions, which is what lets the reconstruction replay EVERY step of
/// a refutation (not just the 2-hint ones) through `PropCalculus.resolve`.
///
/// These are pure integer-level tests: the LRAT texts below are verbatim output from
/// `cadical -q --lrat --no-binary` on the accompanying DIMACS, so they exercise the shapes a real
/// solver emits — 1-hint (subsumption), 2-hint (plain resolution) and 3+-hint (a genuine
/// propagation chain) — without needing the executable.
type SatChainTests() =
    inherit Sylvia.Tests.Prover.TestsRuntime()

    /// Replay a whole LRAT proof through `rupChain`, checking every link is a genuine binary
    /// resolution and every step's chain subsumes the clause the solver declared. Returns the
    /// per-step chains, in order.
    let replay (inputs: int list list) (lrat: string) : (int * RupChain) list =
        let lits = Dictionary<int, int list>()
        inputs |> List.iteri (fun i c -> lits.[i + 1] <- c)
        let clauseOf id = match lits.TryGetValue id with | true, c -> Some c | _ -> None
        [ for step in parseLrat lrat do
            match step with
            | Delete _ -> ()
            | Add(id, cl, hints) ->
                match rupChain clauseOf cl hints with
                | Error e -> failwithf "step %d: %s" id e
                | Ok chain ->
                    // Every link must be a real binary resolution of the running clause with its
                    // antecedent: the pivot occurs with opposite signs in the two, and the result
                    // is their union minus the two pivot literals.
                    let mutable cur = Set.ofList (clauseOf chain.Start).Value
                    for link in chain.Links do
                        let ante = Set.ofList (clauseOf link.Antecedent).Value
                        let pv = link.Pivot
                        Assert.True((cur.Contains pv && ante.Contains -pv) || (cur.Contains -pv && ante.Contains pv),
                                    sprintf "step %d: %A and %A do not clash on %d" id cur ante pv)
                        let expected = Set.union (cur |> Set.filter (fun l -> abs l <> pv)) (ante |> Set.filter (fun l -> abs l <> pv))
                        Assert.True(Set.ofList link.Result = expected,
                                    sprintf "step %d: resolvent %A, expected %A" id link.Result (Set.toList expected))
                        cur <- Set.ofList link.Result
                    Assert.True(Set.isSubset (Set.ofList chain.Derived) (Set.ofList cl),
                                sprintf "step %d: chain derived %A, not subsumed by the declared %A" id chain.Derived cl)
                    lits.[id] <- cl
                    yield id, chain ]

    // ((p∨q) ∧ (¬p∨q)) ⇒ q — the smallest MERGE refutation. Step 5 has THREE hints.
    let mergeInputs = [ [1; 2]; [-1; 2]; [-2] ]
    let mergeLrat = "4 1 0 3 1 0\n5 0 4 3 2 0\n"

    // (p∨q) ∧ (¬q∨r) ∧ (¬r∨s) ∧ (¬p∨s) ∧ ¬s — the terminal step has three hints.
    let chainInputs = [ [1; 2]; [-2; 3]; [-3; 4]; [-1; 4]; [-4] ]
    let chainLrat = "6 -3 0 5 3 0\n7 -1 0 5 4 0\n8 -2 0 6 2 0\n9 0 7 8 1 0\n"

    // All eight clauses over three variables: merge resolvents throughout, plus a ONE-hint
    // subsumption step (15) and a binary terminal step.
    let allEightInputs =
        [ [1; 2; 3]; [-1; 2; 3]; [1; -2; 3]; [-1; -2; 3]
          [1; 2; -3]; [-1; 2; -3]; [1; -2; -3]; [-1; -2; -3] ]
    let allEightLrat =
        "9 1 2 0 1 5 0\n10 1 -2 0 7 3 0\n11 -1 2 0 6 2 0\n12 -1 -2 0 4 8 0\n\
         13 2 0 9 11 0\n14 -2 0 10 12 0\n15 -2 0 14 0\n16 0 15 13 0\n"

    [<Fact>]
    member _.``every LRAT step unfolds into a valid binary-resolution chain`` () =
        for (name, inputs, lrat) in [ "merge", mergeInputs, mergeLrat
                                      "chain", chainInputs, chainLrat
                                      "all-eight", allEightInputs, allEightLrat ] do
            let steps = replay inputs lrat
            Assert.True(not steps.IsEmpty, sprintf "%s: no steps replayed" name)
            // The proof must end in the empty clause, derived by the chain and not merely declared.
            let (_, last) = List.last steps
            Assert.True(List.isEmpty last.Derived, sprintf "%s: final chain derived %A, expected ⊥" name last.Derived)

    [<Fact>]
    member _.``a 2-hint step is a single-link chain (the binary case is subsumed)`` () =
        // `4 1 0 3 1 0`: resolve ¬q with (p∨q) on q, giving the unit p.
        let steps = replay mergeInputs mergeLrat
        let (_, first) = List.head steps
        Assert.Equal(1, List.length first.Links)
        Assert.Equal<int list>([1], first.Derived)

    [<Fact>]
    member _.``a 3-hint RUP step unfolds into a two-link chain`` () =
        // `5 0 4 3 2 0`: ⊥ from p, ¬q and (¬p∨q) — a propagation chain, not one resolution.
        let steps = replay mergeInputs mergeLrat
        let (_, last) = List.last steps
        Assert.Equal(2, List.length last.Links)
        Assert.True(List.isEmpty last.Derived)

    [<Fact>]
    member _.``a 1-hint step is a link-free chain that subsumes the declared clause`` () =
        // `15 -2 0 14 0`: clause 15 just restates clause 14, so there is nothing to resolve.
        let steps = replay allEightInputs allEightLrat
        let (_, chain) = steps |> List.find (fun (id, _) -> id = 15)
        Assert.Empty(chain.Links)
        Assert.Equal(14, chain.Start)
        Assert.Equal<int list>([-2], chain.Derived)

    [<Fact>]
    member _.``a merge resolvent drops the shared literal exactly once`` () =
        // `13 2 0 9 11 0`: (p∨q) resolved with (¬p∨q) on p — both contribute q.
        let steps = replay allEightInputs allEightLrat
        let (_, chain) = steps |> List.find (fun (id, _) -> id = 13)
        let derived = chain.Derived
        Assert.Equal<int list>([2], derived)
        Assert.Equal(1, derived |> List.filter ((=) 2) |> List.length)

    [<Fact>]
    member _.``malformed hints are rejected rather than silently mis-replayed`` () =
        let clauseOf id = if id = 1 then Some [1; 2] elif id = 2 then Some [-1] else None
        let isError = function Error (_: string) -> true | Ok (_: RupChain) -> false
        // an unknown antecedent
        Assert.True(isError (rupChain clauseOf [] [99]), "unknown hint should be rejected")
        // a RAT step (negative hint) has no forward reading as resolution
        Assert.True(isError (rupChain clauseOf [] [1; -2]), "RAT hint should be rejected")
        // a hint already satisfied by the falsifying assignment never conflicts, so nothing is proved
        Assert.True(isError (rupChain clauseOf [1] [2]), "non-conflicting hints should be rejected")
        // a hint that is neither unit nor falsified is not a propagation
        Assert.True(isError (rupChain clauseOf [] [1]), "a non-unit hint should be rejected")
        // a tautological conclusion has no falsifying assignment to propagate from
        Assert.True(isError (rupChain clauseOf [1; -1] [1; 2]), "tautological clause should be rejected")
