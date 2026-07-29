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


/// Tests for the `Sylvia.Prover.SAT` library (`SatProof`) — the replay layer that turns a solver's
/// LRAT refutation into a kernel-checked `Theorem`.
///
/// The clause plumbing is pure and always runs. The end-to-end `prove` tests need the `cadical`
/// executable and SKIP THEMSELVES when it is absent (reported, not silently passed), so the suite
/// never depends on an external binary being installed.
type SatProofTests(out: Xunit.Abstractions.ITestOutputHelper) =
    inherit Sylvia.Tests.Prover.TestsRuntime()

    do Proof.LogLevel <- 0

    let p, q, r, s = boolvar "p", boolvar "q", boolvar "r", boolvar "s"
    let t, u, v, w = boolvar "t", boolvar "u", boolvar "v", boolvar "w"

    /// The bundled solver, if this checkout has one: walk up from the test assembly for bin/cadical.exe,
    /// else fall back to whatever `Cadical()` resolves (SYLVIA_CADICAL / PATH).
    let solver : Cadical option =
        let rec up (d: IO.DirectoryInfo) =
            if isNull (box d) then None
            elif IO.File.Exists(IO.Path.Combine(d.FullName, "bin", "cadical.exe")) then
                Some(IO.Path.Combine(d.FullName, "bin", "cadical.exe"))
            else up d.Parent
        match up (IO.DirectoryInfo AppContext.BaseDirectory) with
        | Some exe -> Some(Cadical(exePath = exe, timeoutMs = 20000))
        | None -> let c = Cadical() in if c.IsAvailable then Some c else None

    [<Fact>]
    member _.``clausesOf reads the clause list off a CNF Prop, deduping literals`` () =
        // The clauses handed to the solver must be exactly the ones Cnf.toCnf proved ¬φ equal to,
        // with a 1-1 atom mapping — the LRAT ids and variable indices are meaningless otherwise.
        let cnfProp = (p + q) * (!!p + q + q) * !!r
        let cnf = SatProof.clausesOf (p ==> q) cnfProp
        Assert.Equal(3, cnf.NumVars)
        Assert.Equal(3, List.length cnf.Clauses)
        // literal dedup: the second clause's repeated q collapses
        Assert.Equal<int list>([ -1; 2 ], cnf.Clauses.[1])
        // every DIMACS variable maps back to a distinct Sylvia atom
        Assert.Equal(3, cnf.AtomOfVar.Count)

    [<Fact>]
    member _.``dedupCnf proves the dedup it performs, and no-ops when there is nothing to drop`` () =
        // The proof must be exactly `input == deduped`, so it composes by transitivity into ¬φ == A.
        let (d, pf) = SatProof.dedupCnf ((p + p + q) * !!r)
        match pf with
        | None -> Assert.Fail "dedupCnf should have fired on a clause with a repeated literal"
        | Some t ->
            Assert.Equal<FSharp.Quotations.Expr>(expand (((p + p + q) * !!r) == d).Expr, t.Stmt)
            Assert.True(PropCalculus.valid (((p + p + q) * !!r) == d), "dedup changed the meaning")
        let (_, none) = SatProof.dedupCnf ((p + q) * !!r)
        Assert.True(none.IsNone, "dedupCnf must not fire when no clause has a repeated literal")

    [<Fact>]
    member _.``prove returns a kernel-checked theorem OF THE GOAL`` () =
        match solver with
        // Say so rather than passing quietly — a silent skip is indistinguishable from success.
        | None -> out.WriteLine "SKIPPED (no cadical): examples/sat/Reconstruct.fsx is the end-to-end gate"
        | Some sat ->
            for goal in [ p + !!p                                       // excluded middle
                          ((p ==> q) ==> p) ==> p                       // Peirce
                          ((p + q) * (!!p + q)) ==> q                   // a merge resolvent
                          ((p ==> q) * (q ==> r)) ==> (p ==> r) ] do    // a chain
                let th = SatProof.proveWith sat goal
                // Not merely "a theorem" — the statement must BE the goal.
                Assert.True(sequal th.Stmt (expand goal.Expr),
                            sprintf "proved %s, expected %s" (src th.Stmt) (src (expand goal.Expr)))

    [<Fact>]
    member _.``a non-theorem is rejected, and distinguishably from an unavailable solver`` () =
        match solver with
        | None -> out.WriteLine "SKIPPED (no cadical): examples/sat/Reconstruct.fsx is the end-to-end gate"
        | Some sat ->
            match SatProof.tryProveWith sat (p ==> q) with
            | Ok _ -> Assert.Fail "p ⇒ q is not a theorem and must not be proved"
            | Error e ->
                Assert.Contains("NOT a theorem", e)
                Assert.DoesNotContain("not found", e)
            // An unreachable solver must say so rather than claim the goal is false.
            match SatProof.tryProveWith (Cadical(exePath = "no-such-cadical.exe")) (p + !!p) with
            | Ok _ -> Assert.Fail "a missing solver cannot have proved anything"
            | Error e -> Assert.Contains("not found", e)

    [<Fact>]
    member _.``the solver defaults to a RUP-only (replayable) proof, and can be asked not to`` () =
        // CaDiCaL's default preprocessing introduces fresh variables and justifies them with RAT
        // steps, which `rupChain` cannot replay — pigeonhole 5→4 failed on 12 of its 82 steps that
        // way. `--plain` is therefore the DEFAULT here, and this pins it: a caller who constructs a
        // solver in order to reconstruct must get a replayable trace without having to know any of
        // the above. Opting out is for verdict-only use.
        Assert.True(Cadical().Plain, "reconstruction needs a RUP-only trace, so plain must default on")
        Assert.False(Cadical(plain = false).Plain)
        match solver with
        | None -> out.WriteLine "SKIPPED (no cadical): the verdict half of this test needs the solver"
        | Some sat ->
            // Opting out must still DECIDE correctly — only the proof format changes.
            let verdictOnly = Cadical(exePath = sat.ExePath, timeoutMs = 20000, plain = false)
            let cnf = SatProof.clausesOf (p + !!p) (fst (Cnf.toCnf !!(p + !!p)))
            Assert.Equal(Unsat, (verdictOnly.Solve cnf).Status)

    [<Fact>]
    member _.``a dense refutation reconstructs, not just implication chains`` () =
        // Every other end-to-end case here is tiny or a chain, and a chain is the cheapest
        // refutation shape there is: one resolution per atom over narrow clauses. Pigeonhole 4→3 is
        // the smallest goal in the suite whose refutation is genuinely dense, and it is the one that
        // exercises the `--plain` default above. (5→4 is in `examples/sat/Reconstruct.fsx`; at ~20 s
        // it is too slow for the unit suite.)
        match solver with
        | None -> out.WriteLine "SKIPPED (no cadical): examples/sat/Reconstruct.fsx is the end-to-end gate"
        | Some sat ->
            let n = 3
            let ph = Array2D.init (n + 1) n (fun i j -> (boolvar (sprintf "ph%d_%d" i j) :> Prop))
            let someHole = [ for i in 0 .. n -> [ for j in 0 .. n - 1 -> ph.[i, j] ] |> List.reduce (+) ]
            let noClash = [ for j in 0 .. n - 1 do
                              for i in 0 .. n do
                                for k in i + 1 .. n do yield !!(ph.[i, j] * ph.[k, j]) ]
            let goal = !!((someHole @ noClash) |> List.reduce ( * ))
            let th = SatProof.proveWith sat goal
            Assert.True(sequal th.Stmt (expand goal.Expr),
                        sprintf "proved %s, expected %s" (src th.Stmt) (src (expand goal.Expr)))

    [<Fact>]
    member _.``decide falls back to the atom-capped prover when no backend is installed`` () =
        // The guard is on the EXPONENTIAL provers, not on propositional proof as such — so with no
        // decider registered, a small goal still proves and a large one fails fast with a message
        // that points at the SAT route rather than just quoting the limit.
        SatProof.uninstall ()
        let small = p + !!p
        Assert.True(sequal (PropCalculus.decide small).Stmt (expand small.Expr))
        let big = ((p ==> q) * (q ==> r) * (r ==> s) * (s ==> t) * (t ==> u)) ==> (p ==> u)
        Assert.True(PropCalculus.prop_atom_count (expand big.Expr) > PropCalculus.autoproof_max_atoms)
        let e = Assert.ThrowsAny<exn>(fun () -> PropCalculus.decide big |> ignore)
        Assert.Contains("Sylvia.Prover.SAT", e.Message)

    [<Fact>]
    member _.``installing the SAT backend lifts decide's atom ceiling`` () =
        match solver with
        | None -> out.WriteLine "SKIPPED (no cadical): examples/sat/Reconstruct.fsx is the end-to-end gate"
        | Some sat ->
            // 8 atoms — well past `autoproof_max_atoms`, which is NOT raised: the point is that the
            // limit only governs the fallback, so installing a scalable decider removes the ceiling
            // without making the exponential provers attempt anything they cannot finish.
            let big = ((p ==> q) * (q ==> r) * (r ==> s) * (s ==> t) * (t ==> u) * (u ==> v) * (v ==> w)) ==> (p ==> w)
            Assert.Equal(5, PropCalculus.autoproof_max_atoms)
            Assert.True(PropCalculus.prop_atom_count (expand big.Expr) > PropCalculus.autoproof_max_atoms)
            try
                SatProof.installWith sat
                let th = PropCalculus.decide big
                Assert.True(sequal th.Stmt (expand big.Expr), sprintf "proved %s, expected the goal" (src th.Stmt))
            finally SatProof.uninstall ()

    [<Fact>]
    member _.``decide rejects a decider that answers a different question`` () =
        // `prop_decider` is a registration slot for code outside the kernel assembly, so `decide`
        // re-checks the statement. This is what keeps the slot from widening the trusted base.
        try
            // A perfectly valid theorem — of the WRONG proposition.
            PropCalculus.prop_decider <-
                Some(fun _ -> theorem PropCalculus.prop_calculus (q + !!q)
                                  [ PropCalculus.excluded_middle' q |> PropCalculus.Taut' |> apply ])
            // Must be ABOVE the atom guard, or `decide` never consults the decider at all.
            let big = ((p ==> q) * (q ==> r) * (r ==> s) * (s ==> t) * (t ==> u) * (u ==> v)) ==> (p ==> v)
            Assert.True(PropCalculus.prop_atom_count (expand big.Expr) > PropCalculus.autoproof_max_atoms)
            let e = Assert.ThrowsAny<exn>(fun () -> PropCalculus.decide big |> ignore)
            Assert.Contains("but the goal was", e.Message)
        finally SatProof.uninstall ()

    [<Fact>]
    member _.``installing a backend never makes a small goal slower or worse`` () =
        // The two provers blow up on different axes, so `decide` routes by atom count instead of
        // always preferring the backend. Regression guard: with the backend installed, goals under
        // the atom limit must still go to the in-kernel prover. `∨`-over-`∧` distributivity and xor
        // associativity are 3-atom goals that the in-kernel prover does in ~1 ms and 0 ms, while
        // clausification for the SAT route explodes on their nesting (8.3 s, and a STACK OVERFLOW —
        // which no try/catch can rescue, so this has to be prevented, not handled).
        match solver with
        | None -> out.WriteLine "SKIPPED (no cadical): examples/sat/Reconstruct.fsx is the end-to-end gate"
        | Some sat ->
            try
                SatProof.installWith sat
                for g in [ (p * (q + r)) == ((p * q) + (p * r))
                           ((p != q) != r) == (p != (q != r))
                           p + !!p ] do
                    Assert.True(PropCalculus.prop_atom_count (expand g.Expr) <= PropCalculus.decide_max_anf_atoms)
                    let th = PropCalculus.decide g
                    Assert.True(sequal th.Stmt (expand g.Expr))
            finally SatProof.uninstall ()

    [<Fact>]
    member _.``the routing preference never costs a solver-free caller a goal it could prove`` () =
        // `decide_max_anf_atoms` (3) is a PREFERENCE — above it `decide` would rather use the
        // backend. `autoproof_max_atoms` (5) is a GUARD — beyond it the in-kernel prover stops
        // working. They are separate knobs precisely so that a 4- or 5-atom goal with no backend
        // installed still falls back to the in-kernel prover instead of failing on the preference.
        SatProof.uninstall ()
        Assert.True(PropCalculus.decide_max_anf_atoms < PropCalculus.autoproof_max_atoms)
        let g = ((p ==> q) * (q ==> r) * (r ==> s)) ==> (p ==> s)     // 4 atoms: above pref, below guard
        let n = PropCalculus.prop_atom_count (expand g.Expr)
        Assert.True(n > PropCalculus.decide_max_anf_atoms && n <= PropCalculus.autoproof_max_atoms)
        Assert.True(sequal (PropCalculus.decide g).Stmt (expand g.Expr))
