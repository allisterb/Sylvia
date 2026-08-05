namespace Sylvia.Tests.Perf

open System

open Sylvia
open Formula
open PropCalculus
open Sylvia.SAT

/// Where the SAT-reconstruction pipeline's time actually goes, and what drives it.
///
/// Distinct from `Reconstruction` / `ReconstructionDense`, which are HERMETIC — they carry canned
/// LRAT traces so the profile is pure Sylvia-side replay. That is the right shape for A/B work on
/// the replay itself, but it structurally cannot answer two questions:
///
///   1. How the whole pipeline divides — `Cnf.to_cnf`, clausification, solving, `refute`, the AC
///      `normalize` bridge, and the closing `Contradiction`. Only `refute` is in the canned payloads.
///   2. What makes one LRAT step cost more than another. `docs/prover-perf-handoff.md` §5.4 named
///      clause width as the suspect for pigeonhole 6→5 costing 215 ms/step against a chain's 21 ms,
///      and recorded that it had never been isolated.
///
/// Run with `-- phases`. Needs a solver: the native backend if `sylvia_cadical.dll` is present,
/// otherwise `cadical.exe`. Solving is ~0.4 ms of a multi-second payload, so which one is immaterial
/// to the result — but the phase split is not reconstructible without a real trace.
module Profile =

    /// A top-level proof announces itself even at LogLevel 0, and these formulas are hundreds of
    /// nodes wide. Decompiling and writing them is real cost a caller pays, but it is not the cost
    /// this payload measures. Same reasoning as `Program.runDense`.
    let private quiet (f: unit -> 'a) : 'a =
        let saved = Console.Out
        Console.SetOut IO.TextWriter.Null
        try f () finally Console.SetOut saved

    let private ms (f: unit -> 'a) =
        let sw = Diagnostics.Stopwatch.StartNew()
        let r = quiet f
        sw.Stop()
        r, sw.Elapsed.TotalMilliseconds

    /// `SatProof.transEq` is private; these are the same four lines.
    let private transEq (p1: Theorem) (p2: Theorem) : Theorem =
        match p1.Stmt, p2.Stmt with
        | Equals(x, _), Equals(_, z) ->
            theorem prop_calculus (Prop(expand_as<bool> x) == Prop(expand_as<bool> z))
                    [ Ident p1 |> apply_left; Ident p2 |> apply_left ]
        | _ -> failwith "Profile.transEq: not equalities"

    (* ---------------------------------------------------------------------- *)
    (* Payloads                                                                *)
    (* ---------------------------------------------------------------------- *)

    // Every builder takes a `tag` woven into its variable names. Structurally identical goals with
    // distinct names are the only way to take a COLD measurement twice: `conj_elim_all` goes through
    // `elimR = Memo.p2`, so timing it before `refute` would otherwise leave `refute`'s own setup warm
    // — which is exactly how the first decomposition produced a negative loop time.

    /// `(x₁⇒x₂) ∧ … ∧ (xₙ₋₁⇒xₙ)  ⇒  (x₁⇒xₙ)`. One RUP link per step.
    let chain (tag: string) n =
        let xs = [ for i in 1 .. n -> boolvar (sprintf "%sx%d" tag i) :> Prop ]
        let links = [ for i in 0 .. n - 2 -> xs.[i] ==> xs.[i + 1] ]
        (List.reduce ( * ) links) ==> (xs.[0] ==> xs.[n - 1])

    /// `np` pigeons into `nh` holes. Dense: several RUP links per step.
    let pigeonhole (tag: string) np nh =
        let x =
            Array2D.init np nh (fun i j -> boolvar (sprintf "%sh%d_%d" tag (i + 1) (j + 1)) :> Prop)
        let somewhere =
            [ for i in 0 .. np - 1 -> [ for j in 0 .. nh - 1 -> x.[i, j] ] |> List.reduce (+) ]
        let disjoint =
            [ for j in 0 .. nh - 1 do
                for a in 0 .. np - 1 do
                  for b in a + 1 .. np - 1 -> !!(x.[a, j] * x.[b, j]) ]
        !!(List.reduce ( * ) (somewhere @ disjoint))

    /// A fixed 3-link refutation plus `pad` hypotheses over FRESH variables that the refutation can
    /// never use. Holds LINKS constant while growing clauses and `|A|` — which is why its cost is
    /// almost entirely `conj_elim_all`, not the replay loop.
    let padded (tag: string) pad =
        let v (n: string) = boolvar (tag + n) :> Prop
        let core = [ v "a" ==> v "b"; v "b" ==> v "c" ]
        let junk =
            [ for i in 1 .. pad ->
                (boolvar (sprintf "%su%d" tag i) :> Prop) ==> (boolvar (sprintf "%sv%d" tag i) :> Prop) ]
        (List.reduce ( * ) (core @ junk)) ==> (v "a" ==> v "c")

    (* ---------------------------------------------------------------------- *)
    (* Structure of a trace                                                    *)
    (* ---------------------------------------------------------------------- *)

    /// Replay `rup_chain` over a trace to recover the quantities that could explain per-step cost:
    /// LINKS (each one a full `resolveUnder` over the whole conjunction `A`), input clause width, and
    /// resolvent width. Pure counting — no kernel calls, no timing.
    let private traceStats (cnf: CnfProblem) (run: SatRun) =
        let lits = Collections.Generic.Dictionary<int, int list>()
        match run.Originals with
        | [] -> cnf.Clauses |> List.iteri (fun i c -> lits.[i + 1] <- c)
        | os -> for (id, c) in os do lits.[id] <- c
        let clauseOf id = match lits.TryGetValue id with | true, c -> Some c | _ -> None
        let mutable steps = 0
        let mutable links = 0
        let mutable maxResolvent = 0
        for st in run.Steps do
            match st with
            | Delete _ -> ()
            | Add(id, cl, hints) ->
                steps <- steps + 1
                match rup_chain clauseOf cl hints with
                | Error _ -> ()
                | Ok chain ->
                    links <- links + chain.Links.Length
                    for l in chain.Links do maxResolvent <- max maxResolvent l.Result.Length
                lits.[id] <- cl
        {| Steps = steps
           Links = links
           InputLits = cnf.Clauses |> List.sumBy List.length
           MaxInputWidth = cnf.Clauses |> List.map List.length |> List.max
           MaxResolventWidth = maxResolvent |}

    (* ---------------------------------------------------------------------- *)
    (* The measurements                                                        *)
    (* ---------------------------------------------------------------------- *)

    let private backend () : ISatBackend option =
        if Native.is_available () then Some(Native.CadicalNative(timeoutMs = 120000) :> ISatBackend)
        else
            let cli = Cadical(timeoutMs = 120000)
            if (cli :> ISatBackend).IsAvailable then Some(cli :> ISatBackend) else None

    /// Time each phase of `SatProof.prove_with` separately, and report the trace structure beside it.
    let runPhases () =
        match backend () with
        | None ->
            printfn "No SAT backend available — put sylvia_cadical.dll in bin/ (or set"
            printfn "SYLVIA_CADICAL_NATIVE), or make cadical.exe resolvable. Skipping."
        | Some sat ->

        Proof.LogLevel <- 0
        printfn "backend: %s\n" sat.Description

        let goals =
            [ "chain 8", chain "a" 8
              "chain 16", chain "b" 16
              "chain 24", chain "c" 24
              "chain 32", chain "d" 32
              "pigeonhole 4→3", pigeonhole "e" 4 3
              "pigeonhole 5→4", pigeonhole "f" 5 4 ]

        printfn "PHASE SPLIT of prove_with (ms, warm)"
        printfn "%-16s %8s %9s %7s %10s %7s %9s %7s %8s"
                "goal" "to_cnf" "clausify" "solve" "refute" "dedup" "ACbridge" "close" "total"
        printfn "%s" (String.replicate 96 "-")

        let rows =
            [ for (label, goal) in goals do
                // A fresh process spends over a second in JIT; warm before reading anything.
                for _ in 1 .. 2 do quiet (fun () -> SatProof.prove_with sat goal) |> ignore

                let neg = !!goal
                let (cnfProp, cnfPf), tToCnf = ms (fun () -> Cnf.to_cnf neg)
                let cnf, tClausify = ms (fun () -> SatProof.clauses_of goal cnfProp)
                let run, tSolve = ms (fun () -> sat.Run cnf)
                let (A, rOpt), tRefute = ms (fun () -> SatProof.refute cnf run.Originals run.Steps)
                let rTh = Option.get rOpt
                let (cnfDedup, dedupPf), tDedup = ms (fun () -> SatProof.dedup_cnf cnfProp)
                let bridge, tBridge = ms (fun () -> theorem prop_calculus (cnfDedup == A) [ normalize ])
                let _, tClose =
                    ms (fun () ->
                        let ceq =
                            transEq cnfPf (match dedupPf with Some d -> transEq d bridge | None -> bridge)
                        let negImpF =
                            theorem prop_calculus (neg ==> F)
                                    [ Ident ceq |> apply_left; Taut rTh |> apply ]
                        Contradiction negImpF)
                let total = tToCnf + tClausify + tSolve + tRefute + tDedup + tBridge + tClose
                printfn "%-16s %8.1f %9.1f %7.2f %10.1f %7.1f %9.1f %7.1f %8.0f"
                        label tToCnf tClausify tSolve tRefute tDedup tBridge tClose total
                yield label, traceStats cnf run, tRefute, tBridge, total ]

        printfn "\nShare of total: refute is what to optimize; the AC bridge is not."
        printfn "%-16s %10s %10s %10s" "goal" "to_cnf" "refute" "ACbridge"
        printfn "%s" (String.replicate 50 "-")
        for (label, _, tRefute, tBridge, total) in rows do
            printfn "%-16s %9.0f%% %9.0f%% %9.0f%%"
                    label (100.0 * (total - tRefute - tBridge) / total)
                    (100.0 * tRefute / total) (100.0 * tBridge / total)

        printfn "\nWHAT DRIVES PER-STEP COST — width, or links per step?"
        printfn "%-16s %6s %6s %8s %8s %8s %9s %9s %9s"
                "goal" "steps" "links" "lnk/stp" "inLits" "maxInW" "maxResW" "ms/step" "ms/link"
        printfn "%s" (String.replicate 92 "-")
        for (label, s, tRefute, _, _) in rows do
            printfn "%-16s %6d %6d %8.1f %8d %8d %9d %9.1f %9.2f"
                    label s.Steps s.Links (float s.Links / float s.Steps) s.InputLits
                    s.MaxInputWidth s.MaxResolventWidth
                    (tRefute / float s.Steps)
                    (if s.Links = 0 then 0.0 else tRefute / float s.Links)

        printfn "\nCONTROL — same 3-link refutation throughout; only |A| grows."
        printfn "%-8s %8s %7s %7s %10s %10s" "pad" "inLits" "steps" "links" "refute ms" "ms/link"
        printfn "%s" (String.replicate 56 "-")
        for pad in [ 0; 2; 4; 8; 16; 32 ] do
            let goal = padded (sprintf "p%d" pad) pad
            for _ in 1 .. 2 do quiet (fun () -> SatProof.prove_with sat goal) |> ignore
            let (cnfProp, _) = quiet (fun () -> Cnf.to_cnf !!goal)
            let cnf = SatProof.clauses_of goal cnfProp
            let run = sat.Run cnf
            let _, t = ms (fun () -> SatProof.refute cnf run.Originals run.Steps)
            let s = traceStats cnf run
            printfn "%-8d %8d %7d %7d %10.1f %10.2f"
                    pad s.InputLits s.Steps s.Links t (t / float s.Links)

        printfn "\nrefute ≈ links × f(|A|). Per-step cost follows LINKS, not clause width; per-link"
        printfn "cost follows |A|, because every link's obligation is `A ⇒ clause`."
        Proof.LogLevel <- 1

    (* ---------------------------------------------------------------------- *)
    (* The cost model, and its out-of-sample test                              *)
    (* ---------------------------------------------------------------------- *)

    /// A fitted law of this shape was tried and REJECTED — see docs/prover-perf-handoff.md §1b.
    /// `refute ≈ |A| × (0.107·clauses + 0.080·links)` predicted the held-out pigeonhole 6→5 to +1%
    /// once and to −16% and −73% on two further runs of the same computation, with per-goal errors
    /// from −53% to +27%. It is kept here only so the numbers below have a column to sit against;
    /// do not treat it as a model.
    let private K_SETUP = 0.107
    let private K_LINK = 0.080

    let private predict (clauses: int) (inputLits: int) (links: int) : float =
        float inputLits * (K_SETUP * float clauses + K_LINK * float links)

    /// Decompose `refute` into `conj_elim_all` and the replay loop, and check the model against each.
    ///
    /// `-- model` rather than part of `-- phases` because pigeonhole 6→5 alone takes ~30 s.
    let runModel (only: string option) =
        match backend () with
        | None -> printfn "No SAT backend available. Skipping."
        | Some sat ->

        Proof.LogLevel <- 0
        let prep (goal: Prop) =
            let (cnfProp, _) = quiet (fun () -> Cnf.to_cnf !!goal)
            let cnf = SatProof.clauses_of goal cnfProp
            cnf, sat.Run cnf

        printfn "%-14s %6s %5s %6s %7s %7s %10s %11s %9s"
                "goal" "atoms" "cls" "|A|" "steps" "links" "refuteMs" "prove_with" "ms/link"
        printfn "%s" (String.replicate 84 "-")

        let row (label: string) (mk: string -> Prop) =
          if only.IsNone || only = Some label then
            // Warm the JIT on a SMALL goal, never on the payload itself. Warming with fresh tags
            // grows `elimR`'s Memo without bound (new key per trial, never evicted), and on a big
            // payload that alone made pigeonhole 6->5 measure 3.7x slower.
            quiet (fun () -> SatProof.prove_with sat (chain "warm1" 6)) |> ignore
            quiet (fun () -> SatProof.prove_with sat (chain "warm2" 6)) |> ignore
            // Setup and refute are timed on separate, structurally identical goals: measuring both
            // on one goal would let the first warm `elimR`'s memo for the second.
            let cnfR, runR = prep (mk "r")
            let _, tRefute = ms (fun () -> SatProof.refute cnfR runR.Originals runR.Steps)
            let s = traceStats cnfR runR
            // The whole user-facing call, on yet another copy: this is what "under a second" means.
            let _, tProve = ms (fun () -> SatProof.prove_with sat (mk "w"))
            printfn "%-14s %6d %5d %6d %7d %7d %10.1f %11.1f %9.3f"
                    label cnfR.NumVars cnfR.Clauses.Length s.InputLits s.Steps s.Links tRefute
                    tProve (tRefute / float s.Links)

        row "chain 8"   (fun t -> chain t 8)
        row "chain 16"  (fun t -> chain t 16)
        row "chain 20"  (fun t -> chain t 20)
        row "chain 24"  (fun t -> chain t 24)
        row "chain 32"  (fun t -> chain t 32)
        row "chain 40"  (fun t -> chain t 40)
        row "chain 50"  (fun t -> chain t 50)
        row "chain 64"  (fun t -> chain t 64)
        row "chain 100" (fun t -> chain t 100)
        row "chain 128" (fun t -> chain t 128)
        row "chain 200" (fun t -> chain t 200)
        row "php 4→3"   (fun t -> pigeonhole t 4 3)
        row "php 5→4"   (fun t -> pigeonhole t 5 4)
        printfn ""
        for pad in [ 0; 8; 32 ] do row (sprintf "pad %d" pad) (fun t -> padded t pad)
        printfn "\nHeld out when the model was fitted (~30 s):"
        row "php 6→5"   (fun t -> pigeonhole t 6 5)
        row "php 7→6"   (fun t -> pigeonhole t 7 6)

        printfn "\nrefute ≈ |A| × (%.3f × clauses + %.3f × links). Both terms carry |A|." K_SETUP K_LINK
        Proof.LogLevel <- 1

    (* ---------------------------------------------------------------------- *)
    (* Inside conj_elim_all                                                    *)
    (* ---------------------------------------------------------------------- *)

    /// Why `conj_elim_all` costs what it does. Both probes here returned NEGATIVE results that closed
    /// handoff §5.3 — keep them so nobody re-opens it.
    ///
    ///   A. A `Tactics.Schema` instantiation is priced by the SIZE OF THE STATEMENT it produces, not
    ///      by a fixed per-call overhead. So "the chain_imp calls" and "the A-sized statements they
    ///      carry" were never competing explanations; they are one.
    ///   B. The repeated `rest j` conjunction building — ~2n calls, each O(|A|), so O(n²) in term
    ///      construction despite the docstring's "ONE O(n) pass" — is ~1% of the total. Precomputing
    ///      it buys nothing.
    ///
    /// Together: `conj_elim_all` emits `n` theorems whose statements each contain `A`, so its output
    /// alone is Ω(n·|A|). It cannot be made faster without changing what it produces.
    let runConjElim () =
        Proof.LogLevel <- 0
        let msN n (f: unit -> 'a) =
            quiet f |> ignore
            let sw = Diagnostics.Stopwatch.StartNew()
            for _ in 1 .. n do quiet f |> ignore
            sw.Stop()
            sw.Elapsed.TotalMilliseconds / float n

        printfn "A. Schema instantiation vs the size of the statement it produces"
        printfn "%10s %12s %16s" "conjuncts" "us/call" "us per conjunct"
        printfn "%s" (String.replicate 40 "-")
        let probe = Tactics.Schema.p2 "profile_probe_sa" strengthen_and
        for k in [ 1; 2; 4; 8; 16; 32; 64; 128 ] do
            let cls =
                [ for i in 1 .. k ->
                    (boolvar (sprintf "c%d_%d" k i) :> Prop) + (boolvar (sprintf "d%d_%d" k i) :> Prop) ]
            let big = List.reduce ( * ) cls
            let one = boolvar (sprintf "z%d" k) :> Prop
            let t = msN 200 (fun () -> probe one big) * 1000.0
            printfn "%10d %12.1f %16.3f" k t (t / float k)

        match backend () with
        | None -> printfn "\nNo SAT backend — skipping part B."
        | Some sat ->

        printfn "\nB. conj_elim_all: total vs the `rest j` term building alone"
        printfn "%-14s %5s %8s %14s %14s %8s" "goal" "cls" "|A|" "conj_elim ms" "rest-only ms" "rest%"
        printfn "%s" (String.replicate 68 "-")

        // conj_elim_all's exact `rest` call pattern, with no theorem building at all.
        let restOnly (arr: Prop[]) () =
            let n = arr.Length
            let rest j = arr.[j..] |> Array.reduceBack (fun a b -> a * b)
            let mutable acc = 0
            if n > 1 then
                acc <- acc + (rest 1).GetHashCode()
                for j in 2 .. n - 1 do acc <- acc + (rest j).GetHashCode()
                for i in 0 .. n - 2 do acc <- acc + (rest (i + 1)).GetHashCode()
            acc

        let row (label: string) (mk: string -> Prop) =
            let inputsOf (tag: string) =
                let g = mk tag
                let (cnfProp, _) = quiet (fun () -> Cnf.to_cnf !!g)
                let cnf = SatProof.clauses_of g cnfProp
                cnf, (cnf.Clauses |> List.map (clause_prop cnf))
            let cnfR, inputsR = inputsOf "r"
            let tTotal = msN 1 (fun () -> SatProof.conj_elim_all inputsR)
            let _, inputsS = inputsOf "s"
            let tRest = msN 20 (restOnly (Array.ofList inputsS))
            printfn "%-14s %5d %8d %14.1f %14.2f %7.0f%%"
                    label cnfR.Clauses.Length (cnfR.Clauses |> List.sumBy List.length)
                    tTotal tRest (100.0 * tRest / tTotal)

        row "chain 16" (fun t -> chain t 16)
        row "chain 32" (fun t -> chain t 32)
        row "php 5→4" (fun t -> pigeonhole t 5 4)
        printfn "\nBoth negative. See the docstring, and handoff §5.3."
        Proof.LogLevel <- 1

    (* ---------------------------------------------------------------------- *)
    (* The ceiling on removing A                                               *)
    (* ---------------------------------------------------------------------- *)

    /// How much of a link is the actual inference, and how much is carrying `A`?
    ///
    /// `SatProof.resolveUnder` does two things per link. `resolveStep` is the A-FREE half — the real
    /// resolution, at clause scale. Everything else (`conj`, `combine_implies`, `mp`,
    /// `Calc.chain_imp`) is over statements containing the whole input conjunction. `resolveStep`'s
    /// arguments come straight out of `rup_chain`, so the A-free half can be timed for every link of
    /// a real refutation WITHOUT running the replay, and whatever fraction it is bounds the win from
    /// handoff §5.1.
    ///
    /// TRAP, and it cost a completely wrong answer first time: `SatProof` SHADOWS `resolve` with
    /// `Tactics.Schema.p3 "sat_resolve" resolve`, so its `resolveStep` instantiates a pre-derived
    /// schema. Calling `PropCalculus.resolve` here instead measures the full memoized derivation —
    /// a different and far more expensive function, which put the A-free share at 100–140% of the
    /// loop (i.e. impossible, which is the only reason it got caught).
    let runCeiling () =
        match backend () with
        | None -> printfn "No SAT backend available. Skipping."
        | Some sat ->

        Proof.LogLevel <- 0
        let resolveS = Tactics.Schema.p3 "profile_resolve" resolve
        // Verbatim from SatProof, with the shadowed `resolve` restored.
        let acEq (l: Prop) (r: Prop) : Rule = ident prop_calculus (l == r) [ simp ]
        let resolveStep (cnf: CnfProblem) apos aneg (pv: int) out =
            let cL = apos |> List.filter (fun l -> l <> pv)
            let dL = aneg |> List.filter (fun l -> l <> -pv)
            let cp lits = clause_prop cnf lits
            let C, D, v = cp cL, cp dL, cnf.AtomOfVar.[pv]
            theorem prop_calculus (cp apos * cp aneg ==> cp out) [
                acEq (cp apos) (C + v) |> at [ left_branch; left_branch ]
                acEq (cp aneg) (-v + D) |> at [ left_branch; right_branch ]
                acEq (cp out) (C + D) |> at [ right_branch ]
                resolveS C D v |> Taut |> apply ]

        printfn "%-14s %6s %8s %11s %11s %10s"
                "goal" "links" "|A|" "refute ms" "A-free ms" "ratio"
        printfn "%s" (String.replicate 64 "-")

        let row (label: string) (mk: string -> Prop) =
            quiet (fun () -> SatProof.prove_with sat (chain "warm" 6)) |> ignore
            let prep (tag: string) =
                let g = mk tag
                let (cnfProp, _) = quiet (fun () -> Cnf.to_cnf !!g)
                let cnf = SatProof.clauses_of g cnfProp
                cnf, sat.Run cnf
            let cnfR, runR = prep "r"
            let _, tRefute = ms (fun () -> SatProof.refute cnfR runR.Originals runR.Steps)

            // Walk the trace on a third copy, timing only resolveStep.
            let cnfF, runF = prep "f"
            let lits = Collections.Generic.Dictionary<int, int list>()
            match runF.Originals with
            | [] -> cnfF.Clauses |> List.iteri (fun i c -> lits.[i + 1] <- c)
            | os -> for (id, c) in os do lits.[id] <- c
            let clauseOf id = match lits.TryGetValue id with | true, c -> Some c | _ -> None
            let mutable links = 0
            let mutable tFree = 0.0
            for st in runF.Steps do
                match st with
                | Delete _ -> ()
                | Add(id, cl, hints) ->
                    match rup_chain clauseOf cl hints with
                    | Error _ -> ()
                    | Ok ch ->
                        let mutable cur = lits.[ch.Start]
                        for l in ch.Links do
                            // rup_chain reports Pivot as abs u; resolveUnder orders the two clauses
                            // by which one carries the POSITIVE literal.
                            let ys = lits.[l.Antecedent]
                            let apos, aneg = if List.contains l.Pivot cur then cur, ys else ys, cur
                            let _, t = ms (fun () -> resolveStep cnfF apos aneg l.Pivot l.Result)
                            tFree <- tFree + t
                            links <- links + 1
                            cur <- l.Result
                    lits.[id] <- cl
            printfn "%-14s %6d %8d %11.1f %11.1f %9.2f"
                    label links (cnfR.Clauses |> List.sumBy List.length) tRefute tFree
                    (tRefute / tFree)

        row "chain 16" (fun t -> chain t 16)
        row "chain 32" (fun t -> chain t 32)
        row "php 4→3" (fun t -> pigeonhole t 4 3)
        row "php 5→4" (fun t -> pigeonhole t 5 4)
        printfn "\nBefore §5.1 landed this ratio was 14-25x: `refute` cost that much more than the"
        printfn "bare inference it wraps. At ~1 the antecedent is no longer carried, so the wrapper"
        printfn "is gone. (Under 1 only means the standalone walk misses warmth `refute` now shares.)"
        Proof.LogLevel <- 1
