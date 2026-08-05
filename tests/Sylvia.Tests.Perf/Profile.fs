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

        printfn "%-14s %5s %6s %7s %9s %9s %9s %7s %10s %8s"
                "goal" "cls" "|A|" "links" "setupMs" "loopMs" "refuteMs" "setup%" "fitted" "err"
        printfn "%s" (String.replicate 100 "-")

        let row (label: string) (mk: string -> Prop) =
          if only.IsNone || only = Some label then
            // Warm the JIT on a SMALL goal, never on the payload itself. Warming with fresh tags
            // grows `elimR`'s Memo without bound (new key per trial, never evicted), and on a big
            // payload that alone made pigeonhole 6->5 measure 3.7x slower.
            quiet (fun () -> SatProof.prove_with sat (chain "warm1" 6)) |> ignore
            quiet (fun () -> SatProof.prove_with sat (chain "warm2" 6)) |> ignore
            // Setup and refute are timed on separate, structurally identical goals: measuring both
            // on one goal would let the first warm `elimR`'s memo for the second.
            let cnfS, _ = prep (mk "s")
            let _, tSetup = ms (fun () -> SatProof.conj_elim_all (cnfS.Clauses |> List.map (clause_prop cnfS)))
            let cnfR, runR = prep (mk "r")
            let _, tRefute = ms (fun () -> SatProof.refute cnfR runR.Originals runR.Steps)
            let s = traceStats cnfR runR
            let cls = cnfR.Clauses.Length
            let pred = predict cls s.InputLits s.Links
            printfn "%-14s %5d %6d %7d %9.1f %9.1f %9.1f %6.0f%% %10.0f %7.0f%%"
                    label cls s.InputLits s.Links tSetup (tRefute - tSetup) tRefute
                    (100.0 * tSetup / tRefute) pred (100.0 * (pred - tRefute) / tRefute)

        row "chain 8"   (fun t -> chain t 8)
        row "chain 16"  (fun t -> chain t 16)
        row "chain 24"  (fun t -> chain t 24)
        row "chain 32"  (fun t -> chain t 32)
        row "php 4→3"   (fun t -> pigeonhole t 4 3)
        row "php 5→4"   (fun t -> pigeonhole t 5 4)
        printfn ""
        for pad in [ 0; 8; 32 ] do row (sprintf "pad %d" pad) (fun t -> padded t pad)
        printfn "\nHeld out when the model was fitted (~30 s):"
        row "php 6→5"   (fun t -> pigeonhole t 6 5)

        printfn "\nrefute ≈ |A| × (%.3f × clauses + %.3f × links). Both terms carry |A|." K_SETUP K_LINK
        Proof.LogLevel <- 1
