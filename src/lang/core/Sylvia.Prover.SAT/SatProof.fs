namespace Sylvia
// A bare rule used as a proof step means "apply to the whole expression"; addressed steps say `at`.
#nowarn "3391"

/// SAT-backed propositional proof: turn a CaDiCaL LRAT refutation of `¬φ` into a **kernel-checked**
/// `⊢ φ`.
///
/// This is Sylvia's scalable, trace-emitting propositional decider. `PropCalculus.autoproof_anf`
/// also emits a checkable trace but is exponential in the number of distinct atoms (bounded by
/// `autoproof_max_atoms = 5` and `autoproof_max_steps`); `PropCalculus.valid` scales but is a yes/no
/// *tool* outside the trusted base that emits no proof. This module has neither limitation: it
/// decides with a state-of-the-art SAT solver and then **replays the solver's resolution refutation
/// as native kernel steps**, so the solver is advisory and the resulting `Theorem` is checked the
/// same way any hand-written proof is.
///
/// Scale, for choosing between the two (Release, warm, 2026-07-30). This route: 24-atom chain
/// ~280 ms, 50 atoms ~1.1 s, pigeonhole 5→4 (20 atoms, dense) ~3.9 s. `autoproof_anf`: 3-atom chain
/// 453 ms, 4 atoms 1.9 s, 5 atoms 9.2 s. The crossover on chains is between 3 and 4 atoms, which is
/// what `decide_max_anf_atoms` encodes — but it is shape-dependent above that, and nested `≢` stays
/// overwhelmingly cheaper in-kernel. **Retire atom count as the unit for this route**: its cost
/// tracks LRAT steps × clause-set size × clause width, so a 20-atom pigeonhole costs what a 60-atom
/// chain does.
///
///     goal φ ─Cnf.to_cnf→ (¬φ == A, kernel proof) ─clauses_of→ DIMACS ─CaDiCaL→ UNSAT + LRAT
///            ─resolve-fold→ R : A ⇒ F                                     (STEP 1)
///            ─rewrite ¬φ to A, then Contradiction→ ⊢ φ                     (STEP 2)
///
/// Nothing here is a new trusted primitive: every step is `PropCalculus.resolve`,
/// `combine_implies`, `weaken_or`, `strengthen_and`, `Cnf.to_cnf` or `Calc.chain_imp`. See
/// `docs/prover-sat-reconstruction.md` for the design, and `examples/sat/Reconstruct.fsx` for a
/// runnable demonstration.
///
/// Requires the `cadical` executable: pass one via `prove_with`, or let `Cadical()` resolve the
/// `SYLVIA_CADICAL` environment variable / `cadical.exe` on PATH.
module SatProof =

    open System.Collections.Generic

    open FSharp.Quotations

    open Formula
    open PropCalculus
    open Sylvia.SAT

    let private pOf (e: Expr) : Prop = Prop(expand_as<bool> e)

    (* ---------------------------------------------------------------------- *)
    (* The Gries schemas this replay leans on, derived once and INSTANTIATED    *)
    (* ---------------------------------------------------------------------- *)

    /// Every one of these is an F# function of its `Prop` parameters, so calling it replays its
    /// whole derivation at the caller's arguments — and in a reconstruction those arguments are
    /// entire clause conjunctions, at every one of O(n) steps. Measured per call at 24-atom chain
    /// scale, with fresh arguments so memoization cannot mask the cost:
    ///
    ///     resolve           157 ms, 109 nested proofs        combine_implies    62 ms, 58 proofs
    ///     strengthen_and     13 ms,   9 nested proofs        commute_and       3.5 ms,  5 proofs
    ///
    /// `Tactics.Schema` derives each ONE time at metavariables and serves every later call by
    /// substitution, which is O(|result|) and one kernel step. Memoization cannot do this: the
    /// arguments are distinct at every step, so nothing ever hits the cache.
    ///
    /// This is confined to the reconstruction rather than pushed into `PropCalculus` itself, so no
    /// existing proof changes shape. Promoting the wrappers upstream is a separate decision.
    let private resolve = Tactics.Schema.p3 "sat_resolve" resolve
    let private combine_implies = Tactics.Schema.p3 "sat_combine_implies" combine_implies
    let private strengthen_and = Tactics.Schema.p2 "sat_strengthen_and" strengthen_and
    let private weaken_or = Tactics.Schema.p2 "sat_weaken_or" weaken_or
    let private reflex_implies = Tactics.Schema.p1 "sat_reflex_implies" reflex_implies

    (* ---------------------------------------------------------------------- *)
    (* Equality / implication plumbing (reused trusted lemmas only)             *)
    (* ---------------------------------------------------------------------- *)

    /// `x == y` and `y == z`  ⟼  `x == z`.
    let private transEq (p1: Theorem) (p2: Theorem) : Theorem =
        match p1.Stmt, p2.Stmt with
        | Equals(x, _), Equals(_, z) ->
            theorem prop_calculus (pOf x == pOf z) [ Ident p1 |> apply_left; Ident p2 |> apply_left ]
        | _ -> failwithf "SatProof.transEq: not equalities: %s / %s" (src p1.Stmt) (src p2.Stmt)

    /// `⊢ x`, `⊢ y`  ⟼  `⊢ x ∧ y`.
    let private conj (t1: Theorem) (t2: Theorem) (x: Prop) (y: Prop) : Theorem =
        theorem prop_calculus (x * y) [ Taut t1 |> apply_left; Taut t2 |> apply_right; reduce |> apply ]

    /// `⊢ P`, `⊢ (P ⇒ Q)`  ⟼  `⊢ Q`.
    let private mp (factP: Theorem) (impl: Theorem) (pP: Prop) (qQ: Prop) : Theorem =
        theorem prop_calculus qQ [ ident_conseq_true qQ |> Commute |> apply
                                   Taut factP |> Commute |> apply_left
                                   Taut impl |> apply ]

    let private elimR_impl (x: Prop) (y: Prop) : Theorem =                     // (x ∧ y) ⇒ y
        theorem prop_calculus (x * y ==> y) [ commute_and x y; strengthen_and y x |> Taut |> apply ]
    let private elimR = Memo.p2 elimR_impl

    /// `A ⇒ Cᵢ` for every conjunct of `A = C₀ ∧ … ∧ Cₙ₋₁`, in ONE O(n) pass that shares the
    /// peel-chain `A ⇒ restⱼ` (a naive per-clause elimination is O(n²) in the expensive
    /// `Calc.chain_imp`).
    let conj_elim_all (inputs: Prop list) : Theorem[] =
        let arr = Array.ofList inputs
        let n = arr.Length
        let rest j = arr.[j..] |> Array.reduceBack (fun a b -> a * b)
        if n = 1 then [| reflex_implies arr.[0] |]
        else
            let aToRest = Array.zeroCreate n
            aToRest.[1] <- elimR arr.[0] (rest 1)
            for j in 2 .. n - 1 do aToRest.[j] <- Calc.chain_imp aToRest.[j - 1] (elimR arr.[j - 1] (rest j))
            Array.init n (fun i ->
                if i = 0 then strengthen_and arr.[0] (rest 1)
                elif i = n - 1 then aToRest.[n - 1]
                else Calc.chain_imp aToRest.[i] (strengthen_and arr.[i] (rest (i + 1))))

    (* ---------------------------------------------------------------------- *)
    (* Clause shaping                                                          *)
    (* ---------------------------------------------------------------------- *)

    /// Clause equality up to AC **and idempotence**. `simp` normalizes a flattened ∨-chain and
    /// collapses repeated operands, so this discharges MERGE resolvents (where the two resolved
    /// clauses share a non-pivot literal, and the resolvent picks up a duplicate) as well as plain
    /// reorderings.
    let private acEq (l: Prop) (r: Prop) : Rule = ident prop_calculus (l == r) [ simp ]

    /// `src ⇒ dst` whenever src's literals are a SUBSET of dst's: ∨-weaken by the missing literals
    /// (Gries 3.76a), then AC-match. Covers both the plain reorder case and the LRAT case where a
    /// step declares a weaker clause than its hint chain actually derives.
    let private clauseImp (cnf: CnfProblem) (srcLits: int list) (dstLits: int list) : Theorem =
        let cp lits = clause_prop cnf lits
        let eqImp (a: Prop) (b: Prop) =
            if sequal (expand a.Expr) (expand b.Expr) then reflex_implies a
            else theorem prop_calculus (a ==> b) [ acEq b a |> at [ right_branch ]
                                                   reflex_implies a |> Taut |> apply ]
        match dstLits |> List.filter (fun l -> not (List.contains l srcLits)) |> List.distinct with
        | [] -> eqImp (cp srcLits) (cp dstLits)
        | extras -> Calc.chain_imp (weaken_or (cp srcLits) (cp extras))
                                   (eqImp (cp srcLits + cp extras) (cp dstLits))

    /// One binary resolution: `cp(apos) ∧ cp(aneg) ⇒ cp(out)`, where the two clauses clash on
    /// variable `pv`. Each clause is AC-matched to `resolve`'s `(C ∨ x)` / `(¬x ∨ D)` shape, so the
    /// resolution itself is instantiated at whole clauses with `C`, `D`, `x` opaque — which is what
    /// keeps a wide-clause replay polynomial.
    let private resolveStep (cnf: CnfProblem) (apos: int list) (aneg: int list) (pv: int) (out: int list) : Theorem =
        let cL = apos |> List.filter (fun l -> l <> pv)
        let dL = aneg |> List.filter (fun l -> l <> -pv)
        let cp lits = clause_prop cnf lits
        let C, D, v = cp cL, cp dL, cnf.AtomOfVar.[pv]
        theorem prop_calculus (cp apos * cp aneg ==> cp out) [
            acEq (cp apos) (C + v) |> at [ left_branch; left_branch ]
            acEq (cp aneg) (-v + D) |> at [ left_branch; right_branch ]
            acEq (cp out) (C + D) |> at [ right_branch ]
            resolve C D v |> Taut |> apply ]

    (* ---------------------------------------------------------------------- *)
    (* CNF extraction and the ¬φ == A bridge                                    *)
    (* ---------------------------------------------------------------------- *)

    /// Read a `CnfProblem` directly off a clean CNF `Prop`, so the clauses handed to the solver are
    /// EXACTLY the ones `Cnf.to_cnf` proved `¬φ` equal to. (Clausifying twice — once for the proof,
    /// once for the solver — leaves the LRAT clause ids and variable indices meaningless against
    /// each other; they agree on implication chains and diverge elsewhere.)
    ///
    /// Repeated literals within a clause are dropped. That is an OPTIMIZATION, not a correctness
    /// requirement: every kernel step in the replay costs O(|A|), so smaller clauses are cheaper.
    /// `dedup_cnf` pays for it by proving `cnfProp == A` in two exact moves instead of one.
    let clauses_of (goal: Prop) (cnfProp: Prop) : CnfProblem =
        let atoms = List<Expr>()
        let varOf (e: Expr) =
            // A truth constant is not an atom. Minting a variable for one silently hands the solver a
            // free choice — it satisfies `¬φ` by setting `T` false — so any goal mentioning `T`/`F`
            // comes back "not a theorem". `Cnf.to_cnf` folds constants away; fail loudly if one ever
            // reaches here again rather than answering the wrong question.
            match e with
            | True | False ->
                failwithf "SatProof.clauses_of: the CNF still contains the truth constant %s as a literal — Cnf.to_cnf is expected to have folded it away" (src e)
            | _ -> ()
            let mutable f = -1
            for i in 0 .. atoms.Count - 1 do if f < 0 && sequal atoms.[i] e then f <- i
            if f < 0 then atoms.Add e; atoms.Count else f + 1
        let litOf e = match e with Not a -> -(varOf a) | _ -> varOf e
        let rec lits e = match e with Or(x, y) -> lits x @ lits y | _ -> [ litOf e ]
        let rec cls e = match e with And(x, y) -> cls x @ cls y | _ -> [ lits e ]
        let clauses = cls (expand cnfProp.Expr) |> List.map List.distinct
        let aov = Dictionary<int, Prop>()
        atoms |> Seq.iteri (fun i a -> aov.[i + 1] <- pOf a)
        { NumVars = atoms.Count
          Clauses = clauses
          AtomOfVar = aov :> IReadOnlyDictionary<_, _>
          Goal = goal }

    /// Rewrite every clause of a CNF to its literal-deduped form, in place, by congruence: each
    /// clause equality is a small local `simp` (idempotence collapses the repeat), lifted through
    /// the ∧ tree at an EXACT position, so nothing searches and nothing can mis-target. `None` when
    /// no clause had a repeated literal.
    let rec dedup_cnf (p: Prop) : Prop * Theorem option =
        match expand p.Expr with
        | And(x, y) ->
            let dx, tx = dedup_cnf (pOf x)
            let dy, ty = dedup_cnf (pOf y)
            match tx, ty with
            | None, None -> p, None
            | _ ->
                let steps =
                    [ match tx with Some t -> yield Ident t |> at [ left_branch; left_branch ] | None -> ()
                      match ty with Some t -> yield Ident t |> at [ left_branch; right_branch ] | None -> () ]
                (dx * dy), Some(theorem prop_calculus ((pOf x * pOf y) == (dx * dy)) steps)
        | clause ->
            let rec lits e = match e with Or(a, b) -> lits a @ lits b | _ -> [ pOf e ]
            let ls = lits clause
            let kept =
                ls |> List.fold (fun acc l ->
                        if acc |> List.exists (fun (k: Prop) -> sequal (expand k.Expr) (expand l.Expr))
                        then acc else acc @ [ l ]) []
            if List.length kept = List.length ls then p, None
            else
                let d = kept |> List.reduce (+)
                d, Some(theorem prop_calculus (p == d) [ simp ])

    (* ---------------------------------------------------------------------- *)
    (* STEP 1: the refutation  R : (∧ inputs) ⇒ F                               *)
    (* ---------------------------------------------------------------------- *)

    /// Replay an LRAT refutation as kernel steps, giving the input conjunction `A` and (when the
    /// trace reaches the empty clause) a checked `R : A ⇒ F`.
    ///
    /// EVERY `Add` step is replayed, binary or not: `SAT.rup_chain` unfolds a step's hints into an
    /// explicit chain of binary resolutions (a 2-hint step is simply a one-link chain, so the
    /// ordinary binary case is subsumed rather than special-cased), and the chain's clause is
    /// weakened to the one the step declares. Nothing in the trace is skipped.
    /// `originals` are the solver's own ids for the input clauses, in the order they were given to
    /// it; `[]` means "the ids are `1..m` in CNF order", which is what a text LRAT proof always
    /// implies because CaDiCaL's DIMACS parser reserves that range up front.
    ///
    /// Nothing reserves it when clauses are added through the C++ API, and CaDiCaL then numbers the
    /// inputs differently — on de Morgan, whose CNF contains two unit clauses, the native backend's
    /// first derived clause is id 3 where the CLI's is 6. Seeding the wrong `A ⇒ clause` against an
    /// id is silent: the replay then resolves against a formula the solver never used, and a correct
    /// proof is rejected. Hence the checks below rather than a positional assumption.
    let refute (cnf: CnfProblem) (originals: (int * Clause) list) (steps: LratStep list)
               : Prop * Theorem option =
        let inputs = cnf.Clauses |> List.map (clause_prop cnf)
        let A = inputs |> List.reduceBack (*)
        let lits = Dictionary<int, int list>()
        let imp = Dictionary<int, Theorem>()                        // id ⟼ A ⇒ cp(lits[id])
        let elims = conj_elim_all inputs
        match originals with
        | [] -> cnf.Clauses |> List.iteri (fun i c -> lits.[i + 1] <- c; imp.[i + 1] <- elims.[i])
        | os ->
            // The tracer reports originals in the order they were added, so the i-th corresponds to
            // `cnf.Clauses.[i]`. Both the count and the literals are checked, because getting this
            // correspondence wrong would otherwise surface as a mysterious replay failure.
            if List.length os <> List.length cnf.Clauses then
                failwithf "SatProof: the solver traced %d input clauses but the CNF has %d"
                          (List.length os) (List.length cnf.Clauses)
            List.iteri2
                (fun i (id, reported: Clause) (c: Clause) ->
                    if List.sort reported <> List.sort c then
                        failwithf "SatProof: input clause %d was traced as %A but the CNF has %A"
                                  i reported c
                    lits.[id] <- c
                    imp.[id] <- elims.[i])
                os cnf.Clauses
        let clauseOf id = match lits.TryGetValue id with | true, c -> Some c | _ -> None
        // A ⇒ cp xs  and  A ⇒ cp ys  ⟼  A ⇒ cp out   (one resolution, under the antecedent A)
        let resolveUnder (impX: Theorem) (impY: Theorem) xs ys pv out =
            let apos, aneg = if List.contains pv xs then xs, ys else ys, xs
            let impPos, impNeg = if apos = xs then impX, impY else impY, impX
            let cPos, cNeg = clause_prop cnf apos, clause_prop cnf aneg
            let both = conj impPos impNeg (A ==> cPos) (A ==> cNeg)
            let aToBoth = mp both (combine_implies A cPos cNeg) ((A ==> cPos) * (A ==> cNeg)) (A ==> (cPos * cNeg))
            Calc.chain_imp aToBoth (resolveStep cnf apos aneg pv out)
        let mutable r = None
        for step in steps do
            match step with
            | Delete _ -> ()
            | Add(id, cl, hints) ->
                match rup_chain clauseOf cl hints with
                | Error e -> failwithf "SatProof: LRAT step %d: %s" id e
                | Ok chain ->
                    let mutable cur = lits.[chain.Start]
                    let mutable curImp = imp.[chain.Start]
                    for link in chain.Links do
                        curImp <- resolveUnder curImp imp.[link.Antecedent] cur lits.[link.Antecedent] link.Pivot link.Result
                        cur <- link.Result
                    lits.[id] <- cl
                    imp.[id] <- if cur = cl then curImp else Calc.chain_imp curImp (clauseImp cnf cur cl)
                    if List.isEmpty cl then r <- Some imp.[id]
        A, r

    (* ---------------------------------------------------------------------- *)
    (* STEP 2 and the public entry points                                       *)
    (* ---------------------------------------------------------------------- *)

    /// Prove `goal` using the given solver, returning a kernel-checked `Theorem` of it.
    ///
    /// Raises if the goal is not a propositional theorem (the solver finds `¬goal` satisfiable), if
    /// the solver is unavailable or times out, or if the trace cannot be replayed. Unless `verbose`
    /// is set, proof logging is silenced for the duration and restored afterwards — a reconstruction
    /// emits thousands of kernel steps, which is noise at any call site.
    let prove_with_log (sat: #ISatBackend) (verbose: bool) (goal: Prop) : Theorem =
        let saved = Proof.LogLevel
        try
            if not verbose then Proof.LogLevel <- 0
            let neg = !!goal
            let (cnfProp, cnfPf) = Cnf.to_cnf neg                    // ¬φ == cnfProp, kernel-proved
            match expand cnfProp.Expr with
            // `¬φ == F` is already the refutation, so the solver has nothing to decide: close it
            // against `F ⇒ F` exactly as the replayed case closes against `A ⇒ F`. Reachable for goals
            // whose negation folds away entirely on the truth constants — e.g. `(p ∧ F) = F`, which is
            // what the set-theory zero laws translate to.
            | False ->
                let negImpF =
                    theorem prop_calculus (neg ==> F) [ Ident cnfPf |> apply_left; Taut (reflex_implies F) |> apply ]
                Contradiction negImpF
            // `¬φ == T`: the negation is valid, so the goal is refuted rather than undecided. Say so
            // in the same words the solver's `Sat` verdict uses, since it means the same thing.
            | True ->
                failwithf "SatProof: %s is NOT a theorem (its negation reduces to T)"
                    (prop_calculus.PrintFormula (expand goal.Expr))
            | _ ->
            let cnf = clauses_of goal cnfProp
            let run = sat.Run cnf
            // Distinguish "this is not a theorem" from "the solver could not tell us": a caller
            // deciding whether to fall back to another tactic needs to know which happened.
            match run.Result.Status with
            | Unsat -> ()
            | Sat ->
                failwithf "SatProof: %s is NOT a theorem (¬goal is satisfiable)"
                    (prop_calculus.PrintFormula (expand goal.Expr))
            | NotAvailable ->
                failwithf "SatProof: the solver '%s' was not found — for the CLI backend set the SYLVIA_CADICAL environment variable or put cadical.exe on PATH; for the native backend set SYLVIA_CADICAL_NATIVE or put sylvia_cadical.dll beside the host" sat.Description
            | Timeout -> failwith "SatProof: the solver hit its time budget without deciding the goal"
            | Unknown -> failwith "SatProof: the solver exited without a verdict"
            let A, rOpt = refute cnf run.Originals run.Steps
            let rTh =
                match rOpt with
                | Some t -> t
                | None -> failwith "SatProof: the LRAT trace never derives the empty clause"
            // ¬φ == A in two exact moves: clause-wise literal dedup (congruence), then pure-AC
            // reassociation of the same clause multiset into the right-associated `A`.
            let (cnfDedup, dedupPf) = dedup_cnf cnfProp
            let bridge = theorem prop_calculus (cnfDedup == A) [ normalize ]
            let ceq = transEq cnfPf (match dedupPf with Some d -> transEq d bridge | None -> bridge)
            let negImpF = theorem prop_calculus (neg ==> F) [ Ident ceq |> apply_left; Taut rTh |> apply ]
            Contradiction negImpF
        finally Proof.LogLevel <- saved

    /// `prove_with_log`, quiet.
    let prove_with (sat: #ISatBackend) (goal: Prop) : Theorem = prove_with_log sat false goal

    /// Prove `goal`, resolving the solver from the `SYLVIA_CADICAL` environment variable or
    /// `cadical.exe` on PATH. Use `prove_with` to point at a specific executable or set a timeout.
    let prove (goal: Prop) : Theorem = prove_with (Cadical()) goal

    /// `prove_with`, reporting failure as a message instead of raising — for callers deciding
    /// whether the SAT route applies. The message distinguishes a non-theorem from an unavailable
    /// or undecided solver.
    let try_prove_with (sat: #ISatBackend) (goal: Prop) : Result<Theorem, string> =
        try Ok(prove_with sat goal) with e -> Error(e.Message.Split('\n').[0])

    /// `try_prove_with` with the solver resolved from `SYLVIA_CADICAL` / PATH.
    let try_prove (goal: Prop) : Result<Theorem, string> = try_prove_with (Cadical()) goal

    /// The proof as a REWRITE, for use as a step inside a larger proof: replaces `goal` with `T`.
    ///
    ///     theorem prop_calculus (… subgoal …) [ … ; SatProof.Sat_with sat subgoal |> apply_left ; … ]
    ///
    /// (`SatProof.prove_with sat goal |> Taut |> apply` is the same thing spelled out.)
    let Sat_with (sat: #ISatBackend) (goal: Prop) : Rule = Taut (prove_with sat goal)

    /// `Sat_with` with the solver resolved from `SYLVIA_CADICAL` / PATH.
    let Sat (goal: Prop) : Rule = Taut (prove goal)

    (* ---------------------------------------------------------------------- *)
    (* Registration with PropCalculus                                          *)
    (* ---------------------------------------------------------------------- *)

    /// Register this backend as `PropCalculus.decide`'s decider, so goals past
    /// `decide_max_anf_atoms` are proved by SAT refutation instead of by the exponential in-kernel
    /// prover, and goals past `autoproof_max_atoms` are proved instead of failing fast.
    ///
    /// Installing also covers the in-kernel route FAILING below those thresholds: `decide` re-asks the
    /// `valid` oracle when `autoproof_anf` refuses, and hands a genuine theorem to this backend rather
    /// than propagating the refusal. That matters because the in-kernel route is bounded by
    /// `autoproof_max_steps` as well as by atom count.
    ///
    /// The kernel cannot reference a solver, so the dependency is inverted: this assembly registers
    /// itself. `decide` re-checks that what comes back is a theorem of the goal it asked about, so
    /// installing does not widen the trusted base — and the theorem is a real kernel-checked
    /// derivation either way.
    ///
    /// Installing is explicit rather than automatic (no module initializer): a caller that has not
    /// asked for the SAT route keeps the previous, solver-free behaviour, and `uninstall` restores
    /// it. Idempotent.
    let install_with (sat: #ISatBackend) : unit = PropCalculus.prop_decider <- Some(prove_with sat)

    /// `install_with`, resolving the solver from `SYLVIA_CADICAL` / PATH.
    let install () : unit = install_with (Cadical())

    /// Restore `PropCalculus.decide`'s solver-free fallback (`autoproof_anf`, atom- and step-capped).
    let uninstall () : unit = PropCalculus.prop_decider <- None
