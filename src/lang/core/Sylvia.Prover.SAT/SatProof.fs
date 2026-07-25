namespace Sylvia
// A bare rule used as a proof step means "apply to the whole expression"; addressed steps say `at`.
#nowarn "3391"

/// SAT-backed propositional proof: turn a CaDiCaL LRAT refutation of `¬φ` into a **kernel-checked**
/// `⊢ φ`.
///
/// This is Sylvia's scalable, trace-emitting propositional decider. `PropCalculus.autoproof_anf`
/// also emits a checkable trace but is exponential in the number of distinct atoms (guarded at
/// `autoproof_max_atoms = 5`); `PropCalculus.valid` scales but is a yes/no *tool* outside the
/// trusted base that emits no proof. This module has neither limitation: it decides with a
/// state-of-the-art SAT solver and then **replays the solver's resolution refutation as native
/// kernel steps**, so the solver is advisory and the resulting `Theorem` is checked the same way
/// any hand-written proof is.
///
///     goal φ ─Cnf.toCnf→ (¬φ == A, kernel proof) ─clausesOf→ DIMACS ─CaDiCaL→ UNSAT + LRAT
///            ─resolve-fold→ R : A ⇒ F                                     (STEP 1)
///            ─rewrite ¬φ to A, then Contradiction→ ⊢ φ                     (STEP 2)
///
/// Nothing here is a new trusted primitive: every step is `PropCalculus.resolve`,
/// `combine_implies`, `weaken_or`, `strengthen_and`, `Cnf.toCnf` or `Calc.chainImp`. See
/// `docs/prover-sat-reconstruction.md` for the design, and `examples/sat/Reconstruct.fsx` for a
/// runnable demonstration.
///
/// Requires the `cadical` executable: pass one via `proveWith`, or let `Cadical()` resolve the
/// `SYLVIA_CADICAL` environment variable / `cadical.exe` on PATH.
module SatProof =

    open System.Collections.Generic

    open FSharp.Quotations

    open Formula
    open PropCalculus
    open Sylvia.SAT

    let private pOf (e: Expr) : Prop = Prop(expand_as<bool> e)

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
    /// `Calc.chainImp`).
    let conjElimAll (inputs: Prop list) : Theorem[] =
        let arr = Array.ofList inputs
        let n = arr.Length
        let rest j = arr.[j..] |> Array.reduceBack (fun a b -> a * b)
        if n = 1 then [| reflex_implies arr.[0] |]
        else
            let aToRest = Array.zeroCreate n
            aToRest.[1] <- elimR arr.[0] (rest 1)
            for j in 2 .. n - 1 do aToRest.[j] <- Calc.chainImp aToRest.[j - 1] (elimR arr.[j - 1] (rest j))
            Array.init n (fun i ->
                if i = 0 then strengthen_and arr.[0] (rest 1)
                elif i = n - 1 then aToRest.[n - 1]
                else Calc.chainImp aToRest.[i] (strengthen_and arr.[i] (rest (i + 1))))

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
        let cp lits = clauseProp cnf lits
        let eqImp (a: Prop) (b: Prop) =
            if sequal (expand a.Expr) (expand b.Expr) then reflex_implies a
            else theorem prop_calculus (a ==> b) [ acEq b a |> at [ right_branch ]
                                                   reflex_implies a |> Taut |> apply ]
        match dstLits |> List.filter (fun l -> not (List.contains l srcLits)) |> List.distinct with
        | [] -> eqImp (cp srcLits) (cp dstLits)
        | extras -> Calc.chainImp (weaken_or (cp srcLits) (cp extras))
                                  (eqImp (cp srcLits + cp extras) (cp dstLits))

    /// One binary resolution: `cp(apos) ∧ cp(aneg) ⇒ cp(out)`, where the two clauses clash on
    /// variable `pv`. Each clause is AC-matched to `resolve`'s `(C ∨ x)` / `(¬x ∨ D)` shape, so the
    /// resolution itself is instantiated at whole clauses with `C`, `D`, `x` opaque — which is what
    /// keeps a wide-clause replay polynomial.
    let private resolveStep (cnf: CnfProblem) (apos: int list) (aneg: int list) (pv: int) (out: int list) : Theorem =
        let cL = apos |> List.filter (fun l -> l <> pv)
        let dL = aneg |> List.filter (fun l -> l <> -pv)
        let cp lits = clauseProp cnf lits
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
    /// EXACTLY the ones `Cnf.toCnf` proved `¬φ` equal to. (Clausifying twice — once for the proof,
    /// once for the solver — leaves the LRAT clause ids and variable indices meaningless against
    /// each other; they agree on implication chains and diverge elsewhere.)
    ///
    /// Repeated literals within a clause are dropped. That is an OPTIMIZATION, not a correctness
    /// requirement: every kernel step in the replay costs O(|A|), so smaller clauses are cheaper.
    /// `dedupCnf` pays for it by proving `cnfProp == A` in two exact moves instead of one.
    let clausesOf (goal: Prop) (cnfProp: Prop) : CnfProblem =
        let atoms = List<Expr>()
        let varOf (e: Expr) =
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
    let rec dedupCnf (p: Prop) : Prop * Theorem option =
        match expand p.Expr with
        | And(x, y) ->
            let dx, tx = dedupCnf (pOf x)
            let dy, ty = dedupCnf (pOf y)
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
    /// EVERY `Add` step is replayed, binary or not: `SAT.rupChain` unfolds a step's hints into an
    /// explicit chain of binary resolutions (a 2-hint step is simply a one-link chain, so the
    /// ordinary binary case is subsumed rather than special-cased), and the chain's clause is
    /// weakened to the one the step declares. Nothing in the trace is skipped.
    let refute (cnf: CnfProblem) (steps: LratStep list) : Prop * Theorem option =
        let inputs = cnf.Clauses |> List.map (clauseProp cnf)
        let A = inputs |> List.reduceBack (*)
        let lits = Dictionary<int, int list>()
        let imp = Dictionary<int, Theorem>()                        // id ⟼ A ⇒ cp(lits[id])
        let elims = conjElimAll inputs
        cnf.Clauses |> List.iteri (fun i c -> lits.[i + 1] <- c; imp.[i + 1] <- elims.[i])
        let clauseOf id = match lits.TryGetValue id with | true, c -> Some c | _ -> None
        // A ⇒ cp xs  and  A ⇒ cp ys  ⟼  A ⇒ cp out   (one resolution, under the antecedent A)
        let resolveUnder (impX: Theorem) (impY: Theorem) xs ys pv out =
            let apos, aneg = if List.contains pv xs then xs, ys else ys, xs
            let impPos, impNeg = if apos = xs then impX, impY else impY, impX
            let cPos, cNeg = clauseProp cnf apos, clauseProp cnf aneg
            let both = conj impPos impNeg (A ==> cPos) (A ==> cNeg)
            let aToBoth = mp both (combine_implies A cPos cNeg) ((A ==> cPos) * (A ==> cNeg)) (A ==> (cPos * cNeg))
            Calc.chainImp aToBoth (resolveStep cnf apos aneg pv out)
        let mutable r = None
        for step in steps do
            match step with
            | Delete _ -> ()
            | Add(id, cl, hints) ->
                match rupChain clauseOf cl hints with
                | Error e -> failwithf "SatProof: LRAT step %d: %s" id e
                | Ok chain ->
                    let mutable cur = lits.[chain.Start]
                    let mutable curImp = imp.[chain.Start]
                    for link in chain.Links do
                        curImp <- resolveUnder curImp imp.[link.Antecedent] cur lits.[link.Antecedent] link.Pivot link.Result
                        cur <- link.Result
                    lits.[id] <- cl
                    imp.[id] <- if cur = cl then curImp else Calc.chainImp curImp (clauseImp cnf cur cl)
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
    let proveWithLog (sat: Cadical) (verbose: bool) (goal: Prop) : Theorem =
        let saved = Proof.LogLevel
        try
            if not verbose then Proof.LogLevel <- 0
            let neg = !!goal
            let (cnfProp, cnfPf) = Cnf.toCnf neg                    // ¬φ == cnfProp, kernel-proved
            let cnf = clausesOf goal cnfProp
            let res = sat.Solve cnf
            // Distinguish "this is not a theorem" from "the solver could not tell us": a caller
            // deciding whether to fall back to another tactic needs to know which happened.
            match res.Status with
            | Unsat -> ()
            | Sat ->
                failwithf "SatProof: %s is NOT a theorem (¬goal is satisfiable)"
                    (prop_calculus.PrintFormula (expand goal.Expr))
            | NotAvailable ->
                failwithf "SatProof: the cadical executable was not found at '%s' — set the SYLVIA_CADICAL environment variable, put cadical.exe on PATH, or pass a configured Cadical to proveWith" sat.ExePath
            | Timeout -> failwithf "SatProof: the solver hit the %dms budget without deciding the goal" sat.TimeoutMs
            | Unknown -> failwith "SatProof: the solver exited without a verdict"
            let A, rOpt = refute cnf (parseLrat res.Lrat)
            let rTh =
                match rOpt with
                | Some t -> t
                | None -> failwith "SatProof: the LRAT trace never derives the empty clause"
            // ¬φ == A in two exact moves: clause-wise literal dedup (congruence), then pure-AC
            // reassociation of the same clause multiset into the right-associated `A`.
            let (cnfDedup, dedupPf) = dedupCnf cnfProp
            let bridge = theorem prop_calculus (cnfDedup == A) [ normalize ]
            let ceq = transEq cnfPf (match dedupPf with Some d -> transEq d bridge | None -> bridge)
            let negImpF = theorem prop_calculus (neg ==> F) [ Ident ceq |> apply_left; Taut rTh |> apply ]
            Contradiction negImpF
        finally Proof.LogLevel <- saved

    /// `proveWithLog`, quiet.
    let proveWith (sat: Cadical) (goal: Prop) : Theorem = proveWithLog sat false goal

    /// Prove `goal`, resolving the solver from the `SYLVIA_CADICAL` environment variable or
    /// `cadical.exe` on PATH. Use `proveWith` to point at a specific executable or set a timeout.
    let prove (goal: Prop) : Theorem = proveWith (Cadical()) goal

    /// `proveWith`, reporting failure as a message instead of raising — for callers deciding
    /// whether the SAT route applies. The message distinguishes a non-theorem from an unavailable
    /// or undecided solver.
    let tryProveWith (sat: Cadical) (goal: Prop) : Result<Theorem, string> =
        try Ok(proveWith sat goal) with e -> Error(e.Message.Split('\n').[0])

    /// `tryProveWith` with the solver resolved from `SYLVIA_CADICAL` / PATH.
    let tryProve (goal: Prop) : Result<Theorem, string> = tryProveWith (Cadical()) goal

    /// The proof as a REWRITE, for use as a step inside a larger proof: replaces `goal` with `T`.
    ///
    ///     theorem prop_calculus (… subgoal …) [ … ; SatProof.SatWith sat subgoal |> apply_left ; … ]
    ///
    /// (`SatProof.proveWith sat goal |> Taut |> apply` is the same thing spelled out.)
    let SatWith (sat: Cadical) (goal: Prop) : Rule = Taut (proveWith sat goal)

    /// `SatWith` with the solver resolved from `SYLVIA_CADICAL` / PATH.
    let Sat (goal: Prop) : Rule = Taut (prove goal)
