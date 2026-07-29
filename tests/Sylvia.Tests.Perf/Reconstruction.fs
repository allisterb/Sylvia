namespace Sylvia.Tests.Perf

open FSharp.Quotations

open Sylvia
open Formula
open PropCalculus
open Sylvia.SAT

/// SAT-reconstruction payload for profiling: the full LRAT → kernel-checked `⊢ φ`
/// pipeline of examples/sat/Reconstruct.fsx (keep the plumbing in sync with that
/// script), but with the CaDiCaL call replaced by an in-process synthesized
/// refutation — the implication-chain CNFs are unit-propagatable, so the binary
/// resolution trace can be generated directly. That makes the payload hermetic
/// (no external solver, no I/O) and the profile shows only Sylvia-side cost:
/// Cnf.toCnf, conjElimAll/Calc.chainImp, resolve folding, normalize, Contradiction.
module Reconstruction =
    let private pnot (x: Prop) : Prop = Prop <@ not %x.Expr @>

    // ---- plumbing copied from examples/sat/Reconstruct.fsx --------------------------------------

    // Extract a CnfProblem directly from a clean CNF Prop (so it matches `Cnf.toCnf`'s equivalence proof).
    let private clausesOf (goal:Prop) (cnfProp:Prop) : CnfProblem =
        let atoms = System.Collections.Generic.List<Expr>()
        let varOf (e:Expr) =
            let mutable f = -1
            for i in 0 .. atoms.Count-1 do if f < 0 && sequal atoms.[i] e then f <- i
            if f < 0 then atoms.Add e; atoms.Count else f + 1
        let litOf e = match e with Not a -> -(varOf a) | _ -> varOf e
        let rec lits e = match e with Or(x,y) -> lits x @ lits y | _ -> [litOf e]
        let rec cls e  = match e with And(x,y) -> cls x @ cls y | _ -> [lits e]
        let clauses = cls (expand cnfProp.Expr)
        let aov = System.Collections.Generic.Dictionary<int,Prop>()
        atoms |> Seq.iteri (fun i a -> aov.[i+1] <- Prop(expand_as<bool> a))
        { NumVars = atoms.Count; Clauses = clauses
          AtomOfVar = aov :> System.Collections.Generic.IReadOnlyDictionary<_,_>; Goal = goal }

    // (x==y),(y==z) ⟼ (x==z)
    let private transEq (p1:Theorem) (p2:Theorem) : Theorem =
        match p1.Stmt, p2.Stmt with
        | Equals(x,_), Equals(_,z) ->
            theorem prop_calculus (Prop(expand_as<bool> x) == Prop(expand_as<bool> z)) [ Ident p1 |> apply_left; Ident p2 |> apply_left ]
        | _ -> failwith "transEq"

    // ---- generic implication plumbing (reused trusted lemmas only) ------------------------------
    let private conj (t1:Theorem) (t2:Theorem) (x:Prop) (y:Prop) : Theorem =       // ⊢x, ⊢y ⟼ ⊢x∧y
        theorem prop_calculus (x * y) [ Taut t1 |> apply_left; Taut t2 |> apply_right; reduce |> apply ]
    let private mp (factP:Theorem) (impl:Theorem) (pP:Prop) (qQ:Prop) : Theorem =  // ⊢P, ⊢(P⇒Q) ⟼ ⊢Q
        theorem prop_calculus qQ [ ident_conseq_true qQ |> Commute |> apply
                                   Taut factP |> Commute |> apply_left; Taut impl |> apply ]
    let private elimR_impl (x:Prop) (y:Prop) : Theorem =                           // (x∧y) ⇒ y
        theorem prop_calculus (x * y ==> y) [ commute_and x y; strengthen_and y x |> Taut |> apply ]
    let private elimR = Memo.p2 elimR_impl

    /// `A ⇒ Cᵢ` for every input clause, in ONE O(n) pass sharing the peel-chain `A ⇒ rest_j`.
    /// This is the Calc.chainImp hot spot — O(n) chainImp calls over the big conjunction A.
    let conjElimAll (inputs:Prop list) : Theorem[] =
        let arr = Array.ofList inputs
        let n = arr.Length
        let rest j = arr.[j..] |> Array.reduceBack (fun a b -> a * b)              // C_j ∧ … ∧ C_{n-1}
        if n = 1 then [| reflex_implies arr.[0] |]
        else
            let aToRest = Array.zeroCreate n                                       // aToRest.[j] : A ⇒ rest_j
            aToRest.[1] <- elimR arr.[0] (rest 1)
            for j in 2 .. n-1 do aToRest.[j] <- Calc.chainImp aToRest.[j-1] (elimR arr.[j-1] (rest j))
            Array.init n (fun i ->
                if i = 0 then strengthen_and arr.[0] (rest 1)                      // A ⇒ C0
                elif i = n-1 then aToRest.[n-1]                                    // A ⇒ rest_{n-1} = A ⇒ C_{n-1}
                else Calc.chainImp aToRest.[i] (strengthen_and arr.[i] (rest (i+1))))

    // ---- one binary resolution → cp(apos) ∧ cp(aneg) ⇒ cp(resolvent) ----------------------------
    let private acEq (l:Prop) (r:Prop) : Rule = ident prop_calculus (l == r) [ simp ]   // AC clause equality (no merge)
    let private resolveStep (cnf:CnfProblem) (h1:int list) (h2:int list) =
        let pivot = h1 |> List.pick (fun l -> if List.contains (-l) h2 then Some (abs l) else None)
        let apos, aneg = if List.contains pivot h1 then h1, h2 else h2, h1
        let cL = apos |> List.filter (fun l -> l <> pivot)
        let dL = aneg |> List.filter (fun l -> l <> -pivot)
        let resolvent = (cL @ dL) |> List.distinct
        let cp lits = clauseProp cnf lits
        let C, D, v = cp cL, cp dL, cnf.AtomOfVar.[pivot]
        resolvent, apos, aneg,
        theorem prop_calculus (cp apos * cp aneg ==> cp resolvent) [
            acEq (cp apos) (C + v) |> at [left_branch; left_branch]
            acEq (cp aneg) (-v + D) |> at [left_branch; right_branch]
            acEq (cp resolvent) (C + D) |> at [right_branch]
            resolve C D v |> Taut |> apply ]

    // ---- STEP 1: assemble R : (∧ inputs) ⇒ F from the (synthesized) trace ------------------------
    let private refute (cnf:CnfProblem) (steps:LratStep list) : Prop * Theorem option =
        let inputs = cnf.Clauses |> List.map (clauseProp cnf)
        let A = inputs |> List.reduceBack (*)
        let lits = System.Collections.Generic.Dictionary<int,int list>()
        let imp = System.Collections.Generic.Dictionary<int,Theorem>()
        let elims = conjElimAll inputs
        cnf.Clauses |> List.iteri (fun i c -> lits.[i+1] <- c; imp.[i+1] <- elims.[i])
        let mutable r = None
        for step in steps do
            match step with
            | Add(id, cl, [h1; h2]) ->
                let resolvent, apos, aneg, sTh = resolveStep cnf lits.[h1] lits.[h2]
                lits.[id] <- cl
                let impPos = if apos = lits.[h1] then imp.[h1] else imp.[h2]
                let impNeg = if aneg = lits.[h1] then imp.[h1] else imp.[h2]
                let cPos, cNeg = clauseProp cnf apos, clauseProp cnf aneg
                let both = conj impPos impNeg (A ==> cPos) (A ==> cNeg)
                let aToBoth = mp both (combine_implies A cPos cNeg) ((A ==> cPos) * (A ==> cNeg)) (A ==> (cPos * cNeg))
                imp.[id] <- Calc.chainImp aToBoth sTh
                if List.isEmpty cl then r <- Some imp.[id]
            | Add(id, cl, _) -> lits.[id] <- cl
            | Delete _ -> ()
        A, r

    // ---- synthesized refutation (replaces CaDiCaL) ----------------------------------------------

    /// Binary-resolution refutation of a unit-propagatable CNF (the chain CNFs here):
    /// start from the positive unit clause and resolve it through the clause containing
    /// its negation until the empty clause. Same `LratStep` shape CaDiCaL would emit.
    let private synth_unit_refutation (clauses: int list list) : LratStep list =
        let byId = clauses |> List.mapi (fun i c -> i + 1, c)
        let mutable nextId = clauses.Length + 1
        let mutable uid, ulit = byId |> List.pick (fun (id, c) -> match c with | [l] when l > 0 -> Some(id, l) | _ -> None)
        let steps = ResizeArray()
        let mutable running = true
        while running do
            let cid, c = byId |> List.find (fun (_, c) -> List.contains (-ulit) c)
            match c |> List.filter (fun l -> l <> -ulit) with
            | [] -> steps.Add(Add(nextId, [], [uid; cid])); running <- false
            | [l'] -> steps.Add(Add(nextId, [l'], [uid; cid])); uid <- nextId; ulit <- l'
            | _ -> failwith "CNF is not a unit-propagatable chain"
            nextId <- nextId + 1
        List.ofSeq steps

    // ---- STEP 2 + driver: ¬φ = A via Cnf.toCnf, then ¬φ ⇒ F, then Contradiction ⟹ ⊢ φ -----------
    let reconstruct (goal:Prop) : Theorem =
        let neg = !!goal
        let (cnfProp, cnfPf) = Cnf.toCnf neg                        // ¬φ == cnfProp (kernel proof)
        let cnf = clausesOf goal cnfProp
        let steps = synth_unit_refutation cnf.Clauses
        let A, rOpt = refute cnf steps
        let rTh = match rOpt with Some t -> t | None -> failwith "no empty-clause derivation"
        let bridge = theorem prop_calculus (cnfProp == A) [ normalize ]  // AC: same clauses, reassociated
        let ceq = transEq cnfPf bridge                              // ¬φ == A
        let negImpF = theorem prop_calculus (neg ==> F) [ Ident ceq |> apply_left; Taut rTh |> apply ]
        Contradiction negImpF

    /// n-atom implication chain: (x1⇒x2) ∧ … ∧ (x_{n-1}⇒x_n) ⇒ (x1⇒x_n).
    let chain_goal (atoms: int) : Prop =
        let vars = List.init atoms (fun i -> boolvar (sprintf "x%d" (i + 1)) :> Prop)
        let links = vars |> List.pairwise |> List.map (fun (a, b) -> a ==> b)
        (links |> List.reduce (*)) ==> (List.head vars ==> List.last vars)

    /// Reconstruct the n-atom chain and check the produced statement matches the goal.
    let reconstruct_chain (atoms: int) : Theorem =
        let goal = chain_goal atoms
        let th = reconstruct goal
        if not (sequal th.Stmt (expand goal.Expr)) then failwith "reconstruction produced the wrong statement"
        th

    /// Isolated Calc.chainImp hot spot: just the conjElimAll peel over n chain clauses.
    let conj_elim_all (n: int) : Theorem[] =
        let clauses = List.init n (fun i -> (boolvar (sprintf "c%d" (i + 1)) :> Prop) + pnot (boolvar (sprintf "d%d" (i + 1))))
        conjElimAll clauses

/// DENSE reconstruction payloads — pigeonhole, for profiling the cost that the chain payloads
/// above cannot show.
///
/// Two things make this different from `Reconstruction`, and both are deliberate.
///
/// **It runs the real library.** `Reconstruction` above carries its own copy of the replay
/// plumbing, which handles only binary `Add(id, cl, [h1; h2])` steps and synthesizes a
/// unit-propagation refutation. Pigeonhole's refutation is neither binary nor unit-propagatable,
/// and profiling a copy profiles the wrong code anyway, so this module calls `Sylvia.Prover.SAT`
/// (`SatProof.clausesOf` / `refute` / `dedupCnf`) — the same path `examples/sat/Reconstruct.fsx`
/// and `PropCalculus.decide` take.
///
/// **The LRAT trace is canned.** CaDiCaL is not invoked: the traces below were emitted by
/// `cadical -q --lrat --no-binary --plain` against the DIMACS this pipeline produces, and are
/// embedded verbatim. That keeps the payload hermetic (no external process, no PATH dependency —
/// `bin/cadical.exe` needs `msys-2.0.dll`, which a profiler host may not have) and, more
/// importantly, keeps the profile free of solver time: solving pigeonhole 6→5 takes CaDiCaL 27 ms
/// against 100 s of kernel replay, so the replay is the only interesting part.
///
/// `--plain` matters. CaDiCaL's default preprocessing introduces fresh variables and justifies
/// them with RAT steps, which `rupChain` cannot replay; see the `Cadical` doc comment.
///
/// If the clause list ever stops matching the canned trace — a change to `Cnf.toCnf`'s output
/// order, or to `clausesOf`'s variable numbering — the guard below fails with a clear message
/// rather than letting the replay fail obscurely deep inside `refute`. Regenerate by dumping
/// `(sat.Solve cnf).Dimacs` and re-running the cadical command above.
module ReconstructionDense =

    let private pOf (e: Expr) : Prop = Prop(expand_as<bool> e)

    let private transEq (p1: Theorem) (p2: Theorem) : Theorem =
        match p1.Stmt, p2.Stmt with
        | Equals(x, _), Equals(_, z) ->
            theorem prop_calculus (pOf x == pOf z) [ Ident p1 |> apply_left; Ident p2 |> apply_left ]
        | _ -> failwith "transEq: not equalities"

    /// Pigeonhole: `holes + 1` pigeons into `holes` holes, as the NEGATION of an unsatisfiable
    /// conjunction — so the goal is a theorem and its refutation is the dense case. Wide clauses
    /// (each "some hole" clause has `holes` literals) and a superpolynomial resolution proof.
    let php_goal (holes: int) : Prop =
        let p = Array2D.init (holes + 1) holes (fun i j -> boolvar (sprintf "ph%d_%d" i j) :> Prop)
        let someHole = [ for i in 0 .. holes -> [ for j in 0 .. holes - 1 -> p.[i, j] ] |> List.reduce (+) ]
        let noClash =
            [ for j in 0 .. holes - 1 do
                for i in 0 .. holes do
                  for k in i + 1 .. holes do yield !!(p.[i, j] * p.[k, j]) ]
        !!((someHole @ noClash) |> List.reduce ( * ))

    /// pigeonhole 4→3: 12 atoms, 22 clauses, 15 LRAT additions. ~1.4 s — the fast iteration case.
    let private php3_lrat = "\
23 -11 -12 0 22 19 13 16 1 3 6 0\n\
24 -7 -12 0 21 19 6 8 1 2 11 0\n\
25 -12 0 19 21 22 24 3 12 14 1 2 5 0\n\
26 -8 0 25 12 14 16 4 7 9 1 2 17 0\n\
27 -7 0 25 6 8 10 4 13 15 1 2 17 0\n\
28 9 0 27 26 3 0\n\
29 -3 0 28 18 0\n\
30 -6 0 28 20 0\n\
31 -1 0 30 25 5 7 2 4 15 0\n\
32 2 0 31 29 1 0\n\
33 -5 0 32 11 0\n\
34 -11 0 32 13 0\n\
35 4 0 33 30 2 0\n\
36 10 0 34 25 4 0\n\
37 0 35 36 9 0\n"

    /// pigeonhole 5→4: 20 atoms, 45 clauses, 48 LRAT additions. ~12 s — the signal case.
    let private php4_lrat = "\
46 -18 -19 -20 0 35 29 45 39 19 25 1 4 8 0\n\
47 -13 -19 -20 0 34 29 44 39 8 13 1 3 17 0\n\
48 -19 -20 0 45 44 39 29 34 35 47 4 18 23 1 3 7 0\n\
49 -1 -14 -20 0 23 21 44 42 6 7 2 3 30 0\n\
50 -14 -20 0 44 42 39 18 21 23 49 1 26 27 2 3 10 0\n\
51 -3 -13 -20 0 13 11 44 42 26 27 2 3 20 0\n\
52 -13 -20 0 44 42 39 8 11 13 51 1 16 17 2 3 30 0\n\
53 -9 -20 0 45 50 52 4 31 28 42 39 7 10 1 2 16 0\n\
54 -20 0 39 42 44 45 50 52 4 28 31 33 53 3 17 20 1 2 6 0\n\
55 -5 -10 3 0 54 24 23 17 6 11 12 1 5 38 35 4 0\n\
56 -7 -10 3 0 54 24 23 17 31 32 5 9 15 1 4 38 0\n\
57 -10 3 0 54 17 20 23 24 55 56 2 36 41 1 8 9 4 5 35 0\n\
58 -8 -9 3 0 54 14 13 7 36 41 1 18 19 4 5 35 0\n\
59 -2 8 17 5 0 54 16 19 2 5 32 0\n\
60 -19 16 8 13 5 0 32 35 2 4 21 0\n\
61 -9 3 0 54 7 10 13 14 58 59 1 38 60 5 22 25 2 4 31 0\n\
62 -5 16 -11 0 54 34 33 11 12 4 5 25 0\n\
63 16 8 -11 0 54 34 33 30 62 2 21 22 4 5 15 0\n\
64 -6 4 19 3 0 54 16 22 1 5 9 0\n\
65 8 -11 3 0 54 34 30 63 38 64 2 6 12 1 5 19 0\n\
66 -18 -11 3 0 65 41 36 33 19 25 1 4 8 0\n\
67 -11 3 0 54 33 34 65 36 41 66 5 9 15 1 4 18 0\n\
68 -13 -2 8 0 11 15 59 0\n\
69 -2 18 -12 0 54 43 40 16 18 68 4 31 35 2 5 12 0\n\
70 18 3 0 54 61 57 67 3 43 40 37 69 1 6 8 9 60 5 0\n\
71 3 0 57 61 67 3 37 40 43 70 19 22 25 1 6 8 2 4 31 0\n\
72 -7 0 71 26 0\n\
73 -11 0 71 27 0\n\
74 -15 0 71 28 0\n\
75 -19 0 71 29 0\n\
76 -14 8 0 72 54 75 21 25 2 5 12 0\n\
77 8 16 0 74 72 54 75 76 4 11 15 2 5 22 0\n\
78 -13 12 0 73 54 75 13 15 3 5 24 0\n\
79 16 0 74 73 54 75 77 40 78 4 23 25 3 5 14 0\n\
80 -4 0 79 38 0\n\
81 -8 0 79 41 0\n\
82 -12 0 79 43 0\n\
83 -14 0 81 76 0\n\
84 -13 0 82 78 0\n\
85 -17 0 81 72 73 82 12 14 2 3 20 0\n\
86 18 0 85 75 54 5 0\n\
87 -2 0 86 19 0\n\
88 -6 0 86 22 0\n\
89 -10 0 86 24 0\n\
90 5 0 88 72 81 2 0\n\
91 9 0 89 82 73 3 0\n\
92 -1 0 90 6 0\n\
93 0 90 91 10 0\n"

    /// Reconstruct a pigeonhole goal from its canned trace: the whole `SatProof.proveWith`
    /// pipeline minus the solver call. `expectVars`/`expectClauses` pin the shape the trace was
    /// generated against.
    let private reconstruct_canned (goal: Prop) (lrat: string) (expectVars: int) (expectClauses: int) : Theorem =
        let neg = !!goal
        let (cnfProp, cnfPf) = Cnf.toCnf neg                              // ¬φ == cnfProp, kernel-proved
        let cnf = SatProof.clausesOf goal cnfProp
        if cnf.NumVars <> expectVars || List.length cnf.Clauses <> expectClauses then
            failwithf "canned LRAT is stale: clausification now gives %d vars / %d clauses, trace was generated against %d / %d — regenerate it (see the module comment)"
                      cnf.NumVars (List.length cnf.Clauses) expectVars expectClauses
        let A, rOpt = SatProof.refute cnf (parseLrat lrat)                // STEP 1: R : A ⇒ F
        let rTh = match rOpt with
                  | Some t -> t
                  | None -> failwith "canned LRAT never derives the empty clause"
        let (cnfDedup, dedupPf) = SatProof.dedupCnf cnfProp               // STEP 2: ¬φ == A
        let bridge = theorem prop_calculus (cnfDedup == A) [ normalize ]
        let ceq = transEq cnfPf (match dedupPf with Some d -> transEq d bridge | None -> bridge)
        let negImpF = theorem prop_calculus (neg ==> F) [ Ident ceq |> apply_left; Taut rTh |> apply ]
        let th = Contradiction negImpF
        // The payload must prove the GOAL, not merely produce a theorem — a profile of the wrong
        // computation is worse than no profile.
        if not (sequal th.Stmt (expand goal.Expr)) then
            failwith "reconstruction produced the wrong statement"
        th

    /// pigeonhole 4→3 — 12 atoms, 22 clauses, 15 LRAT steps.
    let reconstruct_php_4_3 () : Theorem = reconstruct_canned (php_goal 3) php3_lrat 12 22

    /// pigeonhole 5→4 — 20 atoms, 45 clauses, 48 LRAT steps. The main profiling target.
    let reconstruct_php_5_4 () : Theorem = reconstruct_canned (php_goal 4) php4_lrat 20 45
