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
