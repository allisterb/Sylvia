#load "../proofs/Include.fsx"
#r "../../src/lang/solvers/Sylvia.Solver.CaDiCaL/bin/Debug/net10.0/Sylvia.Solver.CaDiCaL.dll"

// FULL reconstruction loop: turn a CaDiCaL LRAT refutation of ¬φ into a kernel-checked `⊢ φ`.
//
//   goal φ ─Cnf.toCnf→ (¬φ = A, kernel proof) ─clausesOf→ DIMACS ─CaDiCaL→ UNSAT + LRAT
//          ─resolve-fold→ R : A ⇒ F                              (STEP 1 — scales, no atom ceiling)
//          ─¬φ = A, chain, Contradiction→ ⊢ φ                    (STEP 2 — now also scales)
//
// STEP 1 is the fold: each binary LRAT step becomes `PropCalculus.resolve` (AC-matched to the
// canonical clause shapes with `simp`), threaded through the input conjunction with `combine_implies`
// + `Calc.chainImp`. Kernel-checked at every step, no atom-count ceiling.
//
// STEP 2's CNF-equivalence `¬φ = A` now uses `Cnf.toCnf` — a structural recursive CNF converter that
// emits a kernel-checked `¬φ == cnf` proof whose cost is bounded by the CNF size, NOT by an
// atom-count exponential. So the end-to-end `⊢ φ` no longer has the old ≤5-atom ceiling (the 6-atom
// goal below reconstructs). `Cnf.toCnf` is both the clausifier and the equivalence proof; `normalize`
// bridges its CNF to the reconstruction's right-associated conjunction A. (It is not fast — the kernel
// proof assembly is the bottleneck — but it is unbounded in atom count.)
//
// Run:  dotnet fsi examples/sat/Reconstruct.fsx      (requires cadical)

open FSharp.Quotations
open Sylvia
open Formula
open PropCalculus
open Sylvia.SAT

Proof.LogLevel <- 0
let sat = Cadical(exePath = @"C:\Projects\Sylvia\bin\cadical.exe", timeoutMs = 20000)

// Extract a CnfProblem directly from a clean CNF Prop (so it matches `Cnf.toCnf`'s equivalence proof).
// Repeated literals within a clause are dropped: `Cnf.toCnf`'s distribution readily produces them
// (Peirce's law gives a `p ∨ p`). This is an OPTIMIZATION, not a correctness requirement — smaller
// clauses mean a smaller input conjunction `A`, and every kernel step in the replay costs O(|A|)
// (measured: `∨` over `∧` 12.1 s → 7.7 s, xor commutativity 26.8 s → 16.6 s, 12-atom chain
// 2.6 s → 1.9 s). It WAS load-bearing until `absorb_or` and its siblings were given exact rewrite
// addresses; a `p ∨ p` inside `A` used to mis-target the `idemp_or` step those derivations search
// for, which `strengthen_and` — and hence `conjElimAll` — is built on. `dedupCnf` pays for it by
// proving `cnfProp == A` in two exact moves instead of one.
let clausesOf (goal:Prop) (cnfProp:Prop) : CnfProblem =
    let atoms = System.Collections.Generic.List<Expr>()
    let varOf (e:Expr) =
        let mutable f = -1
        for i in 0 .. atoms.Count-1 do if f < 0 && sequal atoms.[i] e then f <- i
        if f < 0 then atoms.Add e; atoms.Count else f + 1
    let litOf e = match e with Not a -> -(varOf a) | _ -> varOf e
    let rec lits e = match e with Or(x,y) -> lits x @ lits y | _ -> [litOf e]
    let rec cls e  = match e with And(x,y) -> cls x @ cls y | _ -> [lits e]
    let clauses = cls (expand cnfProp.Expr) |> List.map List.distinct
    let aov = System.Collections.Generic.Dictionary<int,Prop>()
    atoms |> Seq.iteri (fun i a -> aov.[i+1] <- Prop(expand_as<bool> a))
    { NumVars = atoms.Count; Clauses = clauses
      AtomOfVar = aov :> System.Collections.Generic.IReadOnlyDictionary<_,_>; Goal = goal }

// (x==y),(y==z) ⟼ (x==z)
let transEq (p1:Theorem) (p2:Theorem) : Theorem =
    match p1.Stmt, p2.Stmt with
    | Equals(x,_), Equals(_,z) ->
        theorem prop_calculus (Prop(expand_as<bool> x) == Prop(expand_as<bool> z)) [ Ident p1 |> apply_left; Ident p2 |> apply_left ]
    | _ -> failwith "transEq"

// Rewrite every clause of a CNF to its literal-DEDUPED form, in place, by congruence: each clause
// equality is a small local `simp` (idempotence collapses the repeat) and is lifted through the ∧
// tree at an EXACT position, so nothing searches and nothing can mis-target. Returns `None` when
// no clause had a repeated literal. This is what keeps the input conjunction `A` — which the whole
// refutation is threaded through — free of the `p ∨ p` clauses `Cnf.toCnf`'s distribution produces.
let rec dedupCnf (p:Prop) : Prop * Theorem option =
    let pOf (e:Expr) = Prop(expand_as<bool> e)
    match expand p.Expr with
    | And(x, y) ->
        let dx, tx = dedupCnf (pOf x)
        let dy, ty = dedupCnf (pOf y)
        match tx, ty with
        | None, None -> p, None
        | _ ->
            let steps =
                [ match tx with Some t -> yield Ident t |> at [left_branch; left_branch] | None -> ()
                  match ty with Some t -> yield Ident t |> at [left_branch; right_branch] | None -> () ]
            (dx * dy), Some(theorem prop_calculus ((pOf x * pOf y) == (dx * dy)) steps)
    | clause ->
        let rec lits e = match e with Or(a, b) -> lits a @ lits b | _ -> [pOf e]
        let ls = lits clause
        let kept = ls |> List.fold (fun acc l -> if acc |> List.exists (fun (k:Prop) -> sequal (expand k.Expr) (expand l.Expr)) then acc else acc @ [l]) []
        if List.length kept = List.length ls then p, None
        else
            let d = kept |> List.reduce (+)
            d, Some(theorem prop_calculus (p == d) [ simp ])

let mutable failures = 0
let ok label cond = (if not cond then failures <- failures + 1); printfn "  %s  %s" (if cond then "✓" else "✗") label

// ---- generic implication plumbing (reused trusted lemmas only) ----------------------------------
let conj (t1:Theorem) (t2:Theorem) (x:Prop) (y:Prop) : Theorem =           // ⊢x, ⊢y ⟼ ⊢x∧y
    theorem prop_calculus (x * y) [ Taut t1 |> apply_left; Taut t2 |> apply_right; reduce |> apply ]
let mp (factP:Theorem) (impl:Theorem) (pP:Prop) (qQ:Prop) : Theorem =      // ⊢P, ⊢(P⇒Q) ⟼ ⊢Q
    theorem prop_calculus qQ [ ident_conseq_true qQ |> Commute |> apply
                               Taut factP |> Commute |> apply_left; Taut impl |> apply ]
let elimR_impl (x:Prop) (y:Prop) : Theorem =                               // (x∧y) ⇒ y
    theorem prop_calculus (x * y ==> y) [ commute_and x y; strengthen_and y x |> Taut |> apply ]
let elimR = Memo.p2 elimR_impl
// `A ⇒ Cᵢ` for every input clause, in ONE O(n) pass sharing the peel-chain `A ⇒ rest_j`
// (the naive per-clause `conjElim` was O(n²) in the expensive `Calc.chainImp`). `A = C0 ∧ rest_1`.
let conjElimAll (inputs:Prop list) : Theorem[] =
    let arr = Array.ofList inputs
    let n = arr.Length
    let rest j = arr.[j..] |> Array.reduceBack (fun a b -> a * b)           // C_j ∧ … ∧ C_{n-1}
    if n = 1 then [| reflex_implies arr.[0] |]
    else
        let aToRest = Array.zeroCreate n                                    // aToRest.[j] : A ⇒ rest_j
        aToRest.[1] <- elimR arr.[0] (rest 1)
        for j in 2 .. n-1 do aToRest.[j] <- Calc.chainImp aToRest.[j-1] (elimR arr.[j-1] (rest j))
        Array.init n (fun i ->
            if i = 0 then strengthen_and arr.[0] (rest 1)                   // A ⇒ C0
            elif i = n-1 then aToRest.[n-1]                                 // A ⇒ rest_{n-1} = A ⇒ C_{n-1}
            else Calc.chainImp aToRest.[i] (strengthen_and arr.[i] (rest (i+1))))

// ---- clause shaping: AC equality (with merge/dedup) and subset weakening ------------------------
// `simp` AC-normalizes and now also collapses a repeated operand of a flattened ∨-chain, so this
// closes MERGE resolvents (two clauses sharing a non-pivot literal) as well as plain reorderings.
let acEq (l:Prop) (r:Prop) : Rule = ident prop_calculus (l == r) [ simp ]
// `src ⇒ dst` whenever src's literals are a SUBSET of dst's: weaken by the missing literals
// (Gries 3.76a) and AC-match the rest. Covers the reorder case (no extras) and the LRAT case where
// a step declares a weaker clause than its hint chain actually derives.
let clauseImp (cnf:CnfProblem) (srcLits:int list) (dstLits:int list) : Theorem =
    let cp lits = clauseProp cnf lits
    let eqImp (a:Prop) (b:Prop) =                                          // a ⇒ b, same clause up to AC
        if sequal (expand a.Expr) (expand b.Expr) then reflex_implies a
        else theorem prop_calculus (a ==> b) [ acEq b a |> at [right_branch]
                                               reflex_implies a |> Taut |> apply ]
    match dstLits |> List.filter (fun l -> not (List.contains l srcLits)) |> List.distinct with
    | [] -> eqImp (cp srcLits) (cp dstLits)
    | extras -> Calc.chainImp (weaken_or (cp srcLits) (cp extras)) (eqImp (cp srcLits + cp extras) (cp dstLits))

// ---- one binary resolution → cp(apos) ∧ cp(aneg) ⇒ cp(out), clashing on variable `pv` -----------
let resolveStep (cnf:CnfProblem) (apos:int list) (aneg:int list) (pv:int) (out:int list) : Theorem =
    let cL = apos |> List.filter (fun l -> l <> pv)
    let dL = aneg |> List.filter (fun l -> l <> -pv)
    let cp lits = clauseProp cnf lits
    let C, D, v = cp cL, cp dL, cnf.AtomOfVar.[pv]
    theorem prop_calculus (cp apos * cp aneg ==> cp out) [
        acEq (cp apos) (C + v) |> at [left_branch; left_branch]
        acEq (cp aneg) (-v + D) |> at [left_branch; right_branch]
        acEq (cp out) (C + D) |> at [right_branch]
        resolve C D v |> Taut |> apply ]

// ---- STEP 1: assemble R : (∧ inputs) ⇒ F  from the LRAT trace -----------------------------------
// EVERY `Add` step is replayed, binary or not: `SAT.rupChain` unfolds a step's hints into an
// explicit chain of binary resolutions (a 2-hint step is simply a one-link chain), and the chain's
// clause is weakened to the declared one. Nothing in the trace is skipped.
let refute (cnf:CnfProblem) (steps:LratStep list) : Prop * Theorem option =
    let inputs = cnf.Clauses |> List.map (clauseProp cnf)
    let A = inputs |> List.reduceBack (*)
    let lits = System.Collections.Generic.Dictionary<int,int list>()
    let imp = System.Collections.Generic.Dictionary<int,Theorem>()          // id ⟼ A ⇒ cp(lits[id])
    let elims = conjElimAll inputs
    cnf.Clauses |> List.iteri (fun i c -> lits.[i+1] <- c; imp.[i+1] <- elims.[i])
    let clauseOf id = match lits.TryGetValue id with | true, c -> Some c | _ -> None
    // A ⇒ cp xs  and  A ⇒ cp ys  ⟼  A ⇒ cp out   (one resolution, under the antecedent A)
    let resolveUnder (impX:Theorem) (impY:Theorem) xs ys pv out =
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
            | Error e -> failwithf "LRAT step %d: %s" id e
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

// ---- STEP 2: ¬φ = A via Cnf.toCnf (scalable), then ¬φ ⇒ F, then Contradiction ⟹ ⊢ φ ------------
let reconstruct (goal:Prop) : Theorem =
    let neg = !!goal
    let (cnfProp, cnfPf) = Cnf.toCnf neg                         // ¬φ == cnfProp  (kernel proof, no atom ceiling)
    let cnf = clausesOf goal cnfProp                             // DIMACS clauses read off that CNF
    // Solve THAT clause list, not a separately-derived one: the LRAT clause ids and variable
    // indices are only meaningful against the exact DIMACS we hand the solver. (`sat.Prove goal`
    // would re-clausify with `cnfOfNegatedGoal`, whose clause order, literal order and
    // tautology-dropping need not agree with `Cnf.toCnf`'s.)
    let res = sat.Solve cnf
    if res.Status <> Unsat then failwithf "goal not proved: ¬φ is %A" res.Status
    let A, rOpt = refute cnf (parseLrat res.Lrat)
    let rTh = match rOpt with Some t -> t | None -> failwith "no binary empty-clause derivation"
    // ¬φ == A in two exact moves: clause-wise literal dedup (congruence), then pure-AC
    // reassociation of the same clause multiset into the right-associated `A`.
    let (cnfDedup, dedupPf) = dedupCnf cnfProp
    let bridge = theorem prop_calculus (cnfDedup == A) [ normalize ]
    let ceq = transEq cnfPf (match dedupPf with Some d -> transEq d bridge | None -> bridge)  // ¬φ == A
    let negImpF = theorem prop_calculus (neg ==> F) [ Ident ceq |> apply_left; Taut rTh |> apply ]
    Contradiction negImpF

let p, q, r, s, t = boolvar "p", boolvar "q", boolvar "r", boolvar "s", boolvar "t"
let u, v, w = boolvar "u", boolvar "v", boolvar "w"
let x, y, z, a = boolvar "x", boolvar "y", boolvar "z", boolvar "a"
let check label (goal:Prop) =
    try
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let th = reconstruct goal
        sw.Stop()
        printfn "  %s :  ⊢ %s   (%dms)" label (prop_calculus.PrintFormula th.Stmt) sw.ElapsedMilliseconds
        ok label (sequal th.Stmt (expand goal.Expr))
    with e -> ok label false; printfn "      %s" (e.Message.Split('\n').[0])

printfn "Reconstructing ⊢ φ from CaDiCaL LRAT refutations (kernel-checked end to end):"
check "excluded middle  p ∨ ¬p"           (p + !!p)
check "Peirce  ((p⇒q)⇒p)⇒p"               (((p ==> q) ==> p) ==> p)
check "chain  (p⇒q)∧(q⇒r) ⇒ (p⇒r)"        (((p ==> q) * (q ==> r)) ==> (p ==> r))
// 5 atoms — past the old autoproof_anf ceiling of 5 (slow, but it closes):
check "5-atom chain"                      ((p ==> q) * (q ==> r) * (r ==> s) * (s ==> t) ==> (p ==> t))
// 8 atoms — the scaling benchmark from docs/prover-sat-reconstruction.md (142 s pre-optimization):
check "8-atom chain"                      ((p ==> q) * (q ==> r) * (r ==> s) * (s ==> t) * (t ==> u) * (u ==> v) * (v ==> w) ==> (p ==> w))
// 12 atoms — well past anything measured pre-optimization:
check "12-atom chain"                     ((p ==> q) * (q ==> r) * (r ==> s) * (s ==> t) * (t ==> u) * (u ==> v) * (v ==> w) * (w ==> x) * (x ==> y) * (y ==> z) * (z ==> a) ==> (p ==> a))

// Goals whose refutations are NOT plain binary chains — these exercise the two gaps that used to
// make the replay give up (see docs/prover-sat-reconstruction.md §7):
//   * merge resolution — the two resolved clauses share a NON-pivot literal, so the resolvent has
//     a duplicate that only a dedup-capable clause normalizer can discharge;
//   * non-binary RUP steps — a step with 1, or 3+, hints, unfolded into a resolution chain.
printfn "\nDenser refutations (merge resolvents + non-binary RUP steps):"
check "merge  (p∨q)∧(¬p∨q) ⇒ q"           (((p + q) * (!!p + q)) ==> q)
check "3-var all-8-clause refutation"     (!!((p+q+r) * (!!p+q+r) * (p+ !!q+r) * (!!p+ !!q+r) * (p+q+ !!r) * (!!p+q+ !!r) * (p+ !!q+ !!r) * (!!p+ !!q+ !!r)))
check "resolution chain to s"             (((p + q) * (!!q + r) * (!!r + s) * (!!p + s)) ==> s)
check "∨ distributes over ∧"              ((p * (q + r)) == ((p * q) + (p * r)))
// `≢` is handled by Cnf.toCnf (via Gries 3.10) — but each one doubles the CNF under direct
// distribution, so nested xor is where the clausifier's exponential-in-FORMULA-SIZE blowup bites
// (xor associativity: 441 clauses). Tseitin encoding is the fix; see the design notes.
check "xor commutes  (p≢q) ≡ (q≢p)"       ((p != q) == (q != p))
check "pigeonhole 3→2"                    (!!((p+q) * (r+s) * (t+u) * (!!p + !!r) * (!!q + !!s) * (!!p + !!t) * (!!q + !!u) * (!!r + !!t) * (!!s + !!u)))
check "≡ chain  (p≡q)∧(q≡r)∧(r≡s) ⇒ p≡s"  (((p == q) * (q == r) * (r == s)) ==> (p == s))

printfn "\n%s  (%d failed)" (if failures = 0 then "ALL GREEN" else "FAILURES") failures
