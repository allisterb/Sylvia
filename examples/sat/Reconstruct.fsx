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
let clausesOf (goal:Prop) (cnfProp:Prop) : CnfProblem =
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
let transEq (p1:Theorem) (p2:Theorem) : Theorem =
    match p1.Stmt, p2.Stmt with
    | Equals(x,_), Equals(_,z) ->
        theorem prop_calculus (Prop(expand_as<bool> x) == Prop(expand_as<bool> z)) [ Ident p1 |> apply_left; Ident p2 |> apply_left ]
    | _ -> failwith "transEq"

let mutable failures = 0
let ok label cond = (if not cond then failures <- failures + 1); printfn "  %s  %s" (if cond then "✓" else "✗") label

// ---- generic implication plumbing (reused trusted lemmas only) ----------------------------------
let conj (t1:Theorem) (t2:Theorem) (x:Prop) (y:Prop) : Theorem =           // ⊢x, ⊢y ⟼ ⊢x∧y
    theorem prop_calculus (x * y) [ Taut t1 |> apply_left; Taut t2 |> apply_right; reduce |> apply ]
let mp (factP:Theorem) (impl:Theorem) (pP:Prop) (qQ:Prop) : Theorem =      // ⊢P, ⊢(P⇒Q) ⟼ ⊢Q
    theorem prop_calculus qQ [ ident_conseq_true qQ |> Commute |> apply
                               Taut factP |> Commute |> apply_left; Taut impl |> apply ]
let elimR (x:Prop) (y:Prop) : Theorem =                                    // (x∧y) ⇒ y
    theorem prop_calculus (x * y ==> y) [ commute_and x y; strengthen_and y x |> Taut |> apply ]
let rec conjElim (cs:Prop list) (k:int) : Theorem =                        // (∧cs) ⇒ cs[k]
    match cs with
    | [c] -> reflex_implies c
    | head::tail ->
        let rest = tail |> List.reduceBack (*)
        if k = 0 then strengthen_and head rest
        elif List.length tail = 1 then elimR head rest
        else Calc.chainImp (elimR head rest) (conjElim tail (k-1))

// ---- one binary resolution → cp(apos) ∧ cp(aneg) ⇒ cp(resolvent) --------------------------------
let acEq (l:Prop) (r:Prop) : Rule = ident prop_calculus (l == r) [ simp ]   // AC clause equality (no merge)
let resolveStep (cnf:CnfProblem) (h1:int list) (h2:int list) =
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

// ---- STEP 1: assemble R : (∧ inputs) ⇒ F  from the LRAT trace -----------------------------------
let refute (cnf:CnfProblem) (steps:LratStep list) : Prop * Theorem option =
    let inputs = cnf.Clauses |> List.map (clauseProp cnf)
    let A = inputs |> List.reduceBack (*)
    let lits = System.Collections.Generic.Dictionary<int,int list>()
    let imp = System.Collections.Generic.Dictionary<int,Theorem>()
    cnf.Clauses |> List.iteri (fun i c -> lits.[i+1] <- c; imp.[i+1] <- conjElim inputs i)
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
        | Add(id, cl, _) -> lits.[id] <- cl   // non-binary RUP step (rare) — not folded here
        | Delete _ -> ()
    A, r

// ---- STEP 2: ¬φ = A via Cnf.toCnf (scalable), then ¬φ ⇒ F, then Contradiction ⟹ ⊢ φ ------------
let reconstruct (goal:Prop) : Theorem =
    let neg = !!goal
    let (cnfProp, cnfPf) = Cnf.toCnf neg                         // ¬φ == cnfProp  (kernel proof, no atom ceiling)
    let cnf = clausesOf goal cnfProp                             // DIMACS clauses read off that CNF
    let res = sat.Prove goal
    if res.Status <> Unsat then failwith "goal not valid (¬φ satisfiable)"
    let A, rOpt = refute cnf (parseLrat res.Lrat)
    let rTh = match rOpt with Some t -> t | None -> failwith "no binary empty-clause derivation"
    let bridge = theorem prop_calculus (cnfProp == A) [ normalize ]   // AC: same clauses, reassociated
    let ceq = transEq cnfPf bridge                              // ¬φ == A
    let negImpF = theorem prop_calculus (neg ==> F) [ Ident ceq |> apply_left; Taut rTh |> apply ]
    Contradiction negImpF

let p, q, r, s, t = boolvar "p", boolvar "q", boolvar "r", boolvar "s", boolvar "t"
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

printfn "\n%s  (%d failed)" (if failures = 0 then "ALL GREEN" else "FAILURES") failures
