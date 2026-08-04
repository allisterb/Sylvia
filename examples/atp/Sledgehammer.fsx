#load "../proofs/Include.fsx"
#r "../../src/lang/atp/Sylvia.ATP.E/bin/Debug/net10.0/Sylvia.ATP.E.dll"
#r "../../src/lang/solvers/Sylvia.Solver.CaDiCaL/bin/Debug/net10.0/Sylvia.Solver.CaDiCaL.dll"
#r "../../src/lang/core/Sylvia.Prover.SAT/bin/Debug/net10.0/Sylvia.Prover.SAT.dll"

// A Sledgehammer-style loop: use the E prover as an ADVISORY oracle to (a) confirm a goal is provable
// from a body of named Sylvia lemmas and (b) select the RELEVANT few, then RECONSTRUCT a genuine,
// kernel-checked Sylvia proof from exactly those facts. E never enters the trusted base — it only
// filters, confirms, and supplies witness TERMS; Sylvia certifies.
//
// Five reconstruction routes, tried in order:
//   A  propositional      — the residual `(∧ used) ⇒ goal` is a tautology
//   B  ∃-elimination      — goal `(∃x|R:P) ⇒ Q`, via `witness` (9.30)
//   C  ∃-introduction     — goal `∃x|:Q` from GROUND facts, via `exists_intro` (9.28) at E's witness
//   D  ∀-instantiation    — goal `∃x|:Q` from UNIVERSAL facts, via `inst` (9.13) at E's witness
//   E  ∃-elim + ∀-inst    — no ground witness exists: eliminate an existential FACT to a fresh x̂,
//                           then instantiate the universals at x̂
//
// Routes D and E were the standing boundary. They were blocked not by E (which answers in ~40 ms) but
// by Sylvia's own residual prover: the certificates carry the quantified facts as opaque atoms
// (`∀x.…`, `∃x.…`) alongside their instances, and `autoproof_anf` is exponential in atom count.
// `PropCalculus.decide` with the SAT backend installed removes that ceiling, and both routes close.
//
// Run:  dotnet fsi examples/atp/Sledgehammer.fsx     (requires eprover and cadical; see below)

open Sylvia
open Formula
open PropCalculus
open PredCalculus
// Capture the prover's `Theorem` constructor before `open Sylvia.ATP` shadows the value with the
// EStatus.Theorem case (types are unaffected; only the constructor-as-value clashes).
let private mkThm (pf: Proof) : Theorem = Theorem pf
open Sylvia.ATP

Proof.LogLevel <- 0

let eExe = @"C:\Projects\Sylvia\bin\eprover-E-3.3.5\eprover.exe"
let e = EProver(exePath = eExe, timeoutMs = 20000)

// Certify with the SAT-backed decider rather than `autoproof_anf`. Reconstruction certificates carry
// the quantified facts as opaque ATOMS (`∀x.…`, `∃x.…`) alongside their instances, and the ANF prover
// is exponential in atom count — that, not E, is what blocked ∀-instantiation here. `decide` routes
// past it: `SatProof` replays a CaDiCaL refutation as kernel steps, with no atom ceiling. The proof is
// kernel-checked either way; only the search is delegated.
SatProof.install_with (Sylvia.SAT.Cadical(exePath = @"C:\Projects\Sylvia\bin\cadical.exe", timeoutMs = 20000))

let mutable failures = 0
let ok label cond =
    if not cond then failures <- failures + 1
    printfn "  %s  %s" (if cond then "✓" else "✗") label

type Outcome =
    /// E confirmed provability, selected `Facts`, and Sylvia reconstructed a kernel-checked proof
    /// of `(∧ Facts) ⇒ goal` (or of `goal` itself when no facts were needed).
    | Reconstructed of Theorem * string list
    /// E confirmed provability and selected `Facts`, but the residual is beyond Sylvia's current
    /// (propositional) automation — hand these facts to a human / LLM to finish.
    | ProvableButManual of string list
    /// E did not establish the goal (e.g. CounterSatisfiable / Timeout / GaveUp).
    | Unproved of EStatus

let private conj (ps: Prop list) = ps |> List.reduce (fun a b -> a * b)   // ∧

// A non-lemma proof logs every step regardless of Proof.LogLevel;
// silence stdout around it so the demo shows only the final kernel-checked statement.
let private silence (f: unit -> 'a) : 'a =
    let old = System.Console.Out
    System.Console.SetOut System.IO.TextWriter.Null
    try f () finally System.Console.SetOut old

open FSharp.Quotations

// Rebuild a Pred<int> from a bound variable and a body expression (a λ that substitutes on application).
let private predOf (xv: Var) (e: Expr) : Pred<int> = Pred<int>(func = Expr.Cast<int -> bool>(Expr.Lambda(xv, e)))

// A reconstruction hands the residual prover some hypotheses that are THEMSELVES THEOREMS — the
// `inst` / `exists_intro` instances that supply the quantifier reasoning. Leaving them in the final
// statement would weaken it: the claim would become "the goal follows from the used facts AND these",
// when what we want is "the goal follows from the used facts". These two discharge them.

/// `⊢ t₁`, …, `⊢ tₙ`  ⟼  `H = t₁ ∧ … ∧ tₙ` together with `⊢ H`.
let rec private conjThms (ts: Theorem list) : Prop * Theorem =
    let pr (t: Theorem) = Prop(expand_as<bool> t.Stmt)
    match ts with
    | [] -> failwith "conjThms: nothing to conjoin"
    | [ t ] -> pr t, t
    | t :: rest ->
        let (pRest, tRest) = conjThms rest
        (pr t * pRest), theorem prop_calculus (pr t * pRest)
                            [ Taut t |> apply_left; Taut tRest |> apply_right; reduce |> apply ]

/// `M : (F ∧ H) ⇒ G` and `⊢ H`  ⟼  `F ⇒ G`. Sound because `⊢ H` makes `G` and `H ⇒ G` equal, so the
/// rewrite is an equality step, and `rshunt` re-forms exactly the antecedent `M` proves.
let private dischargeHyps (M: Theorem) (F: Prop) (H: Prop) (hThm: Theorem) (G: Prop) : Theorem =
    let gEq = ident prop_calculus (G == (H ==> G)) [
                  ident_conseq_true G |> Commute |> at_left        // (T ⇒ G) = (H ⇒ G)
                  Taut hThm |> at [ right_branch; left_branch ] ]  // (T ⇒ G) = (T ⇒ G)
    theorem prop_calculus (F ==> G) [
        gEq |> at [ right_branch ]                                 // F ⇒ (H ⇒ G)
        rshunt                                                     // (F ∧ H) ⇒ G
        Taut M |> apply ]

// Reconstruction A — propositional: prove (∧ used) ⇒ goal with `decide` (SAT-backed, no atom ceiling).
let private tryProp (used: (string * Prop) list) (goal: Prop) : Theorem option =
    let target = if used.IsEmpty then goal else (conj (used |> List.map snd)) ==> goal
    try
        let thm = silence (fun () -> decide target)
        if thm.Proof.Complete then Some thm else None
    with _ -> None

// Reconstruction B — ∃-elimination: for a goal  (∃x|R:P) ⇒ Q, introduce a fresh witness (Metatheorem
// 9.30, `witness`) to reduce it to the obligation (R[x̂] ∧ P[x̂]) ⇒ Q′ (Q′ folds in the used facts),
// then discharge that quantifier-free obligation with `decide`.
let private tryWitness (used: (string * Prop) list) (goal: Prop) : Theorem option =
    match expand goal.Expr with
    | Implies(Exists(_, [xv], rangeE, bodyE), qE) when xv.Type = typeof<int> ->
        let x = ScalarVar<int>(xv.Name)
        let R = predOf xv rangeE
        let P = predOf xv bodyE
        let baseQ = Prop(expand_as<bool> qE)
        let qEff = if used.IsEmpty then baseQ else (conj (used |> List.map snd)) ==> baseQ
        try Some (witness x R P qEff (fun xh -> silence (fun () -> decide ((R.[xh] * P.[xh]) ==> qEff))))
        with _ -> None
    | _ -> None

// Reconstruction C — ∃-introduction: for a bare existential goal  ∃x|: Q, ask E for the WITNESS term
// (`AnswerFor`), introduce it with `exists_intro` (Gries 9.28, a theorem `Q[E] ⇒ ∃x|:Q`), and let the
// residual prover chain  used ⊢ Q[E]  through that theorem to the goal. This is the genuinely E-*guided*
// case: E's proof supplies the specific instance E.
//
// This route handles GROUND used-facts, where the residual needs no instantiation. Universal facts
// go to route D below.
let private isIdent (t: string) = t.Length > 0 && t |> Seq.forall (fun c -> System.Char.IsLetterOrDigit c || c = '_')

let private tryExistsIntro (used: (string * Prop) list) (goal: Prop) : Theorem option =
    match expand goal.Expr with
    | Exists(_, [xv], True, bodyE) when xv.Type = typeof<int>
                                        // Universal facts need instantiating first — that is route D,
                                        // which runs next if this one declines.
                                        && used |> List.forall (fun (_, f) -> match expand f.Expr with ForAll _ -> false | _ -> true) ->
        match e.AnswerFor(used, goal) with
        | (Theorem, witName :: _) when isIdent witName ->
            let x = ScalarVar<int>(xv.Name)
            let Q = predOf xv bodyE
            if used.IsEmpty then None else
            try
                let exI = silence (fun () -> exists_intro x Q (intconst witName))   // Q[witName] ⇒ (∃x|:Q), a Theorem
                let F = conj (used |> List.map snd)
                let (H, hThm) = silence (fun () -> conjThms [ exI ])
                let M = silence (fun () -> decide ((F * H) ==> goal))
                Some(silence (fun () -> dischargeHyps M F H hThm goal))
            with _ -> None
        | _ -> None
    | _ -> None

// Reconstruction D — ∀-INSTANTIATION at E's witness. For a goal `∃x|:Q` whose used facts include
// UNIVERSALS: take E's witness term t, instantiate every universal fact at t with `inst` (9.13,
// `(∀y|:P) ⇒ P[t]`), add `exists_intro` at t (9.28, `Q[t] ⇒ (∃x|:Q)`), and close the residual with
// `decide`. The residual is propositional over atoms {the instantiated predicates, the quantified
// facts themselves, the goal} — which is exactly the shape that used to defeat `autoproof_anf`.
let private tryForallInst (used: (string * Prop) list) (goal: Prop) : Theorem option =
    match expand goal.Expr with
    | Exists(_, [xv], True, bodyE) when xv.Type = typeof<int> ->
        match e.AnswerFor(used, goal) with
        | (Theorem, witName :: _) when isIdent witName ->
            let x = ScalarVar<int>(xv.Name)
            let Q = predOf xv bodyE
            let term = intconst witName
            try
                let exI = silence (fun () -> exists_intro x Q term)
                let insts =
                    used |> List.choose (fun (_, f) ->
                        match expand f.Expr with
                        | ForAll(_, [yv], True, bE) when yv.Type = typeof<int> ->
                            Some(silence (fun () -> inst (ScalarVar<int>(yv.Name)) (predOf yv bE) term))
                        | _ -> None)
                if insts.IsEmpty then None
                else
                    let F = conj (used |> List.map snd)
                    let (H, hThm) = silence (fun () -> conjThms (insts @ [ exI ]))
                    let M = silence (fun () -> decide ((F * H) ==> goal))
                    Some(silence (fun () -> dischargeHyps M F H hThm goal))
            with _ -> None
        | _ -> None
    | _ -> None

// Reconstruction E — ∃-ELIMINATION *plus* ∀-INSTANTIATION. Goal `∃x|:Q`, used facts containing an
// EXISTENTIAL and some UNIVERSALS: there is no ground witness to instantiate at, so eliminate the
// existential fact to a FRESH witness x̂ (9.30 `witness`), then instantiate the universals at x̂ and
// introduce the goal's existential at x̂ — all inside the witness obligation, where x̂ is in scope.
let private tryWitnessForallInst (used: (string * Prop) list) (goal: Prop) : Theorem option =
    match expand goal.Expr with
    | Exists(_, [gv], True, gBody) when gv.Type = typeof<int> ->
        let exFact =
            used |> List.tryPick (fun (n, f) ->
                match expand f.Expr with
                | Exists(_, [yv], rE, bE) when yv.Type = typeof<int> -> Some(n, yv, rE, bE)
                | _ -> None)
        match exFact with
        | None -> None
        | Some(exName, yv, rE, bE) ->
            let others = used |> List.filter (fun (n, _) -> n <> exName) |> List.map snd
            if others.IsEmpty then None else
            let y = ScalarVar<int>(yv.Name)
            let R, P = predOf yv rE, predOf yv bE
            let qEff = (conj others) ==> goal
            let gx = ScalarVar<int>(gv.Name)
            let Q = predOf gv gBody
            try
                Some(silence (fun () ->
                    witness y R P qEff (fun xh ->
                        // In here x̂ is a genuine fresh variable, so it can carry the instantiation.
                        let exI = exists_intro gx Q xh                       // Q[x̂] ⇒ (∃x|:Q)
                        let insts =
                            used |> List.choose (fun (n, f) ->
                                if n = exName then None else
                                match expand f.Expr with
                                | ForAll(_, [zv], True, zb) when zv.Type = typeof<int> ->
                                    Some(inst (ScalarVar<int>(zv.Name)) (predOf zv zb) xh)
                                | _ -> None)
                        let F = R.[xh] * P.[xh]
                        let (H, hThm) = conjThms (insts @ [ exI ])
                        let M = decide ((F * H) ==> qEff)
                        dischargeHyps M F H hThm qEff)))
            with _ -> None
    | _ -> None

/// The loop: E filters + confirms; Sylvia certifies where it can — propositionally, by ∃-elimination
/// through `witness` (goal `(∃x|R:P) ⇒ Q`), or by ∃-introduction guided by E's witness term (goal `∃x|:Q`).
let sledgehammer (facts: (string * Prop) list) (goal: Prop) : Outcome =
    let res = e.Prove(facts, goal)
    match res.Status with
    | Theorem ->
        let used = facts |> List.filter (fun (n, _) -> List.contains n res.UsedFacts)
        let usedNames = used |> List.map fst
        match tryProp used goal with
        | Some thm -> Reconstructed(thm, usedNames)
        | None ->
        match tryWitness used goal with
        | Some thm -> Reconstructed(thm, usedNames)
        | None ->
        match tryExistsIntro used goal with
        | Some thm -> Reconstructed(thm, usedNames)
        | None ->
        match tryForallInst used goal with
        | Some thm -> Reconstructed(thm, usedNames)
        | None ->
        match tryWitnessForallInst used goal with
        | Some thm -> Reconstructed(thm, usedNames)
        | None -> ProvableButManual usedNames
    | s -> Unproved s

// ============================================================================
// (1) FULL LOOP — propositional goal buried in a body of lemmas (mostly distractors).
//     Goal `s` follows from f1,f2,f3; f4..f7 are irrelevant. E selects the relevant three;
//     Sylvia reconstructs a checked proof of  ((p⇒q) ∧ (q⇒s) ∧ p) ⇒ s.
// ============================================================================
let p = boolvar "p"
let q = boolvar "q"
let s = boolvar "s"
let t = boolvar "t"
let u = boolvar "u"
let w = boolvar "w"

let facts =
    [ "f1", (p ==> q)
      "f2", (q ==> s)
      "f3", p
      "f4", (t ==> u)      // distractors
      "f5", w
      "f6", (u ==> t)
      "f7", (w ==> u) ]

printfn "===== (1) propositional goal `s` among 7 lemmas ====="
match sledgehammer facts s with
| Reconstructed(thm, used) ->
    printfn "  E selected : %A" used
    printfn "  Sylvia proved (kernel-checked): %s" (prop_calculus.PrintFormula thm.Proof.Stmt)
    ok "loop closed: E filtered to {f1,f2,f3} and Sylvia certified" (List.sort used = ["f1"; "f2"; "f3"] && thm.Proof.Complete)
| ProvableButManual used -> ok "unexpected: not reconstructed" false; printfn "   facts=%A" used
| Unproved st -> ok "unexpected: unproved" false; printfn "   status=%A" st

// ============================================================================
// (2) BOUNDARY — a genuinely first-order goal. E filters + confirms; Sylvia can't yet auto-reconstruct.
//     ∀x.(p x ⇒ q x),  ∀x.(q x ⇒ r x),  ∃x. p x   ⊢   ∃x. r x
// ============================================================================
let x = intvar "x"
let pp = intpred "p"
let qq = intpred "q"
let rr = intpred "r"
let folFacts =
    [ "ax_pq", qall x T (pp.[x] ==> qq.[x])
      "ax_qr", qall x T (qq.[x] ==> rr.[x])
      "ax_p",  qex x T pp.[x] ]
let folGoal = qex x T rr.[x]

// There is no GROUND witness here — the one E uses is a Skolem constant out of `∃x.p x`. So this
// needs ∃-elimination to a fresh x̂ FIRST, and then ∀-instantiation of the two rules at that x̂:
// `witness` (9.30) supplies x̂, `inst` (9.13) instantiates, `exists_intro` (9.28) closes.
printfn "\n===== (2) ∃x.r x from UNIVERSAL facts + an existential (∃-elim then ∀-inst) ====="
match sledgehammer folFacts folGoal with
| Reconstructed(thm, used) ->
    printfn "  E selected %A; Sylvia proved (kernel-checked):\n    %s" used (pred_calculus.PrintFormula (expand thm.Stmt))
    // Check the STATEMENT, not just that something came back: the shunted form
    // `(∃x|:p x) ⇒ ((ax_pq ∧ ax_qr) ⇒ goal)` is exactly `(all three facts) ⇒ goal`.
    let expected = (qex x T pp.[x]) ==> ((conj [ qall x T (pp.[x] ==> qq.[x]); qall x T (qq.[x] ==> rr.[x]) ]) ==> folGoal)
    ok "∃-elimination to a fresh witness, then ∀-instantiation at it"
        (thm.Proof.Complete && List.sort used = ["ax_p"; "ax_pq"; "ax_qr"]
         && sequal (expand thm.Stmt) (expand expected.Expr))
| ProvableButManual used -> ok "unexpected: not reconstructed" false; printfn "   facts=%A" used
| Unproved st -> ok "unexpected: unproved" false; printfn "   status=%A" st

// ============================================================================
// (3) ∃-ELIMINATION — a first-order goal  (∃x|R:P) ⇒ Q  now reconstructs NATIVELY via `witness`
//     (Metatheorem Witness 9.30). E confirms; Sylvia introduces a fresh witness, reducing the goal to
//     a quantifier-free obligation the ANF prover discharges.  (∃x|: p x ∧ (p x ⇒ q)) ⇒ q
// ============================================================================
let qP = Pred<int>(func = <@ fun (_: int) -> %q.Expr @>)
let exElimGoal = (exists (x, truepred, pp * (pp ==> qP))) ==> q

printfn "\n===== (3) ∃-elimination goal reconstructed via witness ====="
match sledgehammer [] exElimGoal with
| Reconstructed(thm, _) ->
    printfn "  Sylvia proved (kernel-checked): %s" (pred_calculus.PrintFormula (expand thm.Stmt))
    ok "first-order ∃-goal reconstructed natively (witness + decide)"
        (thm.Proof.Complete && sequal (expand thm.Stmt) (expand exElimGoal.Expr))
| ProvableButManual _ -> ok "unexpected: not reconstructed (witness should fire)" false
| Unproved st -> ok "unexpected: unproved" false; printfn "   status=%A" st

// ============================================================================
// (4) ∃-INTRODUCTION — E supplies the witness TERM. Facts p(a), p(a) ⇒ q(a); goal ∃x. q x. E answers
//     `a`; Sylvia introduces it via exists_intro (9.28) and the ANF prover chains  used ⊢ q(a),
//     q(a) ⇒ ∃x.q x. This is the genuinely E-guided case — E's proof picks the instance.
// ============================================================================
let a = intconst "a"
let groundFacts = [ "g_pa", pp.[a]; "g_imp", (pp.[a] ==> qq.[a]) ]
let exIntroGoal = qex x T (qq.[x])

printfn "\n===== (4) ∃-introduction goal reconstructed via E's witness term ====="
match sledgehammer groundFacts exIntroGoal with
| Reconstructed(thm, used) ->
    printfn "  E selected %A + a witness; Sylvia proved (kernel-checked):\n    %s" used (pred_calculus.PrintFormula (expand thm.Stmt))
    let expected = (conj (groundFacts |> List.map snd)) ==> exIntroGoal
    ok "∃-introduction reconstructed via exists_intro at E's witness"
        (thm.Proof.Complete && sequal (expand thm.Stmt) (expand expected.Expr))
| ProvableButManual _ -> ok "unexpected: not reconstructed (exists_intro should fire)" false
| Unproved st -> ok "unexpected: unproved" false; printfn "   status=%A" st

// ============================================================================
// (5) ∀-INSTANTIATION — a ground fact p(a) plus UNIVERSAL rules; goal ∃x. r x. E confirms and answers
//     `a`; Sylvia instantiates both rules at `a` (inst, 9.13), introduces the goal's existential at
//     `a` (exists_intro, 9.28), and closes the residual with `decide`. This was THE standing boundary
//     of the whole E arc: the certificate carries the quantified facts as opaque atoms alongside
//     their instances, and `autoproof_anf` is exponential in atom count. It was never an E limitation
//     — E answered in ~40 ms throughout.
// ============================================================================
let instFacts =
    [ "i_pa", pp.[a]
      "i_pq", qall x T (pp.[x] ==> qq.[x])
      "i_qr", qall x T (qq.[x] ==> rr.[x]) ]
let instGoal = qex x T (rr.[x])

printfn "\n===== (5) ∀-instantiation: ∃x. r x from p(a) + universal rules ====="
match sledgehammer instFacts instGoal with
| Reconstructed(thm, used) ->
    printfn "  E selected %A + witness; Sylvia proved (kernel-checked):\n    %s" used (pred_calculus.PrintFormula (expand thm.Stmt))
    // The statement must be exactly `(∧ used facts) ⇒ goal` — the `inst`/`exists_intro` instances
    // that carried the quantifier reasoning are discharged, not left as extra hypotheses.
    let expected = (conj (instFacts |> List.map snd)) ==> instGoal
    ok "∀-instantiation reconstructed: universals instantiated at E's witness, closed by SAT"
        (thm.Proof.Complete && sequal (expand thm.Stmt) (expand expected.Expr))
| ProvableButManual used -> ok "unexpected: not reconstructed (∀-instantiation should fire)" false; printfn "   facts=%A" used
| Unproved st -> ok "unexpected: unproved" false; printfn "   status=%A" st

// ============================================================================
// (6) NEGATIVE — a non-theorem must not be "reconstructed".
// ============================================================================
printfn "\n===== (6) non-theorem is rejected (not reconstructed) ====="
match sledgehammer [ ("g1", (p ==> q)) ] s with
| Unproved st -> ok "non-theorem `s` from only (p⇒q): rejected" (st = CounterSatisfiable); printfn "  status=%A" st
| _ -> ok "unexpected: non-theorem accepted" false

// ============================================================================
// (7) NEGATIVE, FIRST-ORDER — the quantifier routes must not fabricate. Same SHAPE as (2) and (5)
//     (universals + an existential, existential goal) but the p⇒q link is missing, so ∃x.r x does
//     not follow. Nothing may be reconstructed.
// ============================================================================
printfn "\n===== (7) first-order non-theorem is rejected (the ∀/∃ routes must not fabricate) ====="
let badFacts = [ "b_qr", qall x T (qq.[x] ==> rr.[x]); "b_p", qex x T pp.[x] ]
match sledgehammer badFacts folGoal with
| Reconstructed(thm, _) ->
    ok "unexpected: fabricated a proof" false
    printfn "   proved: %s" (pred_calculus.PrintFormula (expand thm.Stmt))
| ProvableButManual used -> ok "unexpected: E claimed it provable" false; printfn "   facts=%A" used
| Unproved st -> ok "∃x.r x without the p⇒q link: rejected" (st = CounterSatisfiable); printfn "  status=%A" st

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
