#load "../proofs/Include.fsx"
#r "../../src/lang/atp/Sylvia.ATP.E/bin/Debug/net10.0/Sylvia.ATP.E.dll"

// A Sledgehammer-style loop: use the E prover as an ADVISORY oracle to (a) confirm a goal is provable
// from a body of named Sylvia lemmas and (b) select the RELEVANT few, then RECONSTRUCT a genuine,
// kernel-checked Sylvia proof from exactly those facts. E never enters the trusted base — it only
// filters and confirms; Sylvia's own complete propositional prover (`autoproof_anf`) certifies.
//
// The reconstruction closes the loop for the propositional-modulo-lemmas fragment (the residual
// `(∧ used-facts) ⇒ goal` is a propositional tautology). For genuinely quantified FOL goals, E still
// filters and confirms provability, but native reconstruction awaits FOL-level Sylvia automation, so
// we report the narrowed fact set for a hand / LLM proof.
//
// Run:  dotnet fsi examples/atp/Sledgehammer.fsx     (requires eprover; see eExe below)

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

// `autoproof_anf` builds a non-lemma proof, which logs every step regardless of Proof.LogLevel;
// silence stdout around it so the demo shows only the final kernel-checked statement.
let private silence (f: unit -> 'a) : 'a =
    let old = System.Console.Out
    System.Console.SetOut System.IO.TextWriter.Null
    try f () finally System.Console.SetOut old

open FSharp.Quotations

// Rebuild a Pred<int> from a bound variable and a body expression (a λ that substitutes on application).
let private predOf (xv: Var) (e: Expr) : Pred<int> = Pred<int>(func = Expr.Cast<int -> bool>(Expr.Lambda(xv, e)))

// Reconstruction A — propositional: prove (∧ used) ⇒ goal with the complete ANF prover.
let private tryProp (used: (string * Prop) list) (goal: Prop) : Theorem option =
    let target = if used.IsEmpty then goal else (conj (used |> List.map snd)) ==> goal
    try
        let thm = silence (fun () -> mkThm (autoproof_anf target))
        if thm.Proof.Complete then Some thm else None
    with _ -> None

// Reconstruction B — ∃-elimination: for a goal  (∃x|R:P) ⇒ Q, introduce a fresh witness (Metatheorem
// 9.30, `witness`) to reduce it to the obligation (R[x̂] ∧ P[x̂]) ⇒ Q′ (Q′ folds in the used facts),
// then discharge that quantifier-free obligation with the ANF prover.
let private tryWitness (used: (string * Prop) list) (goal: Prop) : Theorem option =
    match expand goal.Expr with
    | Implies(Exists(_, [xv], rangeE, bodyE), qE) when xv.Type = typeof<int> ->
        let x = ScalarVar<int>(xv.Name)
        let R = predOf xv rangeE
        let P = predOf xv bodyE
        let baseQ = Prop(expand_as<bool> qE)
        let qEff = if used.IsEmpty then baseQ else (conj (used |> List.map snd)) ==> baseQ
        try Some (witness x R P qEff (fun xh -> silence (fun () -> mkThm (autoproof_anf ((R.[xh] * P.[xh]) ==> qEff)))))
        with _ -> None
    | _ -> None

// Reconstruction C — ∃-introduction: for a bare existential goal  ∃x|: Q, ask E for the WITNESS term
// (`AnswerFor`), introduce it with `exists_intro` (Gries 9.28, a theorem `Q[E] ⇒ ∃x|:Q`), and let the
// ANF prover chain  used ⊢ Q[E]  through that theorem to the goal. This is the genuinely E-*guided*
// case: E's proof supplies the specific instance E.
//
// NOTE: this handles GROUND used-facts (the residual is small). It does NOT yet instantiate UNIVERSAL
// facts at the witness — that needs `inst` (9.13), but the resulting certificate carries large
// quantified atoms (`∀x.…`), and `autoproof_anf` is complete-but-exponential on those (≈22 s at 4 such
// atoms, non-terminating at 6). ∀-instantiation therefore waits on a scalable propositional
// reconstruction prover (atom-abstraction, or a Horn method) — see docs §7. It is NOT an E limitation.
let private isIdent (t: string) = t.Length > 0 && t |> Seq.forall (fun c -> System.Char.IsLetterOrDigit c || c = '_')

let private tryExistsIntro (used: (string * Prop) list) (goal: Prop) : Theorem option =
    match expand goal.Expr with
    | Exists(_, [xv], True, bodyE) when xv.Type = typeof<int>
                                        // only attempt when NO used-fact is universal (else the certificate
                                        // blows up autoproof_anf — see note above)
                                        && used |> List.forall (fun (_, f) -> match expand f.Expr with ForAll _ -> false | _ -> true) ->
        match e.AnswerFor(used, goal) with
        | (Theorem, witName :: _) when isIdent witName ->
            let x = ScalarVar<int>(xv.Name)
            let Q = predOf xv bodyE
            try
                let exI = silence (fun () -> exists_intro x Q (intconst witName))   // Q[witName] ⇒ (∃x|:Q), a Theorem
                let target = conj ((used |> List.map snd) @ [ Prop(expand_as<bool> exI.Stmt) ]) ==> goal
                let thm = silence (fun () -> mkThm (autoproof_anf target))
                if thm.Proof.Complete then Some thm else None
            with _ -> None
        | _ -> None
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

// The witness here is a Skolem constant (from ∃x.p x) and the facts are UNIVERSAL, so closing q(ŵ)/r(ŵ)
// needs ∀-instantiation at that witness — beyond the current reconstruction. Still the boundary.
printfn "\n===== (2) ∃x.r x from UNIVERSAL facts (needs ∀-instantiation — still the boundary) ====="
match sledgehammer folFacts folGoal with
| Reconstructed(_, used) -> ok "unexpected: reconstructed" false; printfn "   used=%A" used
| ProvableButManual used ->
    printfn "  E confirms provable; relevant facts = %A" used
    ok "E filtered + confirmed; ∀-instantiation not yet automated → boundary reported" (List.sort used = ["ax_p"; "ax_pq"; "ax_qr"])
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
    ok "first-order ∃-goal reconstructed natively (witness + ANF)"
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
    ok "∃-introduction reconstructed via exists_intro at E's witness" thm.Proof.Complete
| ProvableButManual _ -> ok "unexpected: not reconstructed (exists_intro should fire)" false
| Unproved st -> ok "unexpected: unproved" false; printfn "   status=%A" st

// ============================================================================
// (5) ∀-INSTANTIATION BOUNDARY — a ground fact p(a) plus UNIVERSAL rules; goal ∃x. r x. E confirms +
//     answers `a`, and instantiating the universals at `a` (inst, 9.13) would close it — but the
//     resulting certificate carries large quantified atoms (`∀x.…`) that `autoproof_anf` cannot
//     normalise in reasonable time. So this is reported, not reconstructed, pending a scalable
//     propositional prover (NOT an E limitation — E answered in ~40 ms).
// ============================================================================
let instFacts =
    [ "i_pa", pp.[a]
      "i_pq", qall x T (pp.[x] ==> qq.[x])
      "i_qr", qall x T (qq.[x] ==> rr.[x]) ]
let instGoal = qex x T (rr.[x])

printfn "\n===== (5) ∀-instantiation is the boundary: ∃x. r x from p(a) + universal rules ====="
match sledgehammer instFacts instGoal with
| Reconstructed(_, _) -> ok "unexpected: reconstructed (∀-instantiation not implemented yet)" false
| ProvableButManual used ->
    printfn "  E confirms provable; relevant facts = %A" used
    ok "E filtered + confirmed; ∀-instantiation awaits a scalable prop prover → boundary reported" (not used.IsEmpty)
| Unproved st -> ok "unexpected: unproved" false; printfn "   status=%A" st

// ============================================================================
// (6) NEGATIVE — a non-theorem must not be "reconstructed".
// ============================================================================
printfn "\n===== (6) non-theorem is rejected (not reconstructed) ====="
match sledgehammer [ ("g1", (p ==> q)) ] s with
| Unproved st -> ok "non-theorem `s` from only (p⇒q): rejected" (st = CounterSatisfiable); printfn "  status=%A" st
| _ -> ok "unexpected: non-theorem accepted" false

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
