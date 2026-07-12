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

/// The loop: E filters + confirms; Sylvia certifies where it can.
let sledgehammer (facts: (string * Prop) list) (goal: Prop) : Outcome =
    let res = e.Prove(facts, goal)
    match res.Status with
    | Theorem ->
        let used = facts |> List.filter (fun (n, _) -> List.contains n res.UsedFacts)
        let usedNames = used |> List.map fst
        // Reconstruct  (∧ used) ⇒ goal  with the complete ANF prover. Succeeds iff the residual is
        // propositional; throws otherwise (quantified goals) → report the narrowed facts instead.
        let target = if used.IsEmpty then goal else (conj (used |> List.map snd)) ==> goal
        try
            let thm = silence (fun () -> mkThm (autoproof_anf target))
            if thm.Proof.Complete then Reconstructed(thm, usedNames) else ProvableButManual usedNames
        with _ -> ProvableButManual usedNames
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

printfn "\n===== (2) first-order goal ∃x.r x (beyond Sylvia automation) ====="
match sledgehammer folFacts folGoal with
| Reconstructed(_, used) -> ok "unexpected: FOL auto-reconstructed" false; printfn "   used=%A" used
| ProvableButManual used ->
    printfn "  E confirms provable; relevant facts = %A" used
    ok "E filtered + confirmed; boundary reported (awaits FOL automation)" (List.sort used = ["ax_p"; "ax_pq"; "ax_qr"])
| Unproved st -> ok "unexpected: unproved" false; printfn "   status=%A" st

// ============================================================================
// (3) NEGATIVE — a non-theorem must not be "reconstructed".
// ============================================================================
printfn "\n===== (3) non-theorem is rejected (not reconstructed) ====="
match sledgehammer [ ("g1", (p ==> q)) ] s with
| Unproved st -> ok "non-theorem `s` from only (p⇒q): rejected" (st = CounterSatisfiable); printfn "  status=%A" st
| _ -> ok "unexpected: non-theorem accepted" false

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
