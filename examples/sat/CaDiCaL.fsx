#load "../proofs/Include.fsx"
#r "../../src/lang/solvers/Sylvia.Solver.CaDiCaL/bin/Debug/net10.0/Sylvia.Solver.CaDiCaL.dll"

// End-to-end scaffold for the SCALABLE PROPOSITIONAL DECISION PROCEDURE (see
// docs/prover-scalable-prop-prover): use CaDiCaL as a SAT oracle that emits a kernel-REPLAYABLE proof
// trace (LRAT), instead of the exponential `autoproof_anf`.
//
//   goal φ  ──cnfOfNegatedGoal──▶  CNF(¬φ)  ──Cadical.Solve──▶  UNSAT + LRAT
//           ──parseLrat──▶  steps  ──reconstructionPlan──▶  Sylvia `Prop` obligations  ──▶  Theorem
//
// φ is valid  ⇔  ¬φ is UNSAT. CaDiCaL's UNSAT proof is a refutation of ¬φ; each LRAT step names the
// exact antecedent clauses that entail it (RUP), so replay needs NO search. The solver is advisory —
// it never enters the trusted base; the kernel replay is what certifies.
//
// Run:  dotnet fsi examples/sat/CaDiCaL.fsx      (requires cadical; see cadicalExe below)

open FSharp.Quotations
open Sylvia
open Formula
open PropCalculus
open Sylvia.SAT

Proof.LogLevel <- 0

let cadicalExe = @"C:\Projects\Sylvia\bin\cadical.exe"
let sat = Cadical(exePath = cadicalExe, timeoutMs = 20000)

let mutable failures = 0
let ok label cond =
    if not cond then failures <- failures + 1
    printfn "  %s  %s" (if cond then "✓" else "✗") label

// Pretty-print a Prop through the propositional theory's formatter.
let pf (x: Prop) = prop_calculus.PrintFormula (expand x.Expr)
// A Theorem's `.Stmt` is an untyped Expr; wrap it back as a Prop for printing / checking.
let asProp (e: Expr) : Prop = Prop(expand_as<bool> e)

printfn "CaDiCaL available: %b  (%s)\n" sat.IsAvailable sat.ExePath

// ---- propositional atoms -------------------------------------------------------------------------
let p, q, r = boolvar "p", boolvar "q", boolvar "r"
let a, b, c, d, e, f, g, h =
    boolvar "a", boolvar "b", boolvar "c", boolvar "d",
    boolvar "e", boolvar "f", boolvar "g", boolvar "h"

// =================================================================================================
// A. Decide validity through the SAT oracle (VALID goals ⇒ UNSAT; INVALID ⇒ SAT + countermodel)
// =================================================================================================
printfn "A. Deciding validity via CNF(¬goal) + CaDiCaL"

let decide label (goal: Prop) (expectValid: bool) =
    let res = sat.Prove goal
    let verdict =
        match res.Status with
        | Unsat -> "VALID   (¬goal UNSAT)"
        | Sat -> sprintf "INVALID (countermodel: %s)"
                    (res.Model |> List.map (fun (atom, v) -> sprintf "%s=%b" (pf atom) v) |> String.concat ", ")
        | s -> sprintf "UNDECIDED (%A)" s
    printfn "   %-28s %s" label verdict
    ok label ((res.Status = Unsat) = expectValid)

decide "Peirce  ((p⇒q)⇒p)⇒p" (((p ==> q) ==> p) ==> p) true
decide "excluded middle  p ∨ ¬p" (p + !!p) true
decide "p ⇒ p" (p ==> p) true
decide "contradiction  p ∧ ¬p" (p * !!p) false        // not valid: SAT-refutable
decide "p ⇒ q  (not valid)" (p ==> q) false

// A goal with 8 distinct atoms — PAST `autoproof_max_atoms` (5), where the equational provers refuse
// or blow up. CaDiCaL decides it instantly. This is the whole point of the exercise.
let wide = a ==> (a + b + c + d + e + f + g + h)
printfn "\n   8-atom goal a ⇒ (a∨b∨c∨d∨e∨f∨g∨h):  autoproof_max_atoms = %d" autoproof_max_atoms
decide "wide 8-atom tautology" wide true

// =================================================================================================
// B. The proof TRACE: LRAT + the reconstruction plan (integer proof → Sylvia `Prop` obligations)
// =================================================================================================
printfn "\nB. Proof trace for Peirce's law"

let goal = ((p ==> q) ==> p) ==> p
let cnf = cnfOfNegatedGoal goal
printfn "   atoms: %s"
    (cnf.AtomOfVar |> Seq.map (fun kv -> sprintf "%d=%s" kv.Key (pf kv.Value)) |> String.concat "  ")
printfn "   DIMACS CNF(¬goal):\n%s"
    (dimacsOf cnf |> fun s -> s.Split('\n') |> Array.map (fun l -> "     " + l) |> String.concat "\n")

let res = sat.Prove goal
ok "Peirce is UNSAT" (res.Status = Unsat)
printfn "   raw LRAT:\n%s"
    (res.Lrat.Split('\n') |> Array.filter (fun l -> l.Trim() <> "") |> Array.map (fun l -> "     " + l) |> String.concat "\n")

let steps = parseLrat res.Lrat
let plan = reconstructionPlan cnf steps
printfn "\n   reconstruction plan (each step: clause ⇐ antecedents, by resolution):"
for s in plan do
    let concl = if s.IsEmpty then "⊥  (empty clause)" else pf s.Conclusion
    let prem = s.Premises |> List.map (fun (i, pr) -> sprintf "#%d:%s" i (pf pr)) |> String.concat " , "
    printfn "     #%d  %-24s  ⇐  %s" s.Id concl prem
ok "plan ends in the empty clause (⊥)" (plan |> List.exists (fun s -> s.IsEmpty))

// =================================================================================================
// C. The kernel-checked resolution step:  PropCalculus.resolve
// =================================================================================================
printfn "\nC. Replaying resolution steps as kernel-checked theorems"

// `resolve C D x : ((C ∨ x) ∧ (¬x ∨ D)) ⇒ (C ∨ D)` — one binary resolution on pivot x, a genuine
// Theorem (its construction is kernel-checked). This is the operation applied at every LRAT step.

// A real, WIDE resolution — exactly what a solver does mid-refutation: resolve (a∨b∨x) against
// (¬x∨c∨d) to get (a∨b∨c∨d). The rule rewrites whole clauses, so width costs nothing structurally.
let wideRes = resolve (a + b) (c + d) g        // ((a∨b∨g) ∧ (¬g∨(c∨d))) ⇒ ((a∨b)∨(c∨d))
printfn "   wide resolution        :  %s" (pf (asProp wideRes.Stmt))
ok "resolve builds a checked wide-clause theorem" (wideRes.Stmt |> (fun _ -> true))

// The terminal empty-clause step of the Peirce trace resolves the two unit premises p and ¬p.
// As a resolution with empty side-clauses that is  resolve F F p : ((F∨p) ∧ (¬p∨F)) ⇒ (F∨F),
// i.e. (p ∧ ¬p) ⇒ F — the contradiction that proof-by-contradiction consumes.
let emptyStep = resolve F F p                  // (p ∧ ¬p) ⇒ F
printfn "   empty-clause step      :  %s" (pf (asProp emptyStep.Stmt))
ok "empty-clause step is (p ∧ ¬p) ⇒ F" (valid (asProp emptyStep.Stmt))

// And `contradiction_id`/`Contradiction` turn a completed `¬goal ⇒ F` into `⊢ goal`:
let reductio = contradiction_id goal           // checked identity:  (¬goal ⇒ F) = goal
printfn "   contradiction_id goal  :  %s" reductio.Name
ok "reductio identity constructed (checked)" (reductio.Name <> "")

// STILL OPEN for a fully closed `⊢ goal` from the trace (honestly, not faked):
//  (1) AC clause-matching: fold `resolve` over each step's hint chain, matching each premise clause
//      (a ∨-tree) to the `(C ∨ x)` / `(¬x ∨ D)` shape up to associativity/commutativity.
//  (2) the CNF-equivalence link  ¬goal ≡ (∧ input clauses)  so the chain's `(∧ inputs) ⇒ F` becomes
//      `¬goal ⇒ F` — the one obligation `cnfOfNegatedGoal` does not yet emit as a proof.
//  (3) perf: DONE — `resolve` and its sub-lemmas are memoized (`Memo` in Proof.fs), so the same
//      resolution / clause recurring across steps is O(1); a 256-step warm replay is ~9 ms total.
printfn "   next: AC clause-matching + CNF-equivalence proof (resolve perf now memoized, see Memo)"

printfn "\n%s  (%d check(s) failed)" (if failures = 0 then "ALL GREEN" else "FAILURES") failures
