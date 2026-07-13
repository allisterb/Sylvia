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
// C. Where the trace meets the kernel — the terminal contradiction step is already a real rule
// =================================================================================================
printfn "\nC. Closing the refutation into a Theorem of the goal"

// The final `IsEmpty` step establishes  ¬goal ⊢ ⊥  (i.e.  ¬goal ⇒ F). That is EXACTLY the antecedent
// of proof-by-contradiction, which Sylvia already owns as a checked identity:
let reductio = contradiction_id goal          // Rule from a checked Theorem:  (¬goal ⇒ F) = goal
printfn "   contradiction_id goal  :  %s" reductio.Name
ok "reductio identity constructed (checked)" (reductio.Name <> "")

// WHAT REMAINS (the crux, deliberately not faked): replay each non-empty `ResolutionStep` as a kernel
// derivation of  (∧ premises) ⇒ conclusion  by RESOLUTION, chaining down to  ¬goal ⇒ F, then apply
// `contradiction_id` / `Contradiction` to obtain `⊢ goal`. Sylvia's kernel is equational (Gries) and
// has no clausal-resolution primitive yet, so the next build step is a `resolve` derived rule:
//     resolve : (C ∨ x) ⇒ (D ∨ ¬x) ⇒ (C ∨ D)      [binary resolution, via cut / case_analysis]
// Each LRAT hint chain is a fold of `resolve` over its antecedents — no search, the ids ARE the plan.
printfn "   TODO(next): a `resolve` derived rule in PropCalculus; fold it over each step's premises,"
printfn "               then close with `Contradiction`. The plan above is the exact recipe."

printfn "\n%s  (%d check(s) failed)" (if failures = 0 then "ALL GREEN" else "FAILURES") failures
