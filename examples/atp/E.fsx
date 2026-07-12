#load "../proofs/Include.fsx"
#r "../../src/lang/atp/Sylvia.ATP.E/bin/Debug/net10.0/Sylvia.ATP.E.dll"

// Advisory use of the E theorem prover (Sylvia.ATP.E). E answers "is this FOL goal a theorem, and
// which axioms did the proof use?" — it is NOT part of Sylvia's trusted base. Point it at the
// quantified / relational goals that lie BEYOND Sylvia's own equational decision procedures.
//
// Requires E (eprover) installed; pass its path to `EProver`, or set the SYLVIA_EPROVER env var.
// Run:  dotnet fsi examples/atp/E.fsx

open Sylvia
open Formula
open PredCalculus
open Sylvia.ATP

let eExe = @"C:\Projects\Sylvia\bin\eprover-E-3.3.5\eprover.exe"
let e = EProver(exePath = eExe, timeoutMs = 20000)

let mutable failures = 0
let ok label cond =
    if not cond then failures <- failures + 1
    printfn "  %s  %s" (if cond then "✓" else "✗") label

printfn "E available at %s : %b" e.ExePath e.IsAvailable

// A pure first-order theory, authored as Sylvia propositions:
//   ∀x.(p x ⇒ q x),  ∀x.(q x ⇒ r x),  ∃x. p x   ⊢   ∃x. r x
// This ∀/∃ chaining is beyond every Sylvia tactic — a natural job for the ATP.
let x = intvar "x"
let p = intpred "p"
let q = intpred "q"
let r = intpred "r"

let axPQ = "ax_pq", qall x T (p.[x] ==> q.[x])
let axQR = "ax_qr", qall x T (q.[x] ==> r.[x])
let axP  = "ax_p",  qex x T p.[x]
let goal = qex x T r.[x]

printfn "\nTPTP sent to E:\n%s" (e.Problem([ axPQ; axQR; axP ], "goal", goal))

let thm = e.Prove([ axPQ; axQR; axP ], goal)
printfn "theorem case : status=%A  usedFacts=%A" thm.Status thm.UsedFacts
ok "∃x.r x is a Theorem"                    (thm.Status = Theorem)
ok "E surfaces the minimal fact set"        (List.sort thm.UsedFacts = [ "ax_p"; "ax_pq"; "ax_qr" ])

// Drop ∃x. p x — the goal is no longer entailed; E must report CounterSatisfiable.
let non = e.Prove([ axPQ; axQR ], goal)
printfn "non-theorem  : status=%A" non.Status
ok "without ∃x.p x, E rejects (CounterSatisfiable)" (non.Status = CounterSatisfiable)

// Constants and function symbols. A term function f : int -> int and constants a, b (ScalarConst,
// which the translator renders as lower-case nullary functors — distinct from term variables):
//   ∀x. f (f x) = x,   f a = b   ⊢   f b = a
let y = intvar "y"
let f = symbolic_var<int -> int> "f"
let a = intconst "a"
let b = intconst "b"
let fap t = <@ (%f) %t @>
let axFInv = "ax_finv", qall x T (Prop <@ %(fap (fap x.Expr)) = %x.Expr @>)
let axFab  = "ax_fab",  Prop <@ %(fap a.Expr) = %b.Expr @>
let goalFba = Prop <@ %(fap b.Expr) = %a.Expr @>
let fres = e.Prove([ axFInv; axFab ], goalFba)
ok "f b = a  is a Theorem (constants + function term + equality)" (fres.Status = Theorem)

// Binary relation r(x,y) (a function-typed symbol applied twice → r(X,Y)). Classic FOL:
// a symmetric, transitive, serial relation is reflexive — well beyond any Sylvia tactic.
let z = intvar "z"
let rho = symbolic_var<int -> int -> bool> "rho"
let rel s t = Prop <@ (%rho) %s %t @>
let sym    = "sym",    qall x T (qall y T (rel x.Expr y.Expr ==> rel y.Expr x.Expr))
let trans  = "trans",  qall x T (qall y T (qall z T ((rel x.Expr y.Expr * rel y.Expr z.Expr) ==> rel x.Expr z.Expr)))
let serial = "serial", qall x T (qex y T (rel x.Expr y.Expr))
let refl   = qall x T (rel x.Expr x.Expr)
let rres = e.Prove([ sym; trans; serial ], refl)
printfn "relation thm : status=%A  usedFacts=%A" rres.Status rres.UsedFacts
ok "symmetric+transitive+serial ⇒ reflexive  is a Theorem" (rres.Status = Theorem)

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
