#load "../proofs/Include.fsx"
#r "../../src/lang/solvers/Sylvia.Solver.CaDiCaL/bin/Debug/net10.0/Sylvia.Solver.CaDiCaL.dll"
#r "../../src/lang/core/Sylvia.Prover.SAT/bin/Debug/net10.0/Sylvia.Prover.SAT.dll"

// SAT-backed propositional proof: a CaDiCaL LRAT refutation of ¬φ, replayed as kernel steps into a
// checked `⊢ φ`. The pipeline itself now lives in the `Sylvia.Prover.SAT` library
// (`SatProof.prove` / `proveWith`); this script is the demonstration and the end-to-end gate.
//
//   goal φ ─Cnf.toCnf→ (¬φ == A, kernel proof) ─clausesOf→ DIMACS ─CaDiCaL→ UNSAT + LRAT
//          ─resolve-fold→ R : A ⇒ F                                     (STEP 1)
//          ─rewrite ¬φ to A, then Contradiction→ ⊢ φ                     (STEP 2)
//
// Neither step has an atom-count ceiling: STEP 1 replays every LRAT step through
// `PropCalculus.resolve` (`SAT.rupChain` unfolds non-binary steps into binary chains), and STEP 2's
// CNF equivalence comes from `Cnf.toCnf`, whose cost is bounded by the CNF's SIZE rather than by an
// exponential in the atom count. The solver never enters the trusted base — the kernel replay is
// what certifies. See docs/prover-sat-reconstruction.md.
//
// Run:  dotnet fsi examples/sat/Reconstruct.fsx      (requires cadical)

open Sylvia
open Formula
open PropCalculus
open Sylvia.SAT

Proof.LogLevel <- 0
let sat = Cadical(exePath = @"C:\Projects\Sylvia\bin\cadical.exe", timeoutMs = 20000)

let mutable failures = 0
let ok label cond = (if not cond then failures <- failures + 1); printfn "  %s  %s" (if cond then "✓" else "✗") label

let p, q, r, s, t = boolvar "p", boolvar "q", boolvar "r", boolvar "s", boolvar "t"
let u, v, w = boolvar "u", boolvar "v", boolvar "w"
let x, y, z, a = boolvar "x", boolvar "y", boolvar "z", boolvar "a"

let check label (goal:Prop) =
    try
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let th = SatProof.proveWith sat goal
        sw.Stop()
        printfn "  %s :  ⊢ %s   (%dms)" label (prop_calculus.PrintFormula th.Stmt) sw.ElapsedMilliseconds
        // The result must be a theorem OF THE GOAL, not merely some theorem.
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
// make the replay give up (see docs/prover-sat-reconstruction.md §4.7-4.9):
//   * merge resolution — the two resolved clauses share a NON-pivot literal, so the resolvent has
//     a duplicate that only a dedup-capable clause normalizer can discharge;
//   * non-binary RUP steps — a step with 1, or 3+, hints, unfolded into a resolution chain.
printfn "\nDenser refutations (merge resolvents + non-binary RUP steps):"
check "merge  (p∨q)∧(¬p∨q) ⇒ q"           (((p + q) * (!!p + q)) ==> q)
check "3-var all-8-clause refutation"     (!!((p+q+r) * (!!p+q+r) * (p+ !!q+r) * (!!p+ !!q+r) * (p+q+ !!r) * (!!p+q+ !!r) * (p+ !!q+ !!r) * (!!p+ !!q+ !!r)))
check "resolution chain to s"             (((p + q) * (!!q + r) * (!!r + s) * (!!p + s)) ==> s)
check "∨ distributes over ∧"              ((p * (q + r)) == ((p * q) + (p * r)))
// `≢` is handled by Cnf.toCnf (via Gries 3.10). Nested xor is the worst case for the recursive
// descent — associativity distributes out to 441 clauses — but 433 of those are TAUTOLOGIES, which
// `Cnf.toCnf` now prunes clause-by-clause with a kernel proof, leaving the same 8 clauses the
// solver-side clausifier produces. Before that pruning this goal overflowed the replay's stack.
check "xor commutes  (p≢q) ≡ (q≢p)"       ((p != q) == (q != p))
check "xor assoc  ((p≢q)≢r) ≡ (p≢(q≢r))"  ((((p != q) != r) == (p != (q != r))))
check "pigeonhole 3→2"                    (!!((p+q) * (r+s) * (t+u) * (!!p + !!r) * (!!q + !!s) * (!!p + !!t) * (!!q + !!u) * (!!r + !!t) * (!!s + !!u)))
check "≡ chain  (p≡q)∧(q≡r)∧(r≡s) ⇒ p≡s"  (((p == q) * (q == r) * (r == s)) ==> (p == s))

// ---- DENSE refutations, the class the chains above cannot speak for ----------------------------
// Every goal up to here is either tiny or an implication chain, and a chain is the cheapest
// refutation shape there is: one resolution per atom, over narrow clauses. That made the whole
// benchmark set blind to two things at once.
//
// The first was a COMPLETENESS cliff. CaDiCaL's default preprocessing introduces fresh variables and
// justifies them with RAT steps, which `rupChain` cannot replay — pigeonhole 5→4 failed outright on
// 12 of its 82 steps while every chain sailed through. `Cadical` now defaults to `--plain` for
// exactly this reason (see its doc comment); these goals are what would catch a regression.
//
// The second is that cost tracks LRAT STEPS × CLAUSE-SET SIZE, not atom count. Measured, Release:
// 4→3 is 12 atoms / 15 steps / 1.8 s, but 5→4 is 20 atoms / 48 steps / 11.5 s, and 6→5 is 30 atoms /
// 156 steps / 102 s. A 50-atom chain, by contrast, is 3.1 s. So 5→4 costs what a chain three times
// its size would. 6→5 is deliberately NOT in this gate — it is the honest ceiling, not a test.
let pigeonhole n =
    let ph = Array2D.init (n + 1) n (fun i j -> (boolvar (sprintf "ph%d_%d" i j) :> Prop))
    let someHole = [ for i in 0 .. n -> [ for j in 0 .. n - 1 -> ph.[i, j] ] |> List.reduce (+) ]
    let noClash = [ for j in 0 .. n - 1 do
                      for i in 0 .. n do
                        for k in i + 1 .. n do yield !!(ph.[i, j] * ph.[k, j]) ]
    !!((someHole @ noClash) |> List.reduce ( * ))

printfn "\nDense refutations (wide clauses, superpolynomial resolution — the slow, honest cases):"
check "pigeonhole 4→3"                    (pigeonhole 3)
check "pigeonhole 5→4"                    (pigeonhole 4)

// The library's other entry points.
printfn "\nAPI surface:"
// A non-theorem is reported, not raised — and is distinguishable from "the solver could not tell".
match SatProof.tryProveWith sat (p ==> q) with
| Ok _ -> ok "tryProve rejects a non-theorem" false
| Error e -> ok "tryProve rejects a non-theorem" true; printfn "      %s" e
// As a proof STEP: discharge a subgoal of a larger hand-written proof.
try
    let sub = ((p ==> q) * p) ==> q                                   // modus ponens, by SAT
    let th = theorem prop_calculus (sub + r) [ SatProof.SatWith sat sub |> apply_left; simp |> apply ]
    ok "SatProof.SatWith closes a subgoal inside a hand proof" (sequal th.Stmt (expand (sub + r).Expr))
with e -> ok "SatProof.SatWith closes a subgoal inside a hand proof" false; printfn "      %s" (e.Message.Split('\n').[0])

// ---- lifting the atom ceiling ------------------------------------------------------------------
// `PropCalculus.decide` is the theory-level entry point. On its own it falls back to `autoproof_anf`,
// which is complete but exponential in the atom count and so is capped at `autoproof_max_atoms`.
// Installing this backend registers it as the decider, and the cap stops mattering — note that the
// cap itself is NOT raised: it still guards the exponential prover, which is the only thing it was
// ever protecting against.
printfn "\nPropCalculus.decide, with and without the SAT backend:"
let eightAtom = ((p ==> q) * (q ==> r) * (r ==> s) * (s ==> t) * (t ==> u) * (u ==> v) * (v ==> w)) ==> (p ==> w)
printfn "  goal has %d atoms; autoproof_max_atoms = %d" (prop_atom_count (expand eightAtom.Expr)) autoproof_max_atoms

SatProof.uninstall ()
(try decide eightAtom |> ignore; ok "decide refuses it with no backend installed" false
 with e -> ok "decide refuses it with no backend installed" true
           printfn "      %s" ((e.Message.Split('\n').[0]).Substring(0, 96) + "…"))
(try ok "decide still proves a small goal from the fallback" (sequal (decide (p + !!p)).Stmt (expand (p + !!p).Expr))
 with _ -> ok "decide still proves a small goal from the fallback" false)

SatProof.installWith sat
(try
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let th = decide eightAtom
    sw.Stop()
    printfn "      ⊢ %s   (%dms)" (prop_calculus.PrintFormula th.Stmt) sw.ElapsedMilliseconds
    ok "decide proves it once the SAT backend is installed" (sequal th.Stmt (expand eightAtom.Expr))
 with e -> ok "decide proves it once the SAT backend is installed" false; printfn "      %s" (e.Message.Split('\n').[0]))
// The registration slot does not widen the trusted base: `decide` re-checks that what came back is
// a theorem of the goal it asked about. (Asked with an OVER-the-limit goal — under it, `decide`
// routes to the in-kernel prover and never consults the decider at all.)
prop_decider <- Some(fun _ -> theorem prop_calculus (q + !!q) [ excluded_middle' q |> Taut' |> apply ])
(try decide eightAtom |> ignore; ok "decide rejects a decider that answers a different question" false
 with _ -> ok "decide rejects a decider that answers a different question" true)
SatProof.uninstall ()

printfn "\n%s  (%d failed)" (if failures = 0 then "ALL GREEN" else "FAILURES") failures
