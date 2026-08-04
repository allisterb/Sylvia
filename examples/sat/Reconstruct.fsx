#load "../proofs/Include.fsx"
#r "../../src/lang/solvers/Sylvia.Solver.CaDiCaL/bin/Debug/net10.0/Sylvia.Solver.CaDiCaL.dll"
#r "../../src/lang/core/Sylvia.Prover.SAT/bin/Debug/net10.0/Sylvia.Prover.SAT.dll"

// SAT-backed propositional proof: a CaDiCaL LRAT refutation of ¬φ, replayed as kernel steps into a
// checked `⊢ φ`. The pipeline itself now lives in the `Sylvia.Prover.SAT` library
// (`SatProof.prove` / `prove_with`); this script is the demonstration and the end-to-end gate.
//
//   goal φ ─Cnf.to_cnf→ (¬φ == A, kernel proof) ─clauses_of→ DIMACS ─CaDiCaL→ UNSAT + LRAT
//          ─resolve-fold→ R : A ⇒ F                                     (STEP 1)
//          ─rewrite ¬φ to A, then Contradiction→ ⊢ φ                     (STEP 2)
//
// Neither step has an atom-count ceiling: STEP 1 replays every LRAT step through
// `PropCalculus.resolve` (`SAT.rup_chain` unfolds non-binary steps into binary chains), and STEP 2's
// CNF equivalence comes from `Cnf.to_cnf`, whose cost is bounded by the CNF's SIZE rather than by an
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
        let th = SatProof.prove_with sat goal
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
// `≢` is handled by Cnf.to_cnf (via Gries 3.10). Nested xor is the worst case for the recursive
// descent — associativity distributes out to 441 clauses — but 433 of those are TAUTOLOGIES, which
// `Cnf.to_cnf` now prunes clause-by-clause with a kernel proof, leaving the same 8 clauses the
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
// justifies them with RAT steps, which `rup_chain` cannot replay — pigeonhole 5→4 failed outright on
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
match SatProof.try_prove_with sat (p ==> q) with
| Ok _ -> ok "try_prove rejects a non-theorem" false
| Error e -> ok "try_prove rejects a non-theorem" true; printfn "      %s" e
// As a proof STEP: discharge a subgoal of a larger hand-written proof.
try
    let sub = ((p ==> q) * p) ==> q                                   // modus ponens, by SAT
    let th = theorem prop_calculus (sub + r) [ SatProof.Sat_with sat sub |> apply_left; simp |> apply ]
    ok "SatProof.Sat_with closes a subgoal inside a hand proof" (sequal th.Stmt (expand (sub + r).Expr))
with e -> ok "SatProof.Sat_with closes a subgoal inside a hand proof" false; printfn "      %s" (e.Message.Split('\n').[0])

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

SatProof.install_with sat
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

// ---- the goals that exposed the ANF prover's COMPLETENESS GAP (now closed) ---------------------
// `autoproof_anf` used to REFUSE these — all valid, all of the shape CNF ⇒ DNF, and the smallest at
// only 2 atoms, which is *inside* the range `decide` routes to it. The cause was the driver's move
// ORDER: `distrib_and_xor` (the one size-increasing rule) outranked the normalizers, so the term was
// fully expanded before anything could be cancelled and the search burned its step budget on
// monomials like `(p ∧ p)` that would have collapsed. Distributing last fixes all of them; see
// `PropCalculus.anf_steps` and docs/prover-automation.md §3.2b.
//
// They stay here as the standing regression guard on that order, because a fixed list of "nice"
// goals is exactly what hid the gap for months: every one of these closes in-kernel now, and if the
// ordering ever regresses they are what says so.
printfn "\nthe goals that used to defeat autoproof_anf (in-kernel, no solver):"
let anfHoles =
    [ "((¬p∨¬p) ∧ ((p∨¬q)∨¬q) ∧ (q∨¬p) ∧ q) ⇒ ((q∧¬q)∨(p∧q))",
      ((((!!p + !!p) * ((p + !!q) + !!q)) * (q + !!p)) * q ==> ((q * !!q) + (p * q)))
      "((p ∧ ((p∨¬q)∨¬q)) ∧ (q∨¬p) ∧ ¬p) ⇒ (¬q∧p)",
      (((p * ((p + !!q) + !!q)) * (q + !!p)) * !!p ==> (!!q * p))
      "((¬p∨¬p) ∧ ((p∨¬p)∨¬p) ∧ q ∧ (p∨p)) ⇒ q",
      ((((!!p + !!p) * ((p + !!p) + !!p)) * q) * (p + p) ==> q)
      "((p∨q) ∧ (r∨s) ∧ (¬p∨¬r) ∧ (¬q∨¬s)) ⇒ ((p∧s)∨(q∧r))",
      (((p + q) * (r + s) * (!!p + !!r) * (!!q + !!s)) ==> ((p * s) + (q * r))) ]

SatProof.uninstall ()          // no backend: these must close on the in-kernel prover alone
for (label, g) in anfHoles do
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let closed = try (autoproof_anf g).Complete with _ -> false
    sw.Stop()
    ok (sprintf "autoproof_anf closes it (%dms):  %s" sw.ElapsedMilliseconds label) (valid g && closed)

// ---- and `decide` still recovers if the ANF route fails for any other reason -------------------
// The fallback (ask the ANF ORACLE; if the goal really is a theorem, give the backend its turn) is
// defence in depth now that the known gap is closed. Exercised by shrinking the step budget so the
// in-kernel route fails on a goal it would otherwise prove easily — cheaper and more deterministic
// than hunting for a goal that defeats it.
printfn "\ndecide recovers when the in-kernel route fails (step budget shrunk to force it):"
let easy = ((p ==> q) * p) ==> q
let savedSteps = autoproof_max_steps
try
    autoproof_max_steps <- 3
    SatProof.uninstall ()
    (try decide easy |> ignore; ok "with no backend, a budget failure is final" false
     with _ -> ok "with no backend, a budget failure is final" true)
    SatProof.install_with sat
    ok "with a backend, decide proves it anyway" (try sequal (decide easy).Stmt (expand easy.Expr) with _ -> false)
    // A genuine non-theorem must NOT reach the solver: the oracle says no, so the ANF message stands.
    (try decide (p == q) |> ignore; ok "a real non-theorem is still refused, without the solver" false
     with e -> ok "a real non-theorem is still refused, without the solver" (e.Message.Contains "could not normalize"))
finally
    autoproof_max_steps <- savedSteps
    SatProof.uninstall ()

printfn "\n%s  (%d failed)" (if failures = 0 then "ALL GREEN" else "FAILURES") failures
