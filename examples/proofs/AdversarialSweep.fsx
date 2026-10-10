#load "Include.fsx"

// Adversarial instantiation sweep over every all-Prop-parameter derived rule and theorem in
// PropCalculus.
//
// WHY. A derived rule is a SCHEMA: it must prove for any Props. But its steps rewrite by
// SUBSTITUTION, and a substitution (a `Derive` rule — `idemp_or p`, an `Ident` of a theorem)
// rewrites the LEFTMOST-OUTERMOST match inside whatever subterm the step is addressed to. So a step
// written with a searching address (`at_left`, `at_right`, or a path that stops short of the node)
// silently rewrites the wrong occurrence as soon as an ARGUMENT contains a competing one — and the
// schema then fails on a lemma that looks unrelated to whatever the caller was doing. Admissible
// rules (`Admit` — `commute`, `distrib`, `golden_rule`, …) fire only at the addressed node and are
// never affected.
//
// This sweep instantiates each schema with one argument replaced by a term that CONTAINS the
// patterns the derivations search for. On 2026-07-25 it found 15 schemas failing, collapsing to
// seven root derivations (absorb_or, absorb_and, ident_and_implies, ident_or_conseq, ident_and_eq,
// ident_eq_and_or_not, shunt', distrib_implies_eq_implies); the rest inherited, including
// trans_implies — which Calc.chain_imp instantiates at whatever the caller is composing.
//
// RE-RUN THIS after adding or editing any derivation. It takes several minutes; the fast subset is
// pinned as a unit test (`derived schemas prove at arguments that contain their own rewrite
// patterns` in tests/Sylvia.Tests.Prover/KernelProofTests.fs).
//
// Run:  dotnet fsi examples/proofs/AdversarialSweep.fsx

open System
open Sylvia
open Formula
open PropCalculus

Proof.LogLevel <- 0
let p, q, r, s = boolvar "p", boolvar "q", boolvar "r", boolvar "s"

/// Terms carrying the patterns the derivations rewrite by searching for.
let traps : (string * Prop) list =
    [ "(p∨p)",       (p + p)
      "(p∧p)",       (p * p)
      "¬¬p",         (!!(!!p))
      "p∨(p∨q)",     (p + (p + q))
      "(p∨q)∧(p∨q)", ((p + q) * (p + q))
      "(p=p)",       (p == p)
      "(p⇒q)",       (p ==> q)
      "(q∧p)",       (q * p)
      "(p∧q)∨p",     ((p * q) + p)
      "¬(p∧q)",      (!!(p * q))
      "(p=q)=p",     ((p == q) == p)
      "T",           T
      "F",           F
      "q∨(q∨p)",     (q + (q + p))
      "(q=p)",       (q == p) ]

let baseArgs : Prop list = [p; q; r; s]
let propTy = typeof<Prop>
let failures = ResizeArray<string * string * string>()
let mutable attempted = 0

// Meta-provers take an arbitrary goal and prove it; instantiating them like a schema just asks them
// to prove a bare variable, which is not a theorem (and is slow).
// NB `Set.ofList`, not `set [...]` — in scope here `set` is Sylvia's set-comprehension builder,
// which takes a quotation.
let metaProvers = Set.ofList [ "auto"; "autoident"; "autodeduce"; "decide" ]

// A schema over a TEMPLATE takes its hole as a `PropVar` (Replace by true/false, Shannon — Gries
// 3.87–3.89). The hole is never trapped: it gets the fixed variable `z`, which no trap mentions. The
// adversarial patterns go in the `Prop` arguments as for every other schema, and each instantiation
// is ALSO run with `z` conjoined into the last `Prop` argument — the template — since a template
// with no hole would only exercise the degenerate case.
let propVarTy = typeof<PropVar>
let z = boolvar "z"

let sweep (name: string) (ps: Reflection.ParameterInfo[]) (invoke: obj[] -> unit) =
    let n = ps.Length
    let isProp (x: Reflection.ParameterInfo) = x.ParameterType = propTy
    let isHole (x: Reflection.ParameterInfo) = x.ParameterType = propVarTy
    let props = ps |> Array.filter isProp |> Array.length
    let hasHole = ps |> Array.exists isHole
    if n > 0 && props > 0 && props <= 4 && not (metaProvers.Contains name) && ps |> Array.forall (fun x -> isProp x || isHole x) then
        let tuples =
            [ yield "baseline", (List.truncate props baseArgs)
              for i in 0 .. props - 1 do
                for (tn, t) in traps do
                    yield sprintf "arg%d=%s" (i + 1) tn,
                          (List.truncate props baseArgs |> List.mapi (fun j a -> if j = i then t else a)) ]
        let variants =
            [ for (label, args) in tuples do
                yield label, args
                if hasHole then
                    yield label + "+hole", (args |> List.mapi (fun j a -> if j = props - 1 then (a * z) else a)) ]
        for (label, args) in variants do
            attempted <- attempted + 1
            // Interleave: each hole parameter gets `z`, each Prop parameter the next argument.
            let mutable k = 0
            let full =
                ps |> Array.map (fun x ->
                    if isHole x then box z
                    else let a = args.[k] in k <- k + 1; box a)
            try invoke full
            with e ->
                let rec root (x: exn) = if isNull x.InnerException then x else root x.InnerException
                let msg = (root e).Message.Split('\n').[0]
                failures.Add(name, label, msg.Substring(0, min 260 msg.Length))

let sw = Diagnostics.Stopwatch.StartNew()
for m in ProofModules.getModuleDerivedRules PropCalculus.Type do
    sweep m.Name m.Parameters (fun args -> m.Method.Invoke(null, args) |> ignore)
for m in ProofModules.getModuleTheorems PropCalculus.Type do
    sweep m.Name m.Parameters (fun args -> m.Method.Invoke(null, args) |> ignore)
sw.Stop()

printfn "\n===== ADVERSARIAL SWEEP: %d instantiations in %ds =====" attempted (sw.ElapsedMilliseconds / 1000L)

// A schema that also fails at BASELINE has a side condition (or is a search tactic), so its
// failures are not this bug class — report it separately rather than mixing it in.
let allNames = failures |> Seq.groupBy (fun (n, _, _) -> n) |> Seq.sortBy fst |> Seq.toList
let sideCondition, misTargeted =
    allNames |> List.partition (fun (_, fs) -> fs |> Seq.exists (fun (_, l, _) -> l = "baseline"))

printfn "\nexcluded — also fails at baseline, so a side condition rather than mis-targeting:\n  %s"
    (String.Join(", ", sideCondition |> List.map fst))
// `replace_eq` (3.51) used to be the one standing exception here: proved with `subst_and`, which
// matches only (Var = Var) ∧ E, it failed at 43 compound instantiations. It is now an instance of
// Substitution (3.84a) and holds at all of them, so there are no expected failures left.

printfn "\n%d failing instantiations across %d schemas"
    (misTargeted |> List.sumBy (snd >> Seq.length)) (List.length misTargeted)
for (name, fs) in misTargeted do
    let labels = fs |> Seq.map (fun (_, l, _) -> l) |> Seq.toList
    let (_, _, msg) = Seq.head fs
    printfn "\n  %-28s %d/%d  [%s]" name (Seq.length fs) (1 + List.length traps * 4) (String.Join(", ", labels))
    printfn "       %s" msg

let unexpected = misTargeted
printfn "\n%s" (if unexpected.IsEmpty then "ALL CLEAR"
                else sprintf "MIS-TARGETED SCHEMAS: %s" (String.Join(", ", unexpected |> List.map fst)))
