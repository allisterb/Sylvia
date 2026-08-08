// Pinned deliberately: proof sizes are version-sensitive, so the number in this line is part of the
// measurement. Keep it in step with `Sylvia.Solver.Z3.fsproj`.
#r "nuget: Microsoft.Z3, 4.12.2"

// MEASUREMENT HARNESS — what shape are Z3's proofs for goals at Sylvia's scale?
//
// This is not a gate and proves nothing: it answers a feasibility question ahead of building any
// Z3 proof reconstruction. For each goal it reports three numbers:
//
//   1. ND proof     — inferences in the full natural-deduction proof object (`Solver.Proof`),
//                     counting only proof RULES (nodes tagged `Z3_OP_PR_*`), not the conclusion
//                     terms they carry. This is the artifact a replay would consume.
//   2. on-clause    — callbacks from `Z3_solver_register_on_clause` with `proof=true`.
//   3. clause log   — the same callbacks with `sat.euf=true` and proof generation OFF.
//
// THE OPTIONS ARE THE POINT, and getting them wrong FAILS SILENTLY. Z3 has two proof vocabularies:
//
//   proof=true      -> natural deduction: mp, asserted, lemma, th-lemma, quant-inst, not-or-elim,
//                      monotonicity, transitivity, rewrite
//   sat.euf=true    -> clausal log with fine-grained hints: rup, euf, farkas, tseitin, inst, smt
//
// The second is the one worth having — `euf` and `farkas` are checkable certificates, where
// `th-lemma` is not. But `sat.euf` is a GLOBAL parameter: passing it to the `Context` constructor
// prints "WARNING: unknown parameter" to stdout and proceeds WITHOUT it, so the run looks configured
// and is not. Set it via `Global.SetParameter`. Two of this session's conclusions were drawn from
// runs misconfigured exactly that way; check for the warning before believing any output here.
//
// Re-run this after any Z3 version bump. Proof sizes are version-sensitive: 4.11.2 -> 4.12.2 took
// the propositional chain from 34 inferences to 13 and EUF congruence from 9 to 4.
//
// Run:  dotnet fsi examples/smt/Z3ProofShape.fsx

open System.Collections.Generic
open Microsoft.Z3

let private iS (c: Context) = c.IntSort

/// Only proof-RULE nodes; the conclusion terms hanging off them are not inferences. Note the .NET
/// wrapper throws "Unknown sort kind" on a proof term's `Sort`, so the rule test goes through
/// `FuncDecl.DeclKind` instead.
let private proof_size (root: Expr) =
    let seen = HashSet<uint>()
    let hist = Dictionary<string,int>()
    let mutable n = 0
    let is_rule (e: Expr) = e.IsApp && (string e.FuncDecl.DeclKind).StartsWith "Z3_OP_PR_"
    let rec go (e: Expr) =
        if is_rule e && seen.Add e.Id then
            n <- n + 1
            let k = (string e.FuncDecl.DeclKind).Replace("Z3_OP_PR_", "").ToLower()
            hist.[k] <- (if hist.ContainsKey k then hist.[k] else 0) + 1
            for a in e.Args do go a
    go root
    n, hist

let private top (h: Dictionary<string,int>) =
    h |> Seq.sortByDescending (fun kv -> kv.Value)
      |> Seq.truncate 5
      |> Seq.map (fun kv -> sprintf "%s:%d" kv.Key kv.Value)
      |> String.concat " "

/// Run `build` under `settings`, registering an on-clause callback; return (count, histogram).
///
/// NOTE the split, which is easy to get wrong and silently wrong when you do. Only a short list of
/// parameters is legal in the `Context` CONSTRUCTOR — `proof`, `model`, `timeout`, `auto_config`, … —
/// and `proof` must be set there. Module-qualified parameters like `sat.euf` and
/// `tactic.default_tactic` are GLOBAL: passing them to the constructor makes Z3 print
/// "WARNING: unknown parameter" and carry on with them unset, so a run that looks configured is not.
let private on_clause_run (settings: (string * string) list) (build: Context -> BoolExpr[]) =
    let ctor, globals = settings |> List.partition (fun (k, _) -> not (k.Contains "."))
    for (k, v) in globals do Global.SetParameter(k, v)
    let d = Dictionary<string,string>()
    for (k, v) in ctor do d.[k] <- v
    use ctx = new Context(d)
    let s = ctx.MkSolver()
    let hist = Dictionary<string,int>()
    let mutable n = 0
    let cb =
        OnClause.OnClauseEh(fun hint _clause ->
            n <- n + 1
            let k = if isNull (box hint) then "«null»" else (try hint.FuncDecl.Name.ToString() with _ -> "«?»")
            hist.[k] <- (if hist.ContainsKey k then hist.[k] else 0) + 1)
    use _oc = new OnClause(s, cb)
    s.Assert(build ctx)
    s.Check() |> ignore
    if not globals.IsEmpty then Global.ResetParameters()   // globals are process-wide; do not leak
    n, hist

let private nd_run (build: Context -> BoolExpr[]) =
    let d = Dictionary<string,string>()
    d.["proof"] <- "true"
    use ctx = new Context(d)
    let s = ctx.MkSolver()
    s.Assert(build ctx)
    if s.Check() = Status.UNSATISFIABLE then proof_size s.Proof else 0, Dictionary<string,int>()

// ---- the goals: Sylvia-scale, spanning the theories we care about -------------------------------

let goals : (string * (Context -> BoolExpr[])) list =
    [ // Propositional — the class we already reconstruct end to end through CaDiCaL.
      "propositional chain 3", (fun (c: Context) ->
        let p, q, r = c.MkBoolConst "p", c.MkBoolConst "q", c.MkBoolConst "r"
        [| c.MkImplies(p, q); c.MkImplies(q, r); c.MkNot(c.MkImplies(p, r)) |])

      // The propositional body `meta_set_ident` produces for a set identity (Gries 11.42a), at two sizes.
      // If these stay flat as the variable count grows, set-theory goals are cheap to reconstruct.
      "set body De Morgan", (fun (c: Context) ->
        let p, q = c.MkBoolConst "p", c.MkBoolConst "q"
        [| c.MkNot(c.MkEq(c.MkNot(c.MkOr(p, q)), c.MkAnd(c.MkNot p, c.MkNot q))) |])
      "set body 6-var De Morgan", (fun (c: Context) ->
        let v = [| for i in 0 .. 5 -> c.MkBoolConst(sprintf "p%d" i) |]
        [| c.MkNot(c.MkEq(c.MkNot(c.MkOr v), c.MkAnd(v |> Array.map c.MkNot))) |])

      // EUF — congruence closure. Its certificates are congruence/transitivity chains, which map
      // onto Leibniz substitution, so this is the theory that fits an equational kernel best.
      "EUF congruence", (fun (c: Context) ->
        let f = c.MkFuncDecl("f", iS c, iS c :> Sort)
        let a, b = c.MkIntConst "a", c.MkIntConst "b"
        [| c.MkEq(a, b); c.MkNot(c.MkEq(f.Apply a, f.Apply b)) |])
      "EUF + case split", (fun (c: Context) ->
        let f = c.MkFuncDecl("f", iS c, iS c :> Sort)
        let a, b, cc = c.MkIntConst "a", c.MkIntConst "b", c.MkIntConst "c"
        [| c.MkOr(c.MkEq(a, b), c.MkEq(a, cc))
           c.MkNot(c.MkEq(f.Apply a, f.Apply b))
           c.MkNot(c.MkEq(f.Apply a, f.Apply cc)) |])

      // Linear arithmetic — where Boehme & Weber's reconstruction success rate collapsed (26% on
      // QF_LIA). Watch for `th-lemma`: that is the step with no cheap certificate.
      "linear arithmetic", (fun (c: Context) ->
        let x, y = c.MkIntConst "x", c.MkIntConst "y"
        [| c.MkGt(x, y); c.MkGt(y, c.MkInt 5); c.MkLt(x, c.MkInt 4) |])
      "arith case split", (fun (c: Context) ->
        let x, y, z = c.MkIntConst "x", c.MkIntConst "y", c.MkIntConst "z"
        [| c.MkOr(c.MkGt(x, c.MkInt 10), c.MkGt(y, c.MkInt 10)); c.MkLt(c.MkAdd(x, y), c.MkInt 5)
           c.MkGt(x, c.MkInt 0); c.MkGt(y, c.MkInt 0); c.MkGt(z, c.MkAdd(x, y)); c.MkLt(z, c.MkInt 3) |])

      // Quantified — `quant-inst` hands us the instance TERM, which is the part that is hard to find.
      "quantified 2 instances", (fun (c: Context) ->
        let f = c.MkFuncDecl("f", iS c, iS c :> Sort)
        let x = c.MkIntConst "x"
        let a, b = c.MkIntConst "a", c.MkIntConst "b"
        let fa = c.MkForall([| x |], c.MkGt(f.Apply x :?> ArithExpr, c.MkInt 0))
        [| fa; c.MkOr(c.MkNot(c.MkGt(f.Apply a :?> ArithExpr, c.MkInt 0)),
                      c.MkNot(c.MkGt(f.Apply b :?> ArithExpr, c.MkInt 0))) |])

      // Dense propositional, for contrast with the CaDiCaL route's LRAT certificate on the same goal.
      "pigeonhole 4->3", (fun (c: Context) ->
        let n = 3
        let ph = Array2D.init (n + 1) n (fun i j -> c.MkBoolConst(sprintf "ph%d_%d" i j))
        Array.append
            [| for i in 0 .. n -> c.MkOr([| for j in 0 .. n - 1 -> ph.[i, j] |]) |]
            [| for j in 0 .. n - 1 do
                 for i in 0 .. n do
                   for k in i + 1 .. n -> c.MkNot(c.MkAnd(ph.[i, j], ph.[k, j])) |]) ]

printfn "Microsoft.Z3 %A\n" (typeof<Context>.Assembly.GetName().Version)
printfn "%-26s | %-26s | %-34s | %s" "goal" "ND proof (proof=true)" "on-clause (proof=true)" "clause log (sat.euf)"
printfn "%s" (String.replicate 122 "-")

for (name, build) in goals do
    let nd, ndh = nd_run build
    let nA, hA = on_clause_run [ "proof", "true"; "tactic.default_tactic", "smt" ] build
    let nB, hB = on_clause_run [ "sat.euf", "true"; "tactic.default_tactic", "smt" ] build
    printfn "%-26s | %-26s | %-34s | %s"
        name (sprintf "%-3d %s" nd (top ndh)) (sprintf "%-3d %s" nA (top hA)) (sprintf "%-3d %s" nB (top hB))

printfn ""
printfn "Read it as: proofs at this scale are TENS of inferences and flat in problem size (the set"
printfn "body is the same at 2 and 6 variables). `Solver.Proof` is the richer artifact — it includes"
printfn "preprocessing, which is where small goals are actually decided, and which the on-clause"
printfn "stream never sees. See docs/prover-z3-reconstruction.md."
