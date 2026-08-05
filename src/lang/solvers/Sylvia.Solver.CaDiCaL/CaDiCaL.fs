namespace Sylvia

open System
open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open System.Collections.Generic

open FSharp.Quotations

open Formula
open FsExpr

/// Optional integration with the CaDiCaL SAT solver (https://github.com/arminbiere/cadical) as a
/// scalable propositional decision procedure that emits a **kernel-replayable proof trace**.
///
/// The intended pipeline (see `docs/prover-scalable-prop-prover`):
///
///   goal φ  ──cnf_of_negated_goal──▶  CNF(¬φ)  ──Cadical.Solve──▶  UNSAT + LRAT proof
///           ──parse_lrat──▶  resolution steps  ──reconstruction_plan──▶  Sylvia `Prop` obligations
///           ──(kernel replay, in an .fsx / PropCalculus)──▶  a checked `Theorem` of φ.
///
/// φ is a theorem iff ¬φ is unsatisfiable (validity ≡ dual UNSAT), so the solver's UNSAT proof is a
/// **refutation of ¬φ**. CaDiCaL emits it as LRAT — each added clause is annotated with the exact
/// antecedent clause ids that entail it by unit propagation, so replay needs NO search. On pure
/// propositional input without heavy inprocessing, every step is RUP (logically entailed), hence a
/// genuine resolution inference that maps forward into the equational kernel; the terminal empty
/// clause is the `⊥` of a proof by contradiction (`PropCalculus.contradiction_id` / `Contradiction`).
///
/// Like `Sylvia.ATP.E`, this module depends only on `Sylvia.Expressions`: it produces the CNF, drives
/// the solver, parses the proof, and builds the *reconstruction plan* (the ordered clause `Prop`s and
/// their antecedents). Turning that plan into a checked `Theorem` uses the prover kernel and is done
/// by the caller (see `examples/sat/CaDiCaL.fsx`) — the solver never enters the trusted base.
module SAT =

    (* ---------------------------------------------------------------------- *)
    (* Sylvia Prop  ->  CNF (DIMACS) over propositional atoms                   *)
    (* ---------------------------------------------------------------------- *)

    /// A DIMACS literal: a non-zero signed integer. `+v` = atom v is true, `-v` = atom v is false.
    type Lit = int

    /// A clause is a disjunction of literals.
    type Clause = Lit list

    /// A CNF encoding of `¬Goal` over a fixed set of propositional atoms. Solving it for UNSAT proves
    /// `Goal`. `AtomOfVar` recovers, for each 1-based DIMACS variable, the Sylvia atom it abstracts —
    /// this is the map the reconstruction uses to turn integer literals back into `Prop`s.
    type CnfProblem =
        { /// Number of distinct propositional atoms (= largest DIMACS variable).
          NumVars: int
          /// The clauses of `¬Goal`, in the order they are written to DIMACS (= CaDiCaL clause ids 1..m).
          Clauses: Clause list
          /// 1-based DIMACS variable  ->  the Sylvia atom `Prop` it stands for.
          AtomOfVar: IReadOnlyDictionary<int, Prop>
          /// The original goal. The CNF encodes its negation.
          Goal: Prop }

    // An internal boolean-formula AST used only for the CNF transform. Implication, bi-implication and
    // xor are eliminated into ¬/∧/∨ up front; anything that is not boolean structure (a bool variable,
    // a predicate application, a non-boolean equality) is an ATOM and gets a variable index.
    type private BF =
        | BTrue
        | BFalse
        | BAtom of int          // 1-based atom index
        | BNot of BF
        | BAnd of BF * BF
        | BOr of BF * BF

    /// Clausify the **negation** of `goal`. Uses a direct NNF-then-distribute conversion: atoms are the
    /// maximal non-(boolean-structural) subterms, deduplicated structurally (`sequal`).
    ///
    /// NOTE: direct distribution is worst-case exponential in the formula's ∨/∧ nesting. That is fine
    /// for every goal measured and keeps atoms in 1-1 correspondence with Sylvia `Prop`s (which the
    /// kernel replay depends on).
    ///
    /// This note used to name a Tseitin/Plaisted-Greenbaum encoding as the scalable upgrade, on the
    /// strength of an apparent blowup on nested `≢`. **That was withdrawn (2026-07-28)**: the blowup
    /// was on the kernel side, in `Cnf.to_cnf`, and it was retained TAUTOLOGICAL clauses rather than
    /// genuine size — 441 clauses to keep 8, where this function had always dropped them in
    /// `normClause`. Pruning inside `Cnf`'s distribution closed the gap and the two clausifiers now
    /// agree clause for clause. Nothing measured since motivates auxiliary variables, and they are not
    /// free here: the reconstruction would have to learn to discharge the definitional clauses.
    /// Measure a real blowup first.
    ///
    /// The two clausifiers must keep agreeing — that is a correctness property, not tidiness. When one
    /// folds something the other does not, the pipeline proves one formula and asks the solver about a
    /// different one; that is exactly how the truth constants went wrong (`Cnf`'s `constOf` notes).
    let cnf_of_negated_goal (goal: Prop) : CnfProblem =
        let atoms = ResizeArray<Expr>()      // index i (0-based) -> atom; DIMACS var = i+1

        let varOf (e: Expr) : int =
            let mutable found = -1
            for i in 0 .. atoms.Count - 1 do
                if found < 0 && sequal atoms.[i] e then found <- i
            if found < 0 then
                atoms.Add e
                atoms.Count                  // 1-based index of the freshly added atom
            else found + 1

        let rec toBF (e: Expr) : BF =
            match e with
            | True -> BTrue
            | False -> BFalse
            | Not a -> BNot(toBF a)
            | And(a, b) -> BAnd(toBF a, toBF b)
            | Or(a, b) -> BOr(toBF a, toBF b)
            | Implies(a, b) -> BOr(BNot(toBF a), toBF b)          // a ⇒ b  ≡  ¬a ∨ b
            | Conseq(a, b) -> BOr(BNot(toBF b), toBF a)           // a ⇐ b  ≡  b ⇒ a  ≡  ¬b ∨ a
            | Equals(a, b) when a.Type = typeof<bool> ->          // a = b  ≡  (a ⇒ b) ∧ (b ⇒ a)
                let x, y = toBF a, toBF b
                BAnd(BOr(BNot x, y), BOr(BNot y, x))
            | NotEquals(a, b) when a.Type = typeof<bool> ->       // a ≠ b  ≡  xor
                let x, y = toBF a, toBF b
                BOr(BAnd(x, BNot y), BAnd(BNot x, y))
            | _ -> BAtom(varOf e)                                 // an atom (incl. non-bool equality)

        // NNF while negating the whole formula (we want ¬goal): push the leading ¬ to the leaves.
        let rec nnf (neg: bool) (bf: BF) : BF =
            match bf with
            | BTrue -> if neg then BFalse else BTrue
            | BFalse -> if neg then BTrue else BFalse
            | BAtom i -> if neg then BNot(BAtom i) else BAtom i
            | BNot a -> nnf (not neg) a
            | BAnd(a, b) -> if neg then BOr(nnf true a, nnf true b) else BAnd(nnf false a, nnf false b)
            | BOr(a, b) -> if neg then BAnd(nnf true a, nnf true b) else BOr(nnf false a, nnf false b)

        // Constant folding so ∧/∨ with T/F collapse before distribution.
        let rec simp (bf: BF) : BF =
            match bf with
            | BAnd(a, b) ->
                match simp a, simp b with
                | BFalse, _ | _, BFalse -> BFalse
                | BTrue, x | x, BTrue -> x
                | x, y -> BAnd(x, y)
            | BOr(a, b) ->
                match simp a, simp b with
                | BTrue, _ | _, BTrue -> BTrue
                | BFalse, x | x, BFalse -> x
                | x, y -> BOr(x, y)
            | x -> x

        // Distribute ∨ over ∧ to a clause set. Assumes NNF (negations only at atoms) and no constants
        // except possibly the whole formula being T (no clauses) or F (one empty clause).
        let rec clauses (bf: BF) : int list list =
            match bf with
            | BAnd(a, b) -> clauses a @ clauses b
            | BOr(a, b) -> [ for x in clauses a do for y in clauses b -> x @ y ]
            | BAtom i -> [ [ i ] ]
            | BNot(BAtom i) -> [ [ -i ] ]
            | BTrue -> []            // empty conjunction — no constraints
            | BFalse -> [ [] ]       // empty clause — immediately UNSAT
            | BNot _ -> failwith "cnf_of_negated_goal: formula not in NNF (bug)"

        // Drop duplicate literals and tautological clauses (containing v and ¬v).
        let normClause (c: int list) : int list option =
            let s = HashSet<int>()
            let mutable taut = false
            for l in c do
                if s.Contains(-l) then taut <- true
                s.Add l |> ignore
            if taut then None else Some(List.ofSeq s)

        let cls =
            toBF (expand goal.Expr)
            |> nnf true                                            // NNF of ¬goal
            |> simp
            |> clauses
            |> List.choose normClause

        let atomOfVar = Dictionary<int, Prop>()
        atoms |> Seq.iteri (fun i e -> atomOfVar.[i + 1] <- prop (expand_as<bool> e))

        { NumVars = atoms.Count
          Clauses = cls
          AtomOfVar = atomOfVar :> IReadOnlyDictionary<_, _>
          Goal = goal }

    /// Render a `CnfProblem` as DIMACS CNF text.
    let dimacs_of (p: CnfProblem) : string =
        let sb = StringBuilder()
        sb.AppendLine(sprintf "p cnf %d %d" p.NumVars (List.length p.Clauses)) |> ignore
        for c in p.Clauses do
            for l in c do
                sb.Append(l).Append(' ') |> ignore
            sb.Append("0").Append('\n') |> ignore
        sb.ToString()

    (* ---------------------------------------------------------------------- *)
    (* LRAT proof steps                                                        *)
    (* ---------------------------------------------------------------------- *)

    /// One step of an LRAT proof.
    ///
    /// `Add(id, literals, hints)` — introduce clause `id`; `literals` are its signed literals (empty =
    /// the ⊥ clause); `hints` are the antecedent clause ids whose unit propagation entails it (a RUP /
    /// resolution chain). `Delete(afterId, ids)` — clauses `ids` may be forgotten (checker bookkeeping;
    /// carries no logical content).
    ///
    /// This is the pivot the whole pipeline turns on, and it is deliberately independent of how the
    /// steps were obtained: the CLI backend recovers them by parsing an LRAT file, the native backend
    /// receives them from CaDiCaL's tracer without a file existing at all. Verified identical on
    /// pigeonhole 4→3 — same ids, literals and antecedents from both routes.
    type LratStep =
        | Add of id: int * literals: int list * hints: int list
        | Delete of afterId: int * ids: int list

    /// Parse text LRAT (as emitted by `cadical --lrat --no-binary`). Ignores blank lines and `c`
    /// comments. Each addition line is  `id  lit* 0  hint* 0`;  each deletion line is  `id d  cid* 0`.
    let parse_lrat (text: string) : LratStep list =
        [ for raw in text.Split('\n') do
            let line = raw.Trim()
            if line <> "" && not (line.StartsWith "c") then
                let toks = line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
                if toks.Length >= 2 && toks.[1] = "d" then
                    // id d c1 c2 ... 0
                    let ids =
                        toks.[2..]
                        |> Array.map int
                        |> Array.takeWhile (fun n -> n <> 0)
                        |> List.ofArray
                    yield Delete(int toks.[0], ids)
                else
                    // id lit* 0 hint* 0
                    let nums = toks |> Array.map int
                    let id = nums.[0]
                    let rest = nums.[1..]
                    let lits = rest |> Array.takeWhile (fun n -> n <> 0) |> List.ofArray
                    let after = rest.[(List.length lits + 1)..]   // skip the literals and their terminating 0
                    let hints = after |> Array.takeWhile (fun n -> n <> 0) |> List.ofArray
                    yield Add(id, lits, hints) ]

    (* ---------------------------------------------------------------------- *)
    (* Runner                                                                  *)
    (* ---------------------------------------------------------------------- *)

    /// Outcome of a CaDiCaL run.
    type SatStatus =
        /// `¬Goal` is unsatisfiable — the goal is VALID. An LRAT proof was written.
        | Unsat
        /// `¬Goal` is satisfiable — the goal is NOT valid; `Model` is a countermodel.
        | Sat
        /// The solver did not decide within the budget (wrapper timeout, or a resource limit).
        | Unknown
        /// The `cadical` executable was not found.
        | NotAvailable
        /// The wrapper-enforced wall-clock timeout fired (the process was killed).
        | Timeout

    /// Result of solving a `CnfProblem`. On `Unsat`, `Lrat` holds the text LRAT proof (the trace to
    /// replay). On `Sat`, `Model` lists each atom's truth value (a countermodel of the goal).
    type SatResult =
        { Status: SatStatus
          Lrat: string
          Model: (Prop * bool) list
          Raw: string
          Dimacs: string }

    /// A solved `CnfProblem`: the verdict, plus the refutation as LRAT steps when `Unsat`.
    ///
    /// `Steps` is what reconstruction consumes. Splitting it out from `SatResult.Lrat` is what lets
    /// the two backends be interchangeable: the CLI route fills it by parsing the proof file, the
    /// native route receives it from the tracer and leaves `Result.Lrat` empty. Nothing downstream
    /// can tell which produced it.
    type SatRun =
        { Result: SatResult
          Steps: LratStep list
          /// The solver's OWN id for each input clause, when it reported them; `[]` when it did not.
          ///
          /// This is not bookkeeping — it is a correctness requirement, and the two backends differ.
          /// The CLI route feeds DIMACS through CaDiCaL's parser, which calls `reserve_ids` on the
          /// `p cnf` header and therefore hands input clauses ids `1..m` in file order. Nothing
          /// reserves ids when clauses are added through the C++ API, and CaDiCaL then assigns them
          /// differently — measured on de Morgan, whose 5 input clauses include two units: the CLI's
          /// first derived clause is id 6, the native route's is id 3.
          ///
          /// So `1..m` is an artefact of the parser, not a property of CaDiCaL. `reconstruction_plan`
          /// assumed it; `reconstruction_plan_of` uses this instead when it is available. */
          Originals: (int * Clause) list }

    /// What `SatProof` needs of a solver. Both backends implement it, so reconstruction is written
    /// once against this and works over either.
    ///
    /// The solver is ADVISORY in both cases and stays out of the trusted base: an `Unsat` verdict is
    /// not a Sylvia proof until `Steps` has been replayed through the kernel.
    type ISatBackend =
        /// How to identify this backend in an error message (an executable path, a DLL name).
        abstract member Description: string
        /// Whether the backend can actually run — the executable or the native library is present.
        abstract member IsAvailable: bool
        /// Solve `¬goal` and, on `Unsat`, return the refutation to replay.
        abstract member Run: CnfProblem -> SatRun

    /// A handle to the CaDiCaL solver (`cadical`).
    ///
    /// `exePath` defaults to the `SYLVIA_CADICAL` environment variable if set, otherwise `cadical.exe`
    /// (resolved on PATH). `timeoutMs` is the wall-clock budget the WRAPPER enforces by killing the
    /// process — matching `Sylvia.ATP.E`, we own the clock rather than trusting the solver's own limit
    /// flags under the Windows/MSYS2 build. The solver runs single-threaded (`--lrat` proof tracing),
    /// no forking.
    ///
    /// `plain` (default TRUE) runs the solver with pre- and inprocessing disabled. This is about the
    /// PROOF, not the verdict. CaDiCaL's default preprocessing introduces FRESH VARIABLES — on a
    /// 20-variable pigeonhole instance its LRAT referenced variables 21-29 — and justifies the
    /// clauses defining them with RAT steps, which are satisfiability-preserving rather than
    /// entailed and so have no reading as resolution. `SAT.rup_chain` is RUP-only by design, so those
    /// steps cannot be replayed: 12 of 82 steps failed on that instance, and reconstruction failed
    /// outright. With `--plain` the same instance produced 48 steps and no RAT at all.
    ///
    /// The cost is bearable because for our purposes solving is never the bottleneck — it is tens of
    /// milliseconds against seconds of kernel replay — and the whole point of the pipeline is a proof
    /// we can check. Pass `plain = false` when you only want a VERDICT (`valid`-style checks, or a
    /// countermodel) and are not going to reconstruct: preprocessing makes the solver stronger on
    /// instances where solving is genuinely hard.
    type Cadical(?exePath: string, ?timeoutMs: int, ?plain: bool) =
        let exe =
            defaultArg exePath (
                match Environment.GetEnvironmentVariable "SYLVIA_CADICAL" with
                | null | "" -> "cadical.exe"
                | p -> p)
        let timeout = defaultArg timeoutMs 10000
        let isPlain = defaultArg plain true

        /// The resolved path to the CaDiCaL executable.
        member _.ExePath = exe

        /// The wall-clock timeout (ms) the wrapper enforces.
        member _.TimeoutMs = timeout

        /// Whether pre/inprocessing is disabled, keeping the LRAT trace RUP-only and replayable.
        member _.Plain = isPlain

        /// Whether the executable exists (meaningful only for an absolute/relative path, not a PATH lookup).
        member _.IsAvailable = File.Exists exe

        /// Solve the CNF of `¬goal`. On UNSAT the goal is proved and the LRAT trace is returned for
        /// reconstruction; on SAT a countermodel is returned. The solver is advisory: an `Unsat`
        /// verdict is NOT a Sylvia proof until the LRAT trace is replayed through the kernel.
        member this.Solve(cnf: CnfProblem) : SatResult =
            let dimacs = dimacs_of cnf
            if not (File.Exists exe) then
                { Status = NotAvailable; Lrat = ""; Model = []; Raw = ""; Dimacs = dimacs }
            else
                let stamp = abs (dimacs.GetHashCode())
                let dir = Path.GetTempPath()
                let cnfFile = Path.Combine(dir, sprintf "sylvia_sat_%d.cnf" stamp)
                let lratFile = Path.Combine(dir, sprintf "sylvia_sat_%d.lrat" stamp)
                File.WriteAllText(cnfFile, dimacs)
                try
                    let psi =
                        ProcessStartInfo(
                            exe,
                            sprintf "-q --lrat --no-binary%s \"%s\" \"%s\""
                                    (if isPlain then " --plain" else "") cnfFile lratFile,
                            RedirectStandardOutput = true, RedirectStandardError = true,
                            UseShellExecute = false, CreateNoWindow = true)
                    use p = Process.Start psi
                    let sb = StringBuilder()
                    p.OutputDataReceived.Add(fun a -> if a.Data <> null then sb.AppendLine a.Data |> ignore)
                    p.BeginOutputReadLine()
                    if not (p.WaitForExit timeout) then
                        (try p.Kill() with _ -> ())
                        { Status = Timeout; Lrat = ""; Model = []; Raw = sb.ToString(); Dimacs = dimacs }
                    else
                        p.WaitForExit()                            // drain async stdout handlers
                        let raw = sb.ToString()
                        // CaDiCaL exit convention: 10 = SAT, 20 = UNSAT, other = undecided.
                        match p.ExitCode with
                        | 20 ->
                            let lrat = if File.Exists lratFile then File.ReadAllText lratFile else ""
                            { Status = Unsat; Lrat = lrat; Model = []; Raw = raw; Dimacs = dimacs }
                        | 10 ->
                            // Parse the `v ...` witness lines into an atom->bool countermodel.
                            let model =
                                raw.Split('\n')
                                |> Array.filter (fun l -> l.StartsWith "v ")
                                |> Array.collect (fun l -> l.Substring(2).Split([| ' '; '\t'; '\r' |], StringSplitOptions.RemoveEmptyEntries))
                                |> Array.choose (fun t -> match Int32.TryParse t with | true, n when n <> 0 -> Some n | _ -> None)
                                |> Array.choose (fun n ->
                                    match cnf.AtomOfVar.TryGetValue(abs n) with
                                    | true, atom -> Some(atom, n > 0)
                                    | _ -> None)
                                |> List.ofArray
                            { Status = Sat; Lrat = ""; Model = model; Raw = raw; Dimacs = dimacs }
                        | _ ->
                            { Status = Unknown; Lrat = ""; Model = []; Raw = raw; Dimacs = dimacs }
                finally
                    try File.Delete cnfFile with _ -> ()
                    try File.Delete lratFile with _ -> ()

        /// Convenience: solve directly from a goal `Prop`.
        member this.Prove(goal: Prop) : SatResult = this.Solve(cnf_of_negated_goal goal)

        interface ISatBackend with
            member _.Description = exe
            member this.IsAvailable = this.IsAvailable
            member this.Run(cnf) =
                let res = this.Solve cnf
                { Result = res
                  Steps = if res.Status = Unsat then parse_lrat res.Lrat else []
                  // The DIMACS parser reserved ids 1..m in file order, so the default seeding is
                  // correct here and there is nothing to override.
                  Originals = [] }

    (* ---------------------------------------------------------------------- *)
    (* RUP step  ->  explicit chain of BINARY resolutions                       *)
    (* ---------------------------------------------------------------------- *)

    /// One binary resolution in an unfolded RUP chain: resolve the running clause with clause
    /// `Antecedent` on variable `Pivot`, giving the new running clause `Result`.
    type ChainLink =
        { Antecedent: int
          Pivot: int
          Result: Clause }

    /// An LRAT step's hints, unfolded into an explicit binary-resolution derivation: start from
    /// clause `Start` and apply `Links` in order, ending at `Derived`.
    type RupChain =
        { /// The falsified antecedent (the conflict) the chain starts from.
          Start: int
          /// The binary resolutions, in application order.
          Links: ChainLink list
          /// The clause the chain actually derives. A SUBSET of the step's declared clause — the
          /// solver may declare a weaker clause, so a replay closes the gap by ∨-weakening.
          Derived: Clause }

    /// Unfold one LRAT `Add` step into a chain of binary resolutions.
    ///
    /// LRAT hints are the antecedents of a *unit-propagation* refutation: assign every literal of
    /// the step's clause to false, then walk the hints IN ORDER — each is unit under the running
    /// assignment and propagates its one remaining literal, until the last is falsified (the
    /// conflict). That is a resolution derivation in disguise: starting from the conflicting clause
    /// and resolving BACKWARDS against each propagating antecedent, on the literal that antecedent
    /// propagated, eliminates exactly the assigned literals and lands on a clause that subsumes the
    /// declared one.
    ///
    /// The ordinary binary step (2 hints) comes out as a single link, so this subsumes — and
    /// replaces — a special-cased binary resolution. `clauseOf` resolves a clause id to its
    /// literals (input clauses and previously derived ones alike).
    let rup_chain (clauseOf: int -> Clause option) (derived: Clause) (hints: int list) : Result<RupChain, string> =
        if hints |> List.exists (fun h -> h <= 0) then
            // A negative hint marks a RAT candidate: satisfiability-preserving, not entailed, so it
            // has no forward reading as resolution. Keep the solver on RUP-only proofs.
            Error "RAT step (negative hint) — only RUP steps replay as resolution"
        elif derived |> List.exists (fun l -> List.contains -l derived) then
            Error "the derived clause is tautological — no falsifying assignment to propagate from"
        else
            let value = Dictionary<int, bool>()                      // var -> value forced so far
            let isTrue l = match value.TryGetValue(abs l) with true, v -> v = (l > 0) | _ -> false
            let isFalse l = match value.TryGetValue(abs l) with true, v -> v <> (l > 0) | _ -> false
            for l in derived do value.[abs l] <- (l < 0)             // falsify the clause being derived
            let props = ResizeArray<int * int>()                     // (antecedent, literal it propagated)
            let mutable conflict = None
            let mutable err = None
            for h in hints do
                if conflict.IsNone && err.IsNone then
                    match clauseOf h with
                    | None -> err <- Some(sprintf "hint %d refers to an unknown clause" h)
                    | Some c ->
                        if c |> List.exists isTrue then ()           // already satisfied: contributes nothing
                        else
                            match c |> List.filter (isFalse >> not) |> List.distinct with
                            | [] -> conflict <- Some h
                            | [ u ] -> value.[abs u] <- (u > 0); props.Add(h, u)
                            | _ -> err <- Some(sprintf "hint %d is not unit under the propagated assignment" h)
            match err, conflict with
            | Some e, _ -> Error e
            | None, None -> Error "the hints never reach a conflict — not a RUP step"
            | None, Some cid ->
                let mutable res = (clauseOf cid).Value |> List.distinct
                let links = ResizeArray<ChainLink>()
                for i in props.Count - 1 .. -1 .. 0 do
                    let (h, u) = props.[i]
                    if res |> List.contains -u then                  // this propagation is used by the chain
                        let c = (clauseOf h).Value
                        res <-
                            (res |> List.filter (fun l -> l <> -u))
                            @ (c |> List.filter (fun l -> l <> u))
                            |> List.distinct
                        links.Add { Antecedent = h; Pivot = abs u; Result = res }
                let declared = Set.ofList derived
                if not (Set.isSubset (Set.ofList res) declared) then
                    Error(sprintf "the chain derives %A, which is not subsumed by the declared clause %A" res derived)
                else
                    Ok { Start = cid; Links = List.ofSeq links; Derived = res }

    (* ---------------------------------------------------------------------- *)
    (* Reconstruction plan  (LRAT trace  ->  Sylvia Prop obligations)          *)
    (* ---------------------------------------------------------------------- *)

    /// One reconstructed refutation step: clause `Id` (as a Sylvia `Prop` — a disjunction of literal
    /// atoms, or `F` for the empty clause) that follows from its `Premises` (earlier clauses, by id)
    /// via a unit-propagation / resolution chain. `IsEmpty` marks the terminal ⊥ step.
    ///
    /// To emit a checked `Theorem` of the goal, replay these in order: each step is an obligation
    /// `⊢ (∧ premise-clauses) ⇒ conclusion` discharged by resolution; the final `IsEmpty` step yields
    /// `¬Goal ⇒ F`, closed by `PropCalculus.contradiction_id` / `Contradiction` into `⊢ Goal`.
    type ResolutionStep =
        { Id: int
          Conclusion: Prop
          Literals: int list
          Premises: (int * Prop) list
          IsEmpty: bool }

    /// The `Prop` for a single DIMACS literal: the atom, or its negation.
    let lit_prop (cnf: CnfProblem) (l: Lit) : Prop =
        let atom = cnf.AtomOfVar.[abs l]
        if l > 0 then atom else !!atom

    /// The `Prop` for a clause: the disjunction of its literals, or `F` when empty (the ⊥ clause).
    let clause_prop (cnf: CnfProblem) (lits: Lit list) : Prop =
        match lits with
        | [] -> F
        | x :: xs -> xs |> List.fold (fun acc l -> acc + lit_prop cnf l) (lit_prop cnf x)

    /// Build the ordered reconstruction plan from a CNF and its LRAT proof. Each `Add` step becomes a
    /// `ResolutionStep` whose premises are looked up from the clauses derived so far; `Delete` steps are
    /// bookkeeping and produce no step. This layer is pure Sylvia data — no kernel calls — so it always
    /// runs; turning it into a `Theorem` is the caller's kernel-replay step.
    ///
    /// Build the plan, seeding the input clauses from `seed` — `(clause id, literals)` as the solver
    /// itself reported them. An empty `seed` falls back to ids `1..m` in DIMACS order.
    ///
    /// The distinction matters: `1..m` holds only because CaDiCaL's DIMACS parser reserves the range
    /// up front, and nothing reserves it when clauses are added through the API. Seeding the wrong
    /// clause against an id does not fail loudly — it builds a plan whose premises are the wrong
    /// formulas, so the kernel replay rejects a step that the solver derived correctly.
    let reconstruction_plan_seeded (cnf: CnfProblem) (seed: (int * Clause) list)
                                   (steps: LratStep list) : ResolutionStep list =
        let env = Dictionary<int, Prop>()
        match seed with
        | [] -> cnf.Clauses |> List.iteri (fun i c -> env.[i + 1] <- clause_prop cnf c)
        | _ -> for (id, lits) in seed do env.[id] <- clause_prop cnf lits
        [ for s in steps do
            match s with
            | Delete _ -> ()
            | Add(id, lits, hints) ->
                let concl = clause_prop cnf lits
                env.[id] <- concl
                let premises =
                    hints
                    |> List.map (fun h ->
                        match env.TryGetValue h with
                        | true, p -> h, p
                        | _ -> h, F)      // a forward/undefined reference — shouldn't happen in a valid proof
                yield { Id = id
                        Conclusion = concl
                        Literals = lits
                        Premises = premises
                        IsEmpty = List.isEmpty lits } ]

    /// Build the plan from a CNF and an LRAT proof whose input clauses are ids `1..m` — the shape a
    /// text LRAT file from `cadical`'s DIMACS parser always has.
    ///
    /// Prefer `reconstruction_plan_of`, which takes the ids from the solver rather than assuming them.
    let reconstruction_plan (cnf: CnfProblem) (steps: LratStep list) : ResolutionStep list =
        reconstruction_plan_seeded cnf [] steps

    /// Build the plan from a completed `SatRun`, using whichever input-clause ids that backend
    /// reported. This is the form to use: it is correct for both backends without the caller having
    /// to know which one produced the run.
    let reconstruction_plan_of (cnf: CnfProblem) (run: SatRun) : ResolutionStep list =
        reconstruction_plan_seeded cnf run.Originals run.Steps

    (* ---------------------------------------------------------------------- *)
    (* Native backend  (sylvia_cadical.dll)                                    *)
    (* ---------------------------------------------------------------------- *)

    /// In-process CaDiCaL via `sylvia_cadical.dll` (built from `src/native/Sylvia.CaDiCaL.Native`).
    ///
    /// Same verdicts and the same proof as `Cadical`, without spawning a process or touching the
    /// disk: measured on pigeonhole 4→3, the captured trace matches `cadical --plain --lrat` id for
    /// id, literal for literal and antecedent for antecedent. What it adds over the CLI:
    ///
    ///   - **No process, no temp files.** The CLI route writes a DIMACS file, spawns `cadical.exe`,
    ///     reads back an LRAT file and parses it. Here the clauses go straight in and the steps come
    ///     straight out.
    ///   - **No MSYS2 on PATH.** `cadical.exe` needs `msys-2.0.dll`, which is why the SAT tests had
    ///     to run from a Bash shell. `sylvia_cadical.dll` is statically linked and loads from a plain
    ///     Windows process.
    ///   - **API misuse is a status, not a crash.** The raw C API aborts the process on a contract
    ///     violation — not an exception, uncatchable, and it takes the CLR with it. The native layer
    ///     guards the state machine and returns an error code instead.
    ///   - **Assumptions and cores** (`sc_assume` / `sc_failed`), and incremental proof capture, which
    ///     a proof FILE cannot express — it is one merged artifact per session with no way to
    ///     attribute steps to the solve that produced them. Not surfaced by `Run` (which is one-shot,
    ///     to stay interchangeable with the CLI backend); they are the next layer up.
    module Native =

        [<Literal>]
        let private LIB = "sylvia_cadical"

        /// An opaque native solver handle.
        ///
        /// A readonly value struct wrapping the pointer: one field, sequentially laid out, so it is
        /// blittable and marshals as the bare pointer it is — no allocation, no wrapper cost, and it
        /// can be used directly in the `extern` signatures below. The point is that a raw `nativeint`
        /// can be passed to these functions by mistake and a wrong pointer here is not an exception:
        /// it is a process abort or a memory corruption. This makes that a compile error instead.
        [<Struct; IsReadOnly; StructLayout(LayoutKind.Sequential)>]
        type ScSolver =
            val private ptr: nativeint
            new(p: nativeint) = { ptr = p }
            member this.IsNull = this.ptr = IntPtr.Zero

        /// Status codes and step kinds from `sylvia_cadical.h`.
        let private SC_OK = 0
        let private SC_SAT = 10
        let private SC_UNSAT = 20
        let private SC_STEP_ORIGINAL = 0
        let private SC_STEP_DERIVED = 1
        let private SC_STEP_DELETED = 2
        let private SC_STEP_ASSUMPTION = 3

        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern IntPtr sc_signature()
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern ScSolver sc_create()
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern void sc_destroy(ScSolver s)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern IntPtr sc_last_error(ScSolver s)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_set_plain(ScSolver s)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_capture_proof(ScSolver s)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_set_timeout_ms(ScSolver s, int64 ms)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_add_clause(ScSolver s, int[] lits, unativeint n)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_declare_vars(ScSolver s, int n)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_solve(ScSolver s, int& status)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_val(ScSolver s, int lit, int& value)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_proof_num_steps(ScSolver s, int64& n)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_proof_num_lits(ScSolver s, int64& n)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_proof_num_antes(ScSolver s, int64& n)
        [<DllImport(LIB, CallingConvention = CallingConvention.Cdecl)>]
        extern int sc_proof_export(ScSolver s, int[] kinds, int64[] ids, int[] redundant,
                                           int[] witness, int[] lits, int64[] litOff, int64[] antes,
                                           int64[] anteOff, int64 stepsCap, int64 litsCap,
                                           int64 antesCap)

        /// Resolve `sylvia_cadical` from `SYLVIA_CADICAL_NATIVE` when it is set, so scripts can point
        /// at `bin/` without the DLL being beside the host process. Falls through to the default
        /// probing (app directory, then PATH) otherwise.
        ///
        /// Registered once, lazily — `SetDllImportResolver` throws if called twice for an assembly.
        let private installResolver =
            lazy (
                try
                    NativeLibrary.SetDllImportResolver(
                        Reflection.Assembly.GetExecutingAssembly(),
                        fun name _ _ ->
                            if name = LIB then
                                match Environment.GetEnvironmentVariable "SYLVIA_CADICAL_NATIVE" with
                                | null | "" -> IntPtr.Zero
                                | path ->
                                    match NativeLibrary.TryLoad path with
                                    | true, h -> h
                                    | _ -> IntPtr.Zero
                            else IntPtr.Zero)
                with _ -> ())          // already registered by another instance: harmless

        let private err (h: ScSolver) =
            match Marshal.PtrToStringAnsi(sc_last_error h) with
            | null -> ""
            | m -> m

        /// The CaDiCaL version the native library wraps, e.g. "cadical-3.0.1".
        let signature () =
            installResolver.Force()
            Marshal.PtrToStringAnsi(sc_signature ())

        /// Whether `sylvia_cadical.dll` can be loaded and called.
        let is_available () =
            try
                signature () |> ignore
                true
            with _ -> false

        /// In-process CaDiCaL. `timeoutMs` is enforced by a cooperative `Terminator` rather than by
        /// killing a process, so a timeout leaves the solver usable instead of losing it.
        ///
        /// `plain` (default TRUE) disables pre- and inprocessing, for the same reason it does on the
        /// CLI backend: preprocessing justifies its fresh variables with RAT steps, which are
        /// satisfiability-preserving rather than entailed and so have no reading as resolution —
        /// `rup_chain` is RUP-only by design and cannot replay them. Pass `false` only when a VERDICT
        /// is wanted and nothing will be reconstructed.
        type CadicalNative(?timeoutMs: int, ?plain: bool) =
            let timeout = defaultArg timeoutMs 10000
            let isPlain = defaultArg plain true

            /// The wall-clock budget (ms) handed to the solver's terminator.
            member _.TimeoutMs = timeout

            /// Whether pre/inprocessing is disabled, keeping the trace RUP-only and replayable.
            member _.Plain = isPlain

            /// Whether the native library is present and callable.
            member _.IsAvailable = is_available ()

            /// Solve the CNF of `¬goal`, returning the verdict and — on `Unsat` — the LRAT steps.
            member _.Solve(cnf: CnfProblem) : SatRun =
                installResolver.Force()
                let dimacs = dimacs_of cnf
                let notAvailable msg =
                    { Result = { Status = NotAvailable; Lrat = ""; Model = []; Raw = msg; Dimacs = dimacs }
                      Steps = []
                      Originals = [] }
                // The whole solve, as a function of the handle, so the caller below owns creation
                // and destruction in one place.
                let run (h: ScSolver) =
                    let check rc what =
                        if rc <> SC_OK then failwithf "sylvia_cadical: %s failed (%d): %s" what rc (err h)
                    if isPlain then check (sc_set_plain h) "sc_set_plain"
                    check (sc_capture_proof h) "sc_capture_proof"
                    check (sc_set_timeout_ms(h, int64 timeout)) "sc_set_timeout_ms"
                    if cnf.NumVars > 0 then check (sc_declare_vars(h, cnf.NumVars)) "sc_declare_vars"
                    for c in cnf.Clauses do
                        let arr = Array.ofList c
                        check (sc_add_clause(h, arr, unativeint arr.Length)) "sc_add_clause"

                    let mutable status = 0
                    check (sc_solve(h, &status)) "sc_solve"

                    match status with
                    | s when s = SC_UNSAT ->
                        let mutable nSteps = 0L
                        let mutable nLits = 0L
                        let mutable nAntes = 0L
                        check (sc_proof_num_steps(h, &nSteps)) "sc_proof_num_steps"
                        check (sc_proof_num_lits(h, &nLits)) "sc_proof_num_lits"
                        check (sc_proof_num_antes(h, &nAntes)) "sc_proof_num_antes"
                        let n = int nSteps
                        let kinds = Array.zeroCreate<int> n
                        let ids = Array.zeroCreate<int64> n
                        let redundant = Array.zeroCreate<int> n
                        let witness = Array.zeroCreate<int> n
                        let lits = Array.zeroCreate<int> (int nLits)
                        let antes = Array.zeroCreate<int64> (int nAntes)
                        let litOff = Array.zeroCreate<int64> (n + 1)
                        let anteOff = Array.zeroCreate<int64> (n + 1)
                        check (sc_proof_export(h, kinds, ids, redundant, witness, lits, litOff,
                                               antes, anteOff, nSteps, nLits, nAntes))
                              "sc_proof_export"

                        // Clause ids are int64 natively but `LratStep` carries int, as the text LRAT
                        // parser always has. Refuse rather than truncate — a wrapped id would produce
                        // a silently wrong replay instead of a failure.
                        let narrow (x: int64) =
                            if x > int64 Int32.MaxValue then
                                failwithf "sylvia_cadical: clause id %d exceeds Int32 — LratStep cannot carry it" x
                            else int x

                        let clauseAt i = [ for j in int litOff.[i] .. int litOff.[i + 1] - 1 -> lits.[j] ]
                        let hintsAt i = [ for j in int anteOff.[i] .. int anteOff.[i + 1] - 1 -> narrow antes.[j] ]

                        // ORIGINAL steps are NOT inert. Nothing reserves the id range when clauses
                        // are added through the API, so CaDiCaL does not number the inputs 1..m the
                        // way the DIMACS parser does — these are the only record of which id holds
                        // which input clause, and the reconstruction's premises depend on it.
                        let originals =
                            [ for i in 0 .. n - 1 do
                                if kinds.[i] = SC_STEP_ORIGINAL then
                                    yield narrow ids.[i], clauseAt i ]

                        let steps =
                            [ for i in 0 .. n - 1 do
                                // DERIVED and ASSUMPTION steps both introduce an entailed clause
                                // together with the antecedents that entail it.
                                if kinds.[i] = SC_STEP_DERIVED || kinds.[i] = SC_STEP_ASSUMPTION then
                                    yield Add(narrow ids.[i], clauseAt i, hintsAt i)
                                elif kinds.[i] = SC_STEP_DELETED then
                                    // The tracer reports deletions one clause at a time, without the
                                    // "after step N" grouping the text format uses. Inert either way:
                                    // deletions carry no logical content for the reconstruction.
                                    yield Delete(0, [ narrow ids.[i] ])
                                elif kinds.[i] <> SC_STEP_ORIGINAL then
                                    failwithf "sylvia_cadical: unknown proof step kind %d" kinds.[i] ]

                        { Result = { Status = Unsat; Lrat = ""; Model = []; Raw = ""; Dimacs = dimacs }
                          Steps = steps
                          Originals = originals }

                    | s when s = SC_SAT ->
                        let model =
                            [ for v in 1 .. cnf.NumVars do
                                let mutable value = 0
                                check (sc_val(h, v, &value)) "sc_val"
                                match cnf.AtomOfVar.TryGetValue v with
                                | true, atom -> yield atom, value > 0
                                | _ -> () ]
                        { Result = { Status = Sat; Lrat = ""; Model = model; Raw = ""; Dimacs = dimacs }
                          Steps = []
                          Originals = [] }

                    | _ ->
                        // The cooperative terminator fired, or a resource limit was hit.
                        { Result = { Status = Timeout; Lrat = ""; Model = []; Raw = ""; Dimacs = dimacs }
                          Steps = []
                          Originals = [] }

                let h = sc_create ()
                if h.IsNull then
                    notAvailable "sc_create returned null"
                else
                    try run h
                    finally sc_destroy h

            /// Convenience: solve directly from a goal `Prop`.
            member this.Prove(goal: Prop) : SatRun = this.Solve(cnf_of_negated_goal goal)

            interface ISatBackend with
                member _.Description = LIB + ".dll"
                member this.IsAvailable = this.IsAvailable
                member this.Run(cnf) = this.Solve cnf
