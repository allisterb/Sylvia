namespace Sylvia

open System
open System.Diagnostics
open System.IO
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
///   goal φ  ──cnfOfNegatedGoal──▶  CNF(¬φ)  ──Cadical.Solve──▶  UNSAT + LRAT proof
///           ──parseLrat──▶  resolution steps  ──reconstructionPlan──▶  Sylvia `Prop` obligations
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
    /// for typical goals and keeps atoms in 1-1 correspondence with Sylvia `Prop`s (which the kernel
    /// replay depends on). The scalable upgrade is a Tseitin/Plaisted-Greenbaum encoding with auxiliary
    /// variables — at the cost of teaching the reconstruction how to discharge the definitional
    /// clauses. See the module doc / design notes.
    let cnfOfNegatedGoal (goal: Prop) : CnfProblem =
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
            | BNot _ -> failwith "cnfOfNegatedGoal: formula not in NNF (bug)"

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
    let dimacsOf (p: CnfProblem) : string =
        let sb = StringBuilder()
        sb.AppendLine(sprintf "p cnf %d %d" p.NumVars (List.length p.Clauses)) |> ignore
        for c in p.Clauses do
            for l in c do
                sb.Append(l).Append(' ') |> ignore
            sb.Append("0").Append('\n') |> ignore
        sb.ToString()

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
    /// entailed and so have no reading as resolution. `SAT.rupChain` is RUP-only by design, so those
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
            let dimacs = dimacsOf cnf
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
        member this.Prove(goal: Prop) : SatResult = this.Solve(cnfOfNegatedGoal goal)

    (* ---------------------------------------------------------------------- *)
    (* LRAT proof parsing                                                       *)
    (* ---------------------------------------------------------------------- *)

    /// One line of a (text) LRAT proof.
    ///
    /// `Add(id, literals, hints)` — introduce clause `id`; `literals` are its signed literals (empty =
    /// the ⊥ clause); `hints` are the antecedent clause ids whose unit propagation entails it (a RUP /
    /// resolution chain). `Delete(afterId, ids)` — clauses `ids` may be forgotten (checker bookkeeping;
    /// carries no logical content).
    type LratStep =
        | Add of id: int * literals: int list * hints: int list
        | Delete of afterId: int * ids: int list

    /// Parse text LRAT (as emitted by `cadical --lrat --no-binary`). Ignores blank lines and `c`
    /// comments. Each addition line is  `id  lit* 0  hint* 0`;  each deletion line is  `id d  cid* 0`.
    let parseLrat (text: string) : LratStep list =
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
    let rupChain (clauseOf: int -> Clause option) (derived: Clause) (hints: int list) : Result<RupChain, string> =
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
    let litProp (cnf: CnfProblem) (l: Lit) : Prop =
        let atom = cnf.AtomOfVar.[abs l]
        if l > 0 then atom else !!atom

    /// The `Prop` for a clause: the disjunction of its literals, or `F` when empty (the ⊥ clause).
    let clauseProp (cnf: CnfProblem) (lits: Lit list) : Prop =
        match lits with
        | [] -> F
        | x :: xs -> xs |> List.fold (fun acc l -> acc + litProp cnf l) (litProp cnf x)

    /// Build the ordered reconstruction plan from a CNF and its LRAT proof. Input clauses are seeded as
    /// ids `1..m` (their DIMACS order — the ids CaDiCaL references). Each `Add` step becomes a
    /// `ResolutionStep` whose premises are looked up from the clauses derived so far; `Delete` steps are
    /// bookkeeping and produce no step. This layer is pure Sylvia data — no kernel calls — so it always
    /// runs; turning it into a `Theorem` is the caller's kernel-replay step.
    let reconstructionPlan (cnf: CnfProblem) (steps: LratStep list) : ResolutionStep list =
        let env = Dictionary<int, Prop>()
        cnf.Clauses |> List.iteri (fun i c -> env.[i + 1] <- clauseProp cnf c)
        [ for s in steps do
            match s with
            | Delete _ -> ()
            | Add(id, lits, hints) ->
                let concl = clauseProp cnf lits
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
