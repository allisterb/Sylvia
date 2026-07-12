namespace Sylvia

open System
open System.Diagnostics
open System.Text
open System.Text.RegularExpressions

open FSharp.Quotations
open FSharp.Quotations.Patterns

open Formula
open FsExpr

/// Optional ADVISORY integration with the E theorem prover (https://eprover.org).
///
/// E is a saturation-based ATP for full first-order logic with equality. Here it is used purely as an
/// advisory oracle — it answers "does a proof of this goal exist?" and "which axioms did the proof
/// use?" — and is NEVER admitted to Sylvia's trusted base. The intended workflow (Sledgehammer-style)
/// is to let E establish provability and surface the minimal fact set, from which Sylvia's own
/// equational kernel can then build a human-readable, kernel-checked proof.
///
/// E complements Sylvia's own automation: the equational decision procedures (e.g. the set-algebra
/// `metaset`/`metasubset` tactics) own the decidable Boolean fragment, while E handles the quantified /
/// relational FOL goals beyond it. It is distinct from `Sylvia.Solver.Z3` (an SMT *solver*, strong on
/// background theories and model finding); E is an *ATP*, strong on general FOL and equality.
module ATP =

    /// The SZS ontology status E reports for a run (plus wrapper-level outcomes).
    type EStatus =
        /// The conjecture is a theorem of the axioms (a proof was found).
        | Theorem
        /// The negated conjecture is satisfiable together with the axioms — the goal is NOT a theorem.
        | CounterSatisfiable
        /// The axiom set alone is unsatisfiable.
        | Unsatisfiable
        /// The axiom set (with conjecture) is satisfiable.
        | Satisfiable
        /// E terminated (saturated or budget) without deciding.
        | GaveUp
        /// A resource limit was hit inside E.
        | ResourceOut
        /// The wrapper-enforced wall-clock timeout fired (E was killed). NEVER treat as a verdict.
        | Timeout
        /// The E executable was not found.
        | NotAvailable
        /// Any other / unrecognised SZS status token.
        | Other of string

    /// Result of an advisory E run. `UsedFacts` are the names of the input axioms that appear in the
    /// proof object (the minimal-ish fact set to seed a Sylvia reconstruction). `Tptp` is the exact
    /// problem sent to E; `Raw` is E's full stdout (empty on timeout / unavailable).
    type EResult =
        { Status: EStatus
          UsedFacts: string list
          Tptp: string
          Raw: string }

    (* ---------------------------------------------------------------------- *)
    (* Sylvia Prop  ->  TPTP FOF translation                                   *)
    (* ---------------------------------------------------------------------- *)

    // A base-sort Var is a TERM (variable/constant); a function-typed Var (e.g. 't -> bool) is a
    // predicate/function SYMBOL. In TPTP, variables are upper-case and symbols lower-case.
    let private isFunctionType (t: Type) = t.Name.StartsWith "FSharpFunc"
    let private up (n: string) = n.ToUpperInvariant()
    let private lo (n: string) = if n.Length > 0 then string (Char.ToLowerInvariant n.[0]) + n.Substring 1 else n

    /// Translate a Sylvia first-order proposition to a TPTP FOF formula string.
    ///
    /// Handles ∀/∃ (with restricted ranges: `∀x|R:B` ↦ `![X]:(R => B)`, `∃x|R:B` ↦ `?[X]:(R & B)`),
    /// ∧/∨/¬/⇒, `=` on booleans (↦ `<=>`) and on terms (↦ `=`), and predicate/function application.
    /// Quantifier-bound base-sort variables become upper-case TPTP variables; any base-sort variable
    /// left free is universally closed with a leading `![…]`. Function-typed variables (predicate /
    /// function symbols) become lower-case functors.
    let tptpOfProp (p: Prop) : string =
        let freeVars = System.Collections.Generic.HashSet<string>()

        let rec term (bound: string list) (e: Expr) : string =
            match e with
            | Var v when isFunctionType v.Type -> lo v.Name
            | Var v ->
                let n = up v.Name
                if not (List.contains n bound) then freeVars.Add n |> ignore
                n
            // A named value (`ScalarConst<'t>`, embedded as ValueWithName) is a CONSTANT — a lower-case
            // nullary functor — NOT a free variable (which would be universally closed). This is how the
            // caller distinguishes e.g. the constant `socrates` from a term variable.
            | ValueWithName(_, _, n) -> lo n
            | Application(f, a) ->
                // flatten a curried application  f a1 a2 …  to  f(a1,a2,…)
                let rec spine acc = function
                    | Application(g, x) -> spine (term bound x :: acc) g
                    | h -> h, acc
                let head, args = spine [ term bound a ] f
                sprintf "%s(%s)" (term bound head) (String.Join(",", args))
            | Call(None, mi, args) -> sprintf "%s(%s)" (lo mi.Name) (String.Join(",", args |> List.map (term bound)))
            | Value(v, _) -> lo (string v)
            | _ -> failwithf "E.tptpOfProp: cannot translate term %A" e

        let rec form (bound: string list) (e: Expr) : string =
            match e with
            | True -> "$true"
            | False -> "$false"
            // A bare boolean variable is a PROPOSITIONAL ATOM (a 0-ary predicate) → lower-case symbol,
            // NOT a universally-closed term variable.
            | Var v when v.Type = typeof<bool> -> lo v.Name
            | Not a -> sprintf "~%s" (form bound a)
            | And(a, b) -> sprintf "(%s & %s)" (form bound a) (form bound b)
            | Or(a, b) -> sprintf "(%s | %s)" (form bound a) (form bound b)
            | Implies(a, b) -> sprintf "(%s => %s)" (form bound a) (form bound b)
            | Equals(a, b) when a.Type = typeof<bool> -> sprintf "(%s <=> %s)" (form bound a) (form bound b)
            | Equals(a, b) -> sprintf "(%s = %s)" (term bound a) (term bound b)
            | ForAll(_, bs, range, body) -> quant "!" "=>" bound bs range body
            | Exists(_, bs, range, body) -> quant "?" "&" bound bs range body
            | _ -> term bound e   // an atom: predicate application

        and quant sym connective bound (bs: Var list) range body =
            let vs = bs |> List.map (fun v -> up v.Name)
            let bound' = vs @ bound
            let b = form bound' body
            let inner =
                match range with
                | True -> b
                | _ -> sprintf "(%s %s %s)" (form bound' range) connective b
            sprintf "%s[%s]: %s" sym (String.Join(",", vs)) inner

        let body = form [] (expand p.Expr)
        if freeVars.Count = 0 then body
        else sprintf "![%s]: (%s)" (String.Join(",", freeVars)) body

    /// A single `fof(...)` clause line.
    let fofLine (role: string) (name: string) (p: Prop) =
        sprintf "fof(%s, %s, ( %s ))." name role (tptpOfProp p)

    /// Assemble a complete TPTP problem: named axioms plus a single conjecture.
    let tptpProblem (axioms: (string * Prop) list) (goalName: string) (goal: Prop) : string =
        let ls = (axioms |> List.map (fun (n, p) -> fofLine "axiom" n p)) @ [ fofLine "conjecture" goalName goal ]
        String.Join("\n", ls) + "\n"

    (* ---------------------------------------------------------------------- *)
    (* Runner                                                                  *)
    (* ---------------------------------------------------------------------- *)

    let private parseStatus (raw: string) : EStatus =
        raw.Split('\n')
        |> Array.tryPick (fun l ->
            let i = l.IndexOf "SZS status"
            if i >= 0 then Some(l.Substring(i + 10).Trim().Split(' ').[0]) else None)
        |> function
            | Some "Theorem" -> Theorem
            | Some "CounterSatisfiable" -> CounterSatisfiable
            | Some "Unsatisfiable" -> Unsatisfiable
            | Some "Satisfiable" -> Satisfiable
            | Some "GaveUp" -> GaveUp
            | Some "ResourceOut" -> ResourceOut
            | Some other -> Other other
            | None -> Other "Unknown"

    // Names of the input formulae that occur in the proof object (`file('<path>', <name>)`).
    let private parseUsedFacts (goalName: string) (raw: string) : string list =
        Regex.Matches(raw, @"file\([^,]*,\s*([a-zA-Z0-9_]+)\s*\)")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Groups.[1].Value)
        |> Seq.filter (fun n -> n <> goalName)
        |> Seq.distinct
        |> List.ofSeq

    /// An advisory handle to the E prover (`eprover`).
    ///
    /// `exePath` defaults to the `SYLVIA_EPROVER` environment variable if set, otherwise `eprover.exe`
    /// (resolved on PATH). `timeoutMs` is the wall-clock budget the WRAPPER enforces by killing the
    /// process — E's own `--cpu-limit` relies on POSIX signals that are unreliable under the Windows
    /// MSYS2 build, so the wrapper owns the clock. The prover is invoked with `--auto` (a single
    /// auto-selected strategy) which never `fork()`s — important because forking is pathologically slow
    /// under MSYS2 emulation.
    type EProver(?exePath: string, ?timeoutMs: int) =
        let exe =
            defaultArg exePath (
                match Environment.GetEnvironmentVariable "SYLVIA_EPROVER" with
                | null | "" -> "eprover.exe"
                | p -> p)
        let timeout = defaultArg timeoutMs 10000

        /// The resolved path to the E executable.
        member _.ExePath = exe

        /// The wall-clock timeout (ms) the wrapper enforces.
        member _.TimeoutMs = timeout

        /// Whether the E executable exists (only meaningful for an absolute/relative path, not a bare
        /// PATH lookup).
        member _.IsAvailable = IO.File.Exists exe

        /// The exact TPTP problem that `Prove` would send for these axioms and goal.
        member _.Problem(axioms, goalName, goal) = tptpProblem axioms goalName goal

        /// Ask E whether `goal` follows from the named `axioms`. Returns the SZS status, the minimal
        /// fact set used in the proof, the TPTP sent, and E's raw output. Advisory only — a `Theorem`
        /// verdict is NOT a Sylvia proof and does not enter the trusted base.
        member _.Prove(axioms: (string * Prop) list, goal: Prop, ?goalName: string) : EResult =
            let gname = defaultArg goalName "goal"
            let tptp = tptpProblem axioms gname goal
            if not (IO.File.Exists exe) then
                { Status = NotAvailable; UsedFacts = []; Tptp = tptp; Raw = "" }
            else
                let tmp = IO.Path.Combine(IO.Path.GetTempPath(), sprintf "sylvia_e_%d.p" (abs (tptp.GetHashCode())))
                IO.File.WriteAllText(tmp, tptp)
                try
                    let psi =
                        ProcessStartInfo(
                            exe,
                            sprintf "--auto --proof-object \"%s\"" tmp,
                            RedirectStandardOutput = true,
                            RedirectStandardError = true,
                            UseShellExecute = false,
                            CreateNoWindow = true)
                    use p = Process.Start psi
                    let sb = StringBuilder()
                    p.OutputDataReceived.Add(fun a -> if a.Data <> null then sb.AppendLine a.Data |> ignore)
                    p.BeginOutputReadLine()
                    if not (p.WaitForExit timeout) then
                        (try p.Kill() with _ -> ())
                        { Status = Timeout; UsedFacts = []; Tptp = tptp; Raw = sb.ToString() }
                    else
                        // The timed WaitForExit can return before the async stdout handlers have
                        // drained; a final blocking WaitForExit flushes them.
                        p.WaitForExit()
                        let raw = sb.ToString()
                        { Status = parseStatus raw
                          UsedFacts = parseUsedFacts gname raw
                          Tptp = tptp
                          Raw = raw }
                finally
                    try IO.File.Delete tmp with _ -> ()
