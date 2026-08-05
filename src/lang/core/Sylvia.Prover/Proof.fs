namespace Sylvia

open System
open System.Reflection

open FSharp.Quotations

open Descriptions
open Formula

/// A theory is a set of axioms and a set of rules that transform one formula into another
type Theory(axioms: Axioms, rules: Rules, ?formula_printer:Expr->string) =
    // `AxEquiv` is a pure function of the term for a given theory, and the `Proof` constructor calls
    // it on the whole statement after EVERY step — so a reconstruction re-answers it for the same
    // expression object thousands of times. Keyed on the INPUT, by reference, so a hit skips the
    // `expand` as well as the recognition: profiled, `expand` is 2.49% of total under `AxEquiv`
    // while `equational_logic_axioms` is 0.11%, so the expand IS the cost. (An earlier attempt at
    // this keyed on `expand a` — caching the cheap half after paying for the expensive one, which
    // is why it showed a 60% hit rate and no benefit.) Per-instance: different theories recognize
    // different axioms. Weak keys, so nothing is retained.
    let axCache = System.Runtime.CompilerServices.ConditionalWeakTable<Expr, obj>()

    member val Axioms = axioms
    member val Rules = rules
    member val PrintFormula = defaultArg formula_printer Display.print_formula
    member x.AxEquiv a =
        match axCache.TryGetValue a with
        | true, cached -> unbox<bool> cached
        | _ ->
            let r = a |> expand |> x.Axioms |> Option.isSome
            axCache.GetValue(a, System.Runtime.CompilerServices.ConditionalWeakTable<Expr, obj>.CreateValueCallback(fun _ -> box r)) |> ignore
            r
    static member (|-) ((c:Theory), a) = c.AxEquiv a
    static member (|-) ((c:Theory), a:Term<_>) = c.AxEquiv a.Expr
    /// The default logical theory used in Sylvia proofs.
    static member val S =     
        let reduce = Admit("Reduce logical constants in (expression)", EquationalLogic._reduce_constants)

        let left_assoc = Admit("Logical operators in (expression) are left-associative", EquationalLogic._left_assoc)
        
        let right_assoc = Admit("Logical operators in (expression) are right-associative", EquationalLogic._right_assoc)
          
        let commute = Admit("Logical operators in (expression) are commutative", EquationalLogic._commute)

        let distrib = Admit("Distribute logical terms in (expression)", EquationalLogic._distrib)
        
        let collect = Admit("Collect distributed logical terms in (expression)", EquationalLogic._collect)

        let idemp = Admit("Substitute idempotent logical terms in (expression)", EquationalLogic._idemp)

        let excluded_middle = Admit("Logical terms in (expression) satisfy the law of excluded middle", EquationalLogic._excluded_middle)

        let golden_rule = Admit("Logical terms in (expression) satisfy the golden rule", EquationalLogic._golden_rule)

        let def_implies = Admit("Substitute definition of implication into (expression)", EquationalLogic._def_implies)

        let shunt = Admit("Shunt implication in (expression)", EquationalLogic._shunt)

        let rshunt = Admit("Reverse shunt implication in (expression)", EquationalLogic._rshunt)

        let mutual_implication = Admit("The (expression) contains a mutual implication", EquationalLogic._mutual_implication)
        
        let subst_and = Admit("Substitute an equivalent subexpression in (expression)", EquationalLogic._subst_and)

        let subst_implies = Admit("Substitute an equivalent subexpression in (expression)", EquationalLogic._subst_implies)

        let subst_and_implies = Admit("Substitute an equivalent subexpression in (expression). ", EquationalLogic._subst_and_implies)

        let subst_true = Admit("Substitute an equivalent subexpression in (expression) with the constant true. ", EquationalLogic._subst_true)

        let subst_false = Admit("Substitute an equivalent subexpression in (expression) with the constant false. ", EquationalLogic._subst_false)

        let subst_or_and = Admit("Use the Shannon substitution in (expression)", EquationalLogic._subst_or_and)

        let distrib_implies = Admit("Distribute implication in (expression)", EquationalLogic._distrib_implies)

        let double_neg = Admit("Rewrite (expression) using double negation and De Morgan's laws", EquationalLogic._double_neg)

        let empty_range = Admit("Substitute the quantifier's empty range in (expression)", EquationalLogic._empty_range)

        let trade_body = Admit("Move the quantifier's range into its body in (expression)", EquationalLogic._trade_body)

        let collect_forall_and = Admit("Collect \u2200 quantifer terms distributed over \u2227 in (expression)", EquationalLogic._collect_forall_and)

        let collect_exists_or = Admit("Collect \u2203 quantifer terms distributed over or in (expression)", EquationalLogic._collect_exists_or)

        let distrib_or_forall = Admit("\u2228 distributes over \u2200 in (expression)", EquationalLogic._distrib_or_forall)

        let split_range_forall = Admit("Split \u2200 quantifer range in (expression)", EquationalLogic._split_range_forall)
        
        let split_range_exists = Admit("Split \u2203 quantifer range in (expression)", EquationalLogic._split_range_exists)

        let normalize = Admit("Normalize associative-commutative logical operators in (expression)", EquationalLogic._normalize)

        let normalize_assoc = Admit("Normalize associativity of logical operators in (expression)", EquationalLogic._normalize_assoc)

        let simp = Admit("Simplify (expression)", EquationalLogic._simp)

        let elim_to_xor = Admit("Rewrite (expression) with ⊕ and ∧", EquationalLogic._elim_to_xor)

        let distrib_and_xor = Admit("Distribute ∧ over ⊕ in (expression)", EquationalLogic._distrib_and_xor)

        let and_normalize = Admit("Normalize ∧ monomial in (expression)", EquationalLogic._and_normalize)

        let xor_normalize = Admit("Normalize ⊕ terms in (expression)", EquationalLogic._xor_normalize)

        Theory(EquationalLogic.equational_logic_axioms, [
            reduce
            left_assoc
            right_assoc
            commute
            distrib
            collect
            idemp
            excluded_middle
            golden_rule
            def_implies
            shunt
            rshunt
            mutual_implication
            subst_and
            subst_implies
            subst_and_implies
            subst_true
            subst_false
            subst_or_and
            distrib_implies
            double_neg
            empty_range
            trade_body
            collect_forall_and
            collect_exists_or
            distrib_or_forall
            split_range_forall
            split_range_exists
            normalize
            normalize_assoc
            simp
            elim_to_xor
            distrib_and_xor
            and_normalize
            xor_normalize
        ])

and Axioms = (Expr -> AxiomDescription option)

/// A rule specifies how a formula can be transformed into another formula
and Rule = 
    /// An admitted rule in a theory
    | Admit of string * (Expr -> Expr) 
    /// // A derived rule in a theory
    | Derive of string * Proof * (Proof -> Expr -> Expr) 
    /// A deduced rule in a theory
    | Deduce of string * Proof * (Proof -> Expr -> Expr)
    /// A fact ESTABLISHED under the antecedent of the goal, which later `Deduce`/`Establish` steps
    /// of the same proof may then use as though it were a conjunct of that antecedent.
    ///
    /// Carries a proof of `(B₁ ∧ … ∧ Bₖ) ⇒ Z` and changes the expression NOT AT ALL — it only
    /// records `Z`. `Deduce` already lets a theorem be used when every `Bᵢ` is a conjunct of the
    /// goal's antecedent; this is what lets the covered set GROW, which is what a forward
    /// derivation (a resolution refutation, say) needs and `Deduce` alone cannot express.
    ///
    /// Sound by induction on the established set Δ, with invariant `A ⊨ Z` for every `Z ∈ Δ`:
    /// the conjuncts of `A` are entailed by `A`, and if every `Bᵢ` is entailed by `A` and
    /// `⊢ (∧Bᵢ) ⇒ Z` is a theorem then `A ⊨ Z`. Because it rewrites nothing, none of the
    /// substitution/Leibniz side conditions that constrain assumptions in equational logic apply.
    | Establish of string * Proof
    /// A defined rule in a theory
    | Define of string * (Expr -> Expr)
with
    member x.Name =
        match x with
        | Admit(n, _) -> n
        | Derive(n, _, _) -> n
        | Deduce(n,_,_) -> n
        | Establish(n, _) -> n
        | Define(n, _) -> n
    member x.Apply =
        match x with
        | Admit(_, r) -> r
        | Derive(_, p, r) -> r p
        | Deduce(_, p, r) -> r p
        | Establish(_, _) -> id          // records a fact; the expression is untouched
        | Define(_, r) -> r
    /// A bare rule used where a RuleApplication is expected (e.g. as a proof step) means "apply
    /// the rule to the whole expression". This is the implicit form of `apply r` / `r |> apply`.
    /// (F# only applies this in a directly-typed position such as a RuleApplication-list element,
    /// NOT through a `|>` pipe — an addressed step must still say where via `at`/`apply_left`/…)
    static member op_Implicit (r: Rule) : RuleApplication = RuleApplication.Apply r

and Rules = Rule list

and Proof(a:Expr, theory: Theory, steps: RuleApplication list, ?lemma:bool) =
    /// The logical theory used for the proof.
    static let mutable logic:Theory = Theory.S
    /// The proof log level.
    static let mutable logLevel:int = 1 

    let ruleNames = List.concat [
            let logicRules = (logic.Rules: Rule list) in logicRules |> List.map (fun (r:Rule) -> r.Name) 
            theory.Rules |> List.map (fun (r:Rule) -> r.Name)
        ]
    let stepNames: string list = steps |> List.map (fun r -> r.RuleName)
    let print_formula = theory.PrintFormula
    let logBuilder = System.Text.StringBuilder()
    let l = defaultArg lemma false
    let isAxiom = l && steps.Length = 0
    
    // Proof logging
    let proof_sep = ""
    // The log functions take a THUNK so a line that will not be output is never
    // formatted: formatting goes through print_formula, which decompiles the
    // expression — a measurable cost on suppressed log paths (see
    // docs/expressions-perf.md). Printed output is identical to the eager version.
    let output (x:string) (s:string) =
        logBuilder.AppendLine(x) |> ignore
        printfn "%s" s

    // Whether a `prooflog` line is written, and whether it is indented, is FIXED for the life of
    // the proof: both depend only on `steps.Length`, the log level, and whether this is a lemma.
    //
    // These used to be decided inside a `_prooflog` that took a `unit -> string` thunk. The thunk
    // was there so a suppressed line never paid for its formatting (which goes through
    // `print_formula`, a decompilation) — but it also meant every logging call allocated a closure
    // even when the level guaranteed nothing would be written, and a reconstruction makes several
    // per step across thousands of proof objects. Deciding once and testing `prooflog_on` AT THE
    // CALL SITE keeps the laziness exactly and drops the closure.
    let prooflog_on =
        let is_short_lemma_proof = l && steps.Length <= 2
        (not l) || (l && logLevel > 2) || (not is_short_lemma_proof && logLevel >= 1)
    /// The two lemma branches indent by 8 spaces; the non-lemma branch does not.
    let prooflog_indent = l
    /// Emit a `prooflog` line. Callers MUST guard with `prooflog_on` — this does not re-check, so
    /// that the message is never built when it would be discarded.
    let prooflog_s (x:string) = output x (if prooflog_indent then "        " + x else x)
    /// `alwayslog` was `_prooflog steps 3 false`, and `isLemma = false` takes the unconditional
    /// branch — so it emits for every proof at every level. There is no predicate to test.
    let alwayslog_s (x:string) = output x x
    do if not l then
        match logLevel with
        | 0 -> alwayslog_s (sprintf "Proof log level is %i. Only necessary output will be printed."  logLevel)
        | 1 -> alwayslog_s (sprintf "Proof log level is %i. Short proofs of lemmas won't be printed."  logLevel)
        | 2 -> alwayslog_s (sprintf "Proof log level is %i. All proofs of lemmas will be printed."  logLevel)
        | 3 -> alwayslog_s (sprintf "Proof log level is %i. All proofs of axioms and lemmas will be printed."  logLevel)
        | _ -> failwith "Unknown proof log level."
    // These headers go through `alwayslog`, which passes `isLemma = false` — so `_prooflog`'s
    // "never format a line that will not be output" thunk does NOT apply to them, and every proof
    // object formats and emits its statement regardless of the log level. A reconstruction builds
    // thousands of lemma proofs over the whole clause conjunction: the `[Lemma]` line at
    // `steps.Length <= 2` alone measured 8.2% of CPU, and `print_formula` is the cost (replacing
    // the `sprintf` with concatenation changed nothing — F# caches a literal format string).
    //
    // At level 0 the banner above promises "Only necessary output will be printed", and thousands
    // of `[Lemma] <whole clause conjunction>.` lines are not that. Gating the lemma/axiom headers
    // on `logLevel > 0` makes level 0 honour its own contract and is worth ~12% of a reconstruction.
    // Levels 1-3 are untouched; the only output difference anywhere in the example suite is those
    // lines disappearing at level 0. A top-level proof still announces itself at every level.
    do
        if isAxiom && logLevel > 0 && logLevel <= 2 then
            alwayslog_s (sprintf "[Axiom] %s." (print_formula a))
        else if isAxiom && logLevel > 2 then
            alwayslog_s (sprintf "[Axiom] %s:" (print_formula a))
        else if l && logLevel > 2 then
            alwayslog_s (sprintf "[Lemma] %s:" (print_formula a))
        else if l && logLevel > 0 && logLevel < 2 && steps.Length <= 2 then
            alwayslog_s (sprintf "[Lemma] %s." (print_formula a))
        else if l && logLevel > 0 && logLevel < 2 && steps.Length >= 2 then
            alwayslog_s (sprintf "[Lemma] %s:" (print_formula a))
        else if not l then
            alwayslog_s (sprintf "Proof of %s:" (print_formula a))

    let mutable _state = a
    let mutable state:(Expr * Lazy<string>) list = []
    let mutable stepCount = 0

    // Facts established under this proof's antecedent by `Rule.Establish` (the set Δ). Indexed by
    // `skey` and VERIFIED with `sequal` on every hit, the same discipline as `Memo`: the key is a
    // cheap discriminator, so a collision is a miss rather than a wrongly-admitted premise. A plain
    // list would be O(|Δ|) per lookup and Δ reaches ~1900 on a dense refutation.
    let established = System.Collections.Generic.Dictionary<string, ResizeArray<Expr>>()
    let is_established (e: Expr) =
        match established.TryGetValue(skey e) with
        | true, xs -> xs |> Seq.exists (sequal e)
        | _ -> false
    let add_established (e: Expr) =
        let k = skey e
        match established.TryGetValue k with
        | true, xs -> if not (xs |> Seq.exists (sequal e)) then xs.Add e
        | _ ->
            let xs = ResizeArray<Expr>()
            xs.Add e
            established.[k] <- xs
    /// A premise is available if it is a conjunct of the goal's antecedent, was established earlier
    /// in this same proof, or is a CONJUNCTION whose parts are each available.
    ///
    /// The last clause matters because `Formula.Argument` reports a conjunctive antecedent as the
    /// whole conjunction PLUS its parts, so a two-premise theorem `(P ∧ Q) ⇒ R` asks for `P ∧ Q`
    /// itself — which is present only if the goal happens to conjoin them in exactly that order.
    /// Splitting is sound in the obvious direction: if `A ⊨ P` and `A ⊨ Q` then `A ⊨ P ∧ Q`. It does
    /// not loosen the guard on the leaves, so a theorem asking for something genuinely unavailable
    /// is still refused, naming it.
    let rec is_covered (conjuncts: Expr list) (v: Expr) =
        (conjuncts |> List.exists (fun v' -> sequal v v'))
        || is_established v
        || (match v with
            | And(l, r) -> is_covered conjuncts l && is_covered conjuncts r
            | _ -> false)
    do if theory |- a  then
            if prooflog_on then
                let axeq = theory.Axioms a
                prooflog_s (sprintf "|- %s. [%s]" (print_formula a) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Axiom of %s" axeq.Value.Name))
                prooflog_s ("Proof complete." + proof_sep)
            stepCount <- steps.Length
       else if logic |- a   then
            if prooflog_on then
                let axeq = logic.Axioms a
                prooflog_s (sprintf "|- %s. [%s]" (print_formula a) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Logical Axiom of %s" axeq.Value.Name))
                prooflog_s ("Proof complete." + proof_sep)
            stepCount <- steps.Length
       else if steps.Length = 0 && not(theory |- a || logic |- a ) then
        if prooflog_on then prooflog_s (sprintf "Proof incomplete. Current state: %s." (print_formula _state))

    // Iterate through proof steps
    do while stepCount < steps.Length do
        let current_ant, current_conseq, current_conjuncts = 
            match _state with 
            | Argument (ant, con, conj) -> (Some ant, Some con, Some conj)
            | _ -> None, None, None
        let step = steps.[stepCount]
        let stepId = stepCount + 1
        let stepName = stepNames.[stepCount]
        let rule = match step with
                    | Auto rf -> rf _state
                    | _ -> step.Rule
        do            
            match rule with
            | Admit(n,_) -> if (not (ruleNames |> List.contains (n))) then 
                                failwithf "Rule at step %i (%s) is not an admitted logical inference rule or part of the admitted rules of the current theory." stepId n
            | Derive(n, p, _) -> 
                if not ((p.Theory = logic) || (p.Theory = theory) || (p.Theory.GetType().IsAssignableFrom(theory.GetType()))) then 
                    failwithf "Substitution rule at step %i (%s) does not use the rules of the current proof logic or theory." stepId n
            | Deduce(n,p,_) -> 
                if current_ant.IsNone then failwithf "Deduction rule at step %i (%s) cannot be used since %s is not a logical implication with an antecedent and consequent." stepId n (print_formula a)
                if not ((p.Theory = logic) || (p.Theory = theory) || (p.Theory.GetType().IsAssignableFrom(theory.GetType()))) then 
                    failwithf "Deduction rule at step %i (%s) does not use the rules of the current proof logic or theory." stepId n
                let conjs = 
                    match p.Stmt with 
                    | Argument(_, _, cj) -> cj 
                    | _ -> failwithf "%s is not a logical implication with an antecedent and consequent." (print_formula p.Stmt)
                // A premise may be a conjunct of the antecedent OR a fact established earlier in this
                // proof by `Establish` — see that rule's soundness note.
                if conjs |> List.forall (is_covered current_conjuncts.Value) |> not then
                    // Report the conjunct that is present in NO current conjunct (the complement of
                    // the forall condition above), so the message names the actually-missing one.
                    failwithf "The conjunct %s in deduction rule at step %i (%s) is not in the antecedent of %s."
                        (conjs |> List.find(is_covered current_conjuncts.Value >> not) |> print_formula) stepId n (print_formula a)
                if not step.RightApplication then failwith "A deduction rule can only be applied to the consequent of a logical implication."
            | Establish(n, p) ->
                if current_ant.IsNone then failwithf "Establish rule at step %i (%s) cannot be used since %s is not a logical implication with an antecedent and consequent." stepId n (print_formula a)
                if not ((p.Theory = logic) || (p.Theory = theory) || (p.Theory.GetType().IsAssignableFrom(theory.GetType()))) then
                    failwithf "Establish rule at step %i (%s) does not use the rules of the current proof logic or theory." stepId n
                let conjs, con =
                    match p.Stmt with
                    | Argument(_, c, cj) -> cj, c
                    | _ -> failwithf "%s is not a logical implication with an antecedent and consequent." (print_formula p.Stmt)
                if conjs |> List.forall (is_covered current_conjuncts.Value) |> not then
                    failwithf "The conjunct %s in establish rule at step %i (%s) is neither in the antecedent of %s nor established earlier in this proof."
                        (conjs |> List.find(is_covered current_conjuncts.Value >> not) |> print_formula) stepId n (print_formula a)
                // Only now, with every premise covered, does the consequent join Δ.
                add_established con
            | _ -> ()
        // For an Auto step, `rule` above already holds the searched-for rule (rf _state); reuse
        // it here so the auto-search runs once per step, not twice.
        let _a =
            match step with
            | Auto _ -> rule.Apply _state
            | _ -> step.ApplyRule _state

        // The step message is lazy so a suppressed log line never pays for the
        // print_formula (decompilation) calls; prevState pins the pre-step state so
        // deferred evaluation cannot observe the `_state <- _a` mutation below.
        let prevState = _state
        let msg = lazy (
            match rule with
            | Admit(_, _) ->
                if not ((sequal _a prevState)) then
                    sprintf "%i. %s: %s \u2192 %s." (stepId) (stepName.Replace("(expression)", step.Pos)) (print_formula prevState) (print_formula _a)
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", step.Pos))
            | Derive(_,_,_) ->
                if not ((sequal _a prevState))  then
                    sprintf "%i. %s." (stepId) (stepName.Replace("(expression)", step.Pos))
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", step.Pos))
            | Deduce(_,_,_) ->
                if not ((sequal _a prevState))  then
                    sprintf "%i. %s." (stepId) (stepName.Replace("(expression)", step.Pos))
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", step.Pos))
            | Establish(_, _) ->
                // Establishing a fact never changes the expression, so "No change" would be noise.
                sprintf "%i. %s." (stepId) (stepName.Replace("(expression)", step.Pos))
            | Define(_, _) ->
                if not ((sequal _a prevState)) then
                    sprintf "%i. %s: %s \u2192 %s." (stepId) (stepName.Replace("(expression)", step.Pos)) (print_formula prevState) (print_formula _a)
                else
                    sprintf "%i. %s: No change." (stepId) (stepName.Replace("(expression)", step.Pos)))
        do if prooflog_on then prooflog_s msg.Value
        _state <- _a
        state <- state @ [(_state, msg)]
        if theory |- _state then
            if prooflog_on then
                let axeq = theory.Axioms _a
                prooflog_s (sprintf "|- %s. [%s]" (print_formula _a) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Axiom of %s" axeq.Value.Name))
                prooflog_s ("Proof complete." + proof_sep)
            stepCount <- steps.Length
        else if logic |- _state then
            if prooflog_on then
                let axeq = logic.Axioms _a
                prooflog_s (sprintf "|- %s. [%s]" (print_formula _a) (if axeq.Value.Name.StartsWith "Definition" then axeq.Value.Name else sprintf "Logical Axiom of %s" axeq.Value.Name))
                prooflog_s ("Proof complete." + proof_sep)
            stepCount <- steps.Length
        else
            if prooflog_on then prooflog_s (sprintf "Proof incomplete. Current state: %s." (print_formula _a))
            stepCount <- stepCount + 1
    let last_ant, last_conseq, last_conjuncts = 
        match _state with 
        | Argument (ant, con, conj) -> (Some ant, Some con, Some conj)
        | _ -> None, None, None

    member val Stmt = a
    member val LastState = _state
    member val L = 
        match a with
        | Equals(l, _) -> l
        | _ -> a
    member val R = 
        match a with
        | Equals(_, r) -> r
        | _ -> a
    member val LastAntecedent = last_ant
    member val LastConseq = last_conseq
    member val LastConjuncts = last_conjuncts
    member val IsLemma = l
    member val Theory = theory
    member val Steps = steps
    abstract Complete:bool; default val Complete = logic |- _state || theory |- _state
    /// Step states with their (lazily formatted) step messages; same content as before,
    /// materialized on access so unread messages never pay for formatting.
    member x.State = state |> List.map (fun (e, m) -> e, m.Value)
    member val Subst = steps |> List.map (fun s  -> s.ApplyRule) |> List.fold(fun e r -> e >> r) id
    member val Msg = fun (s:string) -> if prooflog_on then prooflog_s s
    member x.Log with get() = logBuilder.ToString() and set(_:string) = logBuilder.Clear() |> ignore

    /// Proof log level.
    static member LogLevel with get() = logLevel and set(v) = logLevel <- v
    /// The default logical theory used by proofs. Defaults to S but can be changed to something else.
    static member Logic with get() = logic and set(v) = logic <- v
    static member (|-) (proof:Proof, expr:Expr) = sequal proof.Stmt expr && proof.Complete
    static member (+) (l:Proof, r:RuleApplication) = 
        if l.Complete then 
            failwith "Cannot add a step to a completed proof." 
        else Proof(l.Stmt, l.Theory, l.Steps @ [r])
    static member (+) (l:Proof, r:RuleApplication list) = 
        if l.Complete then 
            failwith "Cannot add a step to a completed proof." 
        else Proof(l.Stmt, l.Theory, l.Steps @ r)

/// Represents how a theory rule is to be applied to a formula
and RuleApplication =
    /// Apply rule to entire formula.
    | Apply of Rule
    /// Apply rule at the first subterm position (leftmost-outermost) where it fires.
    | ApplyFirst of Rule
    /// Auto-instantiate a derived rule: match its L schema (with metavariable args) against
    /// the first matching subterm, inferring the arguments and position.
    | ApplyMatch of Rule
    /// Apply rule to LHS operand of binary operation.
    | ApplyLeft  of Rule
    /// Apply rule to RHS operand of binary operation.
    | ApplyRight of Rule    
    /// Apply rule to range of quantification formula.
    | ApplyRange of Rule
    /// Apply rule to body of quantification formula.
    | ApplyBody of Rule
    /// Branch to LHS of binary operation before rule application
    | LeftBranch of RuleApplication
    /// Branch to RHS of binary operation before rule application
    | RightBranch of RuleApplication
    /// Apply rule to operand of unary operation
    | ApplyUnary of RuleApplication
    /// Select the range of quantification formula before rule application
    | SelectRange of RuleApplication
    /// Select the body of quantification formula before rule application
    | SelectBody of RuleApplication
    /// Automatically search for a sequence of rule applications that completes the proof
    | Auto of (Expr->Rule)
with
    member x.Rule = 
        match x with
        | Apply rule
        | ApplyFirst rule
        | ApplyMatch rule
        | ApplyLeft rule
        | ApplyRight rule
        | ApplyRange rule
        | ApplyBody rule -> rule
        | LeftBranch ra 
        | RightBranch ra 
        | ApplyUnary ra 
        | SelectRange ra
        | SelectBody ra -> ra.Rule
        | Auto _ -> failwith "Auto doesn't have a single rule; it searches for a sequence of rule applications that completes the proof."        

    member x.RuleName = 
        match x with
        | Auto _ -> "Auto discharge"
        | _ -> x.Rule.Name

    member x.ApplyRule(expr:Expr) =
        let print_formula = Proof.Logic.PrintFormula
        match x with
        | Apply rule -> rule.Apply expr
        | ApplyFirst rule -> apply_first_firing rule.Apply expr
        | ApplyMatch rule ->
            match rule with
            | Derive(_, proof, _) | Deduce(_, proof, _) -> apply_first_schema proof.L proof.R expr
            | _ -> failwithf "autoapply requires a derived or deduced substitution rule (with an L = R schema); %s is not one." rule.Name
        | ApplyLeft rule ->
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> let s = rule.Apply l in binary_call(o, m, s, r)
            | And(l,r) -> let s = rule.Apply l in Expr.IfThenElse(s, r, Expr.Value(false))
            | Or(l,r) -> let s = rule.Apply l in Expr.IfThenElse(s, Expr.Value(true), r)
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | ApplyRight rule -> 
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> let s = rule.Apply r in binary_call(o, m, l, s)
            | And(l, r) -> let s = rule.Apply r in Expr.IfThenElse(l, s, Expr.Value(false))
            | Or(l, r) -> let s = rule.Apply r in Expr.IfThenElse(l, Expr.Value(true), s)
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | ApplyRange rule ->
            match expr with
            | Quantifier(op, x, range, body) -> let s = rule.Apply range in let v = vars_to_tuple x in call op (v::s::body::[])
            | _ -> failwithf "%s is not a quantification." (print_formula expr)
        | ApplyBody rule ->
            match expr with
            | Quantifier(op, x, range, body) -> let s = rule.Apply body in let v = vars_to_tuple x in call op (v::range::s::[])
            | _ -> failwithf "%s is not a quantification." (print_formula expr)
        | LeftBranch ra ->
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> let s = ra.ApplyRule l in binary_call(o, m, s, r)
            | And(l,r) -> let s = ra.ApplyRule l in Expr.IfThenElse(s, r, Expr.Value(false))
            | Or(l,r) -> let s = ra.ApplyRule l in Expr.IfThenElse(s, Expr.Value(true), r)
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | RightBranch ra ->
            match expr with
            | Patterns.Call(o, m, l::r::[]) -> let s = ra.ApplyRule r in binary_call(o, m, l, s)
            | And(l, r) -> let s = ra.ApplyRule r in Expr.IfThenElse(l, s, Expr.Value(false))
            | Or(l, r) -> let s = ra.ApplyRule r in Expr.IfThenElse(l, Expr.Value(true), s)
            | _ -> failwithf "%s is not a binary operation." (print_formula expr)
        | ApplyUnary ra ->
            match expr with
            | Patterns.Call(o, m, l::[]) -> let s = ra.ApplyRule l in unary_call(o, m, s)
            | _ -> failwithf "%s is not a unary operation." (print_formula expr)
        | SelectRange ra ->
            match expr with
            | Quantifier(op, x, range, body) -> let s = ra.ApplyRule range in let v = vars_to_tuple x in call op (v::s::body::[])
            | _ -> failwithf "%s is not a quantification." (print_formula expr)
        | SelectBody ra ->
            match expr with
            | Quantifier(op, x, range, body) -> let s = ra.ApplyRule body in let v = vars_to_tuple x in call op (v::range::s::[])
            | _ -> failwithf "%s is not a quantification." (print_formula expr)
        | Auto rf -> let r = rf expr in r.Apply expr    
    member x.Pos =
        match x with
        | Apply _ -> "expression"
        | ApplyFirst _ -> "first match in expression"
        | ApplyMatch _ -> "first schema match in expression"
        | ApplyLeft _ -> "left of expression"
        | ApplyRight _ -> "right of expression"
        | ApplyRange _ -> "quantifier range"
        | ApplyBody _ -> "quantifier body"
        | LeftBranch ra -> sprintf "left>%s of expression" (ra.Pos.Replace(" of expression", ""))
        | RightBranch ra -> sprintf "right>%s of expression" (ra.Pos.Replace(" of expression", ""))
        | ApplyUnary ra -> sprintf "left-right>%s of expression" (ra.Pos.Replace(" of expression", ""))
        | SelectRange ra -> sprintf "quantifier-range>%s of expression" (ra.Pos.Replace(" of expression", ""))
        | SelectBody ra -> sprintf "quantifier-body>%s of expression" (ra.Pos.Replace(" of expression", ""))
        | Auto _ -> "auto search"

    member x.LeftApplication = 
        x.Pos = "left of expression" || System.Text.RegularExpressions.Regex.IsMatch(x.Pos, "left>(\\S)+\\s+of expression")
    
    member x.RightApplication =
        x.Pos = "right of expression" || System.Text.RegularExpressions.Regex.IsMatch(x.Pos, "right>(\\S)+\\s+of expression")

and Theorem (expr: Expr, proof:Proof) =
    let print_formula = proof.Theory.PrintFormula
    let lazyName = lazy (print_formula expr)
    do 
        if not (sequal expr proof.Stmt) then failwithf "The provided proof is not a proof of %s." (print_formula expr)
        if not proof.Complete then 
            if not proof.IsLemma then
                failwithf "The provided proof of theorem %s is not complete." (print_formula expr)
            else failwithf "The provided proof of lemma %s is not complete. The last state is %s. This can happen in derived rules when a unexpected substitution occcurs in steps that are not specific enough."  (print_formula expr) (print_formula proof.LastState) 
    member val Stmt = expr
    member val LastState = proof.LastState
    member val Proof = proof
    member val Theory = proof.Theory
    // `Name` is a DISPLAY property, but `member val` evaluated it eagerly — so constructing any
    // Theorem decompiled its whole statement, even at LogLevel 0 where nothing is printed. In a SAT
    // replay the statements are `A ⇒ Cᵢ` over the entire clause conjunction, thousands of times.
    member x.Name = lazyName.Value
    new (proof:Proof) = Theorem(proof.Stmt, proof)

[<AutoOpen>]
module ProofOps =
    /// Apply rule to entire formula.
    let apply = RuleApplication.Apply

    /// Apply rule at the first subterm position (leftmost-outermost) where it fires,
    /// so a rule can be applied "wherever it fits" without hand-threading branch combinators.
    let applyfirst = RuleApplication.ApplyFirst

    /// Auto-instantiate a derived rule built from metavariable args: unify its L schema
    /// against the first matching subterm to infer both the arguments and the position,
    /// instead of spelling out the exact subterm and threading branch combinators.
    /// e.g. `autoapply (golden_rule' (meta "a") (meta "b"))` applies the golden rule to the
    /// first ∧ subterm anywhere in the expression.
    let autoapply = RuleApplication.ApplyMatch

    /// A metavariable proposition (a bool Var named "?<n>") for building rule schemas to
    /// auto-instantiate with `autoapply`.
    let meta (n: string) : Prop = boolvar ("?" + n)

    /// Apply rule to LHS of binary operation.
    let apply_left = ApplyLeft
    
    /// Apply rule to RHS of binary operation.
    let apply_right = ApplyRight
        
    /// Apply rule to body of quantification formula.
    let apply_body = RuleApplication.ApplyBody

    /// Apply rule to range of quantification formula.
    let apply_range = RuleApplication.ApplyRange

    /// Branch to LHS of binary operation before rule application.
    let left_branch = LeftBranch

    /// Branch to RHS of binary operation before rule application.
    let right_branch = RightBranch
    
    /// Apply rule to operand of unary operation.
    let apply_unary = RuleApplication.ApplyUnary

    /// Select body of quantification formula before rule application.
    let select_body = RuleApplication.SelectBody

    /// Select range of quantification formula before rule application.
    let select_range = RuleApplication.SelectRange

    /// Apply a rule at the position reached by the OUTSIDE-IN navigation `path`, whose steps are
    /// the pure-navigation combinators `left_branch` / `right_branch` / `apply_unary` /
    /// `select_body` / `select_range`. E.g. `commute |> at [ left_branch; select_body ]` commutes
    /// the quantifier body inside the left operand. `at []` is `apply` (the whole expression); the
    /// rule application is fused in at the deepest point, so `at [ …; select_body ] r` equals the
    /// manual `r |> apply_body |> … `. Reads outside-in, keeping the rule (the "what") first and
    /// the address (the "where") as a top-down path — clearer than a reverse-order `|>` chain.
    let at (path: (RuleApplication -> RuleApplication) list) (r: Rule) : RuleApplication =
        List.foldBack (<|) path (apply r)

    /// Concise single-hop `at` addressers (`rule |> at_left` = `rule |> at [ left_branch ]`), for
    /// the common case of applying a rule one step in. Behave identically to apply_left/… but read
    /// in the `at`-family vocabulary alongside the multi-hop `at [ … ]` paths.
    let at_left  = at [left_branch]
    let at_right = at [right_branch]
    let at_body  = at [select_body]
    let at_range = at [select_range]

    /// Branch left n times before rule application.
    let left_branch_n n (r:Rule->RuleApplication) (x:Rule) = 
        let mutable x' = r x
        for i in 0 .. n do x' <- left_branch x'
        x'

    /// Branch right n times before rule application.
    let rec branch_right_n n (r:Rule->RuleApplication) x = 
        let mutable x' = x |> r
        for i in 0 .. n do x' <- right_branch x'
        x'

    let last_state (p:Proof) = p.LastState

    let left_state p = p |> last_state |> expand_left

    let right_state p = p |> last_state |> expand_right

    let left_src p = p |> left_state |> src

    let right_src p = p |> right_state |> src

    let print_formula (p:Proof) = p.Theory.PrintFormula

    let last_conjuncts (p:Proof) = 
        match p.LastConjuncts with
        | Some conj -> conj |> List.map (fun c -> c |> print_formula p)
        | _ -> []

[<AutoOpen>]
module LogicalRules =     
    /// Leibniz's rule : A behaves equivalently in a formula if we substitute a part of A: a with x when x = a.
    let Subst (p:Proof) =
        // Replace only the FIRST (leftmost-outermost) occurrence of p.L; narrow with the
        // addressing combinators (apply_left, left_branch, ...) to target a specific one.
        let subst (p:Proof) e = if p.Complete then replace_first_expr p.L p.R e else e
        if not p.Complete then
            failwithf "The proof of %A is not complete" (p.Theory.PrintFormula p.Stmt)
        Derive(sprintf "Substitute %s \u2261 %s into (expression)" (p.Theory.PrintFormula p.L) (p.Theory.PrintFormula p.R), p, fun proof e -> subst proof e)
        
    /// Substitute an identity with a completed proof into another proof.
    let Ident (ident:Theorem) = ident.Proof |> Subst 
        
    /// Rule of modus ponens: Substitute the consequent of a proven theorem p ==> q with true in a proof where p is one of the conjuncts of the antecedent.
    let Subst' (p:Proof) =
        if not p.Complete then
                failwithf "The proof of %A is not complete" (p.Stmt |> p.Theory.PrintFormula)
        let ant,con =
            match p.Stmt with
            | Argument(a, c, _) -> a, c
            | _ -> failwithf "The theorem %s is not a logical implication." (p.Stmt |> p.Theory.PrintFormula)
        // Replace only the FIRST occurrence of the consequent with true.
        Rule.Deduce(sprintf "Deduce %s from %s and substitute with true into (expression)" (p.Theory.PrintFormula con) (p.Theory.PrintFormula ant), p, fun _ e -> replace_first_expr con T.Expr.Raw e)

    /// Substitute the consequent of a proven theorem p ==> q with true in a proof where p is one of the conjuncts of the antecedent.
    let Deduce(t:Theorem) = t.Proof |> Subst'

    /// Rule of modus ponens: Substitute the LHS q of a proven identity p ==> (q = r) with the RHS r in a proof where p is one of the conjuncts of the antecedent.
    let Subst'' (p:Proof) =
        if not p.Complete then
                failwithf "The proof of %A is not complete" (p.Stmt |> p.Theory.PrintFormula)
        let ant,con =
            match p.Stmt with
            | Argument(a, c, _) -> a, c
            | _ -> failwithf "The theorem %s is not a logical implication." (p.Stmt |> p.Theory.PrintFormula)
        let lcon, rcon =
            match con with
            | Equals(l, r) -> l, r
            | _ -> failwithf "The theorem %s is not an identity." (con |> p.Theory.PrintFormula)
        // Replace only the FIRST occurrence of the identity's LHS with its RHS.
        Rule.Deduce(sprintf "Deduce %s from %s and substitute %s with %s into (expression)" (p.Theory.PrintFormula con) (p.Theory.PrintFormula ant) (p.Theory.PrintFormula lcon) (p.Theory.PrintFormula rcon), p, fun _ e -> replace_first_expr lcon rcon e)

    /// Substitute the LHS q of a proven identity p ==> (q = r) with the RHS r in a proof where p is one of the conjuncts of the antecedent.
    let Deduce'(t:Theorem) = t.Proof |> Subst''

    /// Record the consequent of a proven theorem `(B₁ ∧ … ∧ Bₖ) ⇒ Z` as ESTABLISHED, in a proof whose
    /// goal is an implication and each of whose `Bᵢ` is either a conjunct of that goal's antecedent
    /// or itself already established. The expression is left completely unchanged.
    ///
    /// This is what lets a FORWARD derivation be carried out under the goal's antecedent. `Deduce`
    /// can only use a theorem whose premises are conjuncts of the antecedent, so it cannot chain:
    /// the clause a resolution step produces is not a conjunct of anything. `Establish` grows the
    /// set of usable premises instead of rewriting, and a final `Deduce` discharges the consequent.
    ///
    /// The kernel — not this function — checks premise coverage, so an `Establish` step whose
    /// premises are not available fails the proof rather than silently widening what is assumable.
    let Establish(t:Theorem) =
        let p = t.Proof
        if not p.Complete then
            failwithf "The proof of %A is not complete" (p.Stmt |> p.Theory.PrintFormula)
        let ant, con =
            match p.Stmt with
            | Argument(a, c, _) -> a, c
            | _ -> failwithf "The theorem %s is not a logical implication." (p.Stmt |> p.Theory.PrintFormula)
        Rule.Establish(sprintf "Establish %s from %s" (p.Theory.PrintFormula con) (p.Theory.PrintFormula ant), p)

    let Define (theory:Theory) (expr:Expr) = 
        let rec subst (lhs:Expr, rhs:Expr) = 
            function
            | l when sequal l lhs -> rhs
            | expr -> traverse expr (subst(lhs, rhs))
        match expr with
        | Equals(l, r) -> Rule.Define(sprintf "Substitute definition of %s \u2261 %s into (expression)" (theory.PrintFormula l) (theory.PrintFormula r), subst(l, r))
        | _ -> failwithf "The formula %A is not an identity." (theory.PrintFormula expr)


[<AutoOpen>]
module Auto =
    open System.Collections.Generic

    /// Bounded best-first search for a completing sequence of rule applications. At each
    /// state it tries every `move` (a structural rewrite), simplifies the result with
    /// `simplify`, and keeps the smallest states first (heuristic: shorter term is closer to
    /// a reflexive `x = x` / a truth constant). States are deduped by their textual form and
    /// the search is capped at `budget` expansions. Returns the RuleApplication path (with
    /// the interleaved simplify steps) that reaches a state satisfying `isComplete`, or None.
    /// The path is directly replayable and checkable by the ordinary Proof engine \u2014 the
    /// search only reuses `RuleApplication.ApplyRule`, so it can never fabricate an invalid step.
    let search (isComplete: Expr -> bool) (simplify: RuleApplication) (moves: RuleApplication list) (budget: int) (goal: Expr) : RuleApplication list option =
        // Apply a RuleApplication, discarding no-ops and any step that throws (mis-target).
        let step (ra: RuleApplication) (e: Expr) =
            try let e' = ra.ApplyRule e in if sequal e' e then None else Some e'
            with _ -> None
        // Simplify a state, and report the extra path segment (empty if simp was a no-op).
        let simp (e: Expr) = match step simplify e with Some e' -> e', [ simplify ] | None -> e, []
        let key (e: Expr) = e.ToString()
        let start, startPath = simp goal
        if isComplete start then Some startPath
        else
            let visited = HashSet<string>([ key start ])
            let frontier = List<Expr * RuleApplication list>()
            frontier.Add(start, startPath)
            let mutable result = None
            let mutable expansions = 0
            while result.IsNone && frontier.Count > 0 && expansions < budget do
                // pop the smallest-term state (best-first)
                let mutable bi = 0
                for i in 1 .. frontier.Count - 1 do
                    if (key (fst frontier.[i])).Length < (key (fst frontier.[bi])).Length then bi <- i
                let state, path = frontier.[bi]
                frontier.RemoveAt bi
                expansions <- expansions + 1
                for mv in moves do
                    if result.IsNone then
                        match step mv state with
                        | Some afterMove ->
                            let s, simpPath = simp afterMove
                            let k = key s
                            if not (visited.Contains k) then
                                visited.Add k |> ignore
                                let newPath = path @ (mv :: simpPath)
                                if isComplete s then result <- Some newPath
                                else frontier.Add(s, newPath)
                        | None -> ()
            result

    /// Elaborate a step list: replay it once and replace each search-based `Auto` step with
    /// the concrete rule it produced at that point (`Apply (rf state)`). An `Auto` step re-runs
    /// its search on every `Proof` construction; compiling captures the result so the proof
    /// (or a rule built from it) replays with no further search. Non-Auto steps pass through.
    let compile_steps (goal: Expr) (steps: RuleApplication list) : RuleApplication list =
        let mutable state = goal
        [ for step in steps do
            let concrete =
                match step with
                | RuleApplication.Auto rf -> RuleApplication.Apply(rf state)
                | s -> s
            state <- concrete.ApplyRule state
            yield concrete ]

    /// Deterministic trace-emitting normalizer for a *confluent* rewrite system (e.g. driving
    /// to a canonical normal form). Greedily applies the first `move` (in priority order) that
    /// fires, to a fixpoint — no size heuristic, so size-increasing normalization steps (like
    /// eliminating ∨ into ⊕∧) aren't avoided. Emits the replayable step list that reaches an
    /// `isComplete` state, or None if it reaches a fixpoint short of completion / exceeds budget.
    let normalize_trace (isComplete: Expr -> bool) (moves: RuleApplication list) (budget: int) (goal: Expr) : RuleApplication list option =
        let step (ra: RuleApplication) (e: Expr) =
            try let e' = ra.ApplyRule e in if sequal e' e then None else Some e'
            with _ -> None
        let rec loop (e: Expr) path n =
            if isComplete e then Some path
            elif n <= 0 then None
            else
                match moves |> List.tryPick (fun m -> step m e |> Option.map (fun e' -> m, e')) with
                | Some (m, e') -> loop e' (path @ [ m ]) (n - 1)
                | None -> None
        loop goal [] budget

[<AutoOpen>]
module Proof =
    let proof (theory:Theory) (e:Prop) steps =         
        let f = e.Expr |> expand in Proof(f, theory, steps)
    
    let theorem (theory:Theory) (e:Prop) steps  = 
        let f = e.Expr |> expand in Theorem(f, Proof(f, theory, steps))
    
    let lemma (theory:Theory) (e:Prop) steps =
        let f = e.Expr |> expand in Theorem(f, Proof(f, theory, steps, true))
    
    let axiom (theory:Theory) (e:Prop) = lemma theory e []

    (* Identities *)

    /// An identity that is true in a theory
    let ident (theory:Theory) (e:Prop) steps =
        let f = e.Expr |> expand in
        match f with
        | Equals(_, _) -> Theorem(f, Proof (f, theory, steps, true)) |> Ident
        | _ -> failwithf "The expression %s is not an identity." (theory.PrintFormula f)
    
    let logical_ident steps (e:Prop) = ident Proof.Logic e steps
    
    /// An identity that is axiomatically true in a theory
    let id_ax theory (e:Prop) = ident theory e []
    
    /// An identity that is axiomatically true in the proof's logical theory
    let log_id_ax (e:Prop) = id_ax Proof.Logic e
    
    (* Deductions *)
    let deduce (p:Theorem) = p|> Deduce
    let deduce' p = p |> Deduce'
    let deduce_ident p = deduce' p
    
    (* Definitions *)
    let def (theory:Theory) (e:Prop) = 
        let f = e.Expr |> expand
        match f with
        | Equals(_, _) -> Define theory f
        | _ -> failwithf "The expression %s is not an identity." (theory.PrintFormula f)

    (* Automation *)
    let autoproof (e: Prop) (theory:Theory) (simplify: Rule) (moves: RuleApplication list) (budget: int) : Proof =
        let goal = expand e.Expr
        let isComplete x = theory.AxEquiv x || Proof.Logic.AxEquiv x       
        match search isComplete (apply simplify) moves budget goal with
        | Some steps -> proof theory e steps
        | None -> failwithf "auto could not find a proof of %s within the search budget." (theory.PrintFormula goal)
        
    let autoident (f:Prop->Proof) (e: Prop) = e |> f |> Theorem |>Ident

    let autodeduce (f:Prop->Proof) (e: Prop) = e |> f |> Theorem |>Deduce

    // Discharge a proof obligation by auto-proving it (`f`) and folding the result back in.
    // An identity A = B is folded with Ident (rewrite A into B, closing via reflexivity); any
    // other proven statement (an implication, a bare tautology, ...) is replaced wholesale by
    // T via `taut` — Deduce is NOT used here, it discharges a consequent in a surrounding
    // context, not a whole goal. `taut` is theory-specific so it is passed in.
    let auto (taut:Theorem->Rule) (f:Prop->Proof) (e:Prop) =
        match e.Expr with
        | Equals(_, _) -> e |> f |> Theorem |> Ident
        | _ -> e |> f |> Theorem |> taut

    let Auto (taut:Theorem->Rule) (f:Prop->Proof) (e:Expr) =
        match e with
        | Equals(_, _) -> e |> expand_as<bool> |> Prop |> f |> Theorem |> Ident
        | _ -> e |> expand_as<bool> |> Prop |> f |> Theorem |> taut
  
type ModuleAdmissibleRule = {
    Name:string
    Description:string
    Property:PropertyInfo
}
with
     override x.ToString () : string=           
          $"Rule: {x.Name}. Description: {x.Description}."

type ModuleDerivedRule = {
    Name:string
    Description:string
    Parameters: ParameterInfo array
    Method: MethodInfo
}
with
     override x.ToString() : string= 
          let m = if x.Parameters.Length = 0 then x.Method.Name else x.Method.Name + " " +  (x.Parameters |> Array.map(fun p -> p.Name |> function | NonNull _p -> _p |  _ -> "") |> String.concat " ") 
          $"Rule: {m}. Description: {x.Description}."

type ModuleTheorem = {
        Name: string
        Description: string
        Parameters: ParameterInfo array
        Method: MethodInfo
    }
    with
        override x.ToString() : string =
            let m = if x.Parameters.Length = 0 then x.Method.Name else x.Method.Name + " " + (x.Parameters |> Array.map(fun p -> p.Name |> function | NonNull _p -> _p | _ -> "") |> String.concat " ")
            $"Theorem: {m}. Description: {x.Description}."

type ModuleTactic = {
    Name: string
    Description: string
    Method: MethodInfo
}
with
    override x.ToString() : string =
        $"Tactic: {x.Name}. Description: {x.Description}."

module ProofModules =

    let isTheoremTactic(m:MethodInfo) =
            if m.GetParameters().Length = 1 then
                let p = m.GetParameters()[0]
                if p.ParameterType = typeof<Theorem> then true else false
            else false

    let isRuleTactic(m:MethodInfo) =                        
            if m.GetParameters().Length = 1 then
                let p = m.GetParameters()[0]
                if p.ParameterType = typeof<Rule> then true else false
            else false

    let getModuleMethods (moduleType: Type) (methodType: Type) = 
        moduleType.GetMethods() |> Array.filter(fun m -> m.IsPublic && m.IsStatic &&  not (m.Name.StartsWith("get_")) && m.ReturnType = methodType)

    let getModuleFields (moduleType: Type) (fieldType: Type) = 
        moduleType.GetMethods() |> Array.filter(fun m -> m.IsPublic && m.IsStatic &&  (m.Name.StartsWith("get_")) && m.ReturnType = fieldType)

    let getModuleProperties (moduleType: Type) (propertyType: Type) = 
        moduleType.GetProperties() |> Array.filter(fun _p -> 
            match _p.GetMethod with 
            | NonNull p -> if p.IsPublic && p.IsStatic && _p.PropertyType = propertyType then true else false 
            | _ -> false
        )

    let getModuleAdmissibleRules(moduleType:Type) =
        getModuleProperties moduleType typeof<Rule> |> Array.map(fun p -> 
        {
            Name= p.Name
            Description = match p.GetCustomAttribute(typeof<AdmissibleRuleAttribute>, true) with | NonNull a -> (a :?> AdmissibleRuleAttribute).Description | Null -> ""  
            Property=p
        }) 

    let getModuleDerivedRules(moduleType:Type) : ModuleDerivedRule array=
        getModuleMethods moduleType typeof<Rule> |> Array.map(fun m -> 
        {
            Name= m.Name
            Description = match m.GetCustomAttribute(typeof<DerivedRuleAttribute>, true) with | NonNull a -> (a :?> DerivedRuleAttribute).Description | Null -> "" 
            Parameters = m.GetParameters()
            Method = m
        }) 

    let getModuleTactics(moduleType:Type) : ModuleTactic array=
        
        let isTactic m = isTheoremTactic m || isRuleTactic m

        getModuleMethods moduleType typeof<Rule> |> Array.filter isTactic |> Array.map(fun m -> 
        {
            Name= m.Name
            Description = match m.GetCustomAttribute(typeof<TacticAttribute>, true) with | NonNull a -> (a :?> TacticAttribute).Description | Null -> "" 
            Method = m
        }) 

    let getModuleTheorems(moduleType:Type) : ModuleTheorem array=
        getModuleMethods moduleType typeof<Theorem> |> Array.map(fun m -> 
        {
            Name= m.Name
            Description = match m.GetCustomAttribute(typeof<TheoremAttribute>, true) with | NonNull a -> (a :?> TheoremAttribute).Description | Null -> ""
            Parameters = m.GetParameters()
            Method = m
        })

/// Memoize a parametric derived rule / theorem by a structural key of its `Prop` arguments.
///
/// A parametric lemma such as `trans_implies p q r` is a *function* that RE-DERIVES its whole proof
/// (and its sub-lemmas' proofs, recursively) on every call — the dominant cost when a single proof
/// invokes the same lemma instance many times (e.g. replaying a SAT/LRAT refutation, where the same
/// clauses recur as premises across many resolution steps). These builders are PURE functions of
/// their arguments, so caching their results is sound. The key is a fast structural key (`skey`) of
/// the expanded arguments, and every hit sequal-verifies the stored arguments, so a key collision is
/// merely a cache miss — no wrong theorem is ever returned.
///
/// Wrap a lemma once (`let fast = Memo.p3 slow`) and share the alias; the cache is process-wide and
/// thread-safe. It does NOT make a first-time derivation cheaper — only repeat instances become free.
module Memo =
    // Fast structural key over the EXPANDED expression (so equivalent construction
    // routes key identically), plus sequal VERIFICATION of the stored arguments on
    // every hit: the key is a cheap discriminator, and verification makes a key
    // collision merely a cache miss — never a wrong theorem. Replaces the previous
    // `%A` reflection-dump key, which was O(|expr|) with a very large constant and
    // made memoized lookups themselves expensive (see docs/expressions-perf.md).
    let private k (p: Prop) = skey (expand p.Expr)
    let private sep = "|"
    let private aeq (a: Prop) (b: Prop) = sequal a.Expr.Raw b.Expr.Raw

    /// Memoize a one-`Prop`-argument builder.
    let p1 (f: Prop -> 'r) : Prop -> 'r =
        let d = System.Collections.Concurrent.ConcurrentDictionary<string, Prop * 'r>()
        fun a ->
            let key = k a
            match d.TryGetValue key with
            | true, (a', r) when aeq a a' -> r
            | _ -> let r = f a in d.[key] <- (a, r); r

    /// Memoize a two-`Prop`-argument builder.
    let p2 (f: Prop -> Prop -> 'r) : Prop -> Prop -> 'r =
        let d = System.Collections.Concurrent.ConcurrentDictionary<string, (Prop * Prop) * 'r>()
        fun a b ->
            let key = k a + sep + k b
            match d.TryGetValue key with
            | true, ((a', b'), r) when aeq a a' && aeq b b' -> r
            | _ -> let r = f a b in d.[key] <- ((a, b), r); r

    /// Memoize a three-`Prop`-argument builder.
    let p3 (f: Prop -> Prop -> Prop -> 'r) : Prop -> Prop -> Prop -> 'r =
        let d = System.Collections.Concurrent.ConcurrentDictionary<string, (Prop * Prop * Prop) * 'r>()
        fun a b c ->
            let key = k a + sep + k b + sep + k c
            match d.TryGetValue key with
            | true, ((a', b', c'), r) when aeq a a' && aeq b b' && aeq c c' -> r
            | _ -> let r = f a b c in d.[key] <- ((a, b, c), r); r

        