namespace Sylvia

open FSharp.Quotations

open Formula
open PropCalculus

/// Calculational (mixed-relation) proofs for the propositional calculus, in the style of
/// Gries "A Logical Approach to Discrete Math" Ch.4 ("Relaxing the Proof Format").
///
/// A `calc` block threads an expression through a sequence of steps, each tagged with a
/// connecting relation — `= ` (Leibniz, any subterm) or `⇒` (top-level weakening) — and
/// produces a GENUINE Theorem of `start REL end`, where the step relations are composed by
/// transitivity (`= ∘ ⇒ = ⇒`, etc.). Mixing `⇒` and `⇐` in one chain is rejected (no common
/// transitive relation). Every step's local fact is proved by the ordinary trusted engine;
/// the conclusion is assembled (not asserted) by replaying `=`-steps on the equation's LHS
/// and folding `⇒`-steps through `trans_implies` (Gries 3.82a) — so the result is sound with
/// no new trusted primitive.
///
///     calc (p * q ==> q) {                 //  GOAL stated up front
///         from (p * q)                     //  start line (checked against the goal's LHS)
///         eq  (commute_and p q |> apply)   //  p ∧ q  =  q ∧ p
///         imp (strengthen_and q p)         //  q ∧ p  ⇒  q
///     }                                    //  ⟹ Theorem: (p ∧ q) ⇒ q  (endpoints checked)
///
/// Use `calc goal { from …; … }` to prove a stated goal (the chain is checked to deliver it),
/// or `derive start { … }` to transform a formula and conclude whatever `start REL end` is
/// reached. An all-`=` chain under a `⇒`/`⇐` goal is weakened to match. Steps: `eq` (=),
/// `imp` (⇒, weaken), `conseq` (⇐, strengthen). State a `⇐` goal with `follows_from a b`
/// (= `a <=== b`).
///
/// PROTOTYPE SCOPE: `imp`/`conseq` steps are top-level only (the whole current line must be
/// the supplied theorem's antecedent/consequent); subterm-monotonic `⇒`/`⇐` (weakening inside
/// ∧/∨, which needs polarity tracking) is future work.
module Calc =

    // --- relation algebra ------------------------------------------------------

    /// The connecting relation of a calc step / of the whole chain.
    type Rel = REq | RImp | RConseq

    let relSym = function REq -> "=" | RImp -> "⇒" | RConseq -> "⇐"

    /// Compose the running relation with a step's relation (transitivity). `= ` is the
    /// identity; `⇒`/`⇐` absorb `= `; `⇒` and `⇐` cannot be mixed.
    let composeRel a b =
        match a, b with
        | REq, x | x, REq -> x
        | RImp, RImp -> RImp
        | RConseq, RConseq -> RConseq
        | _ -> failwith "calc: cannot mix ⇒ and ⇐ in one chain (no common transitive relation)"

    // --- sound conclusion primitives (reuse the trusted engine) ----------------

    let private pOf (e: Expr) : Prop = e |> expand_as<bool> |> Prop

    /// A = B  ⟼  A ⇒ B  (weakening: rewrite the antecedent A into B, leaving the axiom B ⇒ B).
    let eqToImp (t: Theorem) : Theorem =
        match t.Stmt with
        | Equals(a, b) -> theorem prop_calculus ((pOf a) ==> (pOf b)) [ Ident t |> apply_left ]
        | _ -> failwithf "calc: %s is not an equation" (src t.Stmt)

    /// A ⇒ B and B ⇒ C  ⟼  A ⇒ C  (transitivity, discharged through trans_implies, Gries 3.82a).
    let chainImp (t1: Theorem) (t2: Theorem) : Theorem =
        match t1.Stmt, t2.Stmt with
        | Implies(a, b), Implies(b2, c) ->
            if not (sequal b b2) then failwithf "calc: %s and %s do not compose" (src t1.Stmt) (src t2.Stmt)
            let pa, pb, pc = pOf a, pOf b, pOf c
            // (A⇒B) ∧ (B⇒C) is a theorem (both conjuncts Taut to T, reduce collapses T ∧ T).
            let hConj = theorem prop_calculus ((pa ==> pb) * (pb ==> pc)) [
                Taut t1 |> apply_left
                Taut t2 |> apply_right
                reduce |> apply ]
            // A ⇒ C: unfold to (T ⇒ (A⇒C)), turn T into the conjunction, close with trans_implies.
            theorem prop_calculus (pa ==> pc) [
                ident_conseq_true (pa ==> pc) |> Commute |> apply
                Taut hConj |> Commute |> apply_left
                Taut (trans_implies pa pb pc) |> apply ]
        | _ -> failwithf "calc: %s / %s are not both implications" (src t1.Stmt) (src t2.Stmt)

    /// Build the consequence proposition `a <=== b`  (= Conseq(a, b) = `b ⇒ a`).
    let private conseqP (a: Prop) (b: Prop) : Prop = Prop <@ (%a.Expr <=== %b.Expr) @>

    /// State a `⇐` goal readably: `follows_from a b` is `a <=== b` (a follows from b, i.e. b ⇒ a).
    let follows_from (a: Prop) (b: Prop) : Prop = conseqP a b

    /// End ⇒ Start  ⟼  Start <=== End  (rewrite the consequence into its implication via the
    /// Consequence axiom, then discharge with the supplied proof).
    let impToConseq (t: Theorem) : Theorem =
        match t.Stmt with
        | Implies(endE, startE) ->
            let ps, pe = pOf startE, pOf endE
            theorem prop_calculus (conseqP ps pe) [
                id_ax prop_calculus ((conseqP ps pe) == (pe ==> ps)) |> apply
                Taut t |> apply ]
        | _ -> failwithf "calc: impToConseq expected an implication, got %s" (src t.Stmt)

    // --- threaded state --------------------------------------------------------

    /// One recorded step: an equational rewrite (rule + before/after), or a supplied
    /// implication used forward (`⇒`, antecedent = current line) or backward (`⇐`, consequent
    /// = current line).
    type Fact =
        | FEq of RuleApplication * Expr * Expr
        | FImp of Theorem
        | FConseq of Theorem

    /// Split a stated goal `A REL B` into its two sides and connecting relation.
    let internal parseGoal (g: Expr) =
        match g with
        | Implies(a, b) -> a, b, RImp
        | Conseq(a, b) -> a, b, RConseq
        | Equals(a, b) when a.Type = typeof<bool> -> a, b, REq
        | _ -> failwithf "calc: goal %s must be an implication (⇒), consequence (⇐), or equivalence (=)" (src g)

    type CalcState =
        { Start: Expr; Current: Expr; Relation: Rel
          /// The stated goal's (right side, relation), when a goal was declared with `calc`.
          /// `None` for the exploratory `derive` form. `Start` is always the goal's left side.
          Goal: (Expr * Rel) option
          /// True until the first step (of any kind) runs — `from` is only allowed while fresh.
          Fresh: bool
          Facts: Fact list; Log: string list }

        static member private mk start goal =
            { Start = start; Current = start; Relation = REq; Goal = goal; Fresh = true; Facts = []
              Log = [ sprintf "    %s" (prop_calculus.PrintFormula start) ] }
        /// Exploratory: transform `start`, conclude whatever `start REL end` is reached.
        static member Init (start: Expr) = CalcState.mk start None
        /// Goal-stating: seed the start from the goal's left side, remembering the target.
        static member InitGoal (goal: Expr) = let (a, b, rel) = parseGoal goal in CalcState.mk a (Some(b, rel))

        member private s.Push rel next fact line =
            { s with Fresh = false; Current = next; Relation = composeRel s.Relation rel; Facts = s.Facts @ [ fact ]; Log = s.Log @ [ line ] }

        /// Restate (and check) the starting formula — must be the FIRST step, and match the
        /// seeded start / goal LHS.
        member s.From (e: Expr) =
            if not s.Fresh then
                failwith "calc: `from` must be the first step of the proof"
            if not (sequal e s.Start) then
                failwithf "calc: `from %s` does not match the starting formula %s" (src e) (src s.Start)
            { s with Fresh = false }

        member s.Eq (ra: RuleApplication) =
            let next = ra.ApplyRule s.Current
            if sequal next s.Current then failwithf "calc: '= { %s }' did not change %s" ra.RuleName (src s.Current)
            s.Push REq next (FEq(ra, s.Current, next)) (sprintf "  = { %s }\n    %s" ra.RuleName (prop_calculus.PrintFormula next))

        member s.Imp (t: Theorem) =
            match t.Stmt with
            | Implies(a, b) when sequal a s.Current ->
                s.Push RImp b (FImp t) (sprintf "  ⇒ { %s }\n    %s" t.Name (prop_calculus.PrintFormula b))
            | Implies(a, _) -> failwithf "calc: '⇒' step antecedent %s does not match the current line %s" (src a) (src s.Current)
            | _ -> failwithf "calc: '⇒' step %s is not an implication" (src t.Stmt)

        /// A `⇐` step: strengthen the current line via a proven implication whose CONSEQUENT is
        /// the current line (top-level only). `t : next ⇒ current` moves the chain to `next`.
        member s.Conseq (t: Theorem) =
            match t.Stmt with
            | Implies(b, a) when sequal a s.Current ->
                s.Push RConseq b (FConseq t) (sprintf "  ⇐ { %s }\n    %s" t.Name (prop_calculus.PrintFormula b))
            | Implies(_, a) -> failwithf "calc: '⇐' step consequent %s does not match the current line %s" (src a) (src s.Current)
            | _ -> failwithf "calc: '⇐' step %s is not an implication" (src t.Stmt)

        /// Assemble the genuine Theorem the chain establishes. With a stated goal, target that
        /// goal's relation (weakening an all-`=` chain to `⇒` when the goal is an implication)
        /// and check the chain actually ended at the goal's right side. Silences per-step
        /// sub-proof logging (restoring the level afterwards) so only the calc trace prints.
        member s.Conclude () : Theorem =
            // A stated goal fixes the relation to prove; otherwise use the composed relation.
            let target =
                match s.Goal with
                | Some(rhs, goalRel) ->
                    if not (sequal s.Current rhs) then
                        failwithf "calc: the chain ended at %s but the goal's right side is %s" (src s.Current) (src rhs)
                    // The chain's relation must be compatible with (at least as strong as) the goal's.
                    match goalRel, s.Relation with
                    | REq, REq -> REq
                    | RImp, (REq | RImp) -> RImp
                    | RConseq, (REq | RConseq) -> RConseq
                    | _ -> failwithf "calc: the goal is %s but the chain established %s" (relSym goalRel) (relSym s.Relation)
                | None -> s.Relation
            let saved = Proof.LogLevel
            // Each fact as a forward `from ⇒ to` implication (for a `⇒` conclusion)...
            let impFwd =
                function
                | FEq(ra, a, b) -> eqToImp (theorem prop_calculus ((pOf a) == (pOf b)) [ left_branch ra ])
                | FImp t | FConseq t -> t
            // ...and as a backward `to ⇒ from` implication (for a `⇐` conclusion).
            let impBwd =
                function
                | FEq(ra, a, b) -> eqToImp (theorem prop_calculus ((pOf b) == (pOf a)) [ right_branch ra ])
                | FImp t | FConseq t -> t
            let thm =
                try
                    Proof.LogLevel <- 0
                    match target with
                    | REq ->
                        theorem prop_calculus ((pOf s.Start) == (pOf s.Current))
                            (s.Facts |> List.map (function FEq(ra, _, _) -> left_branch ra | _ -> failwith "calc: unreachable"))
                    | RImp ->
                        match s.Facts |> List.map impFwd with
                        | [] -> theorem prop_calculus ((pOf s.Start) ==> (pOf s.Current)) []   // reflexive: Start = Current
                        | h :: rest -> List.fold chainImp h rest
                    | RConseq ->
                        // Fold the backward `to ⇒ from` facts in reverse to get End ⇒ Start, then wrap as Start <=== End.
                        match s.Facts |> List.map impBwd |> List.rev with
                        | [] -> impToConseq (theorem prop_calculus ((pOf s.Current) ==> (pOf s.Start)) [])   // reflexive
                        | h :: rest -> impToConseq (List.fold chainImp h rest)
                finally Proof.LogLevel <- saved
            if saved >= 1 then printfn "%s   [%s]" (String.concat "\n" s.Log) (relSym target)
            thm

    // --- the computation expression -------------------------------------------

    /// Calc CE builder. Each custom operation threads the `CalcState` forward, so steps read as
    /// bare keywords with no `do!`; `Run` assembles the Theorem. Used by both `calc` and `derive`.
    type CalcBuilder(seed: CalcState) =
        member _.Yield(_) = seed

        /// Restate the starting formula (checked against the goal's left side; must be first).
        /// Optional — the start is already seeded from the goal; `from` is a readability opener.
        [<CustomOperation("from")>]
        member _.From(s: CalcState, start: Prop) = s.From(expand start.Expr)

        /// A `= ` step: apply an equational rule application to the current line (Leibniz).
        [<CustomOperation("eq")>]
        member _.Eq(s: CalcState, ra: RuleApplication) = s.Eq ra

        /// A `⇒` step: weaken via a proven implication whose antecedent is the current line.
        [<CustomOperation("imp")>]
        member _.Imp(s: CalcState, t: Theorem) = s.Imp t

        /// A `⇐` step: strengthen via a proven implication whose consequent is the current line.
        [<CustomOperation("conseq")>]
        member _.Conseq(s: CalcState, t: Theorem) = s.Conseq t

        member _.Run(s: CalcState) : Theorem = s.Conclude()

    /// Begin a calculational proof of a STATED goal `A REL B`: the chain starts from `A`
    /// (optionally restated with `from A`), transforms with `eq`/`imp`/`conseq`, and is checked
    /// to deliver the goal (weakening an all-`=` chain to `⇒`/`⇐` if needed). Evaluates to a
    /// Theorem of the stated goal.
    ///
    ///     calc (p * q ==> q) {
    ///         from (p * q)
    ///         imp (strengthen_and q p)
    ///     }
    let calc (goal: Prop) = CalcBuilder(CalcState.InitGoal(expand goal.Expr))

    /// Begin an exploratory calculational proof from `start` (no goal stated): transform it and
    /// conclude whatever `start REL end` is reached.
    let derive (start: Prop) = CalcBuilder(CalcState.Init(expand start.Expr))
