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
///     calc (p * q ==> q) {                     //  GOAL stated up front
///         do! from (p * q)                     //  start line (checked against the goal's LHS)
///         do! eq  (commute_and p q |> apply)   //  p ∧ q  =  q ∧ p
///         do! imp (strengthen_and q p)         //  q ∧ p  ⇒  q
///     }                                        //  ⟹ Theorem: (p ∧ q) ⇒ q  (endpoints checked)
///
/// Use `calc goal { from …; … }` to prove a stated goal (the chain is checked to deliver it),
/// or `derive start { … }` to transform a formula and conclude whatever `start REL end` is
/// reached. An all-`=` chain under a `⇒` goal is weakened to `⇒` to match.
///
/// PROTOTYPE SCOPE: `imp` steps are top-level only (the whole current line must be the
/// supplied theorem's antecedent); subterm-monotonic `⇒` (weakening inside ∧/∨, which needs
/// polarity tracking) and `⇐` conclusions are future work.
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

    // --- threaded state --------------------------------------------------------

    /// One recorded step: an equational rewrite (rule + before/after), or a supplied implication.
    type Fact =
        | FEq of RuleApplication * Expr * Expr
        | FImp of Theorem

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
          Facts: Fact list; Log: string list }

        static member private mk start goal =
            { Start = start; Current = start; Relation = REq; Goal = goal; Facts = []
              Log = [ sprintf "    %s" (prop_calculus.PrintFormula start) ] }
        /// Exploratory: transform `start`, conclude whatever `start REL end` is reached.
        static member Init (start: Expr) = CalcState.mk start None
        /// Goal-stating: seed the start from the goal's left side, remembering the target.
        static member InitGoal (goal: Expr) = let (a, b, rel) = parseGoal goal in CalcState.mk a (Some(b, rel))

        member private s.Push rel next fact line =
            { s with Current = next; Relation = composeRel s.Relation rel; Facts = s.Facts @ [ fact ]; Log = s.Log @ [ line ] }

        /// Restate (and check) the starting formula — must match the seeded start / goal LHS.
        member s.From (e: Expr) =
            if not (sequal e s.Start) then
                failwithf "calc: `from %s` does not match the starting formula %s" (src e) (src s.Start)
            s

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
                    match goalRel, s.Relation with
                    | REq, RImp | REq, RConseq -> failwithf "calc: the goal is an equivalence (=) but the chain only established %s" (relSym s.Relation)
                    | _ -> goalRel        // ⇒ goal accepts an = chain (weaken); = goal needs an = chain
                | None -> s.Relation
            let saved = Proof.LogLevel
            let impOf =
                function
                | FEq(ra, a, b) -> eqToImp (theorem prop_calculus ((pOf a) == (pOf b)) [ left_branch ra ])
                | FImp t -> t
            let thm =
                try
                    Proof.LogLevel <- 0
                    match target with
                    | REq ->
                        theorem prop_calculus ((pOf s.Start) == (pOf s.Current))
                            (s.Facts |> List.map (function FEq(ra, _, _) -> left_branch ra | FImp _ -> failwith "calc: unreachable"))
                    | RImp ->
                        match s.Facts |> List.map impOf with
                        | [] -> theorem prop_calculus ((pOf s.Start) ==> (pOf s.Current)) []   // reflexive: Start = Current
                        | h :: rest -> List.fold chainImp h rest
                    | RConseq -> failwith "calc: ⇐ conclusions are not yet supported"
                finally Proof.LogLevel <- saved
            if saved >= 1 then printfn "%s   [%s]" (String.concat "\n" s.Log) (relSym target)
            thm

    // --- the computation expression -------------------------------------------

    /// A calc step: threads the state forward.
    type Step = CalcState -> CalcState

    type CalcBuilder(seed: CalcState) =
        member _.Bind(m: Step, f: unit -> Step) : Step = fun s -> f () (m s)
        member _.Zero() : Step = id
        member _.Delay(f: unit -> Step) : Step = fun s -> f () s
        member _.Combine(m1: Step, m2: Step) : Step = fun s -> m2 (m1 s)
        member _.Run(m: Step) : Theorem = (m seed).Conclude()

    /// Begin a calculational proof of a STATED goal `A REL B`: the block starts from `A`
    /// (restate it with `from A`), transforms it with `= `/`⇒` steps, and at the end checks
    /// the chain delivered exactly the goal (weakening an all-`=` chain to `⇒` if the goal is
    /// an implication). Evaluates to a Theorem of the stated goal.
    ///
    ///     calc (p * q ==> q) {
    ///         do! from (p * q)
    ///         do! imp (strengthen_and q p)
    ///     }
    let calc (goal: Prop) = CalcBuilder(CalcState.InitGoal(expand goal.Expr))

    /// Begin an exploratory calculational proof from `start` (no goal stated): transform it and
    /// conclude whatever `start REL end` is reached.
    let derive (start: Prop) = CalcBuilder(CalcState.Init(expand start.Expr))

    /// Restate the starting formula (checked against the goal's left side). Optional, for
    /// readability, in a `calc`-with-goal block.
    let from (start: Prop) : Step = fun s -> s.From(expand start.Expr)

    /// A `= ` step: apply an equational rule application to the current line (Leibniz).
    let eq (ra: RuleApplication) : Step = fun s -> s.Eq ra

    /// A `⇒` step: weaken the current line via a proven implication whose antecedent is the
    /// whole current line (top-level only in this prototype).
    let imp (t: Theorem) : Step = fun s -> s.Imp t
