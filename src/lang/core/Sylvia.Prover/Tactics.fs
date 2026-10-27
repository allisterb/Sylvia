namespace Sylvia

open FSharp.Quotations
open Formula

/// Tactics are functions that transform a theorem using valid logical rules into another theorem, or a rule into aanother rule, or a theorem into a rule.
module Tactics =

    /// A step that rewrites the statement of an ALREADY-COMPLETED proof to `true`, carrying that
    /// proof as its justification.
    ///
    /// This is the same shape as `Subst`/`Ident` (a `Derive` rule holding the proof that licenses
    /// its rewrite), and it exists to stop the tactics below from re-deriving what they are handed.
    /// A tactic like `Taut` turns `⊢ A` into `⊢ (A = T)`, and used to do so by SPLICING `A`'s own
    /// step list into the new proof — but a `Proof`'s constructor executes its steps, so every use
    /// of a theorem re-ran that theorem's whole derivation (measured: 152 ms per `Taut` of a
    /// 396-step theorem). Since `A` is already proved, `A ≡ T` follows directly; replaying the
    /// derivation of `A` establishes nothing the completed proof does not already carry.
    ///
    /// Sound exactly when the carried proof is complete, which is checked here and re-checked at
    /// rewrite time — the same contract `Subst` has held all along.
    let private theoremIsTrue (p: Proof) : Rule =
        if not p.Complete then
            failwithf "The proof of %s is not complete, so it cannot be replaced with true." (p.Theory.PrintFormula p.Stmt)
        Derive(sprintf "Replace the theorem %s with true in (expression)" (p.Theory.PrintFormula p.Stmt), p,
               fun (pf: Proof) e -> if pf.Complete then FsExpr.replace_first_expr pf.Stmt T.Expr.Raw e else e)

    /// The constant true is a theorem.
    let Truth commute rule = 
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let l = 
            match proof.Stmt with 
            | Equals(l, True)  -> l
            | _ -> failwith "This theorem is not an identity with the constant true."
        let theory = proof.Theory
        let true_id = ident theory ((T == T) == T) [Apply commute]
        let stmt = <@@ (%%l:bool) = (%%T.Expr = %%T.Expr) @@>
        let p = Proof(stmt, proof.Theory, [ ApplyRight true_id; Apply(theoremIsTrue proof) ], true) in 
        Theorem(stmt, p) |> Ident

    /// If A is a theorem then so is A = true
    let Taut ident (t:Theorem) =
        let proof = t.Proof
        let theory = proof.Theory
        let expr = proof.Stmt
        let stmt = <@@ (%%expr:bool) = %%T.Expr @@>
        // Two steps, regardless of how `A` was proved: `(A = T)` ↦ `A` by the supplied identity,
        // then `A` ↦ `T` justified by `A`'s completed proof, and `T` is an axiom. Previously this
        // spliced `proof.Steps`, re-executing the whole derivation of `A` on every call.
        let p = Proof(stmt, theory, [ expr |> ident |> Apply; Apply(theoremIsTrue proof) ], true) in
        Theorem(stmt, p) |> Ident 

    /// If A = B is a theorem then so is (A = B) = true.
    let Taut' ident (rule:Rule) =
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let theory = proof.Theory
        let expr = proof.Stmt
        let stmt = <@@ (%%expr:bool) = %%T.Expr @@>
        let p = Proof(stmt, theory, [ (expr |> ident |> Apply); Apply(theoremIsTrue proof) ], true) in 
        Theorem(stmt, p) |> Ident 

    /// Whether `e` contains a binder (a quantifier or a lambda). A `Call`'s operands are its object
    /// and arguments only: an inline operator (`op_UnaryNegation`, `op_Multiply`, …) also carries
    /// trait-call WITNESSES — closed lambdas implementing the operator — which `ExprShape` exposes as
    /// extra children. They bind nothing in the statement, and walking them refused every statement
    /// with integer arithmetic in it.
    let rec private has_binder (e: Expr) =
        match e with
        | Quantifier _ -> true
        | Patterns.Call(o, _, args) -> (Option.toList o @ args) |> List.exists has_binder
        | ExprShape.ShapeLambda _ -> true
        | ExprShape.ShapeVar _ -> false
        | ExprShape.ShapeCombination(_, args) -> args |> List.exists has_binder
    /// The prefix reserved for schema METAVARIABLES: variables minted by the `Schema` wrappers
    /// below, which no theory, axiom or definition can have been written about. `Instantiate` and
    /// `InstantiateTerms` substitute for these and nothing else.
    let metavar_prefix = "?"

    /// Instantiate a theorem SCHEMA: from `⊢ S` and a substitution `σ` on propositional variables,
    /// `⊢ Sσ` — in ONE kernel step, without replaying `S`'s derivation.
    ///
    /// Sylvia's derived rules and theorems are F# functions, so a lemma applied at fresh arguments
    /// costs a full re-derivation rather than a substitution. That is the dominant cost of the SAT
    /// reconstruction: measured on a 24-atom implication chain, 8626 of the 8626 `Proof` objects it
    /// builds are `trans_implies` replaying its own 45-proof derivation at whatever wide clause
    /// conjunction the caller is composing (106 ms per call). Deriving the schema ONCE at
    /// metavariables and instantiating it here is O(|Sσ|) instead.
    ///
    /// **This is an admissible rule made constant-time, not a new logical capability.** Uniform
    /// substitution of propositional variables is admissible in a system whose axioms are SCHEMES:
    /// every axiom instance appearing in `S`'s derivation remains an axiom instance under `σ`, so
    /// the substituted derivation is a derivation of `Sσ`. Sylvia's `Admit` rules are exactly such
    /// schemes (`Expr -> Expr` functions matching structurally), and the prover already relies on
    /// this every time a derived rule is replayed at new arguments. Nothing is proved here that
    /// could not be proved by replay — only faster. It is NOT derivable from Leibniz (3.83), which
    /// needs `e = f` as a premise; substitution is a separate Gries inference rule.
    ///
    /// The mechanism carries no new kernel primitive either: as with `Taut` above, the instantiated
    /// statement is closed by a `Derive` step holding the parent's COMPLETED proof as its
    /// justification, and the kernel checks the result the same way it checks any proof.
    ///
    /// This combinator is therefore the sole soundness guardian, and it refuses unless:
    ///
    ///  * the parent proof is complete;
    ///  * every variable substituted for is a plain propositional variable (a `bool` `Var`), with no
    ///    duplicates — so `T`/`F` (named constants, not variables) and compound terms are rejected;
    ///  * and it is a schema METAVARIABLE (a name beginning `?`, as minted by `Schema.p1`/`p2`/`p3`).
    ///    The admissibility argument above holds for variables the derivation was merely parametric
    ///    in. A theory may instead NAME a propositional variable in an axiom — a hypothesis `p`, or
    ///    a definition `q = p ∧ r` — and then `p` is a constant of that theory, not a schema
    ///    position: substituting `F` for it would prove `F`. A metavariable minted after the theory
    ///    was built cannot appear in any of its axioms. (Same guard as `InstantiateTerms`.)
    ///  * the parent statement is free of BINDERS. Quantified statements are refused, and this is
    ///    the restriction that keeps the admissibility argument honest: `PredCalculus` derivations
    ///    carry `¬occurs_free` side conditions discharged at derivation time, which substitution can
    ///    invalidate, and a substituted term can be captured by a binder. Neither hazard exists in
    ///    the quantifier-free fragment, which is where every measured cost lives. Extending to the
    ///    quantified case needs its own soundness argument and its own freshness checks.
    ///
    /// The substitution is applied SIMULTANEOUSLY, by this function — a caller cannot hand in the
    /// instantiated statement it would like to have proved.
    let Instantiate (t: Theorem) (subst: (Prop * Prop) list) : Theorem =
        let proof = t.Proof
        let theory = proof.Theory
        let print = theory.PrintFormula
        if not proof.Complete then
            failwithf "The proof of %s is not complete, so it cannot be instantiated." (print t.Stmt)
        // The domain must be propositional VARIABLES: anything else is not a schema position.
        let vars =
            subst |> List.map (fun (v, value) ->
                match expand v.Expr with
                | ExprShape.ShapeVar var when var.Type = typeof<bool> && var.Name.StartsWith metavar_prefix -> var, expand value.Expr
                | ExprShape.ShapeVar var when var.Type = typeof<bool> ->
                    failwithf "%s is not a schema metavariable (a name beginning %s minted by Schema), so substituting for it is not sound: a theory may have axioms about it." var.Name metavar_prefix
                | e -> failwithf "%s is not a propositional variable, so it is not a schema position of %s."
                                 (print e) (print t.Stmt))
        match vars |> List.countBy (fun (v, _) -> v.Name) |> List.tryFind (fun (_, n) -> n > 1) with
        | Some(n, _) -> failwithf "The variable %s is substituted for more than once." n
        | None -> ()
        // Binders would put both capture-avoidance and the discharged side conditions of a
        // quantified derivation in play; neither is covered by the argument above.
        if has_binder t.Stmt then
            failwithf "%s contains a binder, so it cannot be instantiated: substitution under a quantifier needs capture-avoidance and freshness checks this rule does not perform." (print t.Stmt)
        // Simultaneous: `Substitute` visits each variable occurrence once and does not descend into
        // what it substituted, so σ = {a ↦ b, b ↦ c} cannot compose into a ↦ c.
        let lookup (v: Var) =
            vars |> List.tryPick (fun (var, value) ->
                if v.Name = var.Name && v.Type = var.Type then Some value else None)
        let stmt = t.Stmt.Substitute lookup |> expand
        if sequal stmt t.Stmt then t                          // the identity substitution proves nothing new
        else
            let rule =
                Derive(sprintf "Instantiate the theorem %s in (expression)" (print t.Stmt), proof,
                       fun (pf: Proof) e -> if pf.Complete && sequal e stmt then T.Expr.Raw else e)
            Theorem(stmt, Proof(stmt, theory, [ Apply rule ], true))


    /// Instantiate a theorem SCHEMA at TERM metavariables: from `⊢ S(?a)` and `σ = {?a ↦ E}`,
    /// `⊢ S(E)` — in one kernel step, without replaying `S`'s derivation. Gries' Substitution
    /// inference rule (1.1), for variables of any type.
    ///
    /// **The soundness argument is SEMANTIC, and that is why the domain is restricted.** The
    /// syntactic argument behind `Instantiate` — every axiom instance in the derivation stays an
    /// instance under σ — does not survive term substitution: `reduce` folds `2 + 3` but leaves
    /// `?a + 3` alone, and a recognizer may test whether an operand is a literal or a variable, so the
    /// substituted derivation is not literally a derivation. What holds instead: every axiom
    /// recognizer and every `Admit` rule is value-sound (the latter are fuzzed for it), so a theorem
    /// is true under EVERY assignment to a variable that nothing else constrains — and then `S(E)`
    /// is true, being `S` at the value of `E`.
    ///
    /// "Nothing else constrains" is the condition, and it fails for an ordinary variable: a theory
    /// may name one in an axiom (a recursively defined `f`, or a hypothesis like `c = 3`), and
    /// substituting for THAT variable is unsound. So the domain must be metavariables — names
    /// beginning `?`, minted by `Schema.t1`/`t2`/`t3` AFTER the theory was built — which appear in
    /// no axiom, definition or hypothesis, and are therefore genuinely universal.
    ///
    /// Further guards, as in `Instantiate`: the parent proof is complete; no variable twice; each
    /// value has its variable's type; and the parent statement is binder-free, so nothing can be
    /// captured (a value may itself contain binders — a Σ term — since there is nothing to capture
    /// it). The substitution is simultaneous and performed here.
    let InstantiateTerms (t: Theorem) (subst: (Expr * Expr) list) : Theorem =
        let proof = t.Proof
        let theory = proof.Theory
        let print = theory.PrintFormula
        if not proof.Complete then
            failwithf "The proof of %s is not complete, so it cannot be instantiated." (print t.Stmt)
        let vars =
            subst |> List.map (fun (v, value) ->
                match v with
                | ExprShape.ShapeVar var when var.Name.StartsWith metavar_prefix ->
                    if value.Type <> var.Type then
                        failwithf "Cannot substitute %s, of type %s, for %s, of type %s." (print value) value.Type.Name var.Name var.Type.Name
                    var, expand value
                | ExprShape.ShapeVar var ->
                    failwithf "%s is not a schema metavariable (a name beginning %s minted by Schema), so substituting for it is not sound: a theory may have axioms about it." var.Name metavar_prefix
                | e -> failwithf "%s is not a variable, so it is not a schema position of %s." (print e) (print t.Stmt))
        match vars |> List.countBy (fun (v, _) -> v.Name) |> List.tryFind (fun (_, n) -> n > 1) with
        | Some(n, _) -> failwithf "The variable %s is substituted for more than once." n
        | None -> ()
        if has_binder t.Stmt then
            failwithf "%s contains a binder, so it cannot be instantiated: substitution under a quantifier needs capture-avoidance this rule does not perform." (print t.Stmt)
        let lookup (v: Var) =
            vars |> List.tryPick (fun (var, value) ->
                if v.Name = var.Name && v.Type = var.Type then Some value else None)
        let stmt = t.Stmt.Substitute lookup |> expand
        if sequal stmt t.Stmt then t
        else
            let rule =
                Derive(sprintf "Instantiate the theorem %s in (expression)" (print t.Stmt), proof,
                       fun (pf: Proof) e -> if pf.Complete && sequal e stmt then T.Expr.Raw else e)
            Theorem(stmt, Proof(stmt, theory, [ Apply rule ], true))

    /// Derive a theorem SCHEMA once, at metavariables, and instantiate it per call.
    ///
    /// Sylvia's Gries theorems are F# functions of their `Prop` parameters, so every call replays
    /// the derivation at the caller's arguments — cost O(derivation × |argument|) rather than
    /// O(|result|). Wrapping one in `Schema.pN` derives it a single time and serves every later call
    /// through `Instantiate`, whose guards apply unchanged. This is the same shape as `Memo.pN`, and
    /// it succeeds where memoization cannot: memoizing only helps when the SAME arguments recur,
    /// and the arguments in a SAT reconstruction are distinct at every step.
    ///
    /// Only for schemas whose parameters are genuinely arbitrary propositions — which is what
    /// `Instantiate` verifies anyway, since a schema whose statement is not `f` applied to plain
    /// variables cannot be reconstructed by substitution.
    module Schema =
        let private mv (name: string) (i: int) : Prop = boolvar (sprintf "%s%s_%d" metavar_prefix name i)

        /// Wrap a one-`Prop`-argument theorem schema.
        let p1 (name: string) (f: Prop -> Theorem) : Prop -> Theorem =
            let a = mv name 1
            let s = lazy (f a)
            fun x -> Instantiate s.Value [ a, x ]

        /// Wrap a two-`Prop`-argument theorem schema.
        let p2 (name: string) (f: Prop -> Prop -> Theorem) : Prop -> Prop -> Theorem =
            let a, b = mv name 1, mv name 2
            let s = lazy (f a b)
            fun x y -> Instantiate s.Value [ a, x; b, y ]

        /// Wrap a three-`Prop`-argument theorem schema.
        let p3 (name: string) (f: Prop -> Prop -> Prop -> Theorem) : Prop -> Prop -> Prop -> Theorem =
            let a, b, c = mv name 1, mv name 2, mv name 3
            let s = lazy (f a b c)
            fun x y z -> Instantiate s.Value [ a, x; b, y; c, z ]

        // TERM schemas, through `InstantiateTerms`. Each wrapper mints its own metavariables, with
        // a process-wide counter so no two schemas share one, and derives `f` at them on first use.
        let private next_mv = ref 0
        let private tv<'t> (name: string) : Expr<'t> =
            let i = System.Threading.Interlocked.Increment &next_mv.contents
            Expr.Var(Var(sprintf "%s%s_%d" metavar_prefix name i, typeof<'t>)) |> Expr.Cast<'t>
        let private thm_of (name: string) (r: Rule) =
            match r with
            | Derive(_, p, _) -> Theorem(p.Stmt, p)
            | _ -> failwithf "Schema %s: not a derived rule." name

        /// Wrap a one-term-argument identity (a `Rule`, e.g. `x·0 = 0`), derived once.
        let t1 (name: string) (f: Expr<'a> -> Rule) : Expr<'a> -> Rule =
            let a = tv<'a> name
            let s = lazy (thm_of name (f a))
            fun x -> Ident (InstantiateTerms s.Value [ a.Raw, x.Raw ])

        /// Wrap a two-term-argument identity, derived once.
        let t2 (name: string) (f: Expr<'a> -> Expr<'b> -> Rule) : Expr<'a> -> Expr<'b> -> Rule =
            let a, b = tv<'a> name, tv<'b> name
            let s = lazy (thm_of name (f a b))
            fun x y -> Ident (InstantiateTerms s.Value [ a.Raw, x.Raw; b.Raw, y.Raw ])

        /// Wrap a three-term-argument identity, derived once.
        let t3 (name: string) (f: Expr<'a> -> Expr<'b> -> Expr<'c> -> Rule) : Expr<'a> -> Expr<'b> -> Expr<'c> -> Rule =
            let a, b, c = tv<'a> name, tv<'b> name, tv<'c> name
            let s = lazy (thm_of name (f a b c))
            fun x y z -> Ident (InstantiateTerms s.Value [ a.Raw, x.Raw; b.Raw, y.Raw; c.Raw, z.Raw ])

        /// Wrap a one-term-argument THEOREM (an implication, say, rather than an identity), derived once.
        let th1 (name: string) (f: Expr<'a> -> Theorem) : Expr<'a> -> Theorem =
            let a = tv<'a> name
            let s = lazy (f a)
            fun x -> InstantiateTerms s.Value [ a.Raw, x.Raw ]

        /// Wrap a two-term-argument theorem, derived once.
        let th2 (name: string) (f: Expr<'a> -> Expr<'b> -> Theorem) : Expr<'a> -> Expr<'b> -> Theorem =
            let a, b = tv<'a> name, tv<'b> name
            let s = lazy (f a b)
            fun x y -> InstantiateTerms s.Value [ a.Raw, x.Raw; b.Raw, y.Raw ]

        /// Wrap a three-term-argument theorem, derived once.
        let th3 (name: string) (f: Expr<'a> -> Expr<'b> -> Expr<'c> -> Theorem) : Expr<'a> -> Expr<'b> -> Expr<'c> -> Theorem =
            let a, b, c = tv<'a> name, tv<'b> name, tv<'c> name
            let s = lazy (f a b c)
            fun x y z -> InstantiateTerms s.Value [ a.Raw, x.Raw; b.Raw, y.Raw; c.Raw, z.Raw ]

    /// If A is theorem then so is the dual of A.
    let Dual (t:Theorem) dual =
        let proof = t.Proof
        let theory = proof.Theory
        let expr = proof.Stmt
        let stmt = EquationalLogic._dual expr
        let p = Proof(stmt, theory, [ (dual |> Apply); Apply(theoremIsTrue proof) ], true) in
        Theorem(stmt, p)

    /// If A = B is an identity then so is the dual of A with B.
    let Dual' (rule:Rule) dual = 
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let theory = proof.Theory
        let expr = proof.Stmt
        let stmt = EquationalLogic._dual expr
        let p = Proof(stmt, theory, [ (dual |> Apply); Apply(theoremIsTrue proof) ], true) in
        Theorem(stmt, p) |> Ident

    /// If A = B is a theorem then so is B = A.
    let Commute commute rule =
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let stmt = 
            match proof.Stmt with 
            | Equals(l, r) -> <@@ (%%r:bool) = (%%l:bool) @@>
            | _ -> failwith "This theorem is not an identity."
        let p = Proof(stmt, proof.Theory, [ Apply commute; Apply(theoremIsTrue proof) ], true) in 
        Theorem(stmt, p) |> Ident

    /// If (L = R) = X is a theorem then so is (R = L) = X.
    let CommuteL commute rule =
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let (l, r) = 
            match proof.Stmt with 
            | Equals(l, r) -> (l, r)
            | _ -> failwith "This theorem is not an identity."
        let l1 = 
            match l with 
            | Patterns.Call(o, m, l::r::[]) -> binary_call(o, m, r, l)                        
            | And(l,r) -> Expr.IfThenElse(r, l, Expr.Value(false))
            | Or(l,r) -> Expr.IfThenElse(r, Expr.Value(true), l)
            | _ -> failwith "The LHS of this theorem is not an identity."

        let stmt = <@@ ((%%l1:bool)) = (%%r:bool) @@>
        let p = Proof(stmt, proof.Theory, [ ApplyLeft commute; Apply(theoremIsTrue proof) ], true) in 
        Theorem(stmt, p) |> Ident

    /// If X = (L = R) is a theorem then so is X = (R = L).
    let CommuteR commute rule =
        let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
        let (l, r) = 
            match proof.Stmt with 
            | Equals(l, r) -> (l, r)
            | _ -> failwith "This theorem is not an identity."
        let r1 = 
            match r with 
            | Patterns.Call(o, m, l::r::[]) -> binary_call(o, m, r, l)
            | And(l,r) -> Expr.IfThenElse(r, l, Expr.Value(false))
            | Or(l,r) -> Expr.IfThenElse(r, Expr.Value(true), l)
            | _ -> failwith "The rHS of this theorem is not an identity."

        let stmt = <@@ ((%%l:bool)) = (%%r1:bool) @@>
        let p = Proof(stmt, proof.Theory, [ ApplyRight commute; Apply(theoremIsTrue proof) ], true) in 
        Theorem(stmt, p) |> Ident

    /// If (A1 = A2) = A3 is a theorem then so is A1 = (A2 = A3)
    let RightAssoc lassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let stmt = 
             match proof.Stmt with 
             | Equals(Equals(l1, l2), r) -> <@@ (%%l1:bool) = ((%%l2:bool) = (%%r:bool)) @@>
             | _ -> failwith "This theorem is not an identity."
         let p = Proof(stmt, proof.Theory, [ Apply lassoc; Apply(theoremIsTrue proof) ], true) in 
         Theorem(stmt, p) |> Ident

    /// If A1 = (A2 =  A3) is a theorem then so is (A1 = A2) = A3
    let LeftAssoc rassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let stmt = 
             match proof.Stmt with 
             | Equals(l, Equals(r1, r2)) -> <@@ ((%%l:bool) = (%%r1:bool)) = (%%r2:bool) @@>
             | _ -> failwith "This theorem is not an identity."
         let p = Proof(stmt, proof.Theory, [ Apply rassoc; Apply(theoremIsTrue proof) ], true) in 
         Theorem(stmt, p) |> Ident

    /// If ((A1 = A2) = A3) = A4 is a theorem then so is (A1 = (A2 = A3)) = A4
    let RightAssocRecurseLeft lassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let (l, r) = 
             match proof.Stmt with 
             | Equals(l, r) -> (l, r)
             | _ -> failwith "This theorem is not an identity."
         let stmt = 
             match l with 
             | Equals(Equals(l1, l2), r2) -> <@@ ((%%l1:bool) = ((%%l2:bool) = (%%r2:bool))) = (%%r:bool) @@>
             | _ -> failwith "This theorem is not an identity."
         let p = Proof(stmt, proof.Theory, [ ApplyLeft lassoc; Apply(theoremIsTrue proof) ], true) in 
         Theorem(stmt, p) |> Ident

     /// If A1 = ((A2 = A3) = A4) is a theorem then so is A1 = (A2 = (A3 = A4))
    let RightAssocRecurseRight lassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let (l, r) = 
             match proof.Stmt with 
             | Equals(l, r) -> (l, r)
             | _ -> failwith "This theorem is not an identity."
         let stmt = 
             match r with 
             | Equals(Equals(l1, l2), r2) -> <@@ (%%l:bool) = ((%%l1:bool) = ((%%l2:bool) = (%%r2:bool)))  @@>
             | _ -> failwith "This theorem is not an identity."
         let p = Proof(stmt, proof.Theory, [ ApplyRight lassoc; Apply(theoremIsTrue proof) ], true) in 
         Theorem(stmt, p) |> Ident

    /// If (A1 = (A2 = A3)) = A4 is a theorem then so is ((A1 = A2) = A3) = A4
    let LeftAssocRecurseLeft rassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let (l, r) = 
             match proof.Stmt with 
             | Equals(l, r) -> (l, r)
             | _ -> failwith "This theorem is not an identity."
         let stmt = 
             match l with 
             | Equals(l1, Equals(r1, r2)) -> <@@ (((%%l1:bool) = (%%r1:bool)) = (%%r2:bool)) = (%%r:bool) @@>
             | _ -> failwith "The LHS of this theorem is not an identity."
         let p = Proof(stmt, proof.Theory, [ ApplyLeft rassoc; Apply(theoremIsTrue proof) ], true) in 
         Theorem(stmt, p) |> Ident

    /// If A1 = (A2 = (A3 = A4)) is a theorem then so is A1 = ((A2 = A3) = A4)
    let LeftAssocRecurseRight rassoc rule =
         let proof = match rule with | Derive(_,p,_) -> p | _ ->  failwith "This rule is not a derived rule."
         let (l, r) = 
             match proof.Stmt with 
             | Equals(l, r) -> (l, r)
             | _ -> failwith "This theorem is not an identity."
         let stmt = 
             match r with 
             | Equals(l1, Equals(r1, r2)) -> <@@ (%%l:bool) = (((%%l1:bool) = (%%r1:bool)) = (%%r2:bool)) @@>
             | _ -> failwith "The RHS of this theorem is not an identity."
         let p = Proof(stmt, proof.Theory, [ ApplyRight rassoc; Apply(theoremIsTrue proof) ], true) in 
         Theorem(stmt, p) |> Ident

    let MutualImplication theory taut ident reduce stmt =
        let (l, r) =
            match stmt with
            | Equals(l, r) -> (l, r)
            | _ -> failwith "This statement is not an identity."

        let lhs steps =
            let s = <@@ %%l ===> %%r @@>
            let p = Proof(s, theory, steps, true) in
            Theorem(s, p)

        let rhs steps =
            let s = <@@ %%r ===> %%l @@>
            let p = Proof(s, theory, steps, true) in
            Theorem(s, p)

        // Gries 3.80: (l = r) = (l ⇒ r) ∧ (r ⇒ l). After Taut-ing both proven
        // implications to true we are left with T ∧ T; the final `reduce` collapses
        // that to T so the proof actually closes (without it the proof ends at T ∧ T,
        // which is not an axiom, and Theorem construction fails).
        let p lhs rhs = Proof(stmt, theory,  [
            ident |> Apply
            lhs |> taut |> ApplyLeft
            rhs |> taut |> ApplyRight
            reduce |> Apply
        ])

        lhs, rhs, p

    /// Proof by contradiction (reductio ad absurdum, Gries §4.2): from a proof of ¬P ⇒ F,
    /// conclude P. `contradiction_id e` supplies the theory identity (¬e ⇒ F) = e (e.g. via
    /// Gries 3.74 p⇒F = ¬p and double negation); `commute` is the Commute tactic and `taut`
    /// the Taut tactic. The goal P is rewritten into ¬P ⇒ F, then discharged to T by the
    /// supplied proof.
    let Contradiction theory (taut:Theorem->Rule) commute (contradiction_id:Expr->Rule) (t:Theorem) =
        let pexpr =
            match t.Stmt with
            | Implies(Not pp, False) -> pp
            | _ -> failwithf "Proof by contradiction requires a theorem of the form ¬P ⇒ F; %s is not." ((theory:Theory).PrintFormula t.Stmt)
        let steps = [
            contradiction_id pexpr |> commute |> Apply   // P → (¬P ⇒ F)
            t |> taut |> Apply                           // (¬P ⇒ F) → T
        ]
        Theorem(pexpr, Proof(pexpr, theory, steps))

    /// Proof by cases (case analysis, Gries 3.79): from proofs of Q ⇒ P and ¬Q ⇒ P, conclude P.
    /// `case_analysis q p` supplies the theory identity (q ⇒ p) ∧ (¬q ⇒ p) = p; `reduce`
    /// collapses the resulting T ∧ T to T. Q and P are read off the first theorem (Q ⇒ P);
    /// a mismatched second theorem simply fails to close the proof.
    let Cases theory (taut:Theorem->Rule) commute reduce (case_analysis:Expr->Expr->Rule) (t1:Theorem) (t2:Theorem) =
        let (qexpr, pexpr) =
            match t1.Stmt with
            | Implies(q, p) -> q, p
            | _ -> failwithf "Proof by cases requires the first theorem to have the form Q ⇒ P; %s does not." ((theory:Theory).PrintFormula t1.Stmt)
        let steps = [
            case_analysis qexpr pexpr |> commute |> Apply   // P → (Q ⇒ P) ∧ (¬Q ⇒ P)
            t1 |> taut |> ApplyLeft                         // → T ∧ (¬Q ⇒ P)
            t2 |> taut |> ApplyRight                        // → T ∧ T
            reduce |> Apply                                 // → T
        ]
        Theorem(pexpr, Proof(pexpr, theory, steps))
