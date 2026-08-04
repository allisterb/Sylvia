namespace Sylvia
// A bare rule as a proof step implicitly means "apply to the whole expression".
#nowarn "3391"

/// Conjunctive-normal-form conversion for propositional `Prop`s, emitting a **kernel-checked**
/// equivalence proof.
///
/// `Cnf.to_cnf p` returns `(cnf, proof)` where `cnf` is `p` in CNF (a conjunction of clauses, each a
/// disjunction of literals; negations only on atoms; no `⇒`/`=`) and `proof : Theorem` establishes
/// `p == cnf` in `PropCalculus.prop_calculus`.
///
/// Unlike `autoproof_anf` (which is exponential in the number of distinct atoms, and guarded at
/// `autoproof_max_atoms`), this is a **structural recursive descent** — its cost is bounded by the
/// size of the CNF, not by an atom-count exponential. It is the scalable CNF-equivalence step behind
/// the SAT-refutation reconstruction (see `Sylvia.Solver.CaDiCaL` and `examples/sat/Reconstruct.fsx`):
/// it lets the pipeline turn a CaDiCaL LRAT refutation of `¬φ` into a checked `⊢ φ` with no atom
/// ceiling. It composes sub-proofs by **congruence** (`Ident subproof |> at [pos]`), sidestepping the
/// schema matcher entirely, and produces minimal CNF (no over-distribution).
module Cnf =

    open FSharp.Quotations
    open Formula
    open FsExpr
    open PropCalculus

    let private pOf (e: Expr) : Prop = Prop(expand_as<bool> e)

    // Structural view of a Prop's top connective.
    type private V =
        | VAnd of Prop * Prop
        | VOr  of Prop * Prop
        | VNot of Prop
        | VImp of Prop * Prop
        | VIff of Prop * Prop
        | VXor of Prop * Prop
        | VAtom

    let private view (p: Prop) : V =
        match expand p.Expr with
        | Not x -> VNot(pOf x)
        | And(x, y) -> VAnd(pOf x, pOf y)
        | Or(x, y) -> VOr(pOf x, pOf y)
        | Implies(x, y) -> VImp(pOf x, pOf y)
        | Equals(x, y) when x.Type = typeof<bool> -> VIff(pOf x, pOf y)
        | NotEquals(x, y) when x.Type = typeof<bool> -> VXor(pOf x, pOf y)
        | _ -> VAtom

    (* ---- congruence + equality plumbing (compose equational sub-proofs) ---- *)

    let private refl (e: Prop) : Theorem = theorem prop_calculus (e == e) [ ident_eq (e == e) ]

    /// `x == y` and `y == z`  ⟼  `x == z`  (rewrite the LHS through both).
    let private transEq (p1: Theorem) (p2: Theorem) : Theorem =
        match p1.Stmt, p2.Stmt with
        | Equals(x, _), Equals(_, z) ->
            theorem prop_calculus (pOf x == pOf z) [ Ident p1 |> apply_left; Ident p2 |> apply_left ]
        | _ -> failwithf "Cnf.transEq: not equalities: %s / %s" (src p1.Stmt) (src p2.Stmt)

    // Lift two operand-equalities through a binary connective.
    let private congBin (mk: Prop -> Prop -> Prop) (pa: Theorem) (pb: Theorem) : Theorem =
        match pa.Stmt, pb.Stmt with
        | Equals(x, cx), Equals(y, cy) ->
            theorem prop_calculus (mk (pOf x) (pOf y) == mk (pOf cx) (pOf cy))
                [ Ident pa |> at [ left_branch; left_branch ]; Ident pb |> at [ left_branch; right_branch ] ]
        | _ -> failwith "Cnf.congBin: not equalities"
    let private congAnd = congBin (fun x y -> x * y)
    let private congOr = congBin (fun x y -> x + y)

    let private congNot (pe: Theorem) : Theorem =
        match pe.Stmt with
        | Equals(x, cx) -> theorem prop_calculus ((- pOf x) == (- pOf cx)) [ Ident pe |> at [ left_branch ] ]
        | _ -> failwith "Cnf.congNot: not an equality"

    (* ---- one-step equality lemmas, as Theorems (so they compose with transEq) ---- *)

    let private implEq (x: Prop) (y: Prop) : Theorem =
        theorem prop_calculus ((x ==> y) == (-x + y)) [ ident_implies_not_or x y |> apply ]
    let private dnegEq (x: Prop) : Theorem =
        theorem prop_calculus ((-(-x)) == x) [ double_negation x |> apply ]
    let private dmOrEq (x: Prop) (y: Prop) : Theorem =                         // Gries 3.47b
        theorem prop_calculus ((-(x + y)) == (-x * -y)) [ distrib_not_or x y |> apply ]
    let private dmAndEq (x: Prop) (y: Prop) : Theorem =                        // Gries 3.47a
        theorem prop_calculus ((-(x * y)) == (-x + -y)) [ distrib_not_and x y |> apply ]
    let private xorEq (x: Prop) (y: Prop) : Theorem =                          // Gries 3.10
        theorem prop_calculus ((x != y) == (-(x == y))) [ def_not_eq x y |> apply ]
    let private iffEq (x: Prop) (y: Prop) : Theorem =
        theorem prop_calculus ((x == y) == ((x ==> y) * (y ==> x))) [ mutual_implication' x y |> Commute |> apply ]
    // ∨ distributes over ∧, both orientations.
    let private dorL (a: Prop) (u: Prop) (v: Prop) : Theorem =
        theorem prop_calculus ((a + (u * v)) == ((a + u) * (a + v))) [ distrib_or_and a u v |> apply ]
    let private dorR (x: Prop) (y: Prop) (a: Prop) : Theorem =
        theorem prop_calculus (((x * y) + a) == ((x + a) * (y + a))) [
            commute_or (x * y) a |> at_left
            distrib_or_and a x y |> at_left
            commute_or a x |> at [ left_branch; left_branch ]
            commute_or a y |> at [ left_branch; right_branch ] ]

    (* ---- the truth constants ---- *)

    // `T` and `F` are NAMED CONSTANTS, so `view` classifies them as atoms and the descent would
    // happily carry them into the output as literals. That is wrong at the clause level and not
    // merely inelegant: `clauses_of` would mint a DIMACS variable for `T`, and the solver would
    // satisfy `¬φ` by setting it false. So constants are FOLDED AWAY as they are met, using the
    // identity and zero laws. The folds live where the connectives are actually built — `collapseAnd`
    // (∧), `distribOr` (∨) and `toCnfRec`'s `VNot` case — because every other connective is rewritten
    // into ¬/∧/∨ by the descent, so those three are the only places a constant can survive.
    //
    // The `T` side of this was already here, as a consequence of tautological-clause pruning
    // (a pruned clause becomes `T`, which then has to be collapsed out of its context). The `F` side
    // and `¬T`/`¬F` were missing, which made every goal mentioning a truth constant report as a
    // non-theorem through the SAT route. The solver-side clausifier `SAT.cnf_of_negated_goal` has always
    // folded both (its `simp` over `BTrue`/`BFalse`); this is what closes the gap between them.

    let private isT (x: Prop) = match expand x.Expr with True -> true | _ -> false

    /// `Some true` for `T`, `Some false` for `F`, `None` for anything else. The hot paths use this
    /// rather than an `isT`/`isF` pair so that a side is `expand`ed ONCE however many constant cases
    /// have to be considered — `expand` is the single most expensive thing in the conversion.
    let private constOf (x: Prop) : bool option =
        match expand x.Expr with
        | True -> Some true
        | False -> Some false
        | _ -> None

    /// ¬T = F. The mirror of `not_false` (Gries 3.13), which the module has but its dual does not:
    /// rewrite `F` by Definition of false (3.15) to `(¬T = T)`, then `(p = T) = p` (3.3) at p := ¬T.
    /// Takes the constant from the formula rather than using the module's own `T`, so the statement's
    /// left side is the very term being rewritten and composes under `transEq`. Valid only when
    /// `a` IS `T`: the second step relies on `def_false a`'s right side being `(¬a = T)`.
    let private notTEq (a: Prop) : Theorem =
        theorem prop_calculus ((- a) == F) [ def_false a |> at_right; ident_eq (- a) |> at_right ]

    /// ¬F = T  (Gries 3.13), as an equality Theorem so it composes with `transEq`.
    let private notFEq (a: Prop) : Theorem = theorem prop_calculus ((- a) == T) [ not_false |> apply ]

    /// True when the clause holds a complementary literal pair, and so is `T`.
    let private isTautClause (clause: Expr) : bool =
        let rec litsOf (e: Expr) = match e with Or(x, y) -> litsOf x @ litsOf y | _ -> [ e ]
        let ls = litsOf clause
        ls |> List.exists (function Not a -> ls |> List.exists (fun m -> sequal m a) | _ -> false)

    /// `(l ∧ r) == result`, with a constant conjunct collapsed away — `T` by identity (Gries 3.39),
    /// `F` by zero (3.40), which swallows the whole conjunction. `None` when neither side is a
    /// constant and the conjunction therefore stands as it is.
    ///
    /// `F` is tested on the RIGHT first so that `F ∧ F` needs no `commute_and`: commuting it would be
    /// a no-op rewrite, which the kernel rejects.
    let private collapseAnd (l: Prop) (r: Prop) : Prop * Theorem option =
        match constOf l, constOf r with
        | _, Some false -> F, Some(theorem prop_calculus ((l * r) == F) [ zero_and l |> at_left ])
        | Some false, _ -> F, Some(theorem prop_calculus ((l * r) == F) [ commute_and l r |> at_left; zero_and r |> at_left ])
        | Some true, Some true -> T, Some(theorem prop_calculus ((l * r) == T) [ ident_and T |> at_left ])
        | Some true, _ -> r, Some(theorem prop_calculus ((l * r) == r) [ commute_and l r |> at_left; ident_and r |> at_left ])
        | _, Some true -> l, Some(theorem prop_calculus ((l * r) == l) [ ident_and l |> at_left ])
        | None, None -> (l * r), None

    /// Conjoin two already-converted sides, collapsing a `T` away: `(a ∧ b) == result`, given
    /// `pa : a == l` and `pb : b == r`.
    let private conjoin (l: Prop) (pl: Theorem) (r: Prop) (pr: Theorem) : Prop * Theorem =
        let both = congAnd pl pr
        match collapseAnd l r with
        | res, None -> res, both
        | res, Some t -> res, transEq both t

    // Distribute ∨ over ∧: given `ca`, `cb` already in CNF, return `(ca∨cb) in CNF, proof (ca∨cb)==·`.
    //
    // A clause is dropped THE MOMENT distribution builds it, not once at the end (see `prune`).
    // Distribution is multiplicative, so a tautological clause left in an intermediate is multiplied
    // against every clause of every enclosing `∨` — that, not any genuine size blowup, is what made
    // nested `≢` explode: 4-variable xor associativity built tens of thousands of clauses to keep 16.
    // Pruning here keeps the intermediates the size of the answer. `drop` turns it off, for the one
    // caller that needs the unpruned clause set (see `to_cnf`).
    let rec private distribOr (drop: bool) (ca: Prop) (cb: Prop) : Prop * Theorem =
        // A side that is `T` — pruned, or a constant from the input — swallows the disjunction
        // (Gries 3.29); a side that is `F` drops out of it by identity (3.30). As in `collapseAnd`,
        // the right side is tested first for each constant so that `T ∨ T` / `F ∨ F` need no commute.
        match constOf ca, constOf cb with
        | _, Some true ->
            T, theorem prop_calculus ((ca + cb) == T) [ zero_or ca |> at_left ]
        | Some true, _ ->
            T, theorem prop_calculus ((ca + cb) == T) [ commute_or ca cb |> at_left; zero_or cb |> at_left ]
        | _, Some false ->
            ca, theorem prop_calculus ((ca + cb) == ca) [ ident_or ca |> at_left ]
        | Some false, _ ->
            cb, theorem prop_calculus ((ca + cb) == cb) [ commute_or ca cb |> at_left; ident_or cb |> at_left ]
        | None, None ->
        match view ca with
        | VAnd(x, y) ->
            let (l, pl) = distribOr drop x cb
            let (r, pr) = distribOr drop y cb
            let (res, pc) = conjoin l pl r pr
            res, transEq (dorR x y cb) pc
        | _ ->
            match view cb with
            | VAnd(u, v) ->
                let (l, pl) = distribOr drop ca u
                let (r, pr) = distribOr drop ca v
                let (res, pc) = conjoin l pl r pr
                res, transEq (dorL ca u v) pc
            | _ ->
                let c = ca + cb
                if drop && isTautClause (expand c.Expr) then T, theorem prop_calculus (c == T) [ simp ]
                else c, refl c

    /// Convert `p` to CNF, returning `(cnf, proof : p == cnf)`.
    let rec private toCnfRec (drop: bool) (p: Prop) : Prop * Theorem =
        let recur = toCnfRec drop
        match view p with
        | VAtom -> p, refl p
        | VAnd(x, y) ->
            let (cx, px) = recur x
            let (cy, py) = recur y
            conjoin cx px cy py
        | VOr(x, y) ->
            let (cx, px) = recur x
            let (cy, py) = recur y
            let (c, pc) = distribOr drop cx cy
            c, transEq (congOr px py) pc
        | VImp(x, y) -> let (c, pc) = recur ((-x) + y) in c, transEq (implEq x y) pc
        | VIff(x, y) -> let (c, pc) = recur ((x ==> y) * (y ==> x)) in c, transEq (iffEq x y) pc
        | VXor(x, y) -> let (c, pc) = recur (-(x == y)) in c, transEq (xorEq x y) pc
        | VNot a ->
            match view a with
            // A negated CONSTANT is not a literal — fold it, or `clauses_of` mints a DIMACS variable
            // for `T`/`F` and the solver satisfies `¬φ` by choosing its value (see the notes above).
            | VAtom ->
                match constOf a with
                | Some true -> F, notTEq a                                             // ¬T = F
                | Some false -> T, notFEq a                                            // ¬F = T
                | None -> p, refl p                                                    // ¬atom is a literal
            | VNot b -> let (c, pc) = recur b in c, transEq (dnegEq b) pc                  // ¬¬b = b
            | VAnd(x, y) -> let (c, pc) = recur ((-x) + (-y)) in c, transEq (dmAndEq x y) pc
            | VOr(x, y) -> let (c, pc) = recur ((-x) * (-y)) in c, transEq (dmOrEq x y) pc
            | VImp(x, y) -> let (c, pc) = recur (-((-x) + y)) in c, transEq (congNot (implEq x y)) pc
            | VIff(x, y) -> let (c, pc) = recur (-((x ==> y) * (y ==> x))) in c, transEq (congNot (iffEq x y)) pc
            | VXor(x, y) -> let (c, pc) = recur (-(-(x == y))) in c, transEq (congNot (xorEq x y)) pc

    /// Convert `p` to CNF, returning `(cnf, proof : p == cnf)`.
    ///
    /// Distribution emits a great many clauses that are **tautological** — holding both `v` and
    /// `¬v`. Nested `≢` is the extreme case: xor associativity over three variables yields 441
    /// clauses of which **433 are tautologies**, and the 8 that remain are exactly what the
    /// solver-side clausifier `SAT.cnf_of_negated_goal` produces (it drops them in `normClause`).
    /// Carrying the other 433 into the kernel replay is what used to overflow its stack.
    ///
    /// So they are dropped, with a proof — a clause holding a complementary pair is `T`, and
    /// `X ∧ T` is `X`. Dropping happens in `distribOr`, at the moment each clause is built, rather
    /// than in one pass over the finished conjunction: distribution is multiplicative, so a
    /// tautology left in an intermediate is multiplied against every clause of every enclosing `∨`.
    /// The end result is the same either way; pruning early is what keeps the intermediates, and
    /// hence the conversion itself, the size of the answer. Measured on 4-variable xor
    /// associativity: 229 s → 0.4 s.
    ///
    /// The pruning is CLAUSE-LOCAL, by congruence, for two reasons. It must not simplify ACROSS
    /// clauses, because that destroys the clause structure the caller needs: the CNF of
    /// `¬(p ∨ ¬p)` is `¬p ∧ p`, which contains no tautological clause but which a global `simp`
    /// collapses to `F`, leaving nothing to clausify. And justifying the pruning with one `simp` is
    /// unreliable anyway — it fails on `∨`-over-`∧` distributivity, where `simp` also dedups
    /// literals and absorbs, and the two sides do not converge. Locally, each obligation is just
    /// `clause == T` on a single clause, which `simp` discharges from the complementary pair.
    /// Two results are NOT clause sets, and a caller that hands the output to a solver has to treat
    /// them as decided rather than clausify them (`SatProof` does):
    ///
    /// - `T` — `p` is valid. Either every clause was pruned as a tautology, or constant folding
    ///   reduced the whole formula. Only the first is recoverable, so pruning is turned off and the
    ///   conversion retried; if that still yields `T`, the formula really is `T`.
    /// - `F` — `p` is unsatisfiable, by constant folding alone. There is nothing to clausify and
    ///   nothing to ask a solver: for the reconstruction pipeline, where `p` is `¬φ`, this proof IS
    ///   the refutation.
    let to_cnf (p: Prop) : Prop * Theorem =
        match toCnfRec true p with
        | c, _ when isT c -> toCnfRec false p
        | r -> r

    /// True if `p` is in CNF (∧ of clauses; each a ∨ of literals; negation only on atoms).
    let rec is_cnf (p: Prop) : bool =
        let rec isClause q =
            match view q with
            | VOr(x, y) -> isClause x && isClause y
            | VNot a -> (match view a with VAtom -> true | _ -> false)
            | VAtom -> true
            | _ -> false
        match view p with
        | VAnd(x, y) -> is_cnf x && is_cnf y
        | _ -> isClause p
