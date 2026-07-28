namespace Sylvia
// A bare rule as a proof step implicitly means "apply to the whole expression".
#nowarn "3391"

/// Conjunctive-normal-form conversion for propositional `Prop`s, emitting a **kernel-checked**
/// equivalence proof.
///
/// `Cnf.toCnf p` returns `(cnf, proof)` where `cnf` is `p` in CNF (a conjunction of clauses, each a
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

    // Distribute ∨ over ∧: given `ca`, `cb` already in CNF, return `(ca∨cb) in CNF, proof (ca∨cb)==·`.
    let rec private distribOr (ca: Prop) (cb: Prop) : Prop * Theorem =
        match view ca with
        | VAnd(x, y) ->
            let (l, pl) = distribOr x cb
            let (r, pr) = distribOr y cb
            (l * r), transEq (dorR x y cb) (congAnd pl pr)
        | _ ->
            match view cb with
            | VAnd(u, v) ->
                let (l, pl) = distribOr ca u
                let (r, pr) = distribOr ca v
                (l * r), transEq (dorL ca u v) (congAnd pl pr)
            | _ -> (ca + cb), refl (ca + cb)

    /// Convert `p` to CNF, returning `(cnf, proof : p == cnf)`.
    let rec private toCnfRec (p: Prop) : Prop * Theorem =
        match view p with
        | VAtom -> p, refl p
        | VAnd(x, y) ->
            let (cx, px) = toCnfRec x
            let (cy, py) = toCnfRec y
            (cx * cy), congAnd px py
        | VOr(x, y) ->
            let (cx, px) = toCnfRec x
            let (cy, py) = toCnfRec y
            let (c, pc) = distribOr cx cy
            c, transEq (congOr px py) pc
        | VImp(x, y) -> let (c, pc) = toCnfRec ((-x) + y) in c, transEq (implEq x y) pc
        | VIff(x, y) -> let (c, pc) = toCnfRec ((x ==> y) * (y ==> x)) in c, transEq (iffEq x y) pc
        | VXor(x, y) -> let (c, pc) = toCnfRec (-(x == y)) in c, transEq (xorEq x y) pc
        | VNot a ->
            match view a with
            | VAtom -> p, refl p                                                       // ¬atom is a literal
            | VNot b -> let (c, pc) = toCnfRec b in c, transEq (dnegEq b) pc               // ¬¬b = b
            | VAnd(x, y) -> let (c, pc) = toCnfRec ((-x) + (-y)) in c, transEq (dmAndEq x y) pc
            | VOr(x, y) -> let (c, pc) = toCnfRec ((-x) * (-y)) in c, transEq (dmOrEq x y) pc
            | VImp(x, y) -> let (c, pc) = toCnfRec (-((-x) + y)) in c, transEq (congNot (implEq x y)) pc
            | VIff(x, y) -> let (c, pc) = toCnfRec (-((x ==> y) * (y ==> x))) in c, transEq (congNot (iffEq x y)) pc
            | VXor(x, y) -> let (c, pc) = toCnfRec (-(-(x == y))) in c, transEq (congNot (xorEq x y)) pc

    /// Convert `p` to CNF, returning `(cnf, proof : p == cnf)`.
    ///
    /// The recursive descent above emits every clause distribution produces, and most of them can be
    /// **tautological** — a clause holding both `v` and `¬v`. Nested `≢` is the extreme case: xor
    /// associativity over three variables yields 441 clauses of which **433 are tautologies**, and
    /// the 8 that remain are exactly what the solver-side clausifier `SAT.cnfOfNegatedGoal` produces
    /// (it drops them in `normClause`). Carrying the other 433 into the kernel replay is what used to
    /// overflow its stack.
    ///
    /// So the last move is to prune them, with a proof — a clause holding a complementary pair is
    /// `T`, and `X ∧ T` is `X`. Measured: 441 → 8 clauses. This is a pure size reduction; the result
    /// is still clean CNF and still equal to `p`.
    ///
    /// The pruning works CLAUSE BY CLAUSE, by congruence, for two reasons. It must not simplify
    /// ACROSS clauses, because that destroys the clause structure the caller needs: the CNF of
    /// `¬(p ∨ ¬p)` is `¬p ∧ p`, which contains no tautological clause but which a global `simp`
    /// collapses to `F`, leaving nothing to clausify. And justifying the whole pruning with one
    /// `simp` is unreliable anyway — it fails on `∨`-over-`∧` distributivity, where `simp` also
    /// dedups literals and absorbs, and the two sides do not converge. Locally, each obligation is
    /// just `clause == T` on a single clause, which `simp` discharges from the complementary pair.
    let rec private prune (p: Prop) : Prop * Theorem option =
        match expand p.Expr with
        | And(a, b) ->
            let (pa, ta) = prune (pOf a)
            let (pb, tb) = prune (pOf b)
            match ta, tb with
            | None, None -> p, None
            | _ ->
                let isT (x: Prop) = match expand x.Expr with True -> true | _ -> false
                let rewrites =
                    [ match ta with Some t -> yield Ident t |> at [ left_branch; left_branch ] | None -> ()
                      match tb with Some t -> yield Ident t |> at [ left_branch; right_branch ] | None -> () ]
                // Having rewritten each side, collapse a `T` conjunct away (Gries 3.39).
                let result, collapse =
                    if isT pa && isT pb then pb, [ ident_and T |> at_left ]
                    elif isT pa then pb, [ commute_and T pb |> at_left; ident_and pb |> at_left ]
                    elif isT pb then pa, [ ident_and pa |> at_left ]
                    else (pa * pb), []
                result, Some(theorem prop_calculus ((pOf a * pOf b) == result) (rewrites @ collapse))
        | clause ->
            let rec litsOf (e: Expr) = match e with Or(x, y) -> litsOf x @ litsOf y | _ -> [ e ]
            let ls = litsOf clause
            if ls |> List.exists (function Not a -> ls |> List.exists (fun m -> sequal m a) | _ -> false)
            then T, Some(theorem prop_calculus (p == T) [ simp ])
            else p, None

    let toCnf (p: Prop) : Prop * Theorem =
        let (c, pf) = toCnfRec p
        match prune c with
        | _, None -> c, pf
        // Every clause a tautology means the conjunction is `T` — not a clause set at all. Keep the
        // original so the caller still has something to clausify (and finds it unsatisfiable).
        | pruned, _ when (match expand pruned.Expr with True -> true | _ -> false) -> c, pf
        | pruned, Some t -> pruned, transEq pf t

    /// True if `p` is in CNF (∧ of clauses; each a ∨ of literals; negation only on atoms).
    let rec isCnf (p: Prop) : bool =
        let rec isClause q =
            match view q with
            | VOr(x, y) -> isClause x && isClause y
            | VNot a -> (match view a with VAtom -> true | _ -> false)
            | VAtom -> true
            | _ -> false
        match view p with
        | VAnd(x, y) -> isCnf x && isCnf y
        | _ -> isClause p
