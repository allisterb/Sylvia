namespace Sylvia.Tests.Prover

open System

open FSharp.Quotations

open Xunit

open Sylvia
open Sylvia.Formula
open Sylvia.Patterns
open Sylvia.PropCalculus
open Sylvia.PredCalculus

/// Trusted-base verification for the quantifier fragment of S (Gries ch. 8/9).
///
/// The propositional admissible rules are checked by an ANF oracle (see KernelProofTests).
/// That oracle cannot see through ∀/∃, so this suite verifies the quantifier axioms and
/// admissible rewrites by FINITE-DOMAIN GROUNDING: interpret (∀x|R:P) as ∧ over a small
/// domain and (∃x|R:P) as ∨, replacing the dummy by fresh domain constants and leaving
/// predicate applications as opaque atoms. A schematic quantifier law is equivalence-
/// preserving over every interpretation iff its two grounded sides are propositionally
/// equivalent for each domain size — which the ANF oracle then decides on the ground form.
type QuantifierKernelTests() =
    inherit Sylvia.Tests.Prover.TestsRuntime()

    do Proof.LogLevel <- 0

    // ----- finite-domain grounding -------------------------------------------
    let tE : Expr = T.Expr.Raw
    let fE : Expr = F.Expr.Raw
    let band a b = <@@ (%%a: bool) && (%%b: bool) @@>
    let bor  a b = <@@ (%%a: bool) || (%%b: bool) @@>
    let bimp a b = <@@ (not (%%a: bool)) || (%%b: bool) @@>
    let bigAnd = function [] -> tE | xs -> List.reduce band xs
    let bigOr  = function [] -> fE | xs -> List.reduce bor  xs

    let rec cartesian = function [] -> [ [] ] | xs :: rest -> [ for x in xs do for r in cartesian rest -> x :: r ]

    let domConst (v: Var) i = Expr.Var(Var(sprintf "d%d_%s" i v.Name, v.Type))

    // (∀ bound | R : P) ~> AND over all domain assignments of (R ==> P); ∃ ~> OR of (R && P).
    // Nested quantifiers over other dummies are grounded recursively.
    let rec ground n e =
        match e with
        | ForAll(_, bound, r, p) ->
            cartesian [ for _ in bound -> [ 0 .. n - 1 ] ]
            |> List.map (fun idxs ->
                let subst ex = List.fold2 (fun acc (v: Var) i -> replace_var_expr v (domConst v i) acc) ex bound idxs
                bimp (ground n (subst r)) (ground n (subst p)))
            |> bigAnd
        | Exists(_, bound, r, p) ->
            cartesian [ for _ in bound -> [ 0 .. n - 1 ] ]
            |> List.map (fun idxs ->
                let subst ex = List.fold2 (fun acc (v: Var) i -> replace_var_expr v (domConst v i) acc) ex bound idxs
                band (ground n (subst r)) (ground n (subst p)))
            |> bigOr
        | ExprShape.ShapeVar v -> Expr.Var v
        | ExprShape.ShapeLambda(v, b) -> Expr.Lambda(v, ground n b)
        | ExprShape.ShapeCombination(o, es) -> ExprShape.RebuildShapeCombination(o, es |> List.map (ground n))

    // Two quantified formulas are semantically equal iff their groundings are propositionally
    // equivalent for every tested domain size (the ANF oracle decides the ground form).
    let equalOverDomains (lhs: Expr) (rhs: Expr) =
        [ 1; 2; 3 ] |> List.forall (fun n -> EquationalLogic.Anf.equivalent (ground n lhs) (ground n rhs))

    // ----- schema builders ---------------------------------------------------
    let x = intvar "x"
    let y = intvar "y"
    let R = symbolic_pred<int> "R"
    let Pp = symbolic_pred<int> "P"
    let Qq = symbolic_pred<int> "Q"
    let Nn = symbolic_pred<int> "N"
    let p = boolvar "p"   // an x-independent proposition

    let fa (xt: Term<int>) (r: Expr<bool>) (b: Expr<bool>) : Expr = <@@ forall_expr (%xt.Expr) (%r) (%b) @@>
    let ex (xt: Term<int>) (r: Expr<bool>) (b: Expr<bool>) : Expr = <@@ exists_expr (%xt.Expr) (%r) (%b) @@>
    let fa2 (a: Term<int>) (b: Term<int>) (r: Expr<bool>) (bod: Expr<bool>) : Expr = <@@ forall_expr (%a.Expr, %b.Expr) (%r) (%bod) @@>
    let qv = Var("q", typeof<int -> int -> bool>)
    let q2 (a: Term<int>) (b: Term<int>) : Expr<bool> = Expr.Application(Expr.Application(Expr.Var qv, a.Expr), b.Expr) |> expand_as<bool>
    let band' (a: Expr<bool>) (b: Expr<bool>) : Expr<bool> = <@ (%a) && (%b) @>
    let bor'  (a: Expr<bool>) (b: Expr<bool>) : Expr<bool> = <@ (%a) || (%b) @>

    // ========================================================================
    [<Fact>]
    member _.``every admissible quantifier rewrite in Theory.S is equivalence-preserving`` () =
        // Each case is a rewrite rule and a valid input on which it must fire; the grounded
        // input and output must be equal over the domain.
        let cases : (string * (Expr -> Expr) * Expr) list = [
            "empty_range (∀)", EquationalLogic._empty_range, fa x F.Expr (Pp[x]).Expr
            "empty_range (∃)", EquationalLogic._empty_range, ex x F.Expr (Pp[x]).Expr
            "trade_body (∀)",  EquationalLogic._trade_body,  fa x (Nn[x]).Expr (Pp[x]).Expr
            "trade_body (∃)",  EquationalLogic._trade_body,  ex x (Nn[x]).Expr (Pp[x]).Expr
            "collect_forall_and", EquationalLogic._collect_forall_and,
                band (fa x (Nn[x]).Expr (Pp[x]).Expr) (fa x (Nn[x]).Expr (Qq[x]).Expr)
            "collect_exists_or",  EquationalLogic._collect_exists_or,
                bor (ex x (Nn[x]).Expr (Pp[x]).Expr) (ex x (Nn[x]).Expr (Qq[x]).Expr)
            "distrib_or_forall",  EquationalLogic._distrib_or_forall,
                bor (p.Expr) (fa x (Nn[x]).Expr (Qq[x]).Expr)
            "split_range_forall", EquationalLogic._split_range_forall, fa x (bor' (R[x]).Expr (Nn[x]).Expr) (Pp[x]).Expr
            "split_range_exists", EquationalLogic._split_range_exists, ex x (bor' (R[x]).Expr (Nn[x]).Expr) (Pp[x]).Expr
        ]
        let notEquiv = ResizeArray<string>()
        let noFire = ResizeArray<string>()
        for (name, rule, inp) in cases do
            let inp = expand inp
            let out = rule inp
            if sequal inp out then noFire.Add(sprintf "%s: %s (rule did not fire)" name (src inp))
            elif not (equalOverDomains inp out) then notEquiv.Add(sprintf "%s: %s -> %s" name (src inp) (src out))
        Assert.True(notEquiv.Count = 0, sprintf "NON-EQUIVALENCE-PRESERVING quantifier rewrites:\n%s" (String.Join("\n", notEquiv)))
        Assert.True(noFire.Count = 0, sprintf "inputs that did not exercise the rule:\n%s" (String.Join("\n", noFire)))

    [<Fact>]
    member _.``quantifier axioms of S are sound over a finite domain`` () =
        // Each axiom is an equivalence L = R; both sides must ground-equal over the domain.
        let cases : (string * Expr * Expr) list = [
            // Trading 9.2: (∀x|R:P) = (∀x|: ¬R ∨ P)
            "Trading 9.2", fa x (R[x]).Expr (Pp[x]).Expr, fa x T.Expr (bor' (<@ not %(R[x]).Expr @>) (Pp[x]).Expr)
            // Distributivity of ∨ over ∀ 9.5 (¬occ(x,P)): P ∨ (∀x|R:Q) = (∀x|R: P∨Q)
            "ForAllDistribOr 9.5", bor (p.Expr) (fa x (R[x]).Expr (Qq[x]).Expr), fa x (R[x]).Expr (bor' (p.Expr) (Qq[x]).Expr)
            // Generalized De Morgan 9.17: (∃x|R:P) = ¬(∀x|R:¬P)
            "GenDeMorgan 9.17", ex x (R[x]).Expr (Pp[x]).Expr, <@@ not (%%(fa x (R[x]).Expr (<@ not %(Pp[x]).Expr @>)): bool) @@>
            // Distributivity 8.15 (∀): (∀x|R:P) ∧ (∀x|R:Q) = (∀x|R: P∧Q)
            "QuantifierCollect 8.15", band (fa x (R[x]).Expr (Pp[x]).Expr) (fa x (R[x]).Expr (Qq[x]).Expr), fa x (R[x]).Expr (band' (Pp[x]).Expr (Qq[x]).Expr)
            // Range split 8.18 (∀): (∀x|R∨N:P) = (∀x|R:P) ∧ (∀x|N:P)
            "RangeSplit 8.18", fa x (bor' (R[x]).Expr (Nn[x]).Expr) (Pp[x]).Expr, band (fa x (R[x]).Expr (Pp[x]).Expr) (fa x (Nn[x]).Expr (Pp[x]).Expr)
            // Interchange 8.19 (¬occ(y,R),¬occ(x,Q)): (∀x|R:(∀y|Q:P)) = (∀y|Q:(∀x|R:P))
            "Interchange 8.19", fa x (R[x]).Expr (<@ %%(fa y (Qq[y]).Expr (q2 x y)): bool @>), fa y (Qq[y]).Expr (<@ %%(fa x (R[x]).Expr (q2 x y)): bool @>)
            // Nesting 8.20 (¬occ(y,R)): (∀x,y|R∧Q:P) = (∀x|R:(∀y|Q:P))
            "Nesting 8.20", fa2 x y (band' (R[x]).Expr (Qq[y]).Expr) (q2 x y), fa x (R[x]).Expr (<@ %%(fa y (Qq[y]).Expr (q2 x y)): bool @>)
            // Nesting form 1 (Q:=R, ¬occ(y,R)): (∀x,y|R:P) = (∀x|R:(∀y|R:P))
            "Nesting (form1)", fa2 x y (R[x]).Expr (q2 x y), fa x (R[x]).Expr (<@ %%(fa y (R[x]).Expr (q2 x y)): bool @>)
        ]
        let bad = ResizeArray<string>()
        for (name, lhs, rhs) in cases do
            if not (equalOverDomains (expand lhs) (expand rhs)) then bad.Add(sprintf "%s: %s = %s" name (src lhs) (src rhs))
        Assert.True(bad.Count = 0, sprintf "UNSOUND quantifier axioms:\n%s" (String.Join("\n", bad)))

    [<Fact>]
    member _.``distrib_or_forall respects its not-occurs-free side condition`` () =
        // The disjunct being moved inside the ∀ depends on the dummy (R x); the ¬occurs('x','P')
        // side condition must block the rewrite (otherwise it captures x and is unsound).
        let input = expand (bor ((R[x]).Expr) (fa x (Nn[x]).Expr (Qq[x]).Expr))
        let out = EquationalLogic._distrib_or_forall input
        Assert.True(sequal input out, sprintf "distrib_or_forall fired on an x-dependent disjunct: %s -> %s" (src input) (src out))

    [<Fact>]
    member _.``occurs_free detects free variables through predicate applications and ranges`` () =
        let bx = get_vars (x.Expr)
        let by = get_vars (y.Expr)
        Assert.True(occurs_free bx (expand (R[x]).Expr), "x should be free in (R x)")
        Assert.False(occurs_free bx (expand (fa x F.Expr (R[x]).Expr)), "x is bound in (∀x|:R x)")
        Assert.True(occurs_free bx (expand (fa y (R[x]).Expr (Qq[y]).Expr)), "x should be free in the range of (∀y|R x:Q y)")
        Assert.False(occurs_free by (expand (R[x]).Expr), "y should not occur in (R x)")
        Assert.False(occurs_free bx (expand p.Expr), "x should not occur in p")

    [<Fact>]
    member _.``universal instantiation axiom is recognized`` () =
        // (∀x|:P) ⇒ P[x:=E] is an axiom for any E (here E := 3 and E := x).
        let e3 = Scalar<int>(3)
        Assert.True(Theory.S.AxEquiv (expand ((forall'(x, Pp) ==> Pp[e3]).Expr)), "(∀x|:P) ⇒ P[3] should be an instantiation axiom")
        Assert.True(Theory.S.AxEquiv (expand ((forall'(x, Pp) ==> Pp[x]).Expr)), "(∀x|:P) ⇒ P[x] should be an instantiation axiom")
