namespace Sylvia.Tests.Prover

open System

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Xunit

open Sylvia
open Sylvia.Formula
open Sylvia.Patterns

/// Tests for the S equational-logic kernel: that admissible rules are
/// equivalence-preserving, that repaired theorems prove, and that the whole
/// PropCalculus theory still constructs (guards against regressions).
type KernelProofTests() =
    inherit Sylvia.Tests.Prover.TestsRuntime()

    // Keep proof logging quiet during the sweep.
    do Proof.LogLevel <- 0

    let p, q, r, s = boolvar "p", boolvar "q", boolvar "r", boolvar "s"

    // ----- Independent boolean oracle over the raw quotation structure --------
    // Defines the connectives from scratch so it does not depend on any of the
    // prover's own semantics; used to check that a rewrite preserves meaning.
    let rec eval (env: Map<string, bool>) (e: Expr) : bool =
        match e with
        | Bool b                   -> b
        | Var v                    -> env.[v.Name]
        | True                     -> true
        | False                    -> false
        | Not a                    -> not (eval env a)
        | And (a, b)               -> eval env a && eval env b
        | Or  (a, b)               -> eval env a || eval env b
        | Equals (a, b)            -> eval env a = eval env b
        | NotEquals (a, b)         -> eval env a <> eval env b
        | Implies (a, b)           -> (not (eval env a)) || eval env b
        | Conseq (a, b)            -> (not (eval env b)) || eval env a   // p <=== q  ==  q ==> p
        | _ -> failwithf "oracle: unsupported node %s" (src e)

    let freeNames (e: Expr) = e.GetFreeVars() |> Seq.map (fun v -> v.Name) |> Seq.toList

    /// True iff a and b evaluate identically for every assignment to their free vars.
    let equivalent (a: Expr) (b: Expr) =
        let names = (freeNames a @ freeNames b) |> List.distinct
        let n = List.length names
        seq {
            for mask in 0 .. (1 <<< n) - 1 ->
                let env = names |> List.mapi (fun i nm -> nm, (mask >>> i) &&& 1 = 1) |> Map.ofList
                eval env a = eval env b
        }
        |> Seq.forall id

    let applyRule (rule: Expr -> Expr) (input: Prop) =
        let inp = expand input.Expr
        inp, rule inp

    // ===== Rule soundness: admissible rules must preserve equivalence =========

    [<Fact>]
    member _.``idemp leaves p = p unchanged (= is not idempotent)`` () =
        let inp, out = applyRule EquationalLogic._idemp (p == p)
        Assert.True(equivalent inp out, sprintf "%s -> %s not equivalent" (src inp) (src out))
        // must NOT collapse to p
        Assert.False(sequal out p.Expr, "idemp wrongly rewrote (p = p) to p")

    [<Fact>]
    member _.``idemp still reduces p || p and p && p`` () =
        let io1 = applyRule EquationalLogic._idemp (p + p)
        let io2 = applyRule EquationalLogic._idemp (p * p)
        Assert.True(equivalent (fst io1) (snd io1))
        Assert.True(sequal (snd io1) p.Expr, "p || p should reduce to p")
        Assert.True(equivalent (fst io2) (snd io2))
        Assert.True(sequal (snd io2) p.Expr, "p && p should reduce to p")

    [<Fact>]
    member _.``double_neg on implication is equivalence-preserving`` () =
        let inp, out = applyRule EquationalLogic._double_neg (p ==> q)
        Assert.True(equivalent inp out, sprintf "%s -> %s not equivalent" (src inp) (src out))

    [<Fact>]
    member _.``double_neg on negated implication is equivalence-preserving`` () =
        let inp, out = applyRule EquationalLogic._double_neg (!! (p ==> q))
        Assert.True(equivalent inp out, sprintf "%s -> %s not equivalent" (src inp) (src out))

    [<Fact>]
    member _.``subst_false keeps E as antecedent and preserves equivalence`` () =
        let inp, out = applyRule EquationalLogic._subst_false ((p * r) ==> (p + q))
        Assert.True(equivalent inp out, sprintf "%s -> %s not equivalent" (src inp) (src out))

    // ===== Truth constants: rules stay in Prop-land (named T/F, never bare bool) =

    [<Fact>]
    member _.``truth constants are the named Prop T/F, distinct from bare bool literals`` () =
        // A proposition's truth values are the named constants T/F, which are NOT the
        // bare bool literals true/false (propositions are compared only to propositions).
        Assert.False(sequal T.Expr <@@ true @@>, "named T must not be the bare literal true")
        Assert.False(sequal F.Expr <@@ false @@>, "named F must not be the bare literal false")
        Assert.False(sequal T.Expr F.Expr, "T must not equal F")
        Assert.True(sequal T.Expr T.Expr)

    [<Fact>]
    member _.``rule functions emit the named T constant (this is what repaired contr)`` () =
        // _excluded_middle used to emit a bare `true`, which did not match the named T
        // in rules like commute_eq F T, stalling contr. It must now yield the named T.
        let out = EquationalLogic._excluded_middle (expand (p + !!p).Expr)
        Assert.True(sequal out T.Expr, sprintf "excluded_middle produced %s, expected T" (src out))

    // ===== Mis-targeted steps fail loudly (no silent no-ops) ==================

    [<Fact>]
    member _.``apply_left on a non-binary expression throws`` () =
        // Targeting the left of an expression that has no binary structure (here a
        // bare variable) is a proof-authoring error and must not silently no-op.
        let ra = apply_left PropCalculus.commute
        Assert.ThrowsAny<exn>(fun () -> ra.ApplyRule (expand p.Expr) |> ignore) |> ignore

    // ===== Trusted base: every admissible rule preserves equivalence ==========
    // The propositional Admit rules in Theory.S are trusted rewrites; verify each is
    // equivalence-preserving (this is the class of defect that broke idemp/double_neg/
    // subst_false). Quantifier rules (empty_range, trade_body, ...) need a finite-domain
    // model checker and are out of scope for this propositional oracle.

    [<Fact>]
    member _.``every propositional admissible rule in Theory.S is equivalence-preserving`` () =
        let inline e (pr: Prop) = expand pr.Expr
        let raw (q: Expr) = expand q
        let cases : (string * (Expr -> Expr) * Expr list) list = [
            "reduce",             EquationalLogic._reduce_constants,
                [ raw <@@ true && false @@>; raw <@@ true || false @@>; raw <@@ not true @@>
                  raw <@@ (true:bool) = false @@>; raw <@@ (true:bool) <> false @@>; raw <@@ (true:bool) ===> false @@> ]
            "commute",            EquationalLogic._commute,      [ e (p == q); e (p + q); e (p * q) ]
            "left_assoc",         EquationalLogic._left_assoc,   [ e (p == (q == r)); e (p + (q + r)); e (p * (q * r)) ]
            "right_assoc",        EquationalLogic._right_assoc,  [ e ((p == q) == r); e ((p + q) + r); e ((p * q) * r) ]
            "distrib",            EquationalLogic._distrib,
                [ e (!!(p == q)); e (p + (q == r)); e (p + (q * r)); e (p + (q + r)); e (p * (q + r)); e (!!(p + q)); e (!!(p * q)) ]
            "collect",            EquationalLogic._collect,
                [ e (!!p == q); e ((p + q) == (p + r)); e ((p + q) * (p + r)); e ((p * q) + (p * r)); e ((p + q) + (p + r)); e (!!p + !!q); e (!!p * !!q) ]
            "idemp",              EquationalLogic._idemp,        [ e (p + p); e (p * p) ]
            "excluded_middle",    EquationalLogic._excluded_middle, [ e (p + !!p) ]
            "golden_rule",        EquationalLogic._golden_rule,  [ e (p * q) ]
            "def_implies",        EquationalLogic._def_implies,  [ e (p ==> q) ]
            "shunt",              EquationalLogic._shunt,        [ e ((p * q) ==> r) ]
            "rshunt",             EquationalLogic._rshunt,       [ e (p ==> (q ==> r)) ]
            "mutual_implication", EquationalLogic._mutual_implication, [ e (p == q) ]
            "subst_and",          EquationalLogic._subst_and,          [ e ((p == q) * (p + r)) ]
            "subst_implies",      EquationalLogic._subst_implies,      [ e ((p == q) ==> (p + r)) ]
            "subst_and_implies",  EquationalLogic._subst_and_implies,  [ e ((s * (p == q)) ==> (p + r)) ]
            "subst_true",         EquationalLogic._subst_true,         [ e (p ==> (p + q)); e (p * (p + q)) ]
            "subst_false",        EquationalLogic._subst_false,        [ e ((p + q) ==> p); e ((p * r) ==> (p + q)); e (p + (p * q)) ]
            "distrib_implies",    EquationalLogic._distrib_implies,
                [ e (p * (p ==> q)); e (p * (q ==> p)); e (p + (p ==> q)); e (p + (q ==> p)); e ((p + q) ==> (p * q)) ]
            "double_neg",         EquationalLogic._double_neg,
                [ e (p ==> q); e (!!(p ==> q)); raw <@@ (%%p.Expr:bool) <=== (%%q.Expr:bool) @@>
                  e (p == q); e (p * q); e (p + q); e T; e F ]
            "normalize",          EquationalLogic._normalize,
                // reassociation, commutation, nested, and ≡-chain (nested, not the top ≡) cases
                [ e ((p + q) + r); e (q + p); e ((p * q) * r); e (q * p)
                  e ((p + q) * (r + p)); e (r == (q == p)); e (p + (r == q)) ]
            "normalize_assoc",    EquationalLogic._normalize_assoc,
                // reassociation-only inputs (order-preserving, so each must reassociate to fire)
                [ e ((p + q) + r); e ((p * q) * r); e (p + ((q == r) == s)) ]
            "simp_laws",          EquationalLogic._simp_laws,
                // identity, annihilator, complement, idempotence, absorption, double-neg, const-≡
                [ e (p * T); e (p * F); e (p + F); e (p + T)
                  e (p * !!p); e (p + !!p); e (p * p); e (p + p)
                  e (p * (p + q)); e (p + (p * q)); e (!!(!!p)); e (T == p); e (F == p) ]
        ]
        let notEquiv = ResizeArray<string>()
        let noFire = ResizeArray<string>()
        for (name, rule, inputs) in cases do
            for inp in inputs do
                let out = rule inp
                if sequal inp out then noFire.Add(sprintf "%s: %s (rule did not fire)" name (src inp))
                elif not (equivalent inp out) then notEquiv.Add(sprintf "%s: %s -> %s" name (src inp) (src out))
        Assert.True(notEquiv.Count = 0, sprintf "NON-EQUIVALENCE-PRESERVING rewrites:\n%s" (String.Join("\n", notEquiv)))
        // Every input should exercise its rule; a no-op means the test input is miscovered.
        Assert.True(noFire.Count = 0, sprintf "inputs that did not exercise the rule:\n%s" (String.Join("\n", noFire)))

    [<Fact>]
    member _.``subst_or_and (Shannon) fires and preserves equivalence`` () =
        // Build E = p ∨ q and the Shannon expansion (p ∧ E[p:=T]) ∨ (¬p ∧ E[p:=F]).
        let pv = match p.Expr with | Patterns.Var v -> v | _ -> failwith "p is not a Var"
        let bigE = expand (p + q).Expr
        let eT = replace_var_expr pv (T.Expr.Raw) bigE
        let eF = replace_var_expr pv (F.Expr.Raw) bigE
        let pE = Expr.Var pv
        let shannon = <@@ ((%%pE:bool) && (%%eT:bool)) || ((not (%%pE:bool)) && (%%eF:bool)) @@>
        let input = expand <@@ (%%bigE:bool) = (%%shannon:bool) @@>
        let out = EquationalLogic._subst_or_and input
        Assert.False(sequal input out, "subst_or_and did not fire on the Shannon input")
        Assert.True(equivalent input out, sprintf "%s -> %s not equivalent" (src input) (src out))

    // ===== AC-normalization: collapses associativity/commutativity bookkeeping =

    [<Fact>]
    member _.``normalize closes an AC-equivalent disjunction goal in one step`` () =
        // ((p ∨ q) ∨ r) ≡ (r ∨ (q ∨ p)) needs a run of left_assoc/right_assoc/commute
        // steps today; normalizing both sides makes them syntactically identical (SEqual).
        let goal = ((p + q) + r) == (r + (q + p))
        let pr = proof PropCalculus.prop_calculus goal [ apply PropCalculus.normalize ]
        Assert.True(pr.Complete, sprintf "normalize should close the goal; last state %s" (src pr.LastState))

    [<Fact>]
    member _.``normalize closes an AC-equivalent equivalence-chain goal in one step`` () =
        // ≡ is associative and commutative, so (p ≡ q ≡ r) ≡ (r ≡ (q ≡ p)) is valid;
        // normalize flattens each side's ≡-chain into the same canonical form.
        let goal = (((p == q) == r)) == (r == (q == p))
        let pr = proof PropCalculus.prop_calculus goal [ apply PropCalculus.normalize ]
        Assert.True(pr.Complete, sprintf "normalize should close the goal; last state %s" (src pr.LastState))

    [<Fact>]
    member _.``normalize_assoc reassociates but preserves operand order (unlike full normalize)`` () =
        // assoc-only must NOT reorder: q ∨ p stays q ∨ p, whereas full normalize sorts to p ∨ q.
        let inp = expand (q + p).Expr
        let assocOut = EquationalLogic._normalize_assoc inp
        let acOut    = EquationalLogic._normalize inp
        Assert.True(sequal assocOut inp, sprintf "assoc must keep order: %s -> %s" (src inp) (src assocOut))
        Assert.True(sequal acOut (expand (p + q).Expr), sprintf "full normalize should sort to p ∨ q, got %s" (src acOut))

    // ===== simp: deterministic simplifier closes "obvious" propositional goals =

    [<Fact>]
    member _.``simp closes obvious propositional goals in one step`` () =
        // Each of these is an "obvious" tautology a single `apply simp` should discharge.
        let goals : Prop list =
            [ (p + !!p) == T                    // excluded middle
              (p * !!p) == F                    // contradiction
              (p * T) == p                      // identity
              (p + F) == p                      // identity
              (p * (q + !!q)) == p              // complement inside a conjunction
              (!!(!!p)) == p                    // double negation
              (((p + q) + r)) == (r + (q + p))  // AC-equivalence (via normalize inside simp)
              (p * q) == (q * p) ]              // commutativity (normalize inside simp)
        for g in goals do
            let pr = proof PropCalculus.prop_calculus g [ apply PropCalculus.simp ]
            Assert.True(pr.Complete, sprintf "simp should close %s; last state %s" (src (expand g.Expr)) (src pr.LastState))

    [<Fact>]
    member _.``simp leaves a non-theorem unproved (does not overclaim)`` () =
        // simp must not "close" something that isn't valid; p = q is not a tautology.
        let pr = proof PropCalculus.prop_calculus (p == q) [ apply PropCalculus.simp ]
        Assert.False(pr.Complete, "simp must not close the non-theorem p = q")

    [<Fact>]
    member _.``normalize leaves a non-bool equality intact (AC restricted to bool)`` () =
        // The ≡ case is guarded to bool operands: a real (non-bool) `=` is an atom and
        // must NOT be AC-reordered (y = x must not become x = y).
        let x, y = realvar "x", realvar "y"
        let inp = expand (y == x).Expr
        let out = EquationalLogic._normalize inp
        Assert.True(sequal inp out, sprintf "non-bool = must not be AC-reordered: %s -> %s" (src inp) (src out))

    // ===== Repaired theorems: contr and everything that depends on it =========

    [<Fact>]
    member _.``contr proves: p and not p = F`` () =
        PropCalculus.contr p |> ignore

    [<Fact>]
    member _.``mutual_implication' proves`` () =
        PropCalculus.mutual_implication' p q |> ignore

    [<Fact>]
    member _.``case_analysis_2 proves`` () =
        PropCalculus.case_analysis_2 p r |> ignore

    [<Fact>]
    member _.``antisymm_implies proves`` () =
        PropCalculus.antisymm_implies p q |> ignore

    [<Fact>]
    member _.``trans_implies_eq proves`` () =
        PropCalculus.trans_implies_eq p q r |> ignore

    [<Fact>]
    member _.``absorb_or_not proves: p or (not p and q) = p or q`` () =
        PropCalculus.absorb_or_not p q |> ignore

    [<Fact>]
    member _.``weaken_and_or proves: p and q implies p or q`` () =
        PropCalculus.weaken_and_or p q |> ignore

    // ===== Whole-theory regression guard ======================================
    // Every static PropCalculus member returning Rule/Theorem with all-Prop
    // parameters must construct (Theorem/derived-rule construction throws if the
    // proof is incomplete), except two pre-existing failures tracked separately.

    static member private KnownFailing : Set<string> =
        // No known-failing PropCalculus proofs. Add a name here (with a reason) if a
        // proof is temporarily broken so this sweep still guards the rest of the theory.
        Set.empty

    [<Fact>]
    member _.``all PropCalculus rules and theorems construct (except known-failing)`` () =
        let names = [| "p";"q";"r";"s";"t";"u" |]
        let mkArgs n = Array.init n (fun i -> box (boolvar names.[i]))
        let methods =
            PropCalculus.Type.GetMethods()
            |> Array.filter (fun m -> m.IsStatic && m.IsPublic && not (m.Name.StartsWith "get_"))
            |> Array.filter (fun m -> m.ReturnType = typeof<Rule> || m.ReturnType = typeof<Theorem>)
            |> Array.filter (fun m -> m.GetParameters() |> Array.forall (fun pr -> pr.ParameterType = typeof<Prop>))
        let failures =
            methods
            |> Array.filter (fun m -> not (KernelProofTests.KnownFailing.Contains m.Name))
            |> Array.choose (fun m ->
                try m.Invoke(null, mkArgs (m.GetParameters().Length)) |> ignore; None
                with e ->
                    let msg = if isNull (box e.InnerException) then e.Message else e.InnerException.Message
                    Some (sprintf "%s: %s" m.Name (msg.Split('\n').[0])))
        Assert.True(methods.Length > 50, sprintf "expected to discover the theory (found %d methods)" methods.Length)
        Assert.True(failures.Length = 0, sprintf "unexpected proof failures:\n%s" (String.Join("\n", failures)))
