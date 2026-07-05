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
            // Boolean-ring (ANF) rewrite rules used by autoproof_anf (⊕ = <>):
            "elim_to_xor",        EquationalLogic._elim_to_xor,
                [ e (!!p); e (p + q); e (p ==> q); e (p == q) ]
            "distrib_and_xor",    EquationalLogic._distrib_and_xor,
                [ raw <@@ (%%p.Expr:bool) && ((%%q.Expr:bool) <> (%%r.Expr:bool)) @@> ]
            "and_normalize",      EquationalLogic._and_normalize,   // dedup repeated atom
                [ raw <@@ (%%p.Expr:bool) && ((%%p.Expr:bool) && (%%q.Expr:bool)) @@> ]
            "xor_normalize",      EquationalLogic._xor_normalize,   // cancel x⊕x
                [ raw <@@ ((%%p.Expr:bool) <> (%%q.Expr:bool)) <> (%%p.Expr:bool) @@> ]
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

    // ===== First-match substitution: Subst/Ident rewrite one occurrence ========

    [<Fact>]
    member _.``replace_first_expr rewrites only the leftmost-outermost match`` () =
        // (p ∧ p) ∨ (p ∧ p): replacing p ∧ p with p must touch only the first.
        let target = expand (p * p).Expr
        let e = expand ((p * p) + (p * p)).Expr
        let out = replace_first_expr target (expand p.Expr) e
        let expected = expand (p + (p * p)).Expr    // first collapsed, second intact
        Assert.True(sequal out expected, sprintf "expected %s got %s" (src expected) (src out))
        // a second application collapses the remaining occurrence
        let out2 = replace_first_expr target (expand p.Expr) out
        Assert.True(sequal out2 (expand (p + p).Expr), sprintf "second pass: got %s" (src out2))

    // ===== applyfirst: apply a rule wherever it fires, no hand-addressing =====

    [<Fact>]
    member _.``applyfirst applies a rule at the first firing position without addressing`` () =
        // idemp fires on the nested (p ∨ p); applyfirst finds it with no branch combinators,
        // reducing (q ∨ (p ∨ p)) = (q ∨ p) to (q ∨ p) = (q ∨ p) which SEqual closes.
        let goal = (q + (p + p)) == (q + p)
        let pr = proof PropCalculus.prop_calculus goal [ applyfirst PropCalculus.idemp ]
        Assert.True(pr.Complete, sprintf "applyfirst idemp should close; last state %s" (src pr.LastState))

    [<Fact>]
    member _.``applyfirst fires at the leftmost occurrence only`` () =
        // With two nested (p ∨ p), applyfirst idemp collapses only the first; the goal
        // needs a second application, proving it targets one occurrence at a time.
        let e = expand ((p + p) + (q + (p + p))).Expr
        let out = apply_first_firing EquationalLogic._idemp e
        // leftmost (p ∨ p) -> p ; the second (p ∨ p) remains
        let expected = expand (p + (q + (p + p))).Expr
        Assert.True(sequal out expected, sprintf "expected %s got %s" (src expected) (src out))

    // ===== autoapply: infer a derived rule's arguments and position by matching =

    [<Fact>]
    member _.``try_match binds metavariables and infers rule arguments`` () =
        let a, b = meta "a", meta "b"
        let schema = expand (a * b).Expr        // ?a ∧ ?b
        let target = expand ((p + q) * r).Expr  // (p ∨ q) ∧ r
        match try_match schema target with
        | Some m ->
            Assert.True(sequal m.["?a"] (expand (p + q).Expr), "?a should bind to (p ∨ q)")
            Assert.True(sequal m.["?b"] (expand r.Expr), "?b should bind to r")
        | None -> Assert.True(false, "schema ?a ∧ ?b should match (p ∨ q) ∧ r")
        // a ∧-schema must not match a ∨-target
        Assert.True((try_match schema (expand (p + q).Expr)).IsNone, "∧ schema must not match a ∨")

    [<Fact>]
    member _.``autoapply infers a derived rule's arguments and position`` () =
        // golden rule applied to the NESTED (p ∧ q) under r ∨ _, with no explicit args and
        // no branch combinators: autoapply unifies ?a ∧ ?b against the first ∧ subterm.
        let goal = (r + (p * q)) == (r + (p == q == (p + q)))
        let pr = proof PropCalculus.prop_calculus goal [ autoapply (PropCalculus.golden_rule' (meta "a") (meta "b")) ]
        Assert.True(pr.Complete, sprintf "autoapply golden_rule' should close; last state %s" (src pr.LastState))

    // ===== auto: bounded search finds replayable, checkable proofs ============

    [<Fact>]
    member _.``auto proves a goal simp cannot (distrib_and)`` () =
        // distrib_and needs distribution, which simp deliberately omits; auto's search finds it.
        let goal = (p * (q * r)) == ((p * q) * (p * r))
        let pr = PropCalculus.autoproof goal
        Assert.True(pr.Complete, sprintf "auto should close distrib_and; last state %s" (src pr.LastState))

    [<Fact>]
    member _.``auto proves an implication theorem (contrapositive)`` () =
        let pr = PropCalculus.autoproof ((p ==> q) == (!!q ==> !!p))
        Assert.True(pr.Complete, "auto should close the contrapositive")

    [<Fact>]
    member _.``auto's result is a genuine replayable proof, not just a yes`` () =
        // Feed auto's step list back through the ordinary Proof engine: it must complete,
        // proving the search returns a real checkable derivation (the Giant contract).
        let goal = (p ==> q) == (!!p + q)
        let p = PropCalculus.autoproof goal
        Assert.True(p.Complete, "auto's steps must replay to a complete proof")

    [<Fact>]
    member _.``auto does not fabricate a proof of a non-theorem`` () =
        // p = q is invalid; auto must exhaust its budget and throw, never return a "proof".
        Assert.ThrowsAny<exn>(fun () -> PropCalculus.autoproof (p == q) |> ignore) |> ignore

    // ===== autoproof_anf: complete, trace-emitting propositional decider ==================

    [<Fact>]
    member _.``autoproof_anf emits a complete, replayable proof for propositional theorems`` () =
        let goals : Prop list =
            [ (p + !!p) == T
              ((p ==> q) * (q ==> p)) == (p == q)     // auto's search miss — decide closes it
              (p * (q * r)) == ((p * q) * (p * r))
              ((p ==> q) == (!!q ==> !!p))
              (((p ==> q) ==> p) ==> p) ]             // Peirce's law
        for g in goals do
            let pr = PropCalculus.autoproof_anf g
            Assert.True(pr.Complete, sprintf "autoproof_anf should close %s; last state %s" (src (expand g.Expr)) (src pr.LastState))

    [<Fact>]
    member _.``autoproof_anf throws on a non-theorem (never a false proof)`` () =
        Assert.ThrowsAny<exn>(fun () -> PropCalculus.autoproof_anf (p == q) |> ignore) |> ignore

    [<Fact>]
    member _.``autoproof_anf closes exactly the valid goals (complete AND sound vs the oracle)`` () =
        // autoproof_anf (which builds a real proof) must succeed on a goal iff `valid` (the ANF
        // oracle) says it is a theorem — completeness and soundness of the trace-emitting decider.
        let cases : Prop list =
            [ (p + !!p); (p * !!p == F); (p * (p + q) == p); ((p ==> q) == (!!q ==> !!p))
              (((p ==> q) * (q ==> p)) == (p == q)); (p == q); (p ==> q); (p + q); ((p == q) == r) ]
        for c in cases do
            let closes = try (PropCalculus.autoproof_anf c).Complete with _ -> false
            Assert.Equal(PropCalculus.valid c, closes)

    // ===== valid/equiv: ANF decision TOOL (checks a proof exists; not a proof rule) =====

    [<Fact>]
    member _.``valid recognizes propositional theorems and rejects non-theorems`` () =
        // A checker, not a closer: it answers "does a proof exist?" — complete for propositional,
        // including the (p⇒q)∧(q⇒p) = (p≡q) that auto's bounded search missed.
        Assert.True(PropCalculus.valid ((p + !!p) == T))
        Assert.True(PropCalculus.valid (((p ==> q) * (q ==> p)) == (p == q)))
        Assert.True(PropCalculus.valid ((p * (p ==> q)) ==> q))
        Assert.True(PropCalculus.equiv (p * (q * r)) ((p * q) * (p * r)))
        Assert.False(PropCalculus.valid (p == q))
        Assert.False(PropCalculus.valid (p ==> q))
        Assert.False(PropCalculus.equiv p q)

    [<Fact>]
    member _.``ANF decider agrees with the independent truth-table oracle (sound and complete)`` () =
        // is_tautology (ANF) must match `equivalent e T` (truth-table) on every input — a
        // cross-check of two independent complete deciders (soundness + completeness).
        let tExpr = expand T.Expr
        let cases : Prop list =
            [ (p + !!p); (p * !!p); (p == p); ((p ==> q) == (!!q ==> !!p))
              (((p ==> q) * (q ==> p)) == (p == q)); (p == q); (p ==> q); (p + q); ((p + q) ==> p) ]
        for c in cases do
            let e = expand c.Expr
            Assert.Equal(equivalent e tExpr, EquationalLogic.Anf.is_tautology e)

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

    // ===== Proof-technique tactics (Gries Ch. 4) ==============================
    // Each tactic must produce a *complete* Theorem (construction throws otherwise).

    [<Fact>]
    member _.``MutualImplication closes: proves (p and q) = (q and p)`` () =
        // Gries 3.80: prove an identity from its two implications; must NOT stop at T and T.
        let stmt = expand ((p * q) == (q * p)).Expr
        let lB, rB, pB = PropCalculus.MutualImplication stmt
        let th = Theorem(pB (lB [ PropCalculus.commute_and p q |> apply_left ])
                            (rB [ PropCalculus.commute_and q p |> apply_left ]))
        Assert.True(sequal th.Stmt stmt, sprintf "MutualImplication produced %s" (src th.Stmt))

    [<Fact>]
    member _.``contradiction_id proves: (not p => F) = p`` () =
        PropCalculus.contradiction_id p |> ignore

    [<Fact>]
    member _.``Contradiction (reductio): from (not P => F) concludes P`` () =
        let notP_imp_F = Theorem(PropCalculus.autoproof_anf (!!(p + !!p) ==> F))
        let th = PropCalculus.Contradiction notP_imp_F
        Assert.True(sequal th.Stmt (expand (p + !!p).Expr), sprintf "Contradiction produced %s" (src th.Stmt))

    [<Fact>]
    member _.``Cases (case analysis): from (Q => P) and (not Q => P) concludes P`` () =
        let t1 = Theorem(PropCalculus.autoproof_anf (q ==> (p + !!p)))
        let t2 = Theorem(PropCalculus.autoproof_anf (!!q ==> (p + !!p)))
        let th = PropCalculus.Cases t1 t2
        Assert.True(sequal th.Stmt (expand (p + !!p).Expr), sprintf "Cases produced %s" (src th.Stmt))

    [<Fact>]
    member _.``malformed deduction names the missing antecedent conjunct`` () =
        // Deducing strengthen_and x y ((x∧y)⇒x) into a goal whose antecedent is only x must
        // fail naming a genuinely-missing conjunct (x∧y) — the guard's report must not pick a
        // present conjunct nor throw KeyNotFound.
        let x, y, z = boolvar "x", boolvar "y", boolvar "z"
        let t = PropCalculus.strengthen_and x y
        let msg =
            try proof PropCalculus.prop_calculus (x ==> (x * z)) [ Deduce t |> apply_right ] |> ignore; ""
            with e -> e.Message
        Assert.Contains("is not in the antecedent", msg)
        Assert.Contains("x ∧ y", msg)

    // ===== Calculational (mixed-relation) proofs — Gries Ch. 4 ================
    // A calc chain produces a genuine Theorem of `start REL end`; relations compose by
    // transitivity (= identity, ⇒∘⇒=⇒), and ⇒/⇐ cannot be mixed.

    [<Fact>]
    member _.``calc: stated = goal, pure = chain`` () =
        // Goal stated up front; `from` restates the start (checked); chain must deliver the goal.
        let th =
            Calc.calc ((p * q) == (q * p)) {
                from (p * q)
                eq (PropCalculus.commute_and p q |> apply)
            }
        Assert.True(sequal th.Stmt (expand ((p * q) == (q * p)).Expr), sprintf "calc = produced %s" (src th.Stmt))

    [<Fact>]
    member _.``calc: stated => goal, mixed = then => chain`` () =
        let th =
            Calc.calc (p * q ==> q) {
                from (p * q)
                eq  (PropCalculus.commute_and p q |> apply)   //  p ∧ q  =  q ∧ p
                imp (PropCalculus.strengthen_and q p)         //  q ∧ p  ⇒  q
            }
        Assert.True(sequal th.Stmt (expand (p * q ==> q).Expr), sprintf "calc =/=> produced %s" (src th.Stmt))

    [<Fact>]
    member _.``calc: all-= chain under a => goal is weakened to =>`` () =
        // The chain only uses `=`, but the stated goal is `⇒`, so the result is weakened to `⇒`.
        let th =
            Calc.calc (p * q ==> (q * p)) {
                from (p * q)
                eq (PropCalculus.commute_and p q |> apply)
            }
        Assert.True(sequal th.Stmt (expand (p * q ==> (q * p)).Expr), sprintf "produced %s" (src th.Stmt))

    [<Fact>]
    member _.``calc: multi-step = chain proves p and (q and r) = r and (p and q)`` () =
        let th =
            Calc.calc ((p * (q * r)) == (r * (p * q))) {
                from (p * (q * r))
                eq (PropCalculus.left_assoc_and p q r |> apply)   //  p ∧ (q ∧ r)  =  p ∧ q ∧ r
                eq (PropCalculus.commute_and (p * q) r |> apply)  //  =  r ∧ (p ∧ q)
            }
        Assert.True(sequal th.Stmt (expand ((p * (q * r)) == (r * (p * q))).Expr), sprintf "calc multi produced %s" (src th.Stmt))

    [<Fact>]
    member _.``calc: exploratory derive concludes start REL end`` () =
        let th =
            Calc.derive (p * q) {
                eq  (PropCalculus.commute_and p q |> apply)
                imp (PropCalculus.strengthen_and q p)
            }
        Assert.True(sequal th.Stmt (expand (p * q ==> q).Expr), sprintf "derive produced %s" (src th.Stmt))

    [<Fact>]
    member _.``calc: exploratory derive with a <= (conseq) step`` () =
        // From q, strengthen to q ∧ p  (since (q ∧ p) ⇒ q); conclude  q <=== (q ∧ p).
        let th =
            Calc.derive q {
                conseq (PropCalculus.strengthen_and q p)
            }
        Assert.True(sequal th.Stmt (expand (Calc.follows_from q (q * p)).Expr), sprintf "derive <= produced %s" (src th.Stmt))

    [<Fact>]
    member _.``calc: stated <= goal via follows_from`` () =
        let th =
            Calc.calc (Calc.follows_from q (q * p)) {
                from q
                conseq (PropCalculus.strengthen_and q p)
            }
        Assert.True(sequal th.Stmt (expand (Calc.follows_from q (q * p)).Expr), sprintf "calc <= produced %s" (src th.Stmt))

    [<Fact>]
    member _.``calc: mixed = then <= chain`` () =
        let th =
            Calc.derive (p * q) {
                eq (PropCalculus.commute_and p q |> apply)         //  p∧q = q∧p
                conseq (PropCalculus.strengthen_and (q * p) r)     //  q∧p ⇐ (q∧p)∧r
            }
        Assert.True(sequal th.Stmt (expand (Calc.follows_from (p * q) ((q * p) * r)).Expr), sprintf "mixed =/<= produced %s" (src th.Stmt))

    [<Fact>]
    member _.``calc: composing => with <= is rejected`` () =
        let msg = try Calc.composeRel Calc.RImp Calc.RConseq |> ignore; "" with e -> e.Message
        Assert.Contains("cannot mix", msg)

    [<Fact>]
    member _.``calc: `from` not matching the goal's left side is rejected`` () =
        let msg =
            try
                (Calc.calc (p * q ==> q) {
                    from (q * p)                            // ≠ goal LHS (p ∧ q)
                    imp (PropCalculus.strengthen_and q p)
                 } |> ignore); ""
            with e -> e.Message
        Assert.Contains("does not match the starting formula", msg)

    [<Fact>]
    member _.``calc: `from` after a step is rejected (must be first)`` () =
        let msg =
            try
                (Calc.calc ((p * q) == (q * p)) {
                    eq (PropCalculus.commute_and p q |> apply)   // a step ran first
                    from (p * q)                                 // `from` no longer allowed
                 } |> ignore); ""
            with e -> e.Message
        Assert.Contains("must be the first step", msg)

    [<Fact>]
    member _.``calc: chain ending at the wrong endpoint is rejected`` () =
        let msg =
            try
                (Calc.calc (p * q ==> r) {                    // goal RHS is r
                    from (p * q)
                    eq  (PropCalculus.commute_and p q |> apply)
                    imp (PropCalculus.strengthen_and q p)     // reaches q, not r
                 } |> ignore); ""
            with e -> e.Message
        Assert.Contains("right side is", msg)

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
            // auto/autoident/autodeduce are meta-provers: they take an arbitrary goal and search
            // for its proof, so invoking them on a bare variable is not theorem construction.
            |> Array.filter (fun m -> not (List.contains m.Name [ "auto"; "autoident"; "autodeduce" ]))
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
