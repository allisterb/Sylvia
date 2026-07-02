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

    // ===== sequal: T/F named constants unify with bare true/false =============

    [<Fact>]
    member _.``sequal unifies named T with bare true and F with false`` () =
        Assert.True(sequal T.Expr <@@ true @@>, "T should equal bare true")
        Assert.True(sequal F.Expr <@@ false @@>, "F should equal bare false")
        // nested occurrence (the shape that broke contr)
        Assert.True(sequal (F == T).Expr (F == (Prop <@ true @>)).Expr)
        // and it must NOT unify true with false
        Assert.False(sequal T.Expr <@@ false @@>, "true must not equal false")

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
