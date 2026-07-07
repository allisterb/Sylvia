#load "Include.fsx"

// A Theory of Sets (Gries & Schneider, "A Logical Approach to Discrete Math", ch. 11).
//
// This script verifies the FOUNDATION the set theory is built on (steps 1-3 of the set-theory plan;
// see docs/prover-set-theory.md). What it checks:
//   (A) a subclass's injected axioms compose with (are not discarded by) the Boolean-algebra axioms;
//   (B) the inherited Boolean-algebra axioms are still recognized after that composition;
//   (C) the complement law (excluded middle / contradiction) is recognized with the correct polarity;
//   (D) predicate calculus is available as the ambient logical base under `set_theory`;
//   (E) Set Membership (11.3) and Extensionality (11.4) are live, recognized axioms.
//
// Run:  dotnet fsi examples/proofs/SetTheory.fsx

open Sylvia
open FSharp.Quotations
open Formula
open PropCalculus
open PredCalculus
open SetAlgebra

fsi.PrintWidth <- 300
Proof.LogLevel <- 0

let mutable failures = 0
let ok label cond =
    if not cond then failures <- failures + 1
    printfn "  %s  %s" (if cond then "✓" else "✗") label

// Free set variables S, T : Set<int>, as typed expressions for splicing into the theory's own
// operator quotations (Set.set_union / set_intersection / ~-) — the same symbols the theory keys on.
let uS : Expr<Set<int>> = Expr.Var(Var("S", typeof<Set<int>>)) |> Expr.Cast
let uT : Expr<Set<int>> = Expr.Var(Var("T", typeof<Set<int>>)) |> Expr.Cast

let sa = SetAlgebra.set_algebra<int>

printfn "\n===== (B) Inherited Boolean-algebra axioms recognized after composition ====="
ok "Idempotency      S ∪ S = S"              (sa.AxEquiv <@ Set.set_union %uS %uS = %uS @>)
ok "Symmetry         S ∩ T = T ∩ S"          (sa.AxEquiv <@ Set.set_intersection %uS %uT = Set.set_intersection %uT %uS @>)
ok "Identity of ∪     S ∪ ∅ = S"              (sa.AxEquiv <@ Set.set_union %uS Set.Empty = %uS @>)

printfn "\n===== (C) Complement law recognized with correct polarity (Gries 11.32/11.39) ====="
ok "Excluded middle  S ∪ ~S = U  recognized"    (sa.AxEquiv <@ Set.set_union %uS (Set.(~-) %uS) = Set.U @>)
ok "Contradiction    S ∩ ~S = ∅  recognized"    (sa.AxEquiv <@ Set.set_intersection %uS (Set.(~-) %uS) = Set.Empty @>)
ok "S ∪ ~S = ∅  rejected (was wrongly accepted)" (not (sa.AxEquiv <@ Set.set_union %uS (Set.(~-) %uS) = Set.Empty @>))
ok "S ∩ ~S = U  rejected (was wrongly accepted)" (not (sa.AxEquiv <@ Set.set_intersection %uS (Set.(~-) %uS) = Set.U @>))

printfn "\n===== (A) Injected axioms compose through the theory chain (previously dropped) ====="
let marker = <@ Set.set_union %uS %uT = Set.U @>    // not a Boolean-algebra axiom on its own
let extra : Axioms = fun e -> if sequal e (expand marker) then Descriptions.axiom_name "Marker" "Marker" |> Some else None
let sa2 = SetAlgebra.SetAlgebra<int>(axioms = extra)
ok "injected marker axiom recognized in sa2"     (sa2.AxEquiv marker)
ok "base axiom still recognized in sa2"          (sa2.AxEquiv <@ Set.set_union %uS %uS = %uS @>)
ok "marker NOT recognized in plain set_algebra"  (not (sa.AxEquiv marker))

printfn "\n===== (D) Predicate-calculus base available under set_theory ====="
let P = boolvar "P"
let st = SetTheory.set_theory<int>
ok "prop tautology P ⇒ P proves under set_theory" ((theorem st (P ==> P) []).Proof.Complete)

printfn "\n===== (E) Set Membership (11.3) and Extensionality (11.4) are live axioms ====="
let x = intvar "x"
let e = intvar "e"
let R = intpred "R"
let sS = setvar<int> "S"
let sT = setvar<int> "T"

// Membership (11.3), traditional-body form:  e ∈ {x | R x : x} = (∃x | R x : e = x)
ok "Membership (11.3) recognized"
    (st.AxEquiv <@ (%e.Expr |?| set_comp %x.Expr %(R.[x].Expr) %x.Expr) = exists_expr %x.Expr %(R.[x].Expr) (%e.Expr = %x.Expr) @>)
ok "Membership non-instance rejected"
    (not (st.AxEquiv <@ (%e.Expr |?| set_comp %x.Expr %(R.[x].Expr) %x.Expr) = exists_expr %x.Expr %(R.[x].Expr) (%e.Expr = %e.Expr) @>))

// Extensionality (11.4):  S = T = (∀x |: x∈S = x∈T)   (S, T set variables)
ok "Extensionality (11.4) recognized"
    (st.AxEquiv <@ (%sS.Expr = %sT.Expr) = forall_expr %x.Expr %T.Expr ((%((x |?| sS).Expr):bool) = %((x |?| sT).Expr)) @>)
ok "Extensionality with non-true range rejected"
    (not (st.AxEquiv <@ (%sS.Expr = %sT.Expr) = forall_expr %x.Expr %(R.[x].Expr) ((%((x |?| sS).Expr):bool) = %((x |?| sT).Expr)) @>))

printfn "\n===== (F) A worked cross-layer theorem: Gries 11.7   e ∈ {x | R} = R e ====="
// {x | R} = {x | R : x}; membership (11.3) reduces ∈ to ∃, then predicate calculus (Trading 9.19,
// One-Point 8.14) collapses the ∃. Exercises BOTH foundations in one proof.
let proven (f: unit -> 'a) = try f () |> ignore; true with _ -> false
let e_in    = <@ %e.Expr |?| set_comp %x.Expr %(R.[x].Expr) %x.Expr @>            // e ∈ {x | R x : x}
let memRule = id_ax st (Prop <@ %e_in = exists_expr %x.Expr %(R.[x].Expr) (%e.Expr = %x.Expr) @>)
let bodyEq  = Pred<int>(func = <@ fun (z:int) -> %e.Expr = z @>)                   // fun z -> e = z
ok "11.7  e ∈ {x|R} = R e  proven" (proven (fun () ->
    ident st (Prop <@ %e_in = %(R.[e].Expr) @>) [
        memRule |> at_left                                          // → (∃x | R x : e = x)
        trade_exists_and x R bodyEq |> at_left                      // → (∃x |: R x ∧ (e = x))
        commute_and (Prop <@ %(R.[x].Expr) @>) (Prop <@ %e.Expr = %x.Expr @>) |> at [left_branch; select_body]
        trade_exists_and x bodyEq R |> Commute |> at_left           // → (∃x | e = x : R x)
    ]))                                                             // One-Point closes: (∃x|e=x:R x) = R e

printfn "\n===== (G) A worked cross-layer theorem: Gries 11.5   S = {x | x∈S : x} ====="
// Uses Extensionality (11.4) to reduce set equality to (∀y|: y∈S = y∈{..}); the inner membership
// (which is 11.7 instantiated with R := (·∈S)) collapses y∈{x|x∈S:x} to y∈S; then reflexivity and
// (∀y|:true)=true close it. Exercises extensionality, membership, trading and One-Point together.
let y = intvar "y"
let xinS = (x |?| sS).Expr
let comp = <@ set_comp %x.Expr %xinS %x.Expr @>            // {x | x∈S : x}
let yinS = (y |?| sS).Expr
let yinC = <@ %y.Expr |?| %comp @>                         // y ∈ {x | x∈S : x}
let Rmem = Pred<int>(func = <@ fun (z:int) -> z |?| %sS.Expr @>)   // (·∈S) as a predicate
let bodyEqY = Pred<int>(func = <@ fun (z:int) -> %y.Expr = z @>)
// inner lemma = 11.7 at e:=y, R:=(·∈S):  y ∈ {x|x∈S:x} = y∈S
let memRuleY = id_ax st (Prop <@ %yinC = exists_expr %x.Expr %xinS (%y.Expr = %x.Expr) @>)
let inner117 =
    ident st (Prop <@ %yinC = %yinS @>) [
        memRuleY |> at_left
        trade_exists_and x Rmem bodyEqY |> at_left
        commute_and (Prop <@ %xinS @>) (Prop <@ %y.Expr = %x.Expr @>) |> at [left_branch; select_body]
        trade_exists_and x bodyEqY Rmem |> Commute |> at_left
    ]
let extRule = id_ax st (Prop <@ (%sS.Expr = %comp) = forall_expr %y.Expr %T.Expr ((%yinS:bool) = %yinC) @>)
ok "11.5  S = {x | x∈S : x}  proven" (proven (fun () ->
    ident st (Prop <@ %sS.Expr = %comp @>) [
        extRule                                                    // → (∀y|: y∈S = y∈{x|x∈S:x})
        inner117 |> at [select_body; right_branch]                 // y∈{..} → y∈S
        def_true (Prop <@ %yinS @>) |> Commute |> at [select_body] // (y∈S = y∈S) → true
        ident_forall_true' y                                       // (∀y|: true) → true
    ]))

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
