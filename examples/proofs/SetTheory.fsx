#load "Include.fsx"
#r "../../src/lang/solvers/Sylvia.Solver.CaDiCaL/bin/Debug/net10.0/Sylvia.Solver.CaDiCaL.dll"
#r "../../src/lang/core/Sylvia.Prover.SAT/bin/Debug/net10.0/Sylvia.Prover.SAT.dll"

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

// The metatheorem tactics in sections K-M discharge a PROPOSITIONAL body (Definition 11.24) with one
// atom per distinct SET VARIABLE, so which propositional prover closes that body is what bounds how
// many variables a set identity may mention. They used to call `autoproof_anf` directly, which is
// exponential in atom count and guarded at `autoproof_max_atoms` = 5 — a ceiling the rest of the
// prover no longer has. They now go through `PropCalculus.decide`, which routes small bodies to the
// same in-kernel ANF prover and larger ones to the SAT-refutation backend when one is installed.
//
// Installing is OPTIONAL and the reroute is non-regressive: with no solver, `decide` falls back to
// `autoproof_anf` and sections K-M prove exactly what they proved before (section N then reports
// SKIPPED rather than failing). Nothing here widens the trusted base — `decide` re-checks that the
// backend returned a theorem of the goal it asked about, and either route yields a real derivation.
let cadicalPath =
    match System.Environment.GetEnvironmentVariable "SYLVIA_CADICAL" with
    | null | "" -> @"C:\Projects\Sylvia\bin\cadical.exe"
    | p -> p
let satBackend =
    if System.IO.File.Exists cadicalPath then
        SatProof.install_with (SAT.Cadical(exePath = cadicalPath, timeoutMs = 60000))
        true
    else false
printfn "\ndecide backend: %s"
    (if satBackend then sprintf "SAT refutation via %s (set identities have no variable ceiling)" cadicalPath
     else sprintf "none — %s not found; bodies fall back to autoproof_anf (<= %d set variables)" cadicalPath autoproof_max_atoms)

// Symbolic set variables S, T : Set<int>. Set operations on the symbolic `SetTerm` type are the
// arithmetic operators — ∪ = `+`, ∩ = `*`, − = `-`, ~ = unary `-` — mirroring `Prop`'s `+` = ∨ and
// `*` = ∧, which is Definition 11.24 made syntactic. They still build the SAME `Set<'t>` methods the
// theory keys on for both the algebra laws and the membership axioms, so one expression is usable by
// both routes. Subset is still `|<|` (it is not an algebraic operation but a proposition).
// `sS`/`sT` avoid clashing with the truth constant `T`.
let sS = setvar<int> "S"
let sT = setvar<int> "T"
// ~s. `SetTerm.(~-)` declares its return type, so `-s` is usable directly (and is what the theory's
// own laws are written with); `neg` survives here only because it reads as the `~` of the text.
let neg (s: SetTerm<int>) = -s

let sa = SetAlgebra.set_algebra<int>

printfn "\n===== (B) Inherited Boolean-algebra axioms recognized after composition ====="
ok "Idempotency      S ∪ S = S"              (sa.AxEquiv ((sS + sS) == sS).Expr)
ok "Symmetry         S ∩ T = T ∩ S"          (sa.AxEquiv ((sS * sT) == (sT * sS)).Expr)
// NB `|+|`, not `+`: inside a quotation the operands are `Set<int>` VALUES, and the arithmetic
// operators are the SYMBOLIC (`SetTerm`) spelling. `Set<'t>` keeps `|+|`/`|*|` — it has to, since it
// already spends `(*)` on the Cartesian product, and its method names are what the axioms key on.
ok "Identity of ∪     S ∪ ∅ = S"              (sa.AxEquiv <@ (%sS.Expr |+| Set.Empty) = %sS.Expr @>)

printfn "\n===== (C) Complement law recognized with correct polarity (Gries 11.32/11.39) ====="
ok "Excluded middle  S ∪ ~S = U  recognized"    (sa.AxEquiv <@ %((sS + (neg sS)).Expr) = Set.U @>)
ok "Contradiction    S ∩ ~S = ∅  recognized"    (sa.AxEquiv <@ %((sS * (neg sS)).Expr) = Set.Empty @>)
ok "S ∪ ~S = ∅  rejected (was wrongly accepted)" (not (sa.AxEquiv <@ %((sS + (neg sS)).Expr) = Set.Empty @>))
ok "S ∩ ~S = U  rejected (was wrongly accepted)" (not (sa.AxEquiv <@ %((sS * (neg sS)).Expr) = Set.U @>))

printfn "\n===== (A) Injected axioms compose through the theory chain (previously dropped) ====="
let marker = <@ %((sS + sT).Expr) = Set.U @>    // not a Boolean-algebra axiom on its own
let extra : Axioms = fun e -> if sequal e (expand marker) then Descriptions.axiom_name "Marker" "Marker" |> Some else None
let sa2 = SetAlgebra.SetAlgebra<int>(axioms = extra)
ok "injected marker axiom recognized in sa2"     (sa2.AxEquiv marker)
ok "base axiom still recognized in sa2"          (sa2.AxEquiv ((sS + sS) == sS).Expr)
ok "marker NOT recognized in plain set_algebra"  (not (sa.AxEquiv marker))

printfn "\n===== (D) Predicate-calculus base available under set_theory ====="
let P = boolvar "P"
let st = SetTheory.set_theory<int>
ok "prop tautology P ⇒ P proves under set_theory" ((theorem st (P ==> P) []).Proof.Complete)

printfn "\n===== (E) Set Membership (11.3) and Extensionality (11.4) are live axioms ====="
let x = intvar "x"
let e = intvar "e"
let R = intpred "R"

// Membership (11.3), traditional-body form:  e ∈ {x | R x : x} = (∃x | R x : e = x)
ok "Membership (11.3) recognized"
    (st.AxEquiv <@ (%e.Expr |?| set_comp %x.Expr %(R.[x].Expr) %x.Expr) = exists_expr %x.Expr %(R.[x].Expr) (%e.Expr = %x.Expr) @>)
ok "Membership non-instance rejected"
    (not (st.AxEquiv <@ (%e.Expr |?| set_comp %x.Expr %(R.[x].Expr) %x.Expr) = exists_expr %x.Expr %(R.[x].Expr) (%e.Expr = %e.Expr) @>))

// Extensionality (11.4):  S = T = (∀x |: x∈S = x∈T)   (S, T set variables)
ok "Extensionality (11.4) recognized"
    (st.AxEquiv ((sS == sT) == qall x T ((x |?| sS) == (x |?| sT))).Expr)
ok "Extensionality with non-true range rejected"
    (not (st.AxEquiv <@ (%sS.Expr = %sT.Expr) = forall_expr %x.Expr %(R.[x].Expr) ((%((x |?| sS).Expr):bool) = %((x |?| sT).Expr)) @>))

printfn "\n===== (F) A worked cross-layer theorem: Gries 11.7   e ∈ {x | R} = R e ====="
// {x | R} = {x | R : x}; membership (11.3) reduces ∈ to ∃, then predicate calculus (Trading 9.19,
// One-Point 8.14) collapses the ∃. Exercises BOTH foundations in one proof.
let proven (f: unit -> 'a) = try f () |> ignore; true with _ -> false
let e_in    = <@ %e.Expr |?| set_comp %x.Expr %(R.[x].Expr) %x.Expr @>            // e ∈ {x | R x : x}
let memRule = ax_ident st (Prop <@ %e_in = exists_expr %x.Expr %(R.[x].Expr) (%e.Expr = %x.Expr) @>)
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
let memRuleY = ax_ident st (Prop <@ %yinC = exists_expr %x.Expr %xinS (%y.Expr = %x.Expr) @>)
let inner117 =
    ident st (Prop <@ %yinC = %yinS @>) [
        memRuleY |> at_left
        trade_exists_and x Rmem bodyEqY |> at_left
        commute_and (Prop <@ %xinS @>) (Prop <@ %y.Expr = %x.Expr @>) |> at [left_branch; select_body]
        trade_exists_and x bodyEqY Rmem |> Commute |> at_left
    ]
let extRule = ax_ident st (Prop <@ (%sS.Expr = %comp) = forall_expr %y.Expr %T.Expr ((%yinS:bool) = %yinC) @>)
ok "11.5  S = {x | x∈S : x}  proven" (proven (fun () ->
    ident st (Prop <@ %sS.Expr = %comp @>) [
        extRule                                                    // → (∀y|: y∈S = y∈{x|x∈S:x})
        inner117 |> at [select_body; right_branch]                 // y∈{..} → y∈S
        def_true (Prop <@ %yinS @>) |> Commute |> at [select_body] // (y∈S = y∈S) → true
        ident_forall_true' y                                       // (∀y|: true) → true
    ]))

printfn "\n===== (H) Operator membership-reduction axioms (Gries 11.13-11.21) ====="
let v = intvar "v"
let vinS = v |?| sS      // membership is now a Prop directly (SetTerm's `|?|` returns Prop)
let vinT = v |?| sT
// The SAME `+`/`*` operator expressions match both the membership axioms here AND the
// Boolean-algebra laws (checks B/C); subset `|<|` is now a proposition.
ok "11.20 Union       v∈S∪T = v∈S ∨ v∈T"
    (st.AxEquiv ((v |?| (sS + sT)) == (vinS + vinT)).Expr)
ok "11.21 Intersection v∈S∩T = v∈S ∧ v∈T"
    (st.AxEquiv ((v |?| (sS * sT)) == (vinS * vinT)).Expr)
ok "11.18 Complement  v∈~S = ¬(v∈S)"
    (st.AxEquiv ((v |?| (neg sS)) == (!! vinS)).Expr)
ok "11.13 Subset      S⊆T = (∀x|x∈S:x∈T)"
    (st.AxEquiv ((sS |<| sT) == qall x (x |?| sS) (x |?| sT)).Expr)
// coherence: the SAME + expression is also recognized by the Boolean-algebra layer
ok "coherence: S∪T (+) matches algebra idempotency S∪S=S"
    ((SetAlgebra.set_algebra<int>).AxEquiv ((sS + sS) == sS).Expr)

printfn "\n===== (I) A worked set-algebra law via the membership route: Gries 11.28  S ∪ S = S ====="
// Extensionality reduces S∪S=S to (∀v|: v∈(S∪S) = v∈S); the Union axiom (11.20) unfolds v∈(S∪S) to
// v∈S ∨ v∈S; ∨-idempotency collapses it; reflexivity and (∀v|:true)=true close it.
let SuS   = sS + sS
let extU  = ax_ident st ((SuS == sS) == qall v T ((v |?| SuS) == (v |?| sS)))
let unionU = ax_ident st ((v |?| SuS) == (vinS + vinS))
ok "11.28  S ∪ S = S  proven" (proven (fun () ->
    ident st (SuS == sS) [
        extU                                               // → (∀v|: v∈(S∪S) = v∈S)
        unionU |> at [select_body; left_branch]            // v∈(S∪S) → v∈S ∨ v∈S
        idemp_or vinS |> at [select_body; left_branch]      // v∈S ∨ v∈S → v∈S
        def_true vinS |> Commute |> at [select_body]        // (v∈S = v∈S) → true
        ident_forall_true' v                                // (∀v|: true) → true
    ]))

printfn "\n===== (J) De Morgan via the membership route: Gries 11.42a  ~(S∪T) = ~S ∩ ~T ====="
// Extensionality; then each membership is reduced by the operator axioms (complement, union,
// intersection); the propositional De Morgan (¬(p∨q) = ¬p∧¬q) equates the two sides; close as usual.
let nS  : SetTerm<int> = neg sS
let nsT : SetTerm<int> = neg sT
let SuT      : SetTerm<int> = sS + sT
let negSuT   : SetTerm<int> = neg SuT              // ~(S ∪ T)
let nSinT    : SetTerm<int> = nS * nsT           // ~S ∩ ~T
let memv (t:SetTerm<int>) = v |?| t
let compUnion = ax_ident st ((memv negSuT) == (!! (memv SuT)))       // v∈~(S∪T) = ¬(v∈(S∪T))
let unionR    = ax_ident st ((memv SuT)    == (vinS + vinT))         // v∈(S∪T)  = v∈S ∨ v∈T
let interR    = ax_ident st ((memv nSinT)  == ((memv nS) * (memv nsT)))  // v∈(~S∩~T) = v∈~S ∧ v∈~T
let compS     = ax_ident st ((memv nS)     == (!! vinS))             // v∈~S = ¬(v∈S)
let compT     = ax_ident st ((memv nsT)    == (!! vinT))             // v∈~T = ¬(v∈T)
let extDM = ax_ident st ((negSuT == nSinT) == qall v T ((v |?| negSuT) == (v |?| nSinT)))
ok "11.42a  ~(S∪T) = ~S ∩ ~T  proven" (proven (fun () ->
    ident st (negSuT == nSinT) [
        extDM                                                      // (∀v|: v∈~(S∪T) = v∈(~S∩~T))
        compUnion |> at [select_body; left_branch]                 // v∈~(S∪T) → ¬(v∈(S∪T))
        unionR    |> at [select_body; left_branch; apply_unary]    // v∈(S∪T) → v∈S ∨ v∈T
        interR    |> at [select_body; right_branch]                // v∈(~S∩~T) → v∈~S ∧ v∈~T
        compS     |> at [select_body; right_branch; left_branch]   // v∈~S → ¬(v∈S)
        compT     |> at [select_body; right_branch; right_branch]  // v∈~T → ¬(v∈T)
        distrib_not_or vinS vinT |> at [select_body; left_branch]  // ¬(v∈S ∨ v∈T) → ¬(v∈S) ∧ ¬(v∈T)
        def_true ((!! vinS) * (!! vinT)) |> Commute |> at [select_body]   // (X = X) → true
        ident_forall_true' v                                       // (∀v|: true) → true
    ]))

printfn "\n===== (K) Metatheorem 11.25(a): `SetTheory.meta_set_ident` mechanizes the set-algebra laws ====="
// Gries' Metatheorem (11.25a) says a set identity  Es = Fs  is valid iff its propositional
// translation  Ep = Fp  (Definition 11.24: ∅↦false, U↦true, ~↦¬, ∪↦∨, ∩↦∧, set variable S ↦ its
// membership proposition v∈S) is valid. Rather than adding it as a new trusted primitive, it is
// MECHANIZED as the membership-route proof used by hand for 11.28 / De Morgan (sections I/J): apply
// Extensionality; recursively unfold every membership through the operator axioms (which literally
// implement 11.24); discharge the resulting propositional body  Ep = Fp  with the COMPLETE decider
// `decide`; and close with `(∀v|:true) = true`. The result is a genuine, kernel-checked
// Theorem built only from existing recognized axioms — no new trusted rule. Because `decide`
// is complete for (and only for) propositional tautologies on both of its routes, the tactic proves
// exactly the valid set identities over {∪, ∩, ~, variables} and REJECTS invalid ones — an invalid
// body is refused by the ANF prover and comes back SAT (hence raises) from the solver route.
//
// This all now lives in the THEORY (`src/math/Sylvia.AbstractAlgebra/Theories/SetTheory.fs`), not in
// this script: `translate` (Definition 11.24), `unfold` (the membership-reduction recursion),
// `meta_set_ident`, `meta_subset` and `powerset_member`, plus the named laws of §11.3 — and generic over the
// element type rather than pinned to `int` as the script-local versions were. What follows exercises
// the library versions; the script keeps only the checking harness.
open SetTheory

let sU = setvar<int> "U"
let emptyT = empty_set<int>   // ∅ as a structured SetTerm (kept out of a value embedding)
let uT     = universe<int>    // U, the universe
// A rejected identity and a BROKEN tactic both surface as `false` here, which is exactly what makes
// the soundness checks below meaningful — and exactly what hides a bug in the discharge route. Set
// SYLVIA_DEBUG=1 to see why each `false` happened.
let private why (what: string) (e: exn) =
    if System.Environment.GetEnvironmentVariable "SYLVIA_DEBUG" = "1" then
        printfn "      %s refused: %s" what (e.Message.Split('\n').[0])
    false
let proves (what: string) (f: unit -> Theorem) = try (f ()).Proof.Complete with e -> why what e
let metaproven (l: SetTerm<int>) (r: SetTerm<int>) = proves "meta_set_ident" (fun () -> meta_set_ident l r)

// The named Gries laws 11.26–11.42 — each proved with a single `meta_set_ident` call.
ok "11.26 Symmetry of ∪        S∪T = T∪S"              (metaproven (sS + sT) (sT + sS))
ok "11.27 Associativity of ∪   (S∪T)∪U = S∪(T∪U)"      (metaproven ((sS + sT) + sU) (sS + (sT + sU)))
ok "11.28 Idempotency of ∪     S∪S = S"                (metaproven (sS + sS) sS)
ok "11.36 Symmetry of ∩        S∩T = T∩S"              (metaproven (sS * sT) (sT * sS))
ok "11.40 Distributivity ∩/∪   S∩(T∪U) = (S∩T)∪(S∩U)"  (metaproven (sS * (sT + sU)) ((sS * sT) + (sS * sU)))
ok "11.41 Distributivity ∪/∩   S∪(T∩U) = (S∪T)∩(S∪U)"  (metaproven (sS + (sT * sU)) ((sS + sT) * (sS + sU)))
ok "11.42a De Morgan          ~(S∪T) = ~S∩~T"          (metaproven (neg (sS + sT)) ((neg sS) * (neg sT)))
ok "11.42b De Morgan          ~(S∩T) = ~S∪~T"          (metaproven (neg (sS * sT)) ((neg sS) + (neg sT)))
ok "Absorption                S∩(S∪T) = S"             (metaproven (sS * (sS + sT)) sS)
ok "Double complement 11.19   ~~S = S"                 (metaproven (neg (neg sS)) sS)
// Soundness: the tactic must REJECT invalid identities (the complete ANF prover refuses non-tautologies).
ok "INVALID S∪T = S∩T  rejected"                       (not (metaproven (sS + sT) (sS * sT)))
ok "INVALID ~(S∪T) = ~S∪~T  rejected"                  (not (metaproven (neg (sS + sT)) ((neg sS) + (neg sT))))

printfn "\n===== (L) Metatheorem 11.25(b): subset via implication  Es ⊆ Fs ↔ Ep ⇒ Fp ====="
// Gries (11.56) — one set is a subset of another iff its characteristic predicate IMPLIES the other's
// — is exactly Metatheorem 11.25(b). We mechanize it like (a), but the goal `Es ⊆ Fs` is a bare
// proposition (not an equality), so we reduce it to `true`: apply Subset (11.13) to get
// `(∀v | v∈Es : v∈Fs)`; TRADE (9.2) to `(∀v |: v∈Es ⇒ v∈Fs)` (using the simple membership predicates,
// so no recursion is needed for the trade); unfold each side of the implication with the theory's
// `unfold` lemmas to reach the body `Ep ⇒ Fp`; discharge that tautology with `decide` folded
// via `Taut` (a proven proposition → true); close with `(∀v|:true) = true`.

let subproven (l: SetTerm<int>) (r: SetTerm<int>) = proves "meta_subset" (fun () -> meta_subset l r)

ok "11.58 Reflexivity          S ⊆ S"                  (subproven sS sS)
ok "∩ lower bound              S∩T ⊆ S"                (subproven (sS * sT) sS)
ok "∩ lower bound              S∩T ⊆ T"                (subproven (sS * sT) sT)
ok "∪ upper bound              S ⊆ S∪T"                (subproven sS (sS + sT))
ok "∪ upper bound              T ⊆ S∪T"                (subproven sT (sS + sT))
ok "monotone                   S∩T ⊆ S∪T"             (subproven (sS * sT) (sS + sT))
// Soundness: a non-subset must be REJECTED (the implication Ep ⇒ Fp is not a tautology).
ok "INVALID S ⊆ S∩T  rejected"                        (not (subproven sS (sS * sT)))
ok "INVALID S∪T ⊆ S  rejected"                        (not (subproven (sS + sT) sS))

printfn "\n===== (M) ∅ / U membership atoms: the identity, zero and complement laws ====="
// With the constant-membership axioms  v∈∅ = false  and  v∈U = true  (added to SetTheory.fs), the
// `meta_set_ident` tactic now also covers every Gries law that mentions ∅ or U. Metatheorem 11.25(c)
// (`Es = U` valid iff Ep valid) needs no separate tactic — it is just `meta_set_ident Es U`, whose body
// reduces to `Ep = true`.
ok "v∈∅ = false  recognized (Empty axiom)"            (st.AxEquiv ((v |?| emptyT) == F).Expr)
ok "v∈U = true   recognized (Universe axiom)"         (st.AxEquiv ((v |?| uT) == T).Expr)
ok "11.30 Identity of ∪       S∪∅ = S"                (metaproven (sS + emptyT) sS)
ok "11.34 Identity of ∩       S∩U = S"                (metaproven (sS * uT) sS)
ok "11.29 Zero of ∪           S∪U = U"                (metaproven (sS + uT) uT)
ok "11.35 Zero of ∩           S∩∅ = ∅"                (metaproven (sS * emptyT) emptyT)
ok "11.32 Excluded middle     S∪~S = U"               (metaproven (sS + (neg sS)) uT)
ok "11.39 Contradiction       S∩~S = ∅"               (metaproven (sS * (neg sS)) emptyT)
ok "11.25(c) via Es=U         (S∪~S)∪∅ = U"           (metaproven ((sS + (neg sS)) + emptyT) uT)
// Soundness with the constants:
ok "INVALID S∪∅ = U  rejected"                        (not (metaproven (sS + emptyT) uT))
ok "INVALID S∩U = ∅  rejected"                        (not (metaproven (sS * uT) emptyT))

printfn "\n===== (N) Past the 5-variable ceiling: metatheorem bodies discharged by SAT refutation ====="
// Sections K-M all mention at most 3 set variables, so their propositional bodies route to the
// in-kernel ANF prover exactly as they always did. This section is what the `decide` reroute BUYS:
// identities over more set variables than `autoproof_max_atoms` allows. The body of a 6-variable
// identity has 6 atoms, which `autoproof_anf` refuses outright (and would not survive if the guard
// were raised — it is exponential in exactly that number). Routed to the SAT backend, the body is
// refuted by CaDiCaL and the refutation is REPLAYED as kernel steps, so the resulting set-algebra
// Theorem is checked to the same standard as the hand proofs in sections I/J.
let sW, sX, sY = setvar<int> "W", setvar<int> "X", setvar<int> "Y"

/// `ok`, reporting how long the identity took to prove (the body's SAT reconstruction dominates).
let okt label (f: unit -> bool) =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let r = f ()
    sw.Stop()
    ok (sprintf "%-46s (%dms)" label sw.ElapsedMilliseconds) r

let union6  = ((((((sS + sT) + sU) + sW) + sX) + sY))
let inter6  = ((((((sS * sT) * sU) * sW) * sX) * sY))
let compl6i = ((((((neg sS) * (neg sT)) * (neg sU)) * (neg sW)) * (neg sX)) * (neg sY))
let compl6u = ((((((neg sS) + (neg sT)) + (neg sU)) + (neg sW)) + (neg sX)) + (neg sY))

if not satBackend then
    printfn "  -  SKIPPED (no solver: %s not found)" cadicalPath
else
    // 6 set variables — one more than `autoproof_max_atoms`.
    okt "11.42a De Morgan, 6 vars  ~(S∪…∪Y) = ~S∩…∩~Y"
        (fun () -> metaproven (neg union6) compl6i)
    okt "11.42b De Morgan, 6 vars  ~(S∩…∩Y) = ~S∪…∪~Y"
        (fun () -> metaproven (neg inter6) compl6u)
    // An ∪/∩ shuffle: same six variables, every bracket and every order changed.
    okt "assoc+symm shuffle, 6 vars"
        (fun () -> metaproven union6 (sY + (sX + (sW + (sU + (sT + sS))))))
    // Distributivity fanned out over five disjuncts (6 vars, and a much wider CNF).
    okt "11.40 Distributivity ∩/∪, 6 vars"
        (fun () -> metaproven (sS * ((((sT + sU) + sW) + sX) + sY))
                              ((((((sS * sT) + (sS * sU)) + (sS * sW)) + (sS * sX)) + (sS * sY))))
    // With the constants ∅ / U in the mix, so the body carries T/F as well as six atoms.
    okt "11.30/11.35 with constants, 6 vars  (S∪…∪Y)∩U∪∅"
        (fun () -> metaproven (((union6 * uT) + emptyT)) union6)
    // Metatheorem 11.25(b) past the ceiling too — a 6-variable subset obligation.
    okt "11.25(b) subset, 6 vars   S∩…∩Y ⊆ S∪…∪Y"
        (fun () -> subproven inter6 union6)
    // Soundness must survive the reroute: an INVALID 6-variable identity is refused, not proved.
    // (The solver returns SAT, `SatProof` raises, and `metaproven` reports false.)
    okt "INVALID ~(S∪…∪Y) = ~S∪…∪~Y  rejected"
        (fun () -> not (metaproven (neg union6) compl6u))
    okt "INVALID S∩…∩Y = S∪…∪Y  rejected"
        (fun () -> not (metaproven inter6 union6))

    // The ceiling being lifted is a fact about the ROUTE, not about the tactic: uninstall the backend
    // and the identical call fails, because `decide` falls back to the atom-capped ANF prover. This
    // check is what proves the section above is exercising the SAT route and not something cheaper.
    SatProof.uninstall ()
    ok "same identity FAILS with no backend (the old 5-variable ceiling)"
       (not (metaproven (neg union6) compl6i))
    SatProof.install_with (SAT.Cadical(exePath = cadicalPath, timeoutMs = 60000))
    ok "and proves again once the backend is reinstalled"
       (metaproven (neg union6) compl6i)

printfn "\n===== (O) Difference (Gries 11.22):  v ∈ S−T = v∈S ∧ v∉T ====="
// The `Difference` axiom is now live in SetTheory.fs, and `translate`/`unfold` above carry `−` into
// the propositional body as `∧¬`. Difference is NOT one of Definition 11.24's operators, so this is
// an extension of the mechanized metatheorem rather than an instance of it — sound because `−` is
// definable from `∩` and `~`. The first check below is the one that earns the extension: it proves
// the defining identity `S − T = S ∩ ~T` through the very translation being justified, so if the
// `SDiff` case disagreed with 11.22 this would fail rather than quietly prove the wrong thing.
ok "11.22 Difference axiom recognized"
   (st.AxEquiv ((v |?| (sS - sT)) == ((v |?| sS) * !!(v |?| sT))).Expr)
ok "11.22 wrong polarity rejected"
   (not (st.AxEquiv ((v |?| (sS - sT)) == ((v |?| sS) * (v |?| sT))).Expr))

ok "defining identity        S−T = S∩~T"               (metaproven (sS - sT) (sS * (neg sT)))
ok "Gries p.203              ~S = U−S"                 (metaproven (neg sS) (uT - sS))
ok "self-difference          S−S = ∅"                  (metaproven (sS - sS) emptyT)
ok "identity                 S−∅ = S"                  (metaproven (sS - emptyT) sS)
ok "zero                     ∅−S = ∅"                  (metaproven (emptyT - sS) emptyT)
ok "difference from U        U−S = ~S"                 (metaproven (uT - sS) (neg sS))
ok "De Morgan over ∪         S−(T∪U) = (S−T)∩(S−U)"    (metaproven (sS - (sT + sU)) ((sS - sT) * (sS - sU)))
ok "De Morgan over ∩         S−(T∩U) = (S−T)∪(S−U)"    (metaproven (sS - (sT * sU)) ((sS - sT) + (sS - sU)))
ok "∪ distributes            (S∪T)−U = (S−U)∪(T−U)"    (metaproven ((sS + sT) - sU) ((sS - sU) + (sT - sU)))
ok "∩ associates through     S∩(T−U) = (S∩T)−U"        (metaproven (sS * (sT - sU)) ((sS * sT) - sU))
ok "11.25(b) bound           S−T ⊆ S"                  (subproven (sS - sT) sS)
ok "11.25(b) disjoint        S−T ⊆ ~T"                 (subproven (sS - sT) (neg sT))
// Soundness at the new operator: difference is NOT symmetric, and is not intersection.
ok "INVALID S−T = T−S  rejected"                       (not (metaproven (sS - sT) (sT - sS)))
ok "INVALID S−T = S∩T  rejected"                       (not (metaproven (sS - sT) (sS * sT)))
ok "INVALID S ⊆ S−T  rejected"                         (not (subproven sS (sS - sT)))

printfn "\n===== (P) Power set (Gries 11.23):  T ∈ 𝒫S = T ⊆ S ====="
// The power set is the first operator in this chapter that does NOT fit the metatheorem. Membership
// in 𝒫S does not reduce to a propositional combination of memberships of the same element — it
// reduces to a SUBSET proposition, which is itself a ∀ over a different element, and `𝒫S` lives at
// `set(set(t))` rather than `set(t)`. So it sits one layer up, and the way to use it is to let the
// axiom take a power-set goal DOWN to a subset obligation and then discharge that with the other
// metatheorem tactic, 11.25(b). That composition is the point of this section.
ok "11.23 Power set axiom recognized"    (st.AxEquiv ((sT |?| sS.Powerset) == (sT |<| sS)).Expr)
ok "11.23 reversed subset rejected"      (not (st.AxEquiv ((sT |?| sS.Powerset) == (sS |<| sT)).Expr))

// `SetTheory.powerset_member t s` proves `T ∈ 𝒫S` by Power set (11.23) then Metatheorem 11.25(b) on
// the resulting `T ⊆ S`.
let inpow (t: SetTerm<int>) (s: SetTerm<int>) = proves "powerset_member" (fun () -> powerset_member t s)

ok "∅ ∈ 𝒫S     (∅ ⊆ S)"                                (inpow emptyT sS)
ok "S ∈ 𝒫S     (reflexivity 11.58)"                    (inpow sS sS)
ok "S∩T ∈ 𝒫S   (∩ lower bound)"                        (inpow (sS * sT) sS)
ok "S−T ∈ 𝒫S   (difference bound, from section O)"     (inpow (sS - sT) sS)
// Soundness: S∪T is not a subset of S, so it is not a member of 𝒫S and the tactic must refuse.
ok "INVALID S∪T ∈ 𝒫S  rejected"                        (not (inpow (sS + sT) sS))

printfn "\n===== (Q) The named laws of §11.3, as theorems of the library ====="
// Everything above states its goal inline and asks a tactic to prove it. This section checks the
// laws Gries actually NAMES, in the form the theory exports them: each is a function of its set
// arguments (the same shape `PredCalculus` uses for the ch.8/9 theorems), so a proof elsewhere can
// cite `SetTheory.de_morgan_union S T` instead of restating the identity. Each returns a real
// `Theorem`, so what is checked here is that the proof closes.
let named (label: string) (f: unit -> Theorem) = ok label (proves label f)

named "11.19  ~~S = S                      double_complement"      (fun () -> double_complement sS)
named "11.26  S∪T = T∪S                    symm_union"             (fun () -> symm_union sS sT)
named "11.27  (S∪T)∪U = S∪(T∪U)            assoc_union"            (fun () -> assoc_union sS sT sU)
named "11.28  S∪S = S                      idemp_union"            (fun () -> idemp_union sS)
named "11.29  S∪U = U                      zero_union"             (fun () -> zero_union sS)
named "11.30  S∪∅ = S                      ident_union"            (fun () -> ident_union sS)
named "11.32  S∪~S = U                     excluded_middle_union"  (fun () -> excluded_middle_union sS)
named "11.34  S∩U = S                      ident_inter"            (fun () -> ident_inter sS)
named "11.35  S∩∅ = ∅                      zero_inter"             (fun () -> zero_inter sS)
named "11.36  S∩T = T∩S                    symm_inter"             (fun () -> symm_inter sS sT)
named "       (S∩T)∩U = S∩(T∩U)            assoc_inter"            (fun () -> assoc_inter sS sT sU)
named "       S∩S = S                      idemp_inter"            (fun () -> idemp_inter sS)
named "11.39  S∩~S = ∅                     contradiction_inter"    (fun () -> contradiction_inter sS)
named "11.40  S∩(T∪U) = (S∩T)∪(S∩U)        distrib_inter_union"    (fun () -> distrib_inter_union sS sT sU)
named "11.41  S∪(T∩U) = (S∪T)∩(S∪U)        distrib_union_inter"    (fun () -> distrib_union_inter sS sT sU)
named "11.42a ~(S∪T) = ~S∩~T               de_morgan_union"        (fun () -> de_morgan_union sS sT)
named "11.42b ~(S∩T) = ~S∪~T               de_morgan_inter"        (fun () -> de_morgan_inter sS sT)
named "       S∩(S∪T) = S                  absorb_inter_union"     (fun () -> absorb_inter_union sS sT)
named "       S∪(S∩T) = S                  absorb_union_inter"     (fun () -> absorb_union_inter sS sT)
named "11.58  S ⊆ S                        subset_refl"            (fun () -> subset_refl sS)
named "       S∩T ⊆ S                      inter_lower_left"       (fun () -> inter_lower_left sS sT)
named "       S∩T ⊆ T                      inter_lower_right"      (fun () -> inter_lower_right sS sT)
named "       S ⊆ S∪T                      union_upper_left"       (fun () -> union_upper_left sS sT)
named "       T ⊆ S∪T                      union_upper_right"      (fun () -> union_upper_right sS sT)
named "       S∩T ⊆ S∪T                    inter_subset_union"     (fun () -> inter_subset_union sS sT)
named "11.22  S−T = S∩~T                   def_difference"         (fun () -> def_difference sS sT)
named "p.203  ~S = U−S                     complement_as_difference" (fun () -> complement_as_difference sS)
named "       U−S = ~S                     difference_from_universe" (fun () -> difference_from_universe sS)
named "       S−S = ∅                      self_difference"        (fun () -> self_difference sS)
named "       S−∅ = S                      ident_difference"       (fun () -> ident_difference sS)
named "       ∅−S = ∅                      zero_difference"        (fun () -> zero_difference sS)
named "       S−(T∪U) = (S−T)∩(S−U)        de_morgan_difference_union" (fun () -> de_morgan_difference_union sS sT sU)
named "       S−(T∩U) = (S−T)∪(S−U)        de_morgan_difference_inter" (fun () -> de_morgan_difference_inter sS sT sU)
named "       (S∪T)−U = (S−U)∪(T−U)        distrib_difference_union" (fun () -> distrib_difference_union sS sT sU)
named "       S∩(T−U) = (S∩T)−U            assoc_inter_difference" (fun () -> assoc_inter_difference sS sT sU)
named "       S−T ⊆ S                      difference_subset"      (fun () -> difference_subset sS sT)
named "       S−T ⊆ ~T                     difference_subset_complement" (fun () -> difference_subset_complement sS sT)
named "11.23  ∅ ∈ 𝒫S                       empty_in_powerset"      (fun () -> empty_in_powerset sS)
named "11.23  S ∈ 𝒫S                       self_in_powerset"       (fun () -> self_in_powerset sS)
named "11.23  S∩T ∈ 𝒫S                     inter_in_powerset"      (fun () -> inter_in_powerset sS sT)
named "11.23  S−T ∈ 𝒫S                     difference_in_powerset" (fun () -> difference_in_powerset sS sT)

// The tactics are generic over the ELEMENT type — the script-local versions they replace were pinned
// to `int`, and nothing here or in Gries depends on what the elements are. Two other element types:
let cS, cT = setvar<char> "S", setvar<char> "T"
let strS   = setvar<string> "S"
named "generic  ~(S∩T) = ~S∪~T  over char"                         (fun () -> de_morgan_inter cS cT)
named "generic  S∪S = S         over string"                      (fun () -> idemp_union strS)

printfn "\n===== (R) Families of sets (Gries §11.4): membership in (∪x|R:E) and (∩x|R:E) ====="
// §11.4 does NOT add a "big union of a set of sets" operator. ∪ and ∩ are symmetric, associative,
// idempotent and have identities, so each is an operator to which §8.2's `(★x | R : E)` notation
// applies — that is all (11.74)/(11.75) are. A family `S : Set<Set<'t>>` is then handled as the
// instance `(∪u | u ∈ S : u)`, which is how (11.76) Partition is stated.
//
// The two axioms below are the bridge: they reduce membership in a family to an ∃/∀ over membership
// in the body, after which every law about families is ordinary predicate calculus — Gries' own
// remark that "other properties … can be derived from the properties of ∃ and ∀".
let fi = intvar "i"
let fy = intvar "y"
let fz = intvar "z"
let fbody = <@ set_comp %fz.Expr (%fz.Expr < %fi.Expr) %fz.Expr @>       // E = {z | z < i : z}
let frange = <@ %fi.Expr >= 0 @>
let fUnion  = <@ union %fi.Expr %frange %fbody @>
let fInter  = <@ intersect %fi.Expr %frange %fbody @>

ok "11.74 (∪i|R:E) is a quantifier, and its RANGE renders"
   ((st.PrintFormula (expand fUnion)).StartsWith "(⋃ i | i >= 0 : ")
ok "11.75 (∩i|R:E) likewise"
   ((st.PrintFormula (expand fInter)).StartsWith "(⋂ i | i >= 0 : ")
// The dummy is no longer pinned to int, so Gries' (11.76) shape — a dummy ranging over SETS — builds.
let fFam : SetTerm<Set<int>> = setvar<Set<int>> "F"
let fu = SetVar<int> "u"
let fuinF : Prop = (fu :> Term<Set<int>>) |?| fFam
ok "(11.76) shape  (∪u | u ∈ F : u)  builds with a set-typed dummy"
   ((st.PrintFormula (expand <@ union %fu.Expr %fuinF.Expr %fu.Expr @>)) = "(⋃ u | u ∈ F : u)")

ok "11.74 membership  y ∈ (∪i|R:E) = (∃i|R: y∈E)"
   (st.AxEquiv <@ (%fy.Expr |?| %fUnion) = exists_expr %fi.Expr %frange (%fy.Expr |?| %fbody) @>)
ok "11.75 membership  y ∈ (∩i|R:E) = (∀i|R: y∈E)"
   (st.AxEquiv <@ (%fy.Expr |?| %fInter) = forall_expr %fi.Expr %frange (%fy.Expr |?| %fbody) @>)

// Soundness. The last two are what stop a FUTURE generalized quantification (a Σ, say) from being
// read as a set-membership axiom: the axioms key on the OPERATOR, not on the sum/product shape.
ok "∪ paired with ∀ rejected"
   (not (st.AxEquiv <@ (%fy.Expr |?| %fUnion) = forall_expr %fi.Expr %frange (%fy.Expr |?| %fbody) @>))
ok "∩ paired with ∃ rejected"
   (not (st.AxEquiv <@ (%fy.Expr |?| %fInter) = exists_expr %fi.Expr %frange (%fy.Expr |?| %fbody) @>))
ok "mismatched range rejected"
   (not (st.AxEquiv <@ (%fy.Expr |?| %fUnion) = exists_expr %fi.Expr (%fi.Expr > 0) (%fy.Expr |?| %fbody) @>))
ok "mismatched dummy rejected"
   (not (st.AxEquiv <@ (%fy.Expr |?| %fUnion) = exists_expr %fz.Expr %frange (%fy.Expr |?| %fbody) @>))
// CAPTURE: an element mentioning the dummy is free on the left and would be captured on the right.
// (This side condition is CHECKED here, unlike the one on Membership 11.3 in section E.)
ok "element mentioning the dummy rejected (capture)"
   (not (st.AxEquiv <@ (%fi.Expr |?| %fUnion) = exists_expr %fi.Expr %frange (%fi.Expr |?| %fbody) @>))
ok "sum carrying ∩ is not a family union"
   (not (st.AxEquiv <@ (%fy.Expr |?| (Formula.sum Set.set_intersection "⋃" %fi.Expr %frange %fbody))
                        = exists_expr %fi.Expr %frange (%fy.Expr |?| %fbody) @>))
ok "product carrying ∪ is not a family intersection"
   (not (st.AxEquiv <@ (%fy.Expr |?| (Formula.product Set.set_union "⋂" %fi.Expr %frange %fbody))
                        = forall_expr %fi.Expr %frange (%fy.Expr |?| %fbody) @>))

// The §11.4 laws, from the library. Each is Extensionality → the family membership axiom → ONE
// predicate-calculus step, which is the whole point of routing families through membership: only
// One-Point, Nesting and Renaming are stated generically over the quantified operator, so nothing
// else about ∪/∩ as quantifiers is available directly.
//
// `qunion`/`qinter` are the SetTerm-level builders (the shape `PredCalculus.qall`/`qex` have).
let fE : SetTerm<int> = SetTerm<int>(fbody)
let fR : Prop = Prop frange
named "        ~(∪x|R:E) = (∩x|R:~E)     de_morgan_family_union"     (fun () -> de_morgan_family_union fi fR fE)
named "        ~(∩x|R:E) = (∪x|R:~E)     de_morgan_family_inter"     (fun () -> de_morgan_family_inter fi fR fE)
named "        (∪x|false:E) = ∅          empty_range_union"          (fun () -> empty_range_union fi fE)
named "        (∩x|false:E) = U          empty_range_inter"          (fun () -> empty_range_inter fi fE)
named "9.21    S∩(∪x|R:E) = (∪x|R:S∩E)   distrib_inter_family_union" (fun () -> distrib_inter_family_union fi fR sS fE)
named "generic ~(∪k|R:E) = (∩k|R:~E) over char elements"
      (fun () -> de_morgan_family_union (intvar "k") (boolvar "Rc") (setvar<char> "Ec"))

// A SET-typed dummy, i.e. Gries (11.76)'s own shape — written with a plain `SetVar`, which is BOTH
// the dummy and the body. That works because variable-ness is an INTERFACE (`ISymbolicVar<'t>`)
// rather than the `TermVar` base class: F# is single-inheritance, so `SetVar` must inherit `SetTerm`
// to keep ∪/∩/−/∈/⊆, and could not also inherit `TermVar<Set<'t>>`. The quantifier builders take
// `#ISymbolicVar<'t>`, so a `SetVar` and a `ScalarVar` are both acceptable dummies.
let fud = setvar<int> "u"
let fudinF : Prop = (fud :> Term<Set<int>>) |?| fFam
named "(11.76) ~(∪u|u∈F:u) = (∩u|u∈F:~u)  over a family of sets"
      (fun () -> de_morgan_family_union fud fudinF fud)
named "(11.76) a set variable is a dummy AND a term: (∀u|u∈F: u ⊆ S)"
      (fun () -> theorem st (qall fud fudinF (fud |<| sS) ==> qall fud fudinF (fud |<| sS)) [])

// The empty-range law is worth noting twice: EMPTY RANGE (8.13) is one of the axioms that is NOT
// generic over the quantified operator, so the proof only closes because 11.74 reaches an ∃ first.
// (The range must be the NAMED truth constant `F` — `(|False|_|)` matches
// `ValueWithName(_,bool,"False")` only, so a bare `false` literal silently fails to match it.)
let fEmptyU = qunion fi F fE

// The first §11.4 law that is an IMPLICATION rather than an identity, and the first needing
// ∃-INTRODUCTION (9.28) — the witness for `y ∈ (∪u|u∈F:u)` is the member set itself.
let fA : SetTerm<int> = setvar<int> "A"
named "        A ∈ F ⇒ A ⊆ (∪u|u∈F:u)    family_union_upper_bound"
      (fun () -> family_union_upper_bound fud fFam fA)

// (11.76) Partition — a DEFINITION, stated in Gries' own TWO-DUMMY form. The bound is a tuple, which
// `BoundVars` matches, so it is an ordinary quantifier: Nesting (8.20) relates it to the nested
// single-dummy form, and Nesting is one of the three axioms generic over the quantified operator.
let fv = setvar<int> "v"
let fPart = partition fud fv fFam fA
ok "(11.76) Partition builds in the two-dummy form"
   ((st.PrintFormula (expand fPart.Expr)).StartsWith "(∀ u,v | u ∈ F ∧ (v ∈ F ∧ ¬(u = v)) : u ∩ v = ∅) ∧ ")
ok "(11.76) its two-dummy ∀ is a recognized quantifier"
   (match expand fPart.Expr with
    | And(Quantifier(_, [b1; b2], _, _), _) -> b1.Name = "u" && b2.Name = "v"
    | _ -> false)
// Nesting relates Gries' two-dummy form to the nested one — the check that the representation is
// not merely well-formed but agrees with the single-dummy machinery.
let fInS (s: SetTerm<int>) : Prop = (s :> Term<Set<int>>) |?| fFam
ok "(8.20) Nesting relates the two-dummy and nested forms"
   (Theory.S.AxEquiv (expand ((qall2 fud fv ((fInS fud) * ((fInS fv) * !!(fud == fv)))
                                           ((fud * fv) == emptyT))
                              == qall fud (fInS fud)
                                   (qall fv ((fInS fv) * !!(fud == fv)) ((fud * fv) == emptyT))).Expr))

// Families sit OUTSIDE the metatheorem tactics, like the power set: `set_shape` classifies a family
// term as an atom, so `meta_set_ident` treats it as an opaque set variable. That is SOUND but
// incomplete — it proves only what holds of an arbitrary set and cannot see inside the quantifier.
ok "meta_set_ident treats a family as an atom (sound, incomplete): S∪(∪i|R:E) = (∪i|R:E)∪S"
   (metaproven (sS + fEmptyU) (fEmptyU + sS))

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
