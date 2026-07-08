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

// Symbolic set variables S, T : Set<int>. Set operations are the SetTerm operators ∪ = `|+|`,
// ∩ = `|*|`, ~ = `-`, ⊆ = `|<|` — the SAME symbols the theory keys on for both the algebra laws
// and the membership axioms (so one expression is usable by both routes). `sS`/`sT` avoid clashing
// with the truth constant `T`.
let sS = setvar<int> "S"
let sT = setvar<int> "T"
let neg (s:SetTerm<int>) : SetTerm<int> = -s      // ~s, annotated to fix the operator's return type

let sa = SetAlgebra.set_algebra<int>

printfn "\n===== (B) Inherited Boolean-algebra axioms recognized after composition ====="
ok "Idempotency      S ∪ S = S"              (sa.AxEquiv ((sS |+| sS) == sS).Expr)
ok "Symmetry         S ∩ T = T ∩ S"          (sa.AxEquiv ((sS |*| sT) == (sT |*| sS)).Expr)
ok "Identity of ∪     S ∪ ∅ = S"              (sa.AxEquiv <@ (%sS.Expr |+| Set.Empty) = %sS.Expr @>)

printfn "\n===== (C) Complement law recognized with correct polarity (Gries 11.32/11.39) ====="
ok "Excluded middle  S ∪ ~S = U  recognized"    (sa.AxEquiv <@ %((sS |+| (neg sS)).Expr) = Set.U @>)
ok "Contradiction    S ∩ ~S = ∅  recognized"    (sa.AxEquiv <@ %((sS |*| (neg sS)).Expr) = Set.Empty @>)
ok "S ∪ ~S = ∅  rejected (was wrongly accepted)" (not (sa.AxEquiv <@ %((sS |+| (neg sS)).Expr) = Set.Empty @>))
ok "S ∩ ~S = U  rejected (was wrongly accepted)" (not (sa.AxEquiv <@ %((sS |*| (neg sS)).Expr) = Set.U @>))

printfn "\n===== (A) Injected axioms compose through the theory chain (previously dropped) ====="
let marker = <@ %((sS |+| sT).Expr) = Set.U @>    // not a Boolean-algebra axiom on its own
let extra : Axioms = fun e -> if sequal e (expand marker) then Descriptions.axiom_name "Marker" "Marker" |> Some else None
let sa2 = SetAlgebra.SetAlgebra<int>(axioms = extra)
ok "injected marker axiom recognized in sa2"     (sa2.AxEquiv marker)
ok "base axiom still recognized in sa2"          (sa2.AxEquiv ((sS |+| sS) == sS).Expr)
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

printfn "\n===== (H) Operator membership-reduction axioms (Gries 11.13-11.21) ====="
let v = intvar "v"
let vinS = v |?| sS      // membership is now a Prop directly (SetTerm's `|?|` returns Prop)
let vinT = v |?| sT
// The SAME `|+|`/`|*|` operator expressions match both the membership axioms here AND the
// Boolean-algebra laws (checks B/C); subset `|<|` is now a proposition.
ok "11.20 Union       v∈S∪T = v∈S ∨ v∈T"
    (st.AxEquiv ((v |?| (sS |+| sT)) == (vinS + vinT)).Expr)
ok "11.21 Intersection v∈S∩T = v∈S ∧ v∈T"
    (st.AxEquiv ((v |?| (sS |*| sT)) == (vinS * vinT)).Expr)
ok "11.18 Complement  v∈~S = ¬(v∈S)"
    (st.AxEquiv ((v |?| (neg sS)) == (!! vinS)).Expr)
ok "11.13 Subset      S⊆T = (∀x|x∈S:x∈T)"
    (st.AxEquiv ((sS |<| sT) == qall x (x |?| sS) (x |?| sT)).Expr)
// coherence: the SAME |+| expression is also recognized by the Boolean-algebra layer
ok "coherence: S∪T (|+|) matches algebra idempotency S∪S=S"
    ((SetAlgebra.set_algebra<int>).AxEquiv ((sS |+| sS) == sS).Expr)

printfn "\n===== (I) A worked set-algebra law via the membership route: Gries 11.28  S ∪ S = S ====="
// Extensionality reduces S∪S=S to (∀v|: v∈(S∪S) = v∈S); the Union axiom (11.20) unfolds v∈(S∪S) to
// v∈S ∨ v∈S; ∨-idempotency collapses it; reflexivity and (∀v|:true)=true close it.
let SuS   = sS |+| sS
let extU  = id_ax st ((SuS == sS) == qall v T ((v |?| SuS) == (v |?| sS)))
let unionU = id_ax st ((v |?| SuS) == (vinS + vinS))
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
let SuT      : SetTerm<int> = sS |+| sT
let negSuT   : SetTerm<int> = neg SuT              // ~(S ∪ T)
let nSinT    : SetTerm<int> = nS |*| nsT           // ~S ∩ ~T
let memv (t:SetTerm<int>) = v |?| t
let compUnion = id_ax st ((memv negSuT) == (!! (memv SuT)))       // v∈~(S∪T) = ¬(v∈(S∪T))
let unionR    = id_ax st ((memv SuT)    == (vinS + vinT))         // v∈(S∪T)  = v∈S ∨ v∈T
let interR    = id_ax st ((memv nSinT)  == ((memv nS) * (memv nsT)))  // v∈(~S∩~T) = v∈~S ∧ v∈~T
let compS     = id_ax st ((memv nS)     == (!! vinS))             // v∈~S = ¬(v∈S)
let compT     = id_ax st ((memv nsT)    == (!! vinT))             // v∈~T = ¬(v∈T)
let extDM = id_ax st ((negSuT == nSinT) == qall v T ((v |?| negSuT) == (v |?| nSinT)))
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

printfn "\n===== (K) Metatheorem 11.25(a): a tactic that mechanizes the set-algebra laws ====="
// Gries' Metatheorem (11.25a) says a set identity  Es = Fs  is valid iff its propositional
// translation  Ep = Fp  (Definition 11.24: ∅↦false, U↦true, ~↦¬, ∪↦∨, ∩↦∧, set variable S ↦ its
// membership proposition v∈S) is valid. Rather than adding it as a new trusted primitive, we
// MECHANIZE the membership-route proof used by hand for 11.28 / De Morgan (sections I/J): apply
// Extensionality; recursively unfold every membership through the operator axioms (which literally
// implement 11.24); discharge the resulting propositional body  Ep = Fp  with the COMPLETE ANF
// prover `autoproof_anf`; and close with `(∀v|:true) = true`. The result is a genuine, kernel-checked
// Theorem built only from existing recognized axioms — no new trusted rule. Because `autoproof_anf`
// is complete for (and only for) propositional tautologies, the tactic proves exactly the valid set
// identities over {∪, ∩, ~, variables} and REJECTS invalid ones.

open FSharp.Quotations.Patterns

// Classify a set expression's head: operator (∪ / ∩ / ~), the constants ∅ / U, else an atom (a set
// variable). ∅ (`NewUnionCase Empty`) and U (`PropertyGet U`) stay structured because the SetTerm is
// built inside a quotation (writing `Set.Empty` outside one embeds it as an opaque value).
let (|SUnion|SInter|SCompl|SEmpty|SUniv|SAtom|) (s: SetTerm<int>) =
    match expand s.Expr with
    | Call(None, mi, [a; b]) when mi.Name = "op_BarPlusBar"     -> SUnion(SetTerm<int>(Expr.Cast a), SetTerm<int>(Expr.Cast b))
    | Call(None, mi, [a; b]) when mi.Name = "op_BarMultiplyBar" -> SInter(SetTerm<int>(Expr.Cast a), SetTerm<int>(Expr.Cast b))
    | Call(None, mi, [a])    when mi.Name = "op_UnaryNegation"  -> SCompl(SetTerm<int>(Expr.Cast a))
    | NewUnionCase(uc, _)       when uc.Name = "Empty" -> SEmpty
    | PropertyGet(None, pi, []) when pi.Name  = "U"     -> SUniv
    | _ -> SAtom

// Definition 11.24: ∪↦∨, ∩↦∧, ~↦¬, ∅↦false, U↦true, and each set variable ↦ its membership atom v∈S.
let rec translate (s: SetTerm<int>) : Prop =
    match s with
    | SUnion(a, b) -> (translate a) + (translate b)   // ∪ ↦ ∨
    | SInter(a, b) -> (translate a) * (translate b)   // ∩ ↦ ∧
    | SCompl a     -> !! (translate a)                // ~ ↦ ¬
    | SEmpty       -> F                               // ∅ ↦ false
    | SUniv        -> T                               // U ↦ true
    | SAtom        -> memv s

// A rewrite rule  (v ∈ s) = translate s, built by recursion mirroring the operator axioms
// (11.18/11.20/11.21) and the constant-membership axioms (v∈∅ = false, v∈U = true).
let rec unfold (s: SetTerm<int>) : Rule =
    let sub (x: SetTerm<int>) addr = match x with SAtom -> [] | _ -> [ unfold x |> at addr ]   // skip atoms
    match s with
    | SAtom       -> id_ax st ((memv s) == (memv s))                                            // reflexivity
    | SEmpty      -> id_ax st ((memv s) == F)                                                   // v∈∅ = false
    | SUniv       -> id_ax st ((memv s) == T)                                                   // v∈U = true
    | SCompl a    -> ident st ((memv s) == (translate s))
                        ((id_ax st ((memv s) == (!! (memv a))) |> at_left) :: sub a [left_branch; apply_unary])
    | SUnion(a, b)-> ident st ((memv s) == (translate s))
                        ((id_ax st ((memv s) == ((memv a) + (memv b))) |> at_left) :: sub a [left_branch; left_branch] @ sub b [left_branch; right_branch])
    | SInter(a, b)-> ident st ((memv s) == (translate s))
                        ((id_ax st ((memv s) == ((memv a) * (memv b))) |> at_left) :: sub a [left_branch; left_branch] @ sub b [left_branch; right_branch])

// The tactic: prove a set identity  Es = Fs  via Metatheorem 11.25(a).
let metaset (lhs: SetTerm<int>) (rhs: SetTerm<int>) : Theorem =
    let goal = lhs == rhs
    let ext  = id_ax st (goal == qall v T ((v |?| lhs) == (v |?| rhs)))
    let stepL = match lhs with SAtom -> [] | _ -> [ unfold lhs |> at [select_body; left_branch] ]
    let stepR = match rhs with SAtom -> [] | _ -> [ unfold rhs |> at [select_body; right_branch] ]
    let bodyRule = autoproof_anf ((translate lhs) == (translate rhs)) |> Theorem |> Ident   // Ep = Fp (complete)
    theorem st goal ([ ext ] @ stepL @ stepR @ [ Taut' bodyRule |> at [select_body]; ident_forall_true' v ])

let sU = setvar<int> "U"
let emptyT = SetTerm<int>(<@ Set.Empty @>)   // ∅ as a structured SetTerm (kept out of a value embedding)
let uT     = SetTerm<int>(<@ Set.U @>)       // U, the universe
let metaproven (l: SetTerm<int>) (r: SetTerm<int>) = try (metaset l r).Proof.Complete with _ -> false

// The named Gries laws 11.26–11.42 — each proved with a single `metaset` call.
ok "11.26 Symmetry of ∪        S∪T = T∪S"              (metaproven (sS |+| sT) (sT |+| sS))
ok "11.27 Associativity of ∪   (S∪T)∪U = S∪(T∪U)"      (metaproven ((sS |+| sT) |+| sU) (sS |+| (sT |+| sU)))
ok "11.28 Idempotency of ∪     S∪S = S"                (metaproven (sS |+| sS) sS)
ok "11.36 Symmetry of ∩        S∩T = T∩S"              (metaproven (sS |*| sT) (sT |*| sS))
ok "11.40 Distributivity ∩/∪   S∩(T∪U) = (S∩T)∪(S∩U)"  (metaproven (sS |*| (sT |+| sU)) ((sS |*| sT) |+| (sS |*| sU)))
ok "11.41 Distributivity ∪/∩   S∪(T∩U) = (S∪T)∩(S∪U)"  (metaproven (sS |+| (sT |*| sU)) ((sS |+| sT) |*| (sS |+| sU)))
ok "11.42a De Morgan          ~(S∪T) = ~S∩~T"          (metaproven (neg (sS |+| sT)) ((neg sS) |*| (neg sT)))
ok "11.42b De Morgan          ~(S∩T) = ~S∪~T"          (metaproven (neg (sS |*| sT)) ((neg sS) |+| (neg sT)))
ok "Absorption                S∩(S∪T) = S"             (metaproven (sS |*| (sS |+| sT)) sS)
ok "Double complement 11.19   ~~S = S"                 (metaproven (neg (neg sS)) sS)
// Soundness: the tactic must REJECT invalid identities (the complete ANF prover refuses non-tautologies).
ok "INVALID S∪T = S∩T  rejected"                       (not (metaproven (sS |+| sT) (sS |*| sT)))
ok "INVALID ~(S∪T) = ~S∪~T  rejected"                  (not (metaproven (neg (sS |+| sT)) ((neg sS) |+| (neg sT))))

printfn "\n===== (L) Metatheorem 11.25(b): subset via implication  Es ⊆ Fs ↔ Ep ⇒ Fp ====="
// Gries (11.56) — one set is a subset of another iff its characteristic predicate IMPLIES the other's
// — is exactly Metatheorem 11.25(b). We mechanize it like (a), but the goal `Es ⊆ Fs` is a bare
// proposition (not an equality), so we reduce it to `true`: apply Subset (11.13) to get
// `(∀v | v∈Es : v∈Fs)`; TRADE (9.2) to `(∀v |: v∈Es ⇒ v∈Fs)` (using the simple membership predicates,
// so no recursion is needed for the trade); unfold each side of the implication with the section-K
// `unfold` lemmas to reach the body `Ep ⇒ Fp`; discharge that tautology with `autoproof_anf` folded
// via `Taut` (a proven proposition → true); close with `(∀v|:true) = true`.

let memPred (s: SetTerm<int>) : Pred<int> = Pred<int>(func = <@ fun (z:int) -> z |?| %s.Expr @>)

let metasubset (lhs: SetTerm<int>) (rhs: SetTerm<int>) : Theorem =
    let goal   = lhs |<| rhs
    let subAx  = id_ax st (goal == qall v (v |?| lhs) (v |?| rhs))            // Subset 11.13
    let trade  = trade_forall_implies v (memPred lhs) (memPred rhs)           // Trading 9.2: (∀v|N:P)=(∀v|:N⇒P)
    let stepA  = match lhs with SAtom -> [] | _ -> [ unfold lhs |> at [select_body; left_branch] ]   // antecedent
    let stepC  = match rhs with SAtom -> [] | _ -> [ unfold rhs |> at [select_body; right_branch] ]  // consequent
    let bodyThm = autoproof_anf ((translate lhs) ==> (translate rhs)) |> Theorem   // Ep ⇒ Fp (complete)
    theorem st goal ([ subAx; trade ] @ stepA @ stepC @ [ Taut bodyThm |> at [select_body]; ident_forall_true' v ])

let subproven l r = try (metasubset l r).Proof.Complete with _ -> false

ok "11.58 Reflexivity          S ⊆ S"                  (subproven sS sS)
ok "∩ lower bound              S∩T ⊆ S"                (subproven (sS |*| sT) sS)
ok "∩ lower bound              S∩T ⊆ T"                (subproven (sS |*| sT) sT)
ok "∪ upper bound              S ⊆ S∪T"                (subproven sS (sS |+| sT))
ok "∪ upper bound              T ⊆ S∪T"                (subproven sT (sS |+| sT))
ok "monotone                   S∩T ⊆ S∪T"             (subproven (sS |*| sT) (sS |+| sT))
// Soundness: a non-subset must be REJECTED (the implication Ep ⇒ Fp is not a tautology).
ok "INVALID S ⊆ S∩T  rejected"                        (not (subproven sS (sS |*| sT)))
ok "INVALID S∪T ⊆ S  rejected"                        (not (subproven (sS |+| sT) sS))

printfn "\n===== (M) ∅ / U membership atoms: the identity, zero and complement laws ====="
// With the constant-membership axioms  v∈∅ = false  and  v∈U = true  (added to SetTheory.fs), the
// `metaset` tactic now also covers every Gries law that mentions ∅ or U. Metatheorem 11.25(c)
// (`Es = U` valid iff Ep valid) needs no separate tactic — it is just `metaset Es U`, whose body
// reduces to `Ep = true`.
ok "v∈∅ = false  recognized (Empty axiom)"            (st.AxEquiv ((v |?| emptyT) == F).Expr)
ok "v∈U = true   recognized (Universe axiom)"         (st.AxEquiv ((v |?| uT) == T).Expr)
ok "11.30 Identity of ∪       S∪∅ = S"                (metaproven (sS |+| emptyT) sS)
ok "11.34 Identity of ∩       S∩U = S"                (metaproven (sS |*| uT) sS)
ok "11.29 Zero of ∪           S∪U = U"                (metaproven (sS |+| uT) uT)
ok "11.35 Zero of ∩           S∩∅ = ∅"                (metaproven (sS |*| emptyT) emptyT)
ok "11.32 Excluded middle     S∪~S = U"               (metaproven (sS |+| (neg sS)) uT)
ok "11.39 Contradiction       S∩~S = ∅"               (metaproven (sS |*| (neg sS)) emptyT)
ok "11.25(c) via Es=U         (S∪~S)∪∅ = U"           (metaproven ((sS |+| (neg sS)) |+| emptyT) uT)
// Soundness with the constants:
ok "INVALID S∪∅ = U  rejected"                        (not (metaproven (sS |+| emptyT) uT))
ok "INVALID S∩U = ∅  rejected"                        (not (metaproven (sS |*| uT) emptyT))

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
