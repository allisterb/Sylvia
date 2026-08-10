namespace Sylvia
// A bare rule as a proof step implicitly means "apply to the whole expression" (Rule -> RuleApplication);
// acknowledge that implicit conversion (the tactics below pass rules straight into a step list).
#nowarn "3391"

open FSharp.Quotations
open FSharp.Quotations.Patterns

open FsExpr
open Formula
open Descriptions

open SetAlgebra
open PropCalculus
open PredCalculus

/// Theory of sets and set algebra.
module SetTheory =
    let desc = Some << axiom_name "Set Theory"
    
    (* Patterns *)
    
    let (|SetEmpty|_|) =
        function
        | NewUnionCase(uc, e) when uc.Name = "Empty" -> e |> List.map expand |> Some
        | _ -> None

    let (|SetSeq|_|) =
        function
        | NewUnionCase(uc, Sequence e::[]) when uc.Name = "Seq" -> Some e
        | Call(None, mi, l) when mi.Name = "infinite_seq" || mi.Name = "finite_seq" || mi.Name = "sseq" -> l |> List.map expand |> Some
        | _ -> None

    let (|SetComp|_|) =
        function
        | Call(None, mi, (BoundVars(_)::s as c)) when mi.Name = "set_comp" || mi.Name = "finite_set" || mi.Name = "infinite_set_0" || mi.Name = "infinite_set_1" || mi.Name = "set" || mi.Name = "set'" -> c |> List.map expand |> Some
        | _ -> None

    let (|Set|_|) =
        function
        | SetEmpty e
        | SetSeq e
        | SetComp e -> Some e
        | _ -> None

    /// The universe U (`Set.U`, a static property) — the domain of discourse every value belongs to.
    let (|SetUniverse|_|) =
        function
        | PropertyGet(None, pi, []) when pi.Name = "U" -> Some ()
        | _ -> None

    /// e ∈ S  →  (e, S). The set operand S is returned RAW — it may be a set variable (SetVar), a
    /// comprehension, or any set-typed expression. Callers destructure it (e.g. via `SetComp`) as
    /// needed; membership on a bare set variable (as in Extensionality) must not require a literal set.
    let (|ElementOf|_|) =
        function
        | Call(None, mi, l::r::[]) when mi.Name = "op_BarQmarkBar" -> Some(expand l, expand r)
        | _ -> None

    (* Axioms *)

    let private desfc = axiom_desc "Set Theory"

    /// Set membership (Gries 11.3):  F ∈ {x | R : E}  =  (∃x | R : F = E).
    /// The set operand must be a comprehension (SetComp); F is x-free (not checked here).
    let (|Membership|_|) =
        function
        | Equals(ElementOf(F, SetComp(BoundVars(bound)::range::body::_)), Exists(_, bound', range', Equals(F', body')))
            when vequal' bound bound' && sequal3 F range body F' range' body' -> desc "Set Membership"
        | _ -> None

    /// Set extensionality (Gries 11.4):  S = T  =  (∀x |: x∈S = x∈T).
    /// S and T are arbitrary set expressions (usually set variables); the ∀ has a true range.
    let (|Extensionality|_|) =
        function
        | Equals(Equals(s, t), ForAll(_, xv, True, Equals(ElementOf(xe1, s1), ElementOf(xe2, t1))))
            when sequal s s1 && sequal t t1 && vequal' xv (get_vars xe1) && vequal' xv (get_vars xe2) -> desc "Set Extensionality"
        | _ -> None

    (* Operator definitions — each reduces membership in a compound set to a propositional/predicate
       combination of memberships (Gries 11.13-11.23). All are keyed on the `Set<'t>` OPERATOR methods
       (∪ = op_BarPlusBar, ∩ = op_BarMultiplyBar, − = op_BarMinusBar, ~ = op_UnaryNegation,
       ⊆ = op_BarLessBar) — the SAME methods the §11.3 Boolean-algebra layer (SetAlgebra) keys its
       join/meet/complement on — so a single expression is usable by BOTH the membership route and the
       algebra route. NB the SYMBOLIC spelling is arithmetic (`S + T` for ∪, `S * T` for ∩, `S - T` for
       −): `SetTerm`'s operators are a surface notation that still BUILDS these `Set<'t>` methods, so
       what the axioms match is unchanged by how the expression was written.
       (The method name is checked with the `Op "…"` name pattern rather than `Binary <@ (|+|) @>`,
       because `Binary`'s type guard would pin the axiom to one element type; the name check is
       element-type-agnostic.) *)

    /// v ∈ S∪T = v∈S ∨ v∈T   (Gries 11.20, Union).
    let (|UnionMember|_|) =
        function
        | Equals(ElementOf(v, Call(None, Op "op_BarPlusBar", s::t::[])), Or(ElementOf(v1, s1), ElementOf(v2, t1)))
            when sequal2 v s v1 s1 && sequal2 v t v2 t1 -> desc "Set Union"
        | _ -> None

    /// v ∈ S∩T = v∈S ∧ v∈T   (Gries 11.21, Intersection).
    let (|IntersectMember|_|) =
        function
        | Equals(ElementOf(v, Call(None, Op "op_BarMultiplyBar", s::t::[])), And(ElementOf(v1, s1), ElementOf(v2, t1)))
            when sequal2 v s v1 s1 && sequal2 v t v2 t1 -> desc "Set Intersection"
        | _ -> None

    /// v ∈ ~S = ¬(v∈S)   (Gries 11.18, Complement; 11.17 with the implicit universe v∈U elided).
    let (|ComplementMember|_|) =
        function
        | Equals(ElementOf(v, Call(None, Op "op_UnaryNegation", s::[])), Not(ElementOf(v1, s1)))
            when sequal2 v s v1 s1 -> desc "Set Complement"
        | _ -> None

    /// v ∈ S−T = v∈S ∧ ¬(v∈T)   (Gries 11.22, Difference).
    let (|DifferenceMember|_|) =
        function
        | Equals(ElementOf(v, Call(None, Op "op_BarMinusBar", s::t::[])), And(ElementOf(v1, s1), Not(ElementOf(v2, t1))))
            when sequal2 v s v1 s1 && sequal2 v t v2 t1 -> desc "Set Difference"
        | _ -> None

    /// S ⊆ T = (∀x | x∈S : x∈T)   (Gries 11.13, Subset).
    let (|SubsetDef|_|) =
        function
        | Equals(Call(None, Op "op_BarLessBar", s::t::[]), ForAll(_, xv, ElementOf(xe1, s1), ElementOf(xe2, t1)))
            when sequal s s1 && sequal t t1 && vequal' xv (get_vars xe1) && vequal' xv (get_vars xe2) -> desc "Subset"
        | _ -> None

    /// T ∈ 𝒫S = T ⊆ S   (Gries 11.23, Power set).
    ///
    /// Unlike the operators above this does NOT reduce membership to a propositional combination of
    /// memberships of the same element: the right-hand side is a SUBSET proposition, itself a ∀ over
    /// a different element. So the power set sits outside Definition 11.24's grammar and outside what
    /// `meta_set_ident` mechanizes — it is used in hand proofs, one layer up. `𝒫S : set(set(t))`, so the
    /// member `T` here is itself a set and the power set is an instance `PropertyGet`, not an operator.
    let (|PowersetMember|_|) =
        function
        | Equals(ElementOf(v, PropertyGet(Some s, pi, [])), Call(None, Op "op_BarLessBar", v1::s1::[]))
            when pi.Name = "Powerset" && sequal v v1 && sequal s s1 -> desc "Power Set Membership"
        | _ -> None

    /// v ∈ ∅ = false   (the empty set has no members; Gries: ∅ = {x | false}, Exercise 11.4).
    let (|EmptyMember|_|) =
        function
        | Equals(ElementOf(_, SetEmpty _), False) -> desc "Empty Set Membership"
        | _ -> None

    /// v ∈ U = true   (every value belongs to the universe of discourse).
    let (|UniverseMember|_|) =
        function
        | Equals(ElementOf(_, SetUniverse), True) -> desc "Universe Membership"
        | _ -> None

    let set_theory_axioms =
        function
        | Membership x
        | Extensionality x
        | UnionMember x
        | IntersectMember x
        | ComplementMember x
        | DifferenceMember x
        | SubsetDef x
        | PowersetMember x
        | EmptyMember x
        | UniverseMember x -> Some x
        | _ -> None
    (* Theory *)

    // The theory of sets sits over TWO foundations (Gries ch. 11):
    //   1. Predicate calculus — supplied automatically as the ambient logical theory (`Proof.Logic`,
    //      i.e. `Theory.S`). Every proof consults both the theory's own axioms/rules AND the logic's,
    //      so ∀/∃, Trading (9.19), the One-point rule (8.14), etc. are already available here without
    //      re-inheritance. This is what lets set membership (11.3) reduce ∈ to ∃ during a proof.
    //   2. The Boolean algebra of set operators (∪/∩/~/∅/U) — inherited from SetAlgebra : BooleanAlgebra,
    //      the object-level payoff of Metatheorem (11.25).
    // Set-specific axioms (Membership 11.3, Extensionality 11.4, the operator definitions 11.13-11.23)
    // are injected through `?axioms`; the plumbing composes them over the Boolean-algebra axioms
    // instead of discarding them. Size (11.12) is the one definition still missing — it needs a Σ
    // quantifier the pure fragment does not have.
    type SetTheory<'t when 't : equality>(?axioms:Axioms, ?rules:Rules) =
        inherit SetAlgebra<'t>(BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None)) set_theory_axioms, ?rules = rules)

    /// The set theory over element type 't, built ONCE per element type.
    ///
    /// A generic `let` value is re-evaluated on every access, and `Theory` carries per-instance caches
    /// (the axiom-recognition cache in `Proof.fs`) that the tactics below hit hundreds of times per
    /// proof — a fresh instance per `ax_ident` would throw all of them away. A static member of a generic
    /// class is initialized once per type argument, which is exactly the lifetime wanted here.
    type private TheoryOf<'t when 't: equality>() =
        static member val Instance = SetTheory<'t>()

    let set_theory<'t when 't: equality> : SetTheory<'t> = TheoryOf<'t>.Instance

    (* ------------------------------------------------------------------------------------------
       Metatheorem (11.25), mechanized.

       Gries' Metatheorem (11.25) says a set identity `Es = Fs` is valid IFF its propositional
       translation `Ep = Fp` is (Definition 11.24: ∅↦false, U↦true, ~↦¬, ∪↦∨, ∩↦∧, and each set
       variable S ↦ its membership proposition v∈S); likewise `Es ⊆ Fs` iff `Ep ⇒ Fp`, and `Es = U`
       iff `Ep` is valid. Rather than adding that as a new TRUSTED primitive — which would import an
       out-of-kernel translation and a validity oracle into the trusted base — the tactics below
       MECHANIZE the hand proof (the one done by hand for 11.28 and De Morgan): apply Extensionality
       (11.4); recursively unfold every membership through the operator axioms 11.18/11.20/11.21/11.22,
       which literally implement 11.24; discharge the resulting propositional body with the complete
       decider `PropCalculus.decide`; close with `(∀v|:true) = true`. Every result is therefore an
       ordinary kernel-checked `Theorem` built only from already-recognized axioms.

       Completeness and soundness are inherited from `decide`, which is complete for (and only for)
       propositional tautologies on both of its routes: the tactics prove exactly the valid set
       identities/inclusions over the translated fragment, and REJECT invalid ones (the discharge
       throws). Which propositional prover closes the body is also what bounds how many set variables
       an identity may mention — `decide` routes small bodies to the in-kernel ANF prover and larger
       ones to the SAT-refutation backend when one is installed (`SatProof.install()`), so there is no
       fixed variable ceiling here.
       ------------------------------------------------------------------------------------------ *)

    /// A dummy element variable. `TermVar` is abstract and its only concrete subclass, `ScalarVar`,
    /// is constrained to value types — too narrow for a set theory over an arbitrary element type.
    type private ElemVar<'t when 't: equality>(n: string) =
        inherit TermVar<'t>(n)

    /// The shape of a set expression under Definition (11.24), extended with Difference (11.22).
    type private SetShape<'t when 't: equality> =
        | SUnion of SetTerm<'t> * SetTerm<'t>
        | SInter of SetTerm<'t> * SetTerm<'t>
        | SDiff of SetTerm<'t> * SetTerm<'t>
        | SCompl of SetTerm<'t>
        | SEmptySet
        | SUniverseSet
        /// A set variable (or any other set expression): an ATOM of the translation.
        | SAtom

    /// Classify a set expression's head. ∅ (`NewUnionCase Empty`) and U (`PropertyGet U`) are matched
    /// STRUCTURALLY, so they must be built inside a quotation — see `empty_set` / `universe` below.
    let private set_shape (s: SetTerm<'t>) : SetShape<'t> =
        let t (e: Expr) = SetTerm<'t>(Expr.Cast e)
        match expand s.Expr with
        | Call(None, Op "op_BarPlusBar", a::b::[])     -> SUnion(t a, t b)
        | Call(None, Op "op_BarMultiplyBar", a::b::[]) -> SInter(t a, t b)
        | Call(None, Op "op_BarMinusBar", a::b::[])    -> SDiff(t a, t b)
        | Call(None, Op "op_UnaryNegation", a::[])     -> SCompl(t a)
        | SetEmpty _  -> SEmptySet
        | SetUniverse -> SUniverseSet
        | _ -> SAtom

    /// ∅ as a symbolic set term. Built inside a quotation on purpose: writing `Set.Empty` outside one
    /// evaluates it to an opaque value that the `EmptyMember` axiom cannot match.
    let empty_set<'t when 't: equality> : SetTerm<'t> = SetTerm<'t>(<@ Set.Empty @>)

    /// U, the universe of discourse, as a symbolic set term (see `empty_set` on why it is quoted).
    let universe<'t when 't: equality> : SetTerm<'t> = SetTerm<'t>(<@ Set.U @>)

    /// v ∈ s, as a proposition.
    let private memb (v: TermVar<'t>) (s: SetTerm<'t>) : Prop = (v :> Term<'t>) |?| s

    /// The membership predicate `(· ∈ s)`. Used for Trading (9.2) in `meta_subset`, where the compound
    /// structure of `s` must stay untouched until the unfolding steps.
    ///
    /// Built by hand rather than as `<@ fun (z:'t) -> z |?| %s.Expr @>`: with a generic 't the compiler
    /// cannot tell whether 't is itself a `Term<_>`, so `|?|` is ambiguous between the `('t, Set<'t>)`
    /// and `(Term<'t>, Set<'t>)` overloads and inference silently degenerates 't to obj. The call built
    /// here is exactly the one the first overload builds, which is also what `memb` produces — the two
    /// must agree, since Trading rewrites one into the shape the unfolding steps then address.
    let private mem_pred (s: SetTerm<'t>) : Pred<'t> =
        let z = Var("z", typeof<'t>)
        Pred<'t>(func = (Expr.Lambda(z, binary_call(None, SetOps.elementOf<'t>, Expr.Var z, s.Expr)) |> expand_as<'t -> bool>))

    /// A membership dummy `v`, fresh with respect to `avoid` (the set expressions being translated).
    let private membership_var<'t when 't: equality> (avoid: Expr list) : TermVar<'t> =
        let rec pick i =
            let cand = ElemVar<'t>(if i = 0 then "v" else sprintf "v#%d" i) :> TermVar<'t>
            let vars = get_vars (expand cand.Expr)
            if avoid |> List.exists (Sylvia.Patterns.occurs_free vars) then pick (i + 1) else cand
        pick 0

    /// Definition (11.24), structurally: `∪↦∨`, `∩↦∧`, `~↦¬`, `∅↦false`, `U↦true`, and every other
    /// set expression (in practice a set variable) ↦ its membership atom `v∈S`.
    ///
    /// `−` is NOT in 11.24's grammar (`{set variables, ∅, U, ~, ∪, ∩}`) — carrying it to `p ∧ ¬q` is a
    /// deliberate and conservative EXTENSION, sound for the same reason Gries can remark `~S = U − S`:
    /// difference is definable from operators that ARE in the grammar (`S − T = S ∩ ~T`), so the
    /// translated body stays inside the fragment 11.25 talks about. `def_difference` below proves that
    /// defining identity THROUGH this translation, which is what distinguishes "the extension agrees
    /// with 11.22" from "the extension compiles".
    let rec translate (v: TermVar<'t>) (s: SetTerm<'t>) : Prop =
        match set_shape s with
        | SUnion(a, b) -> (translate v a) + (translate v b)          // ∪ ↦ ∨
        | SInter(a, b) -> (translate v a) * (translate v b)          // ∩ ↦ ∧
        | SDiff(a, b)  -> (translate v a) * !!(translate v b)        // − ↦ ∧¬  (Gries 11.22)
        | SCompl a     -> !!(translate v a)                          // ~ ↦ ¬
        | SEmptySet    -> F                                          // ∅ ↦ false
        | SUniverseSet -> T                                          // U ↦ true
        | SAtom        -> memb v s

    /// A rewrite rule `(v ∈ s) = translate v s`, built by a recursion that mirrors the operator axioms
    /// (11.18/11.20/11.21/11.22) and the constant-membership axioms (`v∈∅ = false`, `v∈U = true`). At
    /// each node the membership axiom is applied, then the recursion descends into any COMPOUND operand
    /// (a bare atom is already translated, so its step is skipped — that would be a no-op rewrite).
    let rec private unfold_in (th: Theory) (v: TermVar<'t>) (s: SetTerm<'t>) : Rule =
        let m (x: SetTerm<'t>) = memb v x
        let sub (x: SetTerm<'t>) addr = match set_shape x with | SAtom -> [] | _ -> [ unfold_in th v x |> at addr ]
        match set_shape s with
        | SAtom        -> ax_ident th ((m s) == (m s))                                         // reflexivity
        | SEmptySet    -> ax_ident th ((m s) == F)                                             // v∈∅ = false
        | SUniverseSet -> ax_ident th ((m s) == T)                                             // v∈U = true
        | SCompl a ->
            ident th ((m s) == (translate v s))
                ((ax_ident th ((m s) == (!!(m a))) |> at_left) :: sub a [left_branch; apply_unary])
        | SUnion(a, b) ->
            ident th ((m s) == (translate v s))
                ((ax_ident th ((m s) == ((m a) + (m b))) |> at_left)
                    :: sub a [left_branch; left_branch] @ sub b [left_branch; right_branch])
        | SInter(a, b) ->
            ident th ((m s) == (translate v s))
                ((ax_ident th ((m s) == ((m a) * (m b))) |> at_left)
                    :: sub a [left_branch; left_branch] @ sub b [left_branch; right_branch])
        // Difference's right operand sits under the ¬ that 11.22 introduces, hence the extra
        // `apply_unary` descent — the same one the complement case makes.
        | SDiff(a, b) ->
            ident th ((m s) == (translate v s))
                ((ax_ident th ((m s) == ((m a) * !!(m b))) |> at_left)
                    :: sub a [left_branch; left_branch] @ sub b [left_branch; right_branch; apply_unary])

    /// The rewrite rule `(v ∈ s) = translate v s` (see `translate`), in the set theory over 't.
    let unfold (v: TermVar<'t>) (s: SetTerm<'t>) : Rule = unfold_in (set_theory<'t> :> Theory) v s

    /// Metatheorem (11.25a): prove a set identity `Es = Fs` via its propositional translation.
    ///
    /// Extensionality (11.4) takes the goal to `(∀v |: v∈Es = v∈Fs)`; each side is unfolded to its
    /// translation by `unfold`; the body `Ep = Fp` is discharged by `decide` and folded in with
    /// `Taut'`; `(∀v|:true) = true` closes. Raises if the identity is not valid.
    ///
    /// Metatheorem (11.25c) — `Es = U` valid iff `Ep` is valid — needs no separate tactic: it is
    /// `meta_set_ident Es universe`, whose body reduces to `Ep = true`.
    let meta_set_ident (lhs: SetTerm<'t>) (rhs: SetTerm<'t>) : Theorem =
        let th = set_theory<'t> :> Theory
        let v = membership_var<'t> [ expand lhs.Expr; expand rhs.Expr ]
        let goal = lhs == rhs
        let ext = ax_ident th (goal == qall v T ((memb v lhs) == (memb v rhs)))          // Extensionality 11.4
        let stepL = match set_shape lhs with | SAtom -> [] | _ -> [ unfold_in th v lhs |> at [select_body; left_branch] ]
        let stepR = match set_shape rhs with | SAtom -> [] | _ -> [ unfold_in th v rhs |> at [select_body; right_branch] ]
        let body = decide ((translate v lhs) == (translate v rhs)) |> Ident            // Ep = Fp (complete)
        theorem th goal ([ ext ] @ stepL @ stepR @ [ Taut' body |> at [select_body]; ident_forall_true' v ])

    /// Metatheorem (11.25b): prove a set inclusion `Es ⊆ Fs` via the implication `Ep ⇒ Fp` — Gries
    /// (11.56), one set is a subset of another iff its characteristic predicate implies the other's.
    ///
    /// The goal is a bare proposition rather than an equality, so it is reduced to `true`: Subset
    /// (11.13) gives `(∀v | v∈Es : v∈Fs)`; Trading (9.2) gives `(∀v |: v∈Es ⇒ v∈Fs)` — using the
    /// SIMPLE membership predicates, so the compound structure is untouched until the unfolding steps;
    /// each side of the implication is unfolded; `decide` proves the body and `Taut` (not `Taut'` — the
    /// body is an implication, not an equality) replaces it with `true`. Raises if `lhs ⊄ rhs`.
    let meta_subset (lhs: SetTerm<'t>) (rhs: SetTerm<'t>) : Theorem =
        let th = set_theory<'t> :> Theory
        let v = membership_var<'t> [ expand lhs.Expr; expand rhs.Expr ]
        let goal = lhs |<| rhs
        let sub_ax = ax_ident th (goal == qall v (memb v lhs) (memb v rhs))               // Subset 11.13
        let trade = trade_forall_implies v (mem_pred lhs) (mem_pred rhs)               // Trading 9.2
        let stepA = match set_shape lhs with | SAtom -> [] | _ -> [ unfold_in th v lhs |> at [select_body; left_branch] ]
        let stepC = match set_shape rhs with | SAtom -> [] | _ -> [ unfold_in th v rhs |> at [select_body; right_branch] ]
        let body = decide ((translate v lhs) ==> (translate v rhs))                    // Ep ⇒ Fp (complete)
        theorem th goal ([ sub_ax; trade ] @ stepA @ stepC @ [ Taut body |> at [select_body]; ident_forall_true' v ])

    /// `T ∈ 𝒫S` (Gries 11.23, power set), by composing the two metatheorem tactics.
    ///
    /// The power set is the one operator of the chapter that does NOT fit Metatheorem 11.25: membership
    /// in `𝒫S` does not reduce to a propositional combination of memberships of the same element — it
    /// reduces to a SUBSET proposition, itself a `∀` over a different element, and the type climbs
    /// (`𝒫S : set(set(t))`). So it sits one layer up: 11.23 takes the goal DOWN to a subset obligation,
    /// which 11.25(b) then discharges.
    let powerset_member (t: SetTerm<'t>) (s: SetTerm<'t>) : Theorem =
        let th = set_theory<'t> :> Theory
        // `t |?| s.Powerset` is ambiguous when 't is a type variable: `SetTerm<'t>` IS a
        // `Term<Set<'t>>`, so both the `(Term<'u>, SetTerm<'u>)` and the `(SetTerm<'u>, SetTerm<Set<'u>>)`
        // overloads apply, and inference degenerates 't to obj. Both build the same call
        // (`SetOps.elementOf<Set<'t>>`); the upcast picks the first and keeps this generic.
        let goal = (t :> Term<Set<'t>>) |?| s.Powerset
        theorem th goal [ ax_ident th (goal == (t |<| s)) |> apply       // 11.23: down to a subset obligation
                          Taut (meta_subset t s) |> apply ]            // 11.25(b) discharges it

    (* ------------------------------------------------------------------------------------------
       §11.3 — the named laws of set algebra, each one call of a metatheorem tactic. This is the
       object-level payoff of Gries §11.3: `(set, ∪, ∩, ~, ∅, U)` is a Boolean algebra mirroring
       `(bool, ∨, ∧, ¬, false, true)`, so every propositional law has a set counterpart, obtained
       here through the translation rather than postulated.
       ------------------------------------------------------------------------------------------ *)

    /// ~~S = S   (Gries 11.19, double complement)
    let double_complement (s: SetTerm<'t>) : Theorem = meta_set_ident (-(-s)) s

    /// S ∪ T = T ∪ S   (Gries 11.26, symmetry of ∪)
    let symm_union (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_set_ident (s + t) (t + s)

    /// (S ∪ T) ∪ U = S ∪ (T ∪ U)   (Gries 11.27, associativity of ∪)
    let assoc_union (s: SetTerm<'t>) (t: SetTerm<'t>) (u: SetTerm<'t>) : Theorem =
        meta_set_ident ((s + t) + u) (s + (t + u))

    /// S ∪ S = S   (Gries 11.28, idempotency of ∪)
    let idemp_union (s: SetTerm<'t>) : Theorem = meta_set_ident (s + s) s

    /// S ∪ U = U   (Gries 11.29, zero of ∪)
    let zero_union (s: SetTerm<'t>) : Theorem = meta_set_ident (s + universe<'t>) universe<'t>

    /// S ∪ ∅ = S   (Gries 11.30, identity of ∪)
    let ident_union (s: SetTerm<'t>) : Theorem = meta_set_ident (s + empty_set<'t>) s

    /// S ∪ ~S = U   (Gries 11.32, excluded middle)
    let excluded_middle_union (s: SetTerm<'t>) : Theorem = meta_set_ident (s + (-s)) universe<'t>

    /// S ∩ U = S   (Gries 11.34, identity of ∩)
    let ident_inter (s: SetTerm<'t>) : Theorem = meta_set_ident (s * universe<'t>) s

    /// S ∩ ∅ = ∅   (Gries 11.35, zero of ∩)
    let zero_inter (s: SetTerm<'t>) : Theorem = meta_set_ident (s * empty_set<'t>) empty_set<'t>

    /// S ∩ T = T ∩ S   (Gries 11.36, symmetry of ∩)
    let symm_inter (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_set_ident (s * t) (t * s)

    /// (S ∩ T) ∩ U = S ∩ (T ∩ U)   (associativity of ∩, the dual of 11.27)
    let assoc_inter (s: SetTerm<'t>) (t: SetTerm<'t>) (u: SetTerm<'t>) : Theorem =
        meta_set_ident ((s * t) * u) (s * (t * u))

    /// S ∩ S = S   (idempotency of ∩, the dual of 11.28)
    let idemp_inter (s: SetTerm<'t>) : Theorem = meta_set_ident (s * s) s

    /// S ∩ ~S = ∅   (Gries 11.39, contradiction)
    let contradiction_inter (s: SetTerm<'t>) : Theorem = meta_set_ident (s * (-s)) empty_set<'t>

    /// S ∩ (T ∪ U) = (S ∩ T) ∪ (S ∩ U)   (Gries 11.40, distributivity of ∩ over ∪)
    let distrib_inter_union (s: SetTerm<'t>) (t: SetTerm<'t>) (u: SetTerm<'t>) : Theorem =
        meta_set_ident (s * (t + u)) ((s * t) + (s * u))

    /// S ∪ (T ∩ U) = (S ∪ T) ∩ (S ∪ U)   (Gries 11.41, distributivity of ∪ over ∩)
    let distrib_union_inter (s: SetTerm<'t>) (t: SetTerm<'t>) (u: SetTerm<'t>) : Theorem =
        meta_set_ident (s + (t * u)) ((s + t) * (s + u))

    /// ~(S ∪ T) = ~S ∩ ~T   (Gries 11.42a, De Morgan)
    let de_morgan_union (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem =
        meta_set_ident (-(s + t)) ((-s) * (-t))

    /// ~(S ∩ T) = ~S ∪ ~T   (Gries 11.42b, De Morgan)
    let de_morgan_inter (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem =
        meta_set_ident (-(s * t)) ((-s) + (-t))

    /// S ∩ (S ∪ T) = S   (absorption)
    let absorb_inter_union (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_set_ident (s * (s + t)) s

    /// S ∪ (S ∩ T) = S   (absorption, dual)
    let absorb_union_inter (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_set_ident (s + (s * t)) s

    (* Inclusions — Metatheorem 11.25(b). *)

    /// S ⊆ S   (Gries 11.58, reflexivity of ⊆)
    let subset_refl (s: SetTerm<'t>) : Theorem = meta_subset s s

    /// S ∩ T ⊆ S   (∩ is a lower bound)
    let inter_lower_left (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_subset (s * t) s

    /// S ∩ T ⊆ T   (∩ is a lower bound)
    let inter_lower_right (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_subset (s * t) t

    /// S ⊆ S ∪ T   (∪ is an upper bound)
    let union_upper_left (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_subset s (s + t)

    /// T ⊆ S ∪ T   (∪ is an upper bound)
    let union_upper_right (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_subset t (s + t)

    /// S ∩ T ⊆ S ∪ T
    let inter_subset_union (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_subset (s * t) (s + t)

    (* Difference (11.22). `−` is outside Definition 11.24's grammar, so `def_difference` is the law
       that EARNS the extension: it proves difference's defining identity through the very translation
       being justified, so a `SDiff` case disagreeing with 11.22 fails here rather than quietly proving
       something else. *)

    /// S − T = S ∩ ~T   (Gries 11.22, difference as intersection with the complement)
    let def_difference (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_set_ident (s - t) (s * (-t))

    /// ~S = U − S   (Gries p.203)
    let complement_as_difference (s: SetTerm<'t>) : Theorem = meta_set_ident (-s) (universe<'t> - s)

    /// U − S = ~S
    let difference_from_universe (s: SetTerm<'t>) : Theorem = meta_set_ident (universe<'t> - s) (-s)

    /// S − S = ∅
    let self_difference (s: SetTerm<'t>) : Theorem = meta_set_ident (s - s) empty_set<'t>

    /// S − ∅ = S
    let ident_difference (s: SetTerm<'t>) : Theorem = meta_set_ident (s - empty_set<'t>) s

    /// ∅ − S = ∅
    let zero_difference (s: SetTerm<'t>) : Theorem = meta_set_ident (empty_set<'t> - s) empty_set<'t>

    /// S − (T ∪ U) = (S − T) ∩ (S − U)   (De Morgan over difference)
    let de_morgan_difference_union (s: SetTerm<'t>) (t: SetTerm<'t>) (u: SetTerm<'t>) : Theorem =
        meta_set_ident (s - (t + u)) ((s - t) * (s - u))

    /// S − (T ∩ U) = (S − T) ∪ (S − U)   (De Morgan over difference)
    let de_morgan_difference_inter (s: SetTerm<'t>) (t: SetTerm<'t>) (u: SetTerm<'t>) : Theorem =
        meta_set_ident (s - (t * u)) ((s - t) + (s - u))

    /// (S ∪ T) − U = (S − U) ∪ (T − U)
    let distrib_difference_union (s: SetTerm<'t>) (t: SetTerm<'t>) (u: SetTerm<'t>) : Theorem =
        meta_set_ident ((s + t) - u) ((s - u) + (t - u))

    /// S ∩ (T − U) = (S ∩ T) − U
    let assoc_inter_difference (s: SetTerm<'t>) (t: SetTerm<'t>) (u: SetTerm<'t>) : Theorem =
        meta_set_ident (s * (t - u)) ((s * t) - u)

    /// S − T ⊆ S
    let difference_subset (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = meta_subset (s - t) s

    /// S − T ⊆ ~T   (a difference is disjoint from what was removed)
    let difference_subset_complement (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem =
        meta_subset (s - t) (-t)

    (* Power set (11.23). *)

    /// ∅ ∈ 𝒫S
    let empty_in_powerset (s: SetTerm<'t>) : Theorem = powerset_member empty_set<'t> s

    /// S ∈ 𝒫S   (from reflexivity 11.58)
    let self_in_powerset (s: SetTerm<'t>) : Theorem = powerset_member s s

    /// S ∩ T ∈ 𝒫S   (from the ∩ lower bound)
    let inter_in_powerset (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = powerset_member (s * t) s

    /// S − T ∈ 𝒫S   (from the difference bound)
    let difference_in_powerset (s: SetTerm<'t>) (t: SetTerm<'t>) : Theorem = powerset_member (s - t) s