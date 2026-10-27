namespace Sylvia

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Formula
open Patterns
open Descriptions

type IntExpr = Expr<int>

/// Theory of operations on an integral domain of integers with binary operations (+) and (*) and (*) distributes over (+), 
/// identities 0 and 1, and unary inverse operation (-), where c <> 0  ==> (c * a = c * b = (a = b)). 
/// < is defined by a < b = (b - a) |?| Zpos  
module Integers =      
    (* Symbols *)
    do Symbols.BulitIn.Add(src <@ (*) @>, "\u22C5")
    
    let desc = axiom_desc "Integers"
    
    (* Patterns *)
    
    let (|Zpos|_|) =
        function
        | ValueWithName(z, t, n) when t = typeof<OrderedRing<int>> && (z :?> OrderedRing<int>) = Ring.Zpos -> Some()
        | _ -> None

    (* Axioms *)

    let (|DefLessThan|_|) =
        function
        | Equals(LessThan(l,r), SetTheory.ElementOf(Subtract(r', l'), Zpos)) when sequal2 l r l' r' -> pattern_name "Definition of <" |> Some
        | _ -> None

    // Reflexivity, Transitivity, Translation of ≤ and "a successor is not ≤" were AXIOMS here until
    // 2026-10-27. They are now THEOREMS of the ordered domain (`OrderedDomain`, Gries 15.30–15.46) that
    // `Integers` is — see `le_refl`, `le_trans`, `le_translate`, `succ_not_le` below. What remains is
    // what an ordered domain does not give: discreteness, and evaluation on literals.

    /// `(x ≤ y + 1) = (x ≤ y ∨ x = y + 1)` — DISCRETENESS of the integers: nothing lies strictly
    /// between `y` and `y + 1`.
    ///
    /// The real content of splitting the last term off an integer range (Gries 8.23), and the fact
    /// that makes it an INTEGER theorem: over the reals it is false. Restricted to `int` for that
    /// reason. An ordered domain does not give it (ℚ is one); Gries gets it from Well ordering
    /// (15.49), as (15.50) "no element between 0 and 1". Until that is in, it is stated directly.
    let (|DiscreteLessEq|_|) =
        function
        | Equals(LessEq(x, Add(y, Int32 1)), Or(LessEq(x', y'), Equals(x'', Add(y'', Int32 1))))
            when x.Type = typeof<int> && sequal x x' && sequal x' x'' && sequal y y' && sequal y' y'' ->
            pattern_name "Discreteness of ≤" |> Some
        | _ -> None

    /// `(a ≤ b) = true` or `= false` for integer LITERALS `a`, `b`, by evaluation.
    ///
    /// The same trust as `reduce` folding `2 * 0` to `0` — but an axiom rather than another case of
    /// `reduce`, so proofs that already run `reduce` do not change shape. Only the correct truth
    /// value is recognized: `(1 ≤ 0) = true` is not an axiom.
    let (|LiteralLessEq|_|) =
        function
        | Equals(LessEq(Int32 a, Int32 b), True) when a <= b -> pattern_name "Comparison of integer literals" |> Some
        | Equals(LessEq(Int32 a, Int32 b), False) when a > b -> pattern_name "Comparison of integer literals" |> Some
        | _ -> None

    /// `(a = b) = false` for DISTINCT integer literals `a`, `b`, by evaluation — the same trust as
    /// literal comparison. It is what makes `1 ≠ 0` a theorem, which an ordered domain needs (the
    /// one-element ring satisfies every other axiom, and in it nothing is positive).
    let (|LiteralEq|_|) =
        function
        | Equals(Equals(Int32 a, Int32 b), False) when a <> b -> pattern_name "Comparison of integer literals" |> Some
        | _ -> None

    // Hoisted operator templates: quotation literals written inline in match arms are
    // re-deserialized on every axiom probe (see docs/expressions-perf.md).
    let private eq_op : Expr<int->int->bool> = <@ (=) @>
    let private add_op : Expr<int->int->int> = <@ (+) @>
    let private mul_op : Expr<int->int->int> = <@ (*) @>
    let private sub_op : Expr<int->int->int> = <@ (-) @>
    let private neg_op : Expr<int->int> = <@ (~-) @>
    let private zero_int : Expr<int> = <@ 0 @>
    let private one_int : Expr<int> = <@ 1 @>

    /// The integers' own axioms, ON TOP OF those of an ordered domain (`OrderedDomain`, Gries
    /// 15.1–15.7, 15.13, 15.14, 15.30–15.33, 15.36–15.39), which `Integers` inherits: the ring and
    /// order laws live there now, once. What stays here is what is particular to ℤ — discreteness,
    /// and evaluation of comparisons between literals — plus two older forms kept because
    /// existing proofs may cite them: additive cancellation at a non-zero term (a weaker form of
    /// 15.8) and an additive-inverse ∃ in the shape `(∃x |: v + x = 0)`.
    let integer_axioms =
        function
        | LeftCancelNonZero add_op zero_int x -> Some (desc x)
        | Exists(_, a::[], Bool true, (Equals(Add(Var _, Var a'), Int32 0))) when vequal a a' -> Some (axiom_desc "Integers" (pattern_name "Additive Inverse")) 
        | DefLessThan x
        | DiscreteLessEq x
        | LiteralLessEq x
        | LiteralEq x -> Some (desc x)
        | _ -> None

    (* Expression functions for derived rules *)

    // NB the folded VALUE is built with `Expr.Value`, not with a `<@@ l + r @@>` quotation. A
    // quotation over the pattern-bound `l`/`r` captures them as `ValueWithName(v, "l")` operands and
    // rebuilds the CALL — so `2 * 0` became `l * r` (values 2 and 0, unevaluated) and nothing was
    // ever reduced. Measured on `2 * 0` before the fix; the rule was inert at every type.
    let rec _reduce_constants  =
        function
        // Unary minus of a literal is evaluated too, so the normalizer can turn the `−1` of (15.20)'s
        // `−a = (−1)·a` into a literal coefficient.
        | Negate(Int32 n) -> Expr.Value(-n)
        | Negate(Int64 n) -> Expr.Value(-n)
        | Negate(Double n) -> Expr.Value(-n)
        | Negate(Single n) -> Expr.Value(-n)
        | Negate(Decimal n) -> Expr.Value(-n)

        | Add(UInt16 l, UInt16 r) -> Expr.Value(l + r)
        | Multiply(UInt16 l, UInt16 r) -> Expr.Value(l * r)        
        | Subtract(UInt16 l, UInt16 r) -> Expr.Value(l - r)

        | Add(Int16 l, Int16 r) -> Expr.Value(l + r)
        | Multiply(Int16 l, Int16 r) -> Expr.Value(l * r)        
        | Subtract(Int16 l, Int16 r) -> Expr.Value(l - r)

        | Add(UInt32 l, UInt32 r) -> Expr.Value(l + r)
        | Multiply(UInt32 l, UInt32 r) -> Expr.Value(l * r)        
        | Subtract(UInt32 l, UInt32 r) -> Expr.Value(l - r)

        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
        | Multiply(Int32 l, Int32 r) -> Expr.Value(l * r)        
        | Subtract(Int32 l, Int32 r) -> Expr.Value(l - r)
        
        | Add(UInt64 l, UInt64 r) -> Expr.Value(l + r)
        | Multiply(UInt64 l, UInt64 r) -> Expr.Value(l * r)        
        | Subtract(UInt64 l, UInt64 r) -> Expr.Value(l - r)

        | Add(Int64 l, Int64 r) -> Expr.Value(l + r)
        | Multiply(Int64 l, Int64 r) -> Expr.Value(l * r)        
        | Subtract(Int64 l, Int64 r) -> Expr.Value(l - r)

        | Add(Rational l, Rational r) -> Expr.Value(l + r)
        | Multiply(Rational l, Rational r) -> Expr.Value(l * r)        
        | Subtract(Rational l, Rational r) -> Expr.Value(l - r)

        | Add(Single l, Single r) -> Expr.Value(l + r)
        | Multiply(Single l, Single r) -> Expr.Value(l * r)        
        | Subtract(Single l, Single r) -> Expr.Value(l - r)

        | Add(Double l, Double r) -> Expr.Value(l + r)
        | Multiply(Double l, Double r) -> Expr.Value(l * r)        
        | Subtract(Double l, Double r) -> Expr.Value(l - r)
        
        | Add(Decimal l, Decimal r) -> Expr.Value(l + r)
        | Multiply(Decimal l, Decimal r) -> Expr.Value(l * r)        
        | Subtract(Decimal l, Decimal r) -> Expr.Value(l - r)

        | expr -> traverse expr _reduce_constants

    // Both right-associativity rules used to rewrite `(a·b)·c` to `a·(b + c)` — an ADDITION where the
    // multiplication belongs — so the kernel accepted `(2·3)·4 = 2·(3+4)`, i.e. 24 = 14. The addition
    // rule also had no business touching products; like its `_left_assoc_add` sibling it now has only
    // the `+` case.
    let rec _right_assoc_add =
        function
        | Add(Add(a1, a2), a3) -> call_add a1 (call_add a2 a3)
        | expr -> traverse expr _right_assoc_add

    let rec _right_assoc_mul =
        function
        | Multiply(Multiply(a1, a2), a3) -> call_mul a1 (call_mul a2 a3)
        | expr -> traverse expr _right_assoc_mul

    let rec _left_assoc_add =
        function
        | Add(a1, Add(a2, a3)) -> call_add (call_add a1 a2) a3
        | expr -> traverse expr _left_assoc_add

    let rec _left_assoc_mul =
        function
        | Multiply(a1, Multiply(a2, a3)) -> call_mul (call_mul a1 a2) a3
        | expr -> traverse expr _left_assoc_mul

    /// `(a + b) − c → a + (b − c)`. No other rule reassociates across a subtraction, so without it
    /// `(2·n + 2) − 1` cannot become `2·n + 1`.
    let rec _assoc_add_sub =
        function
        | Subtract(Add(a1, a2), a3) -> call_add a1 (call_sub a2 a3)
        | expr -> traverse expr _assoc_add_sub

    let rec _commute_add =
        function
        | Add(a1, a2) -> call_add a2 a1
        | expr -> traverse expr _commute_add

    let rec _commute_mul =
        function
        | Multiply(a1, a2) -> call_mul a2 a1
        | expr -> traverse expr _commute_mul

    let rec _distrib_mul_add =
        function
        | Multiply(a1, Add(a2, a3)) -> call_add (call_mul a1 a2)  (call_mul a1 a3) 
        | expr -> traverse expr _distrib_mul_add

    let rec _distrib_mul_sub =
        function 
        | Multiply(a1, Subtract(a2, a3)) -> call_sub (call_mul a1 a2) (call_mul a1 a3)
        | expr -> traverse expr _distrib_mul_sub

    let rec _collect_mul_add =
        function
        | Add(Multiply(a1, a2), Multiply(a1', a3)) when sequal a1 a1' -> call_mul a1 (call_add a2 a3) 
        | Subtract(Multiply(a1, a2), Multiply(a1', a3)) when sequal a1 a1' -> call_mul a1 (call_sub a2 a3) 
        | expr -> traverse expr _collect_mul_add

    let rec _collect_mul_sub =
         function
         | Subtract(Multiply(a1, a2), Multiply(a1', a3)) when sequal a1 a1' -> call_mul a1 (call_sub a2 a3) 
         | expr -> traverse expr _collect_mul_sub

    let rec _ident_add =
        function
        | Add(x, Int32 0) -> x
        | expr -> traverse expr _ident_add

    let rec _ident_mul =
        function
        | Multiply(x, Int32 1) -> x
        | expr -> traverse expr _ident_mul

    (* Admitted rules *)

    /// Reduce equal constants in expression. 
    let reduce = Admit("Reduce integer constants in (expression)", _reduce_constants)

    /// Addition is right associative.
    let right_assoc_add = Admit("Addition operations in (expression) are right-associative", _right_assoc_add)

    /// Multiplication is right associative.
    let right_assoc_mul = Admit("Multplications operations in (expression) is right-associative", _right_assoc_mul)

    /// Addition is left associative.
    let left_assoc_add = Admit("Addition operations in (expression) are left-associative", _left_assoc_add)

    /// Multiplication is left associative.
    let left_assoc_mul = Admit("Multiplication operations in (expression) are left-associative", _left_assoc_mul)

    /// (a + b) − c is a + (b − c).
    let assoc_add_sub = Admit("(a + b) − c is a + (b − c) in (expression)", _assoc_add_sub)

    /// Addition is comutative.
    let commute_add = Admit("Addition operation in (expression) is commutative", _commute_add)

    /// Multiplication is comutative.
    let commute_mul = Admit("Multiplication operation in (expression) is commutative", _commute_mul)

    /// Multiplication is distributive over addition.
    let distrib_mul_add = Admit("Multipication distributes over addition in (expression)", _distrib_mul_add)

    /// Multiplication is distributive over subtraction.
    let distrib_mul_sub = Admit("Multipication distributes over addition in (expression)", _distrib_mul_sub)

    /// Collect multiplication terms distributed over addition.
    let collect_mul_add = Admit("Collect multiplication terms distributed over addition in (expression)", _collect_mul_add)

    /// Collect multiplication terms distributed over subtraction.
    let collect_mul_sub = Admit("Collect multiplication terms distributed over addition in (expression)", _collect_mul_sub)

    /// Zero is the identity of the addition operation.
    let ident_add = Admit("0 is the identity of the addition operation in (expression)", _ident_add)

    /// One is the identity of the multiplication operation.
    let ident_mul = Admit("1 is the identity of the multiplication operation in (expression)", _ident_mul)

    /// The integers as an integral domain: `+`, `·`, `0`, `1`, unary and binary `−` at `int`.
    let int_ops : IntegralDomain.DomainOps<int> =
        { Name = "Integers"; Add = <@ (+) @>; Mul = <@ (*) @>; Zero = <@ 0 @>; One = <@ 1 @>; Neg = <@ (~-) @>; Sub = <@ (-) @> }

    /// The integers as an ordered domain: `pos` and the relations it defines, at `int`.
    let int_order : OrderedDomain.OrderOps<int> =
        { Pos = <@ OrderedDomain.pos<int> @>; Lt = <@ (<) @>; Gt = <@ (>) @>; Le = <@ (<=) @>; Ge = <@ (>=) @> }

    /// The theory of integers: an ordered domain at `int` (Gries §15.1), plus discreteness, literal
    /// evaluation and the rewrite rules below. A caller's extra axioms and rules go on top —
    /// `Induction` is one.
    type Integers(?axioms: Axioms, ?rules: Rules) = inherit OrderedDomain.OrderedDomain<int>(int_ops, int_order,
        BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None)) integer_axioms, [
        reduce
        right_assoc_add
        right_assoc_mul
        left_assoc_add
        left_assoc_mul
        commute_add
        commute_mul
        distrib_mul_add
        distrib_mul_sub
        collect_mul_add
        collect_mul_sub
        ident_add
        ident_mul
        assoc_add_sub
    ] @ defaultArg rules [])
    
    let integers = Integers()

    (* ------------------------------------------------------------------------------------------
       The order laws, as THEOREMS. Each is derived once per theory at schema metavariables
       (`Tactics.Schema`) from the ordered-domain theorems, and instantiated by `InstantiateTerms`.
       They replace what were four axioms of this theory.
       ------------------------------------------------------------------------------------------ *)

    type private OrderLemmas =
        { Refl: Expr<int> -> Rule
          Trans: Expr<int> -> Expr<int> -> Expr<int> -> Theorem
          Translate: Expr<int> -> Expr<int> -> Rule
          SuccNotLe: Expr<int> -> Rule
          NonnegAdd: Expr<int> -> Expr<int> -> Theorem }

    let private order_lemmas = System.Runtime.CompilerServices.ConditionalWeakTable<Theory, OrderLemmas>()

    /// `th` as an ordered domain at `int` — which every `Integers` (and `Induction`) theory is.
    let order_domain (th: Theory) : OrderedDomain.OrderedDomain<int> =
        match th with
        | :? OrderedDomain.OrderedDomain<int> as d -> d
        | _ -> failwithf "The integer order laws need an ordered domain at int (e.g. Integers); %s is not one." (th.GetType().Name)

    /// `¬(1 = 0)` — by literal evaluation.
    let one_ne_zero (th: Theory) : Theorem =
        let one0 = Prop(<@ 1 = 0 @>)
        lemma th (!!one0) [
            PropCalculus.ident_eq_false one0 |> PropCalculus.Commute |> apply    // ¬(1 = 0) → ((1 = 0) = false)
        ]

    let private order_of (th: Theory) : OrderLemmas =
        order_lemmas.GetValue(th, fun _ ->
            let d = order_domain th
            let one : Expr<int> = <@ 1 @>
            { Refl = Tactics.Schema.t1 "le_refl" (OrderedDomain.le_refl d)
              Trans = Tactics.Schema.th3 "le_trans" (OrderedDomain.le_trans d)
              Translate = Tactics.Schema.t2 "le_translate" (fun a b -> OrderedDomain.le_translate d a b one)
              SuccNotLe = Tactics.Schema.t1 "succ_not_le" (fun a -> Ident (PropCalculus.mp (OrderedDomain.succ_not_le d a) (one_ne_zero th)))
              NonnegAdd = Tactics.Schema.th2 "nonneg_add" (OrderedDomain.nonneg_add d) })

    /// `(x ≤ x) = true` — Reflexivity (15.46).
    let le_refl (th: Theory) (x: Term<int>) : Rule = (order_of th).Refl x.Expr
    /// `(x ≤ y ∧ y ≤ z) ⇒ x ≤ z` — Transitivity (15.41d).
    let le_trans (th: Theory) (x: Term<int>) (y: Term<int>) (z: Term<int>) : Theorem = (order_of th).Trans x.Expr y.Expr z.Expr
    /// `(x ≤ y) = (x + 1 ≤ y + 1)` — Monotonicity (15.42) for ≤, by one.
    let le_translate (th: Theory) (x: Term<int>) (y: Term<int>) : Rule = (order_of th).Translate x.Expr y.Expr
    /// `(x + 1 ≤ x) = false`.
    let succ_not_le (th: Theory) (x: Term<int>) : Rule = (order_of th).SuccNotLe x.Expr
    /// `(0 ≤ x ∧ 0 ≤ y) ⇒ 0 ≤ x + y`.
    let nonneg_add (th: Theory) (x: Term<int>) (y: Term<int>) : Theorem = (order_of th).NonnegAdd x.Expr y.Expr

    (* ------------------------------------------------------------------------------------------
       Ring normalization — Gries' "(Arithmetic)" step, as a tactic that PRODUCES A PROOF.

       `ring_ident th L R` proves an integer identity `L = R` by rewriting both sides to one
       canonical polynomial and closing on syntactic equality. Every rewrite is an EXISTING rule
       of this theory applied at an explicit address, a theorem of the integral domain — (15.9) zero,
       (15.14) subtraction, (15.20) `−a = (−1)·a` — or the Identity axiom right-to-left through a local
       lemma, and the kernel re-checks every step. This code only SEARCHES: it simulates each
       step with the very function the kernel will run, to know where the next one goes. A bug here
       makes the tactic FAIL; it cannot make it prove something false. Nothing is added to the trusted
       base beyond the associativity / commutativity / distributivity / identity / constant-folding
       rules it uses, all value-checked; minus and zero are handled by theorems, not trusted rules.

       Canonical form: a right-nested sum of monomials, sorted by their atoms, constants last; each
       monomial is its atoms, sorted and left-nested, times a literal coefficient — `((x·y)·z)·c`,
       with `c = 1` written explicitly. An ATOM is anything that is not `+`, `−`, `·`, unary `−` or an
       integer literal (a variable, `f n`, a Σ, …); the normalizer never looks inside one.

       Phases: eliminate `−`; distribute `·` over `+`; flatten sums; per monomial, flatten, sort its
       factors, fold its literals into one trailing coefficient (adding `·1` if there is none) and
       left-nest its atoms; sort the monomials; combine like terms; drop zeros.
       ------------------------------------------------------------------------------------------ *)

    type private Dir = DL | DR | DU

    let rec private getAt (e: Expr) (path: Dir list) =
        match path, e with
        | [], _ -> e
        | DL :: rest, Call(None, _, [l; _]) -> getAt l rest
        | DR :: rest, Call(None, _, [_; r]) -> getAt r rest
        | DU :: rest, Call(None, _, [x]) -> getAt x rest
        | _ -> failwithf "ring: no subterm at that path in %s" (src e)

    // Rebuilds exactly as the kernel's LeftBranch / RightBranch / ApplyUnary do.
    let rec private setAt (e: Expr) (path: Dir list) (v: Expr) =
        match path, e with
        | [], _ -> v
        | DL :: rest, Call(None, m, [l; r]) -> Expr.Call(m, [setAt l rest v; r])
        | DR :: rest, Call(None, m, [l; r]) -> Expr.Call(m, [l; setAt r rest v])
        | DU :: rest, Call(None, m, [x]) -> Expr.Call(m, [setAt x rest v])
        | _ -> failwithf "ring: no subterm at that path in %s" (src e)

    let private isLit = function Int32 _ -> true | _ -> false

    /// The first node, in pre-order and never inside an atom, satisfying `pred`.
    let rec private findPath (pred: Expr -> bool) (e: Expr) : Dir list option =
        if pred e then Some []
        else
            match e with
            | Add(l, r) | Subtract(l, r) | Multiply(l, r) ->
                match findPath pred l with
                | Some p -> Some (DL :: p)
                | None -> findPath pred r |> Option.map (fun p -> DR :: p)
            | Negate x -> findPath pred x |> Option.map (fun p -> DU :: p)
            | _ -> None

    /// Atom order: by structural print. Equal atoms print equally; a collision between distinct atoms
    /// could only mis-sort, which makes the tactic fail, never succeed wrongly.
    let private atomKey (e: Expr) = string e

    /// The factors of a right-nested product, in order.
    let rec private factors (e: Expr) = match e with Multiply(a, b) -> a :: factors b | x -> [ x ]

    /// The atoms of a normalized monomial `((x·y)·z)·c` (or a literal: none), for comparing like terms.
    let private monoAtoms (e: Expr) : Expr list =
        let rec leftAtoms = function Multiply(a, b) -> leftAtoms a @ [ b ] | x -> [ x ]
        match e with
        | Multiply(atoms, c) when isLit c -> leftAtoms atoms
        | x when isLit x -> []
        | x -> [ x ]

    let private monoKey (e: Expr) =
        match monoAtoms e with
        | [] -> "￿"                                   // constants last
        | atoms -> atoms |> List.map atomKey |> String.concat "\u0001"

    let private sameAtoms (a: Expr) (b: Expr) =
        let xs, ys = monoAtoms a, monoAtoms b
        not xs.IsEmpty && xs.Length = ys.Length && List.forall2 sequal xs ys

    // The (15.9) and (15.20) instances the normalizer uses are THEOREMS of the integral domain, and a
    // theorem called at new arguments replays its derivation. So each is derived ONCE per theory, at a
    // schema metavariable, and instantiated per use (`Tactics.Schema.t1`, Gries' Substitution rule).
    // (15.14) is an axiom instance, which costs nothing to state.
    type private RingLemmas = { Neg: Expr<int> -> Rule; Zero: Expr<int> -> Rule }
    let private lemmas = System.Runtime.CompilerServices.ConditionalWeakTable<Theory, RingLemmas>()

    /// Normalize the integer term `e` found at `prefix` inside the goal; returns the steps and the
    /// normal form.
    let private normalize (th: Theory) (prefix: Dir list) (e: Expr) : RuleApplication list * Expr =
        let dom =
            match th with
            | :? IntegralDomain.IntegralDomain<int> as d -> d
            | _ -> failwithf "ring: %s is not an integral domain over int" (th.GetType().Name)
        let lem =
            lemmas.GetValue(th, fun _ ->
                { Neg = Tactics.Schema.t1 "neg" (IntegralDomain.neg_as_mul dom)
                  Zero = Tactics.Schema.t1 "zero" (IntegralDomain.zero_mul dom) })
        let mutable cur = expand e
        let steps = ResizeArray<RuleApplication>()
        let nav (path: Dir list) = (prefix @ path) |> List.map (function DL -> left_branch | DR -> right_branch | DU -> apply_unary)
        let step (path: Dir list) (rule: Rule) =
            let sub = getAt cur path
            let sub' = rule.Apply sub
            if sequal sub sub' then failwithf "ring: %s does not apply to %s" rule.Name (src sub)
            cur <- setAt cur path sub'
            steps.Add(at (nav path) rule)
        let rec fix (pred: Expr -> bool) (act: Dir list -> unit) =
            match findPath pred cur with
            | Some p -> act p; fix pred act
            | None -> ()
        /// `x → x·1`: the Identity axiom right-to-left, as a local lemma proved by rewriting its other side.
        let unitIntro (x: Expr) =
            let t = IntTerm(expand_as<int> x)
            ident th (t == t * 1) [ ax_ident th ((t * 1) == t) |> at_right ]
        /// `x·0 → 0`: (15.9).
        let zeroMul (path: Dir list) =
            match getAt cur path with
            | Multiply(x, _) -> step path (lem.Zero (Expr.Cast<int> x))
            | e -> failwithf "ring: not a product: %s" (src e)
        /// Paths of the spine of a right-nested binary chain rooted at `root`: one per element.
        let spine (isNode: Expr -> bool) (root: Dir list) =
            let rec go p acc =
                let e = getAt cur p
                if isNode e then go (p @ [ DR ]) ((p @ [ DL ]) :: acc) else List.rev (p :: acc)
            go root []

        // 1. Eliminate binary and unary minus: (15.14) a − b → a + (−b), then (15.20) −x → (−1)·x with
        //    the literal −(1) folded to −1 so the next pass does not find it again.
        fix (function Subtract _ -> true | _ -> false) (fun p ->
            match getAt cur p with
            | Subtract(a, b) -> step p (IntegralDomain.sub_def dom (Expr.Cast<int> a) (Expr.Cast<int> b))
            | _ -> ())
        fix (function Negate _ -> true | _ -> false) (fun p ->
            match getAt cur p with
            | Negate x ->
                step p (lem.Neg (Expr.Cast<int> x))
                step (p @ [ DL ]) reduce
            | _ -> ())
        // 2. Distribute · over +, on either side.
        fix (function Multiply(_, Add _) | Multiply(Add _, _) -> true | _ -> false) (fun p ->
            match getAt cur p with
            | Multiply(_, Add _) -> step p distrib_mul_add
            | _ -> step p commute_mul; step p distrib_mul_add)
        // 3. Flatten the sum.
        fix (function Add(Add _, _) -> true | _ -> false) (fun p -> step p right_assoc_add)
        // 4. Each monomial.
        let isAdd = function Add _ -> true | _ -> false
        let isMul = function Multiply _ -> true | _ -> false
        for mp in spine isAdd [] do
            // a. flatten the product
            let rec flat () =
                match findPath (function Multiply(Multiply _, _) -> true | _ -> false) (getAt cur mp) with
                | Some p -> step (mp @ p) right_assoc_mul; flat ()
                | None -> ()
            flat ()
            // b. bubble-sort the factors: atoms by key, literals last
            let key (f: Expr) = if isLit f then (1, "") else (0, atomKey f)
            let mutable swapped = true
            while swapped do
                swapped <- false
                let sp = spine isMul mp
                for i in 0 .. sp.Length - 2 do
                    let sp = spine isMul mp
                    let fi, fj = getAt cur sp.[i], getAt cur sp.[i + 1]
                    if not swapped && compare (key fi) (key fj) > 0 then
                        // the node holding fi: its parent position on the spine
                        let node = sp.[i] |> List.take (sp.[i].Length - 1)
                        if i + 1 = sp.Length - 1 then step node commute_mul
                        else
                            step node left_assoc_mul
                            step (node @ [ DL ]) commute_mul
                            step node right_assoc_mul
                        swapped <- true
            // c. fold the trailing literals into one. `reduce` is ONE bottom-up pass that does not
            //    re-fold a parent (`1·(1·1)` becomes `1·1`), so it is repeated until one literal is left.
            let rec foldLits () =
                let sp = spine isMul mp
                match sp |> List.tryFindIndex (fun p -> isLit (getAt cur p)) with
                | Some k when k < sp.Length - 1 ->
                    step (sp.[k] |> List.take (sp.[k].Length - 1)) reduce
                    foldLits ()
                | _ -> ()
            foldLits ()
            // d. no literal at all: append ·1
            let sp = spine isMul mp
            let last = sp.[sp.Length - 1]
            if not (isLit (getAt cur last)) then step last (unitIntro (getAt cur last))
            // e. a zero coefficient zeroes the monomial
            let rec zero () =
                let sp = spine isMul mp
                if sp.Length > 1 && (match getAt cur sp.[sp.Length - 1] with Int32 0 -> true | _ -> false) then
                    zeroMul (sp.[sp.Length - 2] |> List.take (sp.[sp.Length - 2].Length - 1))
                    zero ()
            zero ()
            // f. left-nest the atoms in front of the coefficient: ((x·y)·z)·c
            let rec leftNest () =
                match getAt cur mp with
                | Multiply(_, Multiply _) -> step mp left_assoc_mul; leftNest ()
                | _ -> ()
            leftNest ()
        // 5. Bubble-sort the monomials, constants last.
        let mutable swapped = true
        while swapped do
            swapped <- false
            let sp = spine isAdd []
            for i in 0 .. sp.Length - 2 do
                let sp = spine isAdd []
                if not swapped && compare (monoKey (getAt cur sp.[i])) (monoKey (getAt cur sp.[i + 1])) > 0 then
                    let node = sp.[i] |> List.take (sp.[i].Length - 1)
                    if i + 1 = sp.Length - 1 then step node commute_add
                    else
                        step node left_assoc_add
                        step (node @ [ DL ]) commute_add
                        step node right_assoc_add
                    swapped <- true
        // 6. Combine like terms; fold the constants.
        let rec combine () =
            let sp = spine isAdd []
            let pair =
                [ 0 .. sp.Length - 2 ] |> List.tryFind (fun i ->
                    let a, b = getAt cur sp.[i], getAt cur sp.[i + 1]
                    sameAtoms a b || (isLit a && isLit b))
            match pair with
            | Some i ->
                let node = sp.[i] |> List.take (sp.[i].Length - 1)
                let a = getAt cur sp.[i]
                if isLit a then step node reduce                       // c₁ + c₂ (the last pair)
                elif i + 1 = sp.Length - 1 then
                    step node collect_mul_add                           // m·c₁ + m·c₂ → m·(c₁ + c₂)
                    step (node @ [ DR ]) reduce
                else
                    step node left_assoc_add
                    step (node @ [ DL ]) collect_mul_add
                    step (node @ [ DL; DR ]) reduce
                combine ()
            | None -> ()
        combine ()
        // 7. Zero coefficients and a zero constant.
        let rec dropZeros () =
            let sp = spine isAdd []
            let z = sp |> List.tryFindIndex (fun p ->
                        match getAt cur p with Int32 0 -> true | Multiply(_, Int32 0) -> true | _ -> false)
            match z with
            | Some i when sp.Length > 1 ->
                let p = sp.[i]
                match getAt cur p with
                | Multiply _ -> zeroMul p
                | _ -> ()
                let node = p |> List.take (p.Length - 1)
                if i = sp.Length - 1 then step node ident_add             // m + 0
                elif isLit (getAt cur (node @ [ DR ])) then step node reduce  // 0 + c
                else
                    step node commute_add                               // 0 + rest → rest + 0
                    step node ident_add
                dropZeros ()
            | Some i ->
                (match getAt cur sp.[i] with Multiply _ -> zeroMul sp.[i] | _ -> ())
            | None -> ()
        dropZeros ()
        List.ofSeq steps, cur

    /// The canonical polynomial form of an integer term (no proof).
    let ring_normal_form (th: Theory) (e: Term<int>) : Expr = snd (normalize th [] e.Expr)

    /// **Ring normalization.** `⊢ L = R` for an integer identity of commutative-ring arithmetic, by
    /// rewriting both sides to one canonical polynomial. Every step is an existing rule at an explicit
    /// address and is re-checked by the kernel. Fails — with both normal forms — when they differ.
    let ring_ident (th: Theory) (L: Term<int>) (R: Term<int>) : Theorem =
        let ls, ln = normalize th [ DL ] L.Expr
        let rs, rn = normalize th [ DR ] R.Expr
        if not (sequal ln rn) then
            failwithf "ring: not an identity of commutative-ring arithmetic.\n  left normalizes to  %s\n  right normalizes to %s"
                (th.PrintFormula ln) (th.PrintFormula rn)
        lemma th (L == R) (ls @ rs)

    /// `ring_ident`, as a rewrite rule `L → R` for use inside a larger proof.
    let ring (th: Theory) (L: Term<int>) (R: Term<int>) : Rule = Ident (ring_ident th L R)
