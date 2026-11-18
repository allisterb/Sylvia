namespace Sylvia
// A bare rule as a proof step implicitly means "apply to the whole expression" (Rule -> RuleApplication).
#nowarn "3391"

open FSharp.Quotations

open FsExpr
open Formula
open Patterns
open Descriptions

open PropCalculus
open PredCalculus
open DomainTerms
open Induction

/// Divisibility — Gries & Schneider ch.15, §15.4 (pp. 315–327), with congruences.
///
/// SIX axioms, all definitions. (15.77) defines the relation `∣`:
///
/// ```
/// c ∣ b  ≡  (∃d |: c·d = b)
/// ```
///
/// (15.90) defines the operators `÷` and `mod`, for `c > 0`:
///
/// ```
/// 0 < c  ⇒  (b = (b ÷ c)·c + b mod c  ∧  0 ≤ b mod c)  ∧  b mod c < c
/// ```
///
/// the definition of gcd (in place of Gries' (15.92), which is a theorem here), and its dual, the
/// definition of lcm (in place of (15.94)):
///
/// ```
/// (0 ≤ b gcd c ∧ (b gcd c ∣ b ∧ b gcd c ∣ c)) ∧ (∀d | d ∣ b ∧ d ∣ c : d ∣ b gcd c)
/// (0 ≤ b lcm c ∧ (b ∣ b lcm c ∧ c ∣ b lcm c)) ∧ (∀m | b ∣ m ∧ c ∣ m : b lcm c ∣ m)
/// ```
///
/// and the definition of prime (Gries p. 322):
///
/// ```
/// prime p  =  1 < p ∧ (∀d | 0 < d ∧ d ∣ p : d = 1 ∨ d = p)
/// ```
///
/// and congruence (15.118):
///
/// ```
/// b ≡ c (mod m)  =  m ∣ (c − b)
/// ```
///
/// All six are definitional extensions. `∣`, `÷`, `mod`, `gcd`, `lcm`, `prime` and `≡` are fresh symbols,
/// so the axioms say nothing about anything else and cannot make the theory inconsistent. (15.90) can
/// be stated because (15.87) proves a quotient and remainder exist and (15.89) proves they are unique.
/// The gcd and lcm definitions can be stated because `gcd_exists` and `lcm_exists` prove such values
/// exist, without citing them (both from Well ordering), and `dvd_antisym_nonneg` makes them unique.
/// The definitions of `∣`, of prime and of congruence are explicit: they abbreviate formulas, so
/// nothing has to exist. `∣`, gcd, lcm and prime are SCHEMES over their arguments and a dummy, which
/// must occur in none of them, or the quantifier would capture it; (15.90) and (15.118) are schemes
/// over their arguments alone. Relatively prime (15.105), `b ⊥ c`, and the power `bⁿ`, as
/// `(Πj | 1 ≤ j ∧ j ≤ n : b)`, are notation (`rel_prime`, `power`), not symbols. Everything else in
/// the section is a
/// theorem: (15.78)–(15.86), Gries' exercise 15.65 `d ∣ b ∧ d ∣ c ⇒ d ∣ (b + c)` and its `−` twin,
/// Theorem (15.87), the division theorem, proved from Well ordering (15.49), (15.89), (15.90)–(15.91)
/// as properties of `÷` and `mod`, (15.92) and (15.94) themselves, (15.96)–(15.104), (15.106)–(15.108)
/// (Bézout), (15.111)–(15.113) (Euclid's lemma), `(b gcd c)·(b lcm c) = abs(b·c)`, a prime divisor of
/// every `n > 1`, (15.116), that there are infinitely many primes, and the congruence theorems: `≡`
/// is an equivalence relation, and (15.119)–(15.127).
///
/// ∃-reasoning stays inside the theory. ∃-introduction is (9.28) `exists_intro`. ∃-elimination is
/// ex.9.27, `((∃x|R:P) ⇒ Q) = (∀x|R: P ⇒ Q)`, followed by two steps: rewriting the ∀'s body to `T`
/// with a theorem proved about the dummy itself (generalization, the step `generalize_step` uses),
/// and (9.8). This was written before Metatheorem Witness (9.30), `PredCalculus.witness`, worked inside
/// a theory. It does now (`docs/prover-induction.md` §28), and either route adds no rule of inference.
///
/// The theory is `Induction` plus the six definitions: (15.81) needs "no integer lies between 0 and 1", which is
/// induction, and (15.87) needs Well ordering, which is too.
module Divisibility =

    /// The divisibility relation `c ∣ b`, "c divides b". Uninterpreted: its meaning is (15.77), and it
    /// is never evaluated.
    [<Symbol "∣">]
    let divides (c: int) (b: int) : bool = invalidOp "divides is a symbolic relation; its meaning is the axiom (15.77)."

    let private div_op : Expr<int -> int -> bool> = <@ divides @>
    let private mul_op : Expr<int -> int -> int> = <@ (*) @>

    /// True iff `v` occurs in `e` at all — free or bound.
    let private occurs_at_all (v: Var) (e: Expr) = get_vars e |> List.exists (vequal v)

    (* Axiom *)

    /// **(15.77) Definition of ∣**: `c ∣ b = (∃d |: c·d = b)`, for any dummy `d` occurring in neither
    /// `c` nor `b`.
    let (|DefDivides|_|) =
        function
        | Equals(Binary div_op (c, b), Exists(_, [d], True, Equals(Binary mul_op (c', Var d'), b')))
            when vequal d d' && sequal c c' && sequal b b' && not (occurs_at_all d c) && not (occurs_at_all d b) ->
            Some (axiom_name "Divisibility" "Definition of ∣")
        | _ -> None

    /// The quotient `b ÷ c`. Uninterpreted, like `∣`: its meaning is (15.90), and it is never evaluated
    /// — in particular it is NOT F#'s `/`, which truncates toward zero (`-7 / 2 = -3`, remainder `-1`).
    [<Symbol "÷">]
    let div (b: int) (c: int) : int = invalidOp "÷ is a symbolic operator; its meaning is the axiom (15.90)."

    /// The remainder `b mod c`. Uninterpreted; see `div`. (`mod` itself is reserved in F#.)
    [<Symbol "mod">]
    let modulo (b: int) (c: int) : int = invalidOp "mod is a symbolic operator; its meaning is the axiom (15.90)."

    let private mod_op : Expr<int -> int -> int> = <@ modulo @>

    /// `b ÷ c` and `b mod c` as integer terms.
    let div_of (b: Term<int>) (c: Term<int>) : IntTerm = IntTerm(<@ div %b.Expr %c.Expr @> |> expand_as<int>)
    let mod_of (b: Term<int>) (c: Term<int>) : IntTerm = IntTerm(<@ modulo %b.Expr %c.Expr @> |> expand_as<int>)

    /// The statement of (15.90) at `b`, `c`:
    /// `0 < c ⇒ (b = (b ÷ c)·c + b mod c ∧ 0 ≤ b mod c) ∧ b mod c < c`.
    let private div_mod_spec (b: Term<int>) (c: Term<int>) : Prop =
        let D = Integers.order_domain Integers.integers
        let b', c' = D.Term b, D.Term c
        let q, m = D.Term (div_of b c), D.Term (mod_of b c)
        (D.Zero <. c') ==> (((b' == q * c' + m) * (D.Zero <=. m)) * (m <. c'))

    /// **(15.90) Definition of ÷ and mod**, for `c > 0`: `b ÷ c` and `b mod c` are the quotient and
    /// remainder of (15.87). A scheme over any `b`, `c`.
    ///
    /// A definitional extension: (15.87) gives such a pair and (15.89) makes it unique, so `÷`/`mod`
    /// name a function the theory already defines, and the axiom cannot make it inconsistent. Gries
    /// writes the side condition `c ≠ 0`, but at `c < 0` the range `0 ≤ r < c` is empty (the same
    /// erratum as (15.87), manual answer 15.67), so the condition is `0 < c`. At `c ≤ 0` the axiom
    /// says nothing, and `b ÷ c` is an integer about which nothing is known.
    let (|DefDivMod|_|) (e: Expr) =
        match e with
        | Implies(_, And(_, LessThan(Binary mod_op (b, c), _)))
            when sequal e (div_mod_spec (IntTerm(Expr.Cast<int> b)) (IntTerm(Expr.Cast<int> c))).Expr.Raw ->
            Some (axiom_name "Divisibility" "Definition of ÷ and mod")
        | _ -> None

    /// `c ∣ b`, as a proposition.
    let dvd (c: Term<int>) (b: Term<int>) : Prop = Prop(<@ divides %c.Expr %b.Expr @> |> expand_as<bool>)

    /// An integer dummy named `name`, `name#1`, `name#2`, … — the first whose name occurs nowhere in
    /// `avoid`, free or bound. Stronger than freshness for free variables, which is what (15.77)'s side
    /// condition asks for.
    let fresh_int (name: string) (avoid: Expr list) : IntVar =
        let names = avoid |> List.collect (fun e -> get_vars (expand e)) |> List.map (fun v -> v.Name) |> Set.ofList
        let rec pick i =
            let n = if i = 0 then name else sprintf "%s#%d" name i
            if names.Contains n then pick (i + 1) else IntVar n
        pick 0

    /// The greatest common divisor `b gcd c`. Uninterpreted, like `∣`: its meaning is the axiom
    /// `(|DefGcd|_|)`, and it is never evaluated.
    [<Symbol "gcd">]
    let gcd (b: int) (c: int) : int = invalidOp "gcd is a symbolic operator; its meaning is the definition of gcd."

    let private gcd_op : Expr<int -> int -> int> = <@ gcd @>

    /// `b gcd c` as an integer term.
    let gcd_of (b: Term<int>) (c: Term<int>) : IntTerm = IntTerm(<@ gcd %b.Expr %c.Expr @> |> expand_as<int>)

    /// The definition of gcd at `b`, `c`, with the dummy `d`:
    /// `(0 ≤ b gcd c ∧ (b gcd c ∣ b ∧ b gcd c ∣ c)) ∧ (∀d | d ∣ b ∧ d ∣ c : d ∣ b gcd c)`.
    let private gcd_spec_with (d: IntVar) (b: Term<int>) (c: Term<int>) : Prop =
        let D = Integers.order_domain Integers.integers
        let b', c', g, x = D.Term b, D.Term c, D.Term (gcd_of b c), D.Term (Induction.tm d)
        ((D.Zero <=. g) * (dvd g b' * dvd g c')) * PredCalculus.qall d (dvd x b' * dvd x c') (dvd x g)

    /// The dummy the library uses in the definition of gcd at `b`, `c`: `d`, or the first `d#i` fresh
    /// for both.
    let private gcd_dummy (b: Term<int>) (c: Term<int>) : IntVar = fresh_int "d" [ b.Expr; c.Expr ]

    /// **Definition of gcd** — a definitional axiom, a scheme over any `b`, `c` and any dummy `d`
    /// occurring in neither:
    ///
    /// ```
    /// (0 ≤ b gcd c ∧ (b gcd c ∣ b ∧ b gcd c ∣ c)) ∧ (∀d | d ∣ b ∧ d ∣ c : d ∣ b gcd c)
    /// ```
    ///
    /// `b gcd c` is the non-negative common divisor that every common divisor divides. This is not
    /// Gries' (15.92), which defines gcd as a maximum, `(↑d | d ∣ b ∧ d ∣ c : d)`, and needs the separate
    /// case `0 gcd 0 = 0`. Quantified `↑` (15.63)–(15.70) is not mechanized. Both halves of (15.92) are
    /// THEOREMS here (`gcd_greatest`, `gcd_zero_zero`), and this form covers `0 gcd 0` without a case.
    ///
    /// A definitional extension: `gcd_exists` proves, WITHOUT this axiom, that such a value exists, by
    /// Bézout (the least positive `u·b + v·c`, from Well ordering (15.49)). Two such values divide
    /// each other and are non-negative, so they are equal (`dvd_antisym_nonneg`). So `gcd` names a
    /// function the theory already determines.
    let (|DefGcd|_|) (e: Expr) =
        match e with
        | And(And(LessEq(_, Binary gcd_op (b, c)), _), ForAll(_, [d], _, _))
            when not (occurs_at_all d b) && not (occurs_at_all d c)
                 && sequal e (gcd_spec_with (IntVar d.Name) (IntTerm(Expr.Cast<int> b)) (IntTerm(Expr.Cast<int> c))).Expr.Raw ->
            Some (axiom_name "Divisibility" "Definition of gcd")
        | _ -> None

    /// The least common multiple `b lcm c`. Uninterpreted, like `gcd`: its meaning is the axiom
    /// `(|DefLcm|_|)`, and it is never evaluated.
    [<Symbol "lcm">]
    let lcm (b: int) (c: int) : int = invalidOp "lcm is a symbolic operator; its meaning is the definition of lcm."

    let private lcm_op : Expr<int -> int -> int> = <@ lcm @>

    /// `b lcm c` as an integer term.
    let lcm_of (b: Term<int>) (c: Term<int>) : IntTerm = IntTerm(<@ lcm %b.Expr %c.Expr @> |> expand_as<int>)

    /// The definition of lcm at `b`, `c`, with the dummy `m`:
    /// `(0 ≤ b lcm c ∧ (b ∣ b lcm c ∧ c ∣ b lcm c)) ∧ (∀m | b ∣ m ∧ c ∣ m : b lcm c ∣ m)`.
    let private lcm_spec_with (m: IntVar) (b: Term<int>) (c: Term<int>) : Prop =
        let D = Integers.order_domain Integers.integers
        let b', c', l, x = D.Term b, D.Term c, D.Term (lcm_of b c), D.Term (Induction.tm m)
        ((D.Zero <=. l) * (dvd b' l * dvd c' l)) * PredCalculus.qall m (dvd b' x * dvd c' x) (dvd l x)

    /// The dummy the library uses in the definition of lcm at `b`, `c`.
    let private lcm_dummy (b: Term<int>) (c: Term<int>) : IntVar = fresh_int "m" [ b.Expr; c.Expr ]

    /// **Definition of lcm** — a definitional axiom, the dual of the definition of gcd, a scheme over
    /// any `b`, `c` and any dummy `m` occurring in neither:
    ///
    /// ```
    /// (0 ≤ b lcm c ∧ (b ∣ b lcm c ∧ c ∣ b lcm c)) ∧ (∀m | b ∣ m ∧ c ∣ m : b lcm c ∣ m)
    /// ```
    ///
    /// `b lcm c` is the non-negative common multiple that divides every common multiple. Gries'
    /// (15.94) defines it as a minimum, `(↓k:ℤ⁺ | b ∣ k ∧ c ∣ k : k)` for `b, c ≠ 0`, and `0` when
    /// either is 0. Both halves are THEOREMS here (`lcm_least`, `lcm_pos`, `lcm_zero`), and this form
    /// covers the zero case without one: 0 is the only multiple of 0.
    ///
    /// A definitional extension: `lcm_exists` proves, WITHOUT this axiom, that such a value exists —
    /// for `b, c ≠ 0` the least positive common multiple (Well ordering (15.49)), which divides every
    /// common multiple because the remainder of one by it would be a smaller one; otherwise 0.
    /// `dvd_antisym_nonneg` makes it unique.
    let (|DefLcm|_|) (e: Expr) =
        match e with
        | And(And(LessEq(_, Binary lcm_op (b, c)), _), ForAll(_, [m], _, _))
            when not (occurs_at_all m b) && not (occurs_at_all m c)
                 && sequal e (lcm_spec_with (IntVar m.Name) (IntTerm(Expr.Cast<int> b)) (IntTerm(Expr.Cast<int> c))).Expr.Raw ->
            Some (axiom_name "Divisibility" "Definition of lcm")
        | _ -> None

    /// The predicate "p is prime". Uninterpreted: its meaning is the axiom `(|DefPrime|_|)`.
    [<Symbol "prime ">]   // a prefix symbol prints flush against its argument; the space gives `prime p`
    let is_prime (p: int) : bool = invalidOp "prime is a symbolic predicate; its meaning is the definition of prime."

    let private prime_op : Expr<int -> bool> = <@ is_prime @>

    /// `prime p`, as a proposition.
    let prime (p: Term<int>) : Prop = Prop(<@ is_prime %p.Expr @> |> expand_as<bool>)

    /// The range `0 < d ∧ d ∣ p` and the body `d = 1 ∨ d = p` of the definition of prime.
    let private prime_range (d: Term<int>) (p: Term<int>) : Prop =
        let D = Integers.order_domain Integers.integers
        (D.Zero <. D.Term d) * dvd d p
    let private prime_body (d: Term<int>) (p: Term<int>) : Prop =
        let D = Integers.order_domain Integers.integers
        (D.Term d == D.One) + (D.Term d == D.Term p)

    /// `(∀d | 0 < d ∧ d ∣ p : d = 1 ∨ d = p)` — "the only positive divisors of p are 1 and p".
    let private prime_all (d: IntVar) (p: Term<int>) : Prop =
        PredCalculus.qall d (prime_range (Induction.tm d) p) (prime_body (Induction.tm d) p)

    /// The definition of prime at `p`, with the dummy `d`.
    let private prime_spec_with (d: IntVar) (p: Term<int>) : Prop =
        let D = Integers.order_domain Integers.integers
        prime p == ((D.One <. D.Term p) * prime_all d p)

    /// **Definition of prime** — Gries p. 322: "an integer p > 1 is prime if the only positive
    /// integers that divide p are 1 and p". A scheme over any `p` and any dummy `d` not in `p`:
    ///
    /// ```
    /// prime p  =  1 < p ∧ (∀d | 0 < d ∧ d ∣ p : d = 1 ∨ d = p)
    /// ```
    ///
    /// An EXPLICIT definition — `prime` abbreviates a formula in the existing symbols — so it is
    /// trivially conservative: there is nothing to prove exists.
    let (|DefPrime|_|) (e: Expr) =
        match e with
        | Equals(Unary prime_op p, And(_, ForAll(_, [d], _, _)))
            when not (occurs_at_all d p)
                 && sequal e (prime_spec_with (IntVar d.Name) (IntTerm(Expr.Cast<int> p))).Expr.Raw ->
            Some (axiom_name "Divisibility" "Definition of prime")
        | _ -> None

    /// **(15.105) Relatively prime**: `b ⊥ c ≡ b gcd c = 1`. Notation, not a new symbol: it builds
    /// the proposition `b gcd c = 1`, so it adds nothing to the theory.
    let rel_prime (b: Term<int>) (c: Term<int>) : Prop =
        let D = Integers.order_domain Integers.integers
        D.Term (gcd_of b c) == D.One

    /// Congruence, `b ≡ c (mod m)`. Uninterpreted: its meaning is the axiom `(|DefCong|_|)`. Gries
    /// writes `b ≡ c` with the modulus `m` over the `≡`, and avoids the usual `b ≡ c (mod m)` because
    /// `≡` is his propositional equivalence. Here equivalence prints as `=`, so the usual form is free.
    [<Symbol "{0} ≡ {1} (mod {2})">]
    let congruent (b: int) (c: int) (m: int) : bool = invalidOp "≡ is a symbolic relation; its meaning is the definition (15.118)."

    /// `b ≡ c (mod m)`, as a proposition.
    let cong (b: Term<int>) (c: Term<int>) (m: Term<int>) : Prop = Prop(<@ congruent %b.Expr %c.Expr %m.Expr @> |> expand_as<bool>)

    /// The definition (15.118) at `b`, `c`, `m`: `b ≡ c (mod m) = m ∣ (c − b)`.
    let private cong_spec (b: Term<int>) (c: Term<int>) (m: Term<int>) : Prop =
        let D = Integers.order_domain Integers.integers
        cong b c m == dvd m (D.Term c - D.Term b)

    /// **(15.118) Definition of congruence**: `b ≡ c (mod m) = m ∣ (c − b)`, a scheme over any `b`, `c`,
    /// `m`. An EXPLICIT definition, like prime: `≡` abbreviates a formula in `∣` and `−`, so it is
    /// trivially conservative.
    let (|DefCong|_|) (e: Expr) =
        match e with
        | Equals(FSharp.Quotations.Patterns.Call(None, _, [b; c; m]), _)
            when b.Type = typeof<int> && c.Type = typeof<int> && m.Type = typeof<int>
                 && sequal e (cong_spec (IntTerm(Expr.Cast<int> b)) (IntTerm(Expr.Cast<int> c)) (IntTerm(Expr.Cast<int> m))).Expr.Raw ->
            Some (axiom_name "Divisibility" "Definition of congruence")
        | _ -> None

    let divisibility_axioms =
        function
        | DefDivides x -> Some x
        | DefDivMod x -> Some x
        | DefGcd x -> Some x
        | DefLcm x -> Some x
        | DefPrime x -> Some x
        | DefCong x -> Some x
        | _ -> None

    (* Theory *)

    /// Integer arithmetic, Σ and Π, induction (12.8) — everything `Induction` has — and the definitions
    /// of `∣`, `÷`/`mod`, `gcd`, `lcm`, `prime` and `≡`. A
    /// caller's own axioms and rules go on top, as with `Induction`. It accepts theorems proved in
    /// the canonical `Induction.induction` (and, through it, `Integers.integers`).
    type Divisibility(?axioms: Axioms, ?rules: Rules, ?extends: Theory list) =
        inherit Induction.Induction(BooleanAlgebra.combine_axioms (defaultArg axioms (fun _ -> None)) divisibility_axioms,
                                    defaultArg rules [], (Induction.induction :> Theory) :: defaultArg extends [])

    /// The theory: `Induction` and the six definitions, with nothing extra.
    let divisibility = Divisibility()

    (* Building blocks *)

    let private it (t: Term<int>) = IntTerm t.Expr

    /// The body `c·k = b` of (15.77) at `k`.
    let private body (c: Term<int>) (b: Term<int>) (k: IntVar) : Prop = (it c * tm k :> Term<int>) == b

    /// `(c ∣ b) = (∃d |: c·d = b)` with the given dummy.
    let private dvd_def_with (th: Theory) (d: IntVar) (c: Term<int>) (b: Term<int>) : Rule =
        ax_ident th (dvd c b == qex d T (body c b d))

    /// **(15.77)** as a rewrite: `c ∣ b → (∃d |: c·d = b)`, with a dummy fresh for `c` and `b`.
    let dvd_def (th: Theory) (c: Term<int>) (b: Term<int>) : Rule = dvd_def_with th (fresh_int "d" [ c.Expr; b.Expr ]) c b

    /// From `⊢ c·k = b`, conclude `⊢ c ∣ b` — ∃-introduction (9.28) at the witness `k`.
    let dvd_intro (th: Theory) (c: Term<int>) (b: Term<int>) (k: Term<int>) (h: Theorem) : Theorem =
        let d = fresh_int "d" [ c.Expr; b.Expr ]
        let P = pred_of d (body c b d)
        let ex = mp (OrderedDomain.in_theory th (exists_intro d P k)) h
        lemma th (dvd c b) [
            dvd_def_with th d c b                                   |> apply
            Taut ex                                                 |> apply
        ]

    /// **∃-elimination for ∣.** From `bodyf k : ⊢ c·k = b ⇒ Q`, proved about the dummy `k` it is
    /// handed (fresh for `c`, `b` and `Q`), conclude `⊢ c ∣ b ⇒ Q`. ex.9.27 turns the ∃ antecedent
    /// into `(∀k |: c·k = b ⇒ Q)`, whose body the theorem rewrites to `T` — generalization — and (9.8)
    /// closes it. Sound for the reason any generalization is: `k` is a dummy, so the theorem holds
    /// at every `k`.
    let dvd_elim (th: Theory) (c: Term<int>) (b: Term<int>) (Q: Prop) (bodyf: IntVar -> Theorem) : Theorem =
        let d = fresh_int "d" [ c.Expr; b.Expr; Q.Expr ]
        let P = pred_of d (body c b d)
        lemma th (dvd c b ==> Q) [
            dvd_def_with th d c b                                   |> at_left
            ident_exists_implies d truepred P Q                     |> apply          // ex.9.27
            Taut (bodyf d)                                          |> at [select_body]
            ident_forall_true' d                                    |> apply          // (9.8)
        ]

    /// **∃-elimination**, for any range: from `⊢ (R.x ∧ P.x) ⇒ Q`, proved about the dummy `x`
    /// itself, conclude `⊢ (∃x | R : P) ⇒ Q`. `x` must not be free in `Q`. As `dvd_elim`: ex.9.27,
    /// Trading (9.2), the theorem as the ∀'s body, and (9.8).
    let exists_elim (th: Theory) (x: IntVar) (R: Pred<int>) (P: Pred<int>) (Q: Prop) (t: Theorem) : Theorem =
        if Patterns.occurs_free [ x.Var ] (expand Q.Expr) then
            failwithf "exists_elim: the dummy %s occurs free in the conclusion." x.Name
        let Rx, Px = R.[tm x], P.[tm x]
        let shunted = lemma th (Rx ==> (Px ==> Q)) [
                          shunt' Rx Px Q |> Commute                 |> apply
                          Taut t                                    |> apply ]
        lemma th (exists(x, R, P) ==> Q) [
            ident_exists_implies x R P Q                            |> apply          // ex.9.27
            trade_forall_implies x R (pred_of x (Px ==> Q))         |> apply          // (9.2)
            Taut shunted                                            |> at [select_body]
            ident_forall_true' x                                    |> apply          // (9.8)
        ]

    (* Propositional glue, and two local devices *)

    /// (a ⇒ g) ⇒ ((b ⇒ g) ⇒ ((a ∨ b) ⇒ g))
    let private glue_or_cases = OrderedDomain.taut3 "dv_or_cases" (fun a b g -> (a ==> g) ==> ((b ==> g) ==> ((a + b) ==> g)))
    /// (z ⇒ ¬h) ⇒ ((h ∧ z) ⇒ r)
    let private glue_absurd = OrderedDomain.taut3 "dv_absurd" (fun h z r -> (z ==> !!h) ==> ((h * z) ==> r))
    /// (n ⇒ (x = y)) ⇒ ((n ∧ x) ⇒ y)
    let private glue_detach : Prop -> Prop -> Prop -> Theorem =
        OrderedDomain.taut3 "dv_detach" (fun (n: Prop) (x: Prop) (y: Prop) -> (n ==> (x == y)) ==> ((n * x) ==> y))

    /// `((A ∨ B) ∧ C) ⇒ R`, from its two cases `C ∧ A ⇒ R` and `C ∧ B ⇒ R`.
    let private by_cases2 (th: Theory) (A: Prop) (B: Prop) (C: Prop) (R: Prop) (cCA: Theorem) (cCB: Theorem) : Theorem =
        lemma th (((A + B) * C) ==> R) [
            commute_and (A + B) C                                   |> at_left
            distrib_and_or C A B                                    |> at_left
            case_analysis_1 (C * A) (C * B) R |> Commute            |> apply          // (3.78)
            Taut cCA                                                |> at_left
            Taut cCB                                                |> at_right
            ident_and T                                             |> apply
        ]

    /// `0 < 1`.
    let zero_lt_one (th: Theory) : Theorem =
        let d = Integers.order_domain th
        lemma th (d.Zero <. d.One) [
            OrderedDomain.pos_lt d d.One |> Commute                 |> apply          // 0 < 1 → pos.1
            Taut (mp (OrderedDomain.one_pos d) (Integers.one_ne_zero th)) |> apply
        ]

    /// `a < b ⇒ a ≤ b` — (15.38).
    let private lt_le (th: Theory) (a: DomainTerm<int>) (b: DomainTerm<int>) : Theorem =
        lemma th ((a <. b) ==> (a <=. b)) [
            ax_ident th ((a <=. b) == ((a <. b) + (a == b)))       |> at_right       // (15.38)
            Deduce (weaken_or (a <. b) (a == b))                    |> apply_right
        ]

    (* Units: the integers whose product is 1 *)

    /// `0 < a ∧ a·k = 1 ⇒ a = 1`. Then `0 < k` too (15.43), so `1 ≤ a` and `1 ≤ k`; and
    /// `a = a·1 ≤ a·k = 1`, so Antisymmetry (15.45).
    let private unit_pos (th: Theory) (a: DomainTerm<int>) (k: DomainTerm<int>) : Theorem =
        let d = Integers.order_domain th
        let zero, one = d.Zero, d.One
        let P1, P2 = (zero <. a), (a * k == one)
        let kpos =
            lemma th ((P1 * P2) ==> (zero <. k)) [
                Deduce' (OrderedDomain.lt_mul d zero k a)           |> apply_right                        // 0 < k → 0·a < k·a
                Integers.ring th (zero * a) zero                    |> at [right_branch; left_branch]
                Integers.ring th (k * a) (a * k)                    |> at [right_branch; right_branch]
                Deduce' (reflex_implies P2)                         |> at [right_branch; right_branch]   // a·k → 1
                Taut (zero_lt_one th)                               |> apply_right
            ]
        let le1 =
            lemma th (((P2 * (zero <=. a)) * (one <=. k)) ==> (a <=. one)) [
                Integers.ring th a (a * one)                        |> at [right_branch; left_branch]
                Deduce' (OrderedDomain.eq_sym th (a * k) one)       |> at [right_branch; right_branch]   // 1 → a·k
                Deduce (OrderedDomain.le_mul_nonneg d a one k)      |> apply_right
            ]
        lemma th ((P1 * P2) ==> (a == one)) [
            Establish kpos                                          |> apply
            Establish (one_le_of_pos th k)                          |> apply
            Establish (lt_le th zero a)                             |> apply
            Establish le1                                           |> apply
            Establish (one_le_of_pos th a)                          |> apply
            Deduce (OrderedDomain.le_antisym d a one)               |> apply_right                       // (15.45)
        ]

    let private unit_cases_at (th: Theory) (x: Term<int>) (y: Term<int>) : Theorem =
        let d = Integers.order_domain th
        let zero, one = d.Zero, d.One
        let x, y = d.Term x, d.Term y
        let H = x * y == one
        let E1, E2 = (x == one), (x == -one)
        let R = E1 + E2
        let A, B, B1, Z = (zero <. x), (x <=. zero), (x <. zero), (x == zero)
        // 0 < x: x = 1.
        let cHA = lemma th ((H * A) ==> R) [
                    Establish (unit_pos th x y)                     |> apply
                    Deduce (weaken_or E1 E2)                        |> apply_right ]
        // x < 0: then 0 < −x and (−x)·(−y) = 1, so −x = 1.
        let neg_pos =
            lemma th (B1 ==> (zero <. -x)) [
                ax_ident th (B1 == pos_of (zero - x))               |> at_left                            // (15.36)
                ax_ident th ((zero <. -x) == pos_of (-x - zero))    |> at_right
                Integers.ring th (zero - x) (-x)                    |> at [left_branch; apply_unary]
                Integers.ring th (-x - zero) (-x)                   |> at [right_branch; apply_unary]
                Deduce (reflex_implies (pos_of (-x)))               |> apply_right ]
        let neg_prod =
            lemma th (H ==> ((-x) * (-y) == one)) [
                Integers.ring th ((-x) * (-y)) (x * y)              |> at [right_branch; left_branch]
                Deduce' (reflex_implies H)                          |> at [right_branch; left_branch]
                IntegralDomain.refl_true th one                     |> apply_right ]
        let NX = (-x) == one
        let from_neg =
            lemma th (NX ==> R) [
                Integers.ring th x (-(-x))                          |> at [right_branch; right_branch; left_branch]
                Deduce' (reflex_implies NX)                         |> at [right_branch; right_branch; left_branch; apply_unary]
                IntegralDomain.refl_true th (-one)                  |> at [right_branch; right_branch]
                zero_or E1                                          |> apply_right ]
        let cHB1 = lemma th ((H * B1) ==> R) [
                    Establish neg_pos                               |> apply
                    Establish neg_prod                              |> apply
                    Establish (unit_pos th (-x) (-y))               |> apply
                    Deduce from_neg                                 |> apply_right ]
        // x = 0: then x·y = 0 ≠ 1.
        let z_not_h =
            lemma th (Z ==> !!H) [
                Deduce' (reflex_implies Z)                          |> at [right_branch; apply_unary; left_branch; left_branch]   // x → 0
                Integers.ring th (zero * y) zero                    |> at [right_branch; apply_unary; left_branch]
                ax_ident th ((zero == one) == F)                    |> at [right_branch; apply_unary]     // literal evaluation
                not_false                                           |> apply_right ]
        let cHZ = mp (OrderedDomain.in_theory th (glue_absurd H Z R)) z_not_h
        let cHB = lemma th ((H * B) ==> R) [
                    ax_ident th (B == (B1 + Z))                     |> at [left_branch; right_branch]     // (15.38)
                    commute_and H (B1 + Z)                          |> at_left
                    Taut (by_cases2 th B1 Z H R cHB1 cHZ)           |> apply ]
        let cover = lemma th (A + B) [
                    OrderedDomain.not_lt d zero x |> Commute        |> at_right                           // x ≤ 0 → ¬(0 < x)
                    excluded_middle' A                              |> apply ]
        lemma th (H ==> R) [
            ident_and H |> Commute                                  |> at_left
            Taut cover |> Commute                                   |> at [left_branch; right_branch]
            commute_and H (A + B)                                   |> at_left
            Taut (by_cases2 th A B H R cHA cHB)                     |> apply
        ]

    (* The theorems (15.78)–(15.86) *)

    /// **(15.78)** `c ∣ c` — witness 1 (15.4).
    let dvd_refl (th: Theory) (c: Term<int>) : Theorem =
        dvd_intro th c c (IntTerm 1) (Integers.ring_ident th (it c * IntTerm 1) c)

    /// **(15.79)** `c ∣ 0` — witness 0 (15.9).
    let dvd_zero (th: Theory) (c: Term<int>) : Theorem =
        dvd_intro th c (IntTerm 0) (IntTerm 0) (Integers.ring_ident th (it c * IntTerm 0) (IntTerm 0))

    /// **(15.80)** `1 ∣ b` — witness `b` (15.4).
    let one_dvd (th: Theory) (b: Term<int>) : Theorem =
        dvd_intro th (IntTerm 1) b b (Integers.ring_ident th (IntTerm 1 * it b) b)

    let private dvd_one_at (th: Theory) (unit_cases: Term<int> -> Term<int> -> Theorem) (c: Term<int>) : Theorem =
        let d = Integers.order_domain th
        let c' = d.Term c
        dvd_elim th c (IntTerm 1) ((c' == d.One) + (c' == -d.One)) (fun k -> unit_cases c k)

    /// `(a·m) ∣ b ⇒ a ∣ b`.
    let private dvd_of_mul_dvd (th: Theory) (a: Term<int>) (m: Term<int>) (b: Term<int>) : Theorem =
        let am = it a * it m
        let Q = dvd a b
        dvd_elim th am b Q (fun k ->
            let amk = am * tm k
            let target = dvd_intro th a amk (it m * tm k) (Integers.ring_ident th (it a * (it m * tm k)) amk)
            lemma th (body am b k ==> Q) [
                Deduce' (OrderedDomain.eq_sym th amk b)             |> at [right_branch; right_branch]    // b → (a·m)·k
                Taut target                                         |> apply_right
            ])

    /// **(15.82)** `d ∣ c ∧ c ∣ b ⇒ d ∣ b` — Transitivity: with `d·k = c`, `c ∣ b` is `(d·k) ∣ b`.
    let dvd_trans (th: Theory) (d: Term<int>) (c: Term<int>) (b: Term<int>) : Theorem =
        let A, B, C = dvd d c, dvd c b, dvd d b
        let inner =
            dvd_elim th d c (B ==> C) (fun k ->
                let dk = it d * tm k
                lemma th (body d c k ==> (B ==> C)) [
                    Deduce' (OrderedDomain.eq_sym th dk c)          |> at [right_branch; left_branch; left_branch]   // c → d·k
                    Taut (dvd_of_mul_dvd th d (tm k) b)             |> apply_right
                ])
        lemma th ((A * B) ==> C) [
            shunt' A B C                                            |> apply
            Taut inner                                              |> apply
        ]

    /// **(15.84)** `b ∣ c ⇒ b ∣ (c·d)` — with `b·k = c`, the witness is `k·d`.
    let dvd_mul (th: Theory) (b: Term<int>) (c: Term<int>) (d: Term<int>) : Theorem =
        let Q = dvd b (it c * it d)
        dvd_elim th b c Q (fun k ->
            let bk = it b * tm k
            let target = dvd_intro th b (bk * it d) (tm k * it d) (Integers.ring_ident th (it b * (tm k * it d)) (bk * it d))
            lemma th (body b c k ==> Q) [
                Deduce' (OrderedDomain.eq_sym th bk c)              |> at [right_branch; right_branch; left_branch]   // c → b·k
                Taut target                                         |> apply_right
            ])

    /// **(15.85)** `b ∣ c ⇒ (b·d) ∣ (c·d)` — with `b·k = c`, the witness is `k`.
    let dvd_mul_both (th: Theory) (b: Term<int>) (c: Term<int>) (d: Term<int>) : Theorem =
        let Q = dvd (it b * it d) (it c * it d)
        dvd_elim th b c Q (fun k ->
            let bk = it b * tm k
            let target = dvd_intro th (it b * it d) (bk * it d) (tm k) (Integers.ring_ident th ((it b * it d) * tm k) (bk * it d))
            lemma th (body b c k ==> Q) [
                Deduce' (OrderedDomain.eq_sym th bk c)              |> at [right_branch; right_branch; left_branch]
                Taut target                                         |> apply_right
            ])

    /// `d ∣ x ∧ d ∣ y ⇒ d ∣ (x ∘ y)`, for `∘` either `+` or `−`: with `d·k₁ = x` and `d·k₂ = y`,
    /// the witness is `k₁ ∘ k₂`.
    let private dvd_combine (th: Theory) (op: DomainTerm<int> -> DomainTerm<int> -> DomainTerm<int>) (d: Term<int>) (x: Term<int>) (y: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let d, x, y = D.Term d, D.Term x, D.Term y
        let A, B, C = dvd d x, dvd d y, dvd d (op x y)
        let inner =
            dvd_elim th d x (B ==> C) (fun k1 ->
                let dk1 = d * D.Term (tm k1)
                let Q2 = dvd d (op dk1 y)
                let inner2 =
                    dvd_elim th d y Q2 (fun k2 ->
                        let dk2 = d * D.Term (tm k2)
                        let w = op (D.Term (tm k1)) (D.Term (tm k2))
                        let target = dvd_intro th d (op dk1 dk2) w (Integers.ring_ident th (d * w) (op dk1 dk2))
                        lemma th (body d y k2 ==> Q2) [
                            Deduce' (OrderedDomain.eq_sym th dk2 y) |> at [right_branch; right_branch; right_branch]   // y → d·k₂
                            Taut target                             |> apply_right
                        ])
                lemma th (body d x k1 ==> (B ==> C)) [
                    Deduce' (OrderedDomain.eq_sym th dk1 x)         |> at [right_branch; right_branch; right_branch; left_branch]   // x → d·k₁
                    Taut inner2                                     |> apply_right
                ])
        lemma th ((A * B) ==> C) [
            shunt' A B C                                            |> apply
            Taut inner                                              |> apply
        ]

    /// `d ∣ x ∧ d ∣ y ⇒ d ∣ (x + y)` — Gries' exercise 15.65.
    let dvd_add (th: Theory) (d: Term<int>) (x: Term<int>) (y: Term<int>) : Theorem = dvd_combine th (fun a b -> a + b) d x y

    /// `d ∣ x ∧ d ∣ y ⇒ d ∣ (x − y)`.
    let dvd_sub (th: Theory) (d: Term<int>) (x: Term<int>) (y: Term<int>) : Theorem = dvd_combine th (fun a b -> a - b) d x y

    let private dvd_antisym_at (th: Theory) (unit_cases: Term<int> -> Term<int> -> Theorem) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let b', c' = D.Term b, D.Term c
        let E1, E2 = (b' == c'), (b' == -c')
        let R = E1 + E2
        let LB, LC = dvd b' c', dvd c' b'
        // ⇐ : substitute, then (15.78), and (−c)·(−1) = c, c·(−1) = −c.
        let c1 = lemma th (E1 ==> (LB * LC)) [
                    Deduce' (reflex_implies E1)                     |> at [right_branch; left_branch; left_branch]     // b ∣ c → c ∣ c
                    Deduce' (reflex_implies E1)                     |> at [right_branch; right_branch; right_branch]   // c ∣ b → c ∣ c
                    Taut (dvd_refl th c')                           |> at [right_branch; left_branch]
                    Taut (dvd_refl th c')                           |> at [right_branch; right_branch]
                    ident_and T                                     |> apply_right ]
        let c2 = lemma th (E2 ==> (LB * LC)) [
                    Deduce' (reflex_implies E2)                     |> at [right_branch; left_branch; left_branch]
                    Deduce' (reflex_implies E2)                     |> at [right_branch; right_branch; right_branch]
                    Taut (dvd_intro th (-c') c' (-one) (Integers.ring_ident th ((-c') * (-one)) c'))  |> at [right_branch; left_branch]
                    Taut (dvd_intro th c' (-c') (-one) (Integers.ring_ident th (c' * (-one)) (-c')))  |> at [right_branch; right_branch]
                    ident_and T                                     |> apply_right ]
        let bwd = mp (mp (OrderedDomain.in_theory th (glue_or_cases E1 E2 (LB * LC))) c1) c2
        // ⇒ : under b·k₁ = c and c·k₂ = b. If c = 0 then b = c·k₂ = 0 = c. Otherwise c·(k₂·k₁) = c, so
        // k₂·k₁ = 1 (15.10b), so k₂ = ±1 (the units), so b = c·k₂ = ±c.
        let core (k1: IntVar) (k2: IntVar) : Theorem =
            let k1, k2 = D.Term (tm k1), D.Term (tm k2)
            let H1, H2 = (b' * k1 == c'), (c' * k2 == b')
            let C = H1 * H2
            let Z = c' == zero
            let left_true = [ commute_or T E2 |> apply_right; zero_or E2 |> apply_right ]
            let cZ = lemma th ((C * Z) ==> R) ([
                        Deduce' (OrderedDomain.eq_sym th (c' * k2) b')   |> at [right_branch; left_branch; left_branch]                // b → c·k₂
                        Deduce' (reflex_implies Z)                       |> at [right_branch; left_branch; left_branch; left_branch]   // c → 0
                        Deduce' (reflex_implies Z)                       |> at [right_branch; left_branch; right_branch]
                        Integers.ring th (zero * k2) zero                |> at [right_branch; left_branch; left_branch]
                        IntegralDomain.refl_true th zero                 |> at [right_branch; left_branch] ] @ left_true)
            let NZ = c' != zero
            let fact = lemma th (C ==> (c' * (k2 * k1) == c')) [
                        Integers.ring th (c' * (k2 * k1)) ((c' * k2) * k1)   |> at [right_branch; left_branch]
                        Deduce' (reflex_implies H2)                      |> at [right_branch; left_branch; left_branch]   // c·k₂ → b
                        Deduce' (reflex_implies H1)                      |> at [right_branch; left_branch]                // b·k₁ → c
                        IntegralDomain.refl_true th c'                   |> apply_right ]
            let cancel1 = mp (OrderedDomain.in_theory th (glue_detach NZ (c' * (k2 * k1) == c') ((k2 * k1) == one)))
                             (IntegralDomain.unique_one D c' (k2 * k1))                                                    // (15.10b)
            let K1, K2 = (k2 == one), (k2 == -one)
            let kc1 = lemma th ((H2 * K1) ==> R) ([
                        Deduce' (OrderedDomain.eq_sym th (c' * k2) b')   |> at [right_branch; left_branch; left_branch]
                        Deduce' (reflex_implies K1)                      |> at [right_branch; left_branch; left_branch; right_branch]
                        Integers.ring th (c' * one) c'                   |> at [right_branch; left_branch; left_branch]
                        IntegralDomain.refl_true th c'                   |> at [right_branch; left_branch] ] @ left_true)
            let kc2 = lemma th ((H2 * K2) ==> R) [
                        Deduce' (OrderedDomain.eq_sym th (c' * k2) b')   |> at [right_branch; right_branch; left_branch]
                        Deduce' (reflex_implies K2)                      |> at [right_branch; right_branch; left_branch; right_branch]
                        Integers.ring th (c' * (-one)) (-c')             |> at [right_branch; right_branch; left_branch]
                        IntegralDomain.refl_true th (-c')                |> at [right_branch; right_branch]
                        zero_or E1                                       |> apply_right ]
            let cNZ = lemma th ((C * NZ) ==> R) [
                        Establish fact                                   |> apply
                        Establish cancel1                                |> apply
                        Establish (unit_cases k2 k1)                     |> apply
                        Deduce (by_cases2 th K1 K2 H2 R kc1 kc2)         |> apply_right ]
            lemma th (C ==> R) [
                ident_and C |> Commute                                   |> at_left
                excluded_middle' Z |> Commute                            |> at [left_branch; right_branch]
                commute_and C (Z + !!Z)                                  |> at_left
                Taut (by_cases2 th Z (!!Z) C R cZ cNZ)                   |> apply ]
        let fwd_inner =
            dvd_elim th b' c' (LC ==> R) (fun k1 ->
                let H1 = body b' c' k1
                let inner = dvd_elim th c' b' (H1 ==> R) (fun k2 ->
                                let H2 = body c' b' k2
                                lemma th (H2 ==> (H1 ==> R)) [
                                    shunt' H2 H1 R |> Commute            |> apply
                                    commute_and H2 H1                    |> at_left
                                    Taut (core k1 k2)                    |> apply ])
                lemma th (H1 ==> (LC ==> R)) [
                    shunt' H1 LC R |> Commute                            |> apply
                    commute_and H1 LC                                    |> at_left
                    shunt' LC H1 R                                       |> apply
                    Taut inner                                           |> apply ])
        let fwd = lemma th ((LB * LC) ==> R) [ shunt' LB LC R |> apply; Taut fwd_inner |> apply ]
        lemma th ((LB * LC) == R) [
            mutual_implication' (LB * LC) R |> Commute                   |> apply
            Taut (conj fwd bwd)                                          |> apply
        ]

    let private not_dvd_succ_at (th: Theory) (dvd_one: Term<int> -> Theorem) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let one = D.One
        let b', c' = D.Term b, D.Term c
        let L, A, S = (one <. b'), dvd b' c', dvd b' (c' + one)
        let E1, E2 = (b' == one), (b' == -one)
        let n1 = lemma th (E1 ==> !!L) [
                    Deduce' (reflex_implies E1)                          |> at [right_branch; apply_unary; right_branch]
                    OrderedDomain.lt_irrefl D one                        |> at [right_branch; apply_unary]
                    not_false                                            |> apply_right ]
        let n2 = lemma th (E2 ==> !!L) [
                    Deduce' (reflex_implies E2)                          |> at [right_branch; apply_unary; right_branch]
                    OrderedDomain.not_lt D one (-one)                    |> apply_right                       // ¬(1 < −1) → −1 ≤ 1
                    Integers.reduce                                      |> at [right_branch; left_branch]
                    ax_ident th ((leq -1 1) == T)                        |> apply_right ]                     // literal evaluation
        let not_unit = mp (mp (OrderedDomain.in_theory th (glue_or_cases E1 E2 (!!L))) n1) n2
        let to_one = lemma th (dvd b' ((c' + one) - c') ==> dvd b' one) [
                        Integers.ring th ((c' + one) - c') one           |> at [left_branch; right_branch]
                        Deduce (reflex_implies (dvd b' one))             |> apply_right ]
        lemma th ((L * A) ==> !!S) [
            ident_implies_false_not S |> Commute                         |> at_right                          // ¬S → (S ⇒ F)
            shunt' (L * A) S F |> Commute                                |> apply
            Establish (dvd_sub th b' (c' + one) c')                      |> apply                             // b ∣ ((c + 1) − c)
            Establish to_one                                             |> apply
            Establish (dvd_one b')                                       |> apply                             // (15.81)
            Establish not_unit                                           |> apply
            Deduce (lemma th ((L * !!L) ==> F) [ contr L |> at_left ])   |> apply_right
        ]

    // The proofs of (15.81), (15.83) and (15.86) run a case analysis each (≈ 2 s); they are derived once
    // per theory at schema metavariables and instantiated per call. Their statements are binder-free.
    type private DivLemmas =
        { UnitCases: Expr<int> -> Expr<int> -> Theorem
          DvdOne: Expr<int> -> Theorem
          Antisym: Expr<int> -> Expr<int> -> Theorem
          NotDvdSucc: Expr<int> -> Expr<int> -> Theorem }

    let private div_lemmas = System.Runtime.CompilerServices.ConditionalWeakTable<Theory, DivLemmas>()

    let rec private lemmas_of (th: Theory) : DivLemmas =
        div_lemmas.GetValue(th, fun _ ->
            let t (e: Expr<int>) = IntTerm e :> Term<int>
            let uc x y = (lemmas_of th).UnitCases (x: Term<int>).Expr (y: Term<int>).Expr
            let d1 x = (lemmas_of th).DvdOne (x: Term<int>).Expr
            { UnitCases = Tactics.Schema.th2 "unit_cases" (fun x y -> unit_cases_at th (t x) (t y))
              DvdOne = Tactics.Schema.th1 "dvd_one" (fun c -> dvd_one_at th uc (t c))
              Antisym = Tactics.Schema.th2 "dvd_antisym" (fun b c -> dvd_antisym_at th uc (t b) (t c))
              NotDvdSucc = Tactics.Schema.th2 "not_dvd_succ" (fun b c -> not_dvd_succ_at th d1 (t b) (t c)) })

    /// `x·y = 1 ⇒ (x = 1 ∨ x = −1)` — the units of ℤ. By cases on the sign of `x`: positive, `x = 1`;
    /// negative, the same for `−x`; zero, `x·y = 0`.
    let unit_cases (th: Theory) (x: Term<int>) (y: Term<int>) : Theorem = (lemmas_of th).UnitCases x.Expr y.Expr

    /// **(15.81)** `c ∣ 1 ⇒ c = 1 ∨ c = −1` — a witness `k` has `c·k = 1` (`unit_cases`).
    let dvd_one (th: Theory) (c: Term<int>) : Theorem = (lemmas_of th).DvdOne c.Expr

    /// **(15.83)** `b ∣ c ∧ c ∣ b ≡ b = c ∨ b = −c` — Gries' manual, answer 15.60: case `c = 0`, and
    /// otherwise the two witnesses multiply to 1.
    let dvd_antisym (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (lemmas_of th).Antisym b.Expr c.Expr

    /// **(15.86)** `1 < b ∧ b ∣ c ⇒ ¬(b ∣ (c + 1))` — otherwise `b` divides `(c + 1) − c = 1`, and
    /// (15.81) leaves only `b = ±1`.
    let not_dvd_succ (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (lemmas_of th).NotDvdSucc b.Expr c.Expr

    (* The division theorem *)

    /// **(15.87) Division theorem** (existence), as Gries' manual corrects it — `c > 0`, since at
    /// `c = 0` the range `0 ≤ r < c` is empty:
    ///
    /// ```
    /// 0 < c ⇒ (∃q |: (∃r | 0 ≤ r ∧ r < c : b = q·c + r))
    /// ```
    ///
    /// Gries proves it with a loop (15.88); this is the textbook proof from Well ordering (15.49). The
    /// set `S = {x | 0 ≤ x ∧ (∃k |: x = b − k·c)}` is non-empty — it contains `b + abs.b·c` (quotient
    /// `−abs.b`) — so it has a least element `x` (`least_element`), with quotient `k`. Then `x < c`, for
    /// otherwise `x − c` (quotient `k + 1`) is a smaller element of S. So `q, r := k, x`.
    let division_theorem (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let avoid = [ b.Expr.Raw; c.Expr.Raw ]
        let q = fresh_int "q" avoid
        let r = fresh_int "r" (q.Expr.Raw :: avoid)
        let x = fresh_int "x" (q.Expr.Raw :: r.Expr.Raw :: avoid)
        let y = fresh_int "y" (q.Expr.Raw :: r.Expr.Raw :: x.Expr.Raw :: avoid)
        let k = fresh_int "k" (q.Expr.Raw :: r.Expr.Raw :: x.Expr.Raw :: y.Expr.Raw :: avoid)
        let tv (v: IntVar) = D.Term (tm v)
        let b', c' = D.Term b, D.Term c
        let X = tv x
        let C0 = zero <. c'
        let ex1 (v: IntVar) (p: Pred<int>) = qex v T p.[tm v]
        let Pk (v: DomainTerm<int>) = pred_of k (v == b' - tv k * c')
        let Pof (v: DomainTerm<int>) = ex1 k (Pk v)
        let P = pred_of x (Pof X)
        let Rr = pred_of r ((zero <=. tv r) * (tv r <. c'))
        let Bq = pred_of r (b' == tv q * c' + tv r)
        let G = qex q T (exists(r, Rr, Bq))
        let wrap (t: Theorem) = lemma th (C0 ==> Prop(Expr.Cast<bool> t.Stmt)) [ Taut t |> apply_right ]
        // S is non-empty: e = b + abs.b·c ≥ 0, since −b ≤ abs.b ≤ abs.b·c.
        let ab = abs_of b'
        let e = b' + ab * c'
        let Pe = mp (OrderedDomain.in_theory th (exists_intro k (Pk e) (-ab))) (Integers.ring_ident th e (b' - (-ab) * c'))
        let ab_le = lemma th (((zero <=. ab) * (one <=. c')) ==> (ab <=. ab * c')) [
                        Integers.ring th ab (ab * one)                   |> at [right_branch; left_branch]
                        Deduce (OrderedDomain.le_mul_nonneg D ab one c') |> apply_right ]
        let negb =
            let A1, A2 = (b' <=. ab), (-b' <=. ab)
            mp (OrderedDomain.in_theory th (lemma prop_calculus ((A1 * A2) ==> A2) [
                    commute_and A1 A2 |> at_left
                    Taut (strengthen_and A2 A1) |> apply ])) (OrderedDomain.abs_ge D b')
        let translate = lemma th ((-b' <=. ab * c') ==> (zero <=. e)) [
                        OrderedDomain.le_translate D (-b') (ab * c') b'  |> at_left
                        Integers.ring th (-b' + b') zero                 |> at [left_branch; left_branch]
                        Integers.ring th (ab * c' + b') e                |> at [left_branch; right_branch]
                        Deduce (reflex_implies (zero <=. e))             |> apply_right ]
        let nonneg = lemma th (C0 ==> (zero <=. e)) [
                        Establish (one_le_of_pos th c')                  |> apply
                        Establish (wrap (OrderedDomain.abs_nonneg D b')) |> apply
                        Establish ab_le                                  |> apply
                        Establish (wrap negb)                            |> apply
                        Establish (OrderedDomain.le_trans D (-b') ab (ab * c')) |> apply
                        Deduce translate                                 |> apply_right ]
        // A least element x, with its quotient k.
        let least = least_element th x y P e
        let Rng = pred_of x ((leq 0 (tm x)) * P.[tm x])
        let minimal (v: Term<int>) = qall y ((leq 0 (tm y)) * (lt (tm y) v)) (!!P.[tm y])
        let Min = pred_of x (minimal (tm x))
        let LE = exists(x, Rng, Min)
        let E = X == b' - tv k * c'
        let N = zero <=. X
        let M = minimal (tm x)
        // x < c: else y₀ = x − c is in S (quotient k + 1), with 0 ≤ y₀ < x — and x is least.
        let y0 = X - c'
        let f1 = lemma th ((c' <=. X) ==> (zero <=. y0)) [
                    OrderedDomain.le_translate D c' X (-c')              |> at_left
                    Integers.ring th (c' + -c') zero                     |> at [left_branch; left_branch]
                    Integers.ring th (X + -c') y0                        |> at [left_branch; right_branch]
                    Deduce (reflex_implies (zero <=. y0))                |> apply_right ]
        let f2 = lemma th (C0 ==> (y0 <. X)) [
                    Integers.ring th y0 (zero + y0)                      |> at [right_branch; left_branch]
                    Integers.ring th X (c' + y0)                         |> at [right_branch; right_branch]
                    OrderedDomain.lt_translate D zero c' y0 |> Commute   |> at_right
                    Deduce (reflex_implies C0)                           |> apply_right ]
        let Py0 = P.[y0]
        let ey = lemma th (E ==> (y0 == b' - (tv k + one) * c')) [
                    Deduce' (reflex_implies E)                           |> at [right_branch; left_branch; left_branch]   // x → b − k·c
                    Integers.ring th ((b' - tv k * c') - c') (b' - (tv k + one) * c') |> at [right_branch; left_branch]
                    IntegralDomain.refl_true th (b' - (tv k + one) * c') |> apply_right ]
        let f3 = lemma th (E ==> Py0) [
                    Establish ey                                         |> apply
                    Deduce (exists_intro k (Pk y0) (tv k + one))         |> apply_right ]                                 // (9.28)
        let Rm = pred_of y ((leq 0 (tm y)) * (lt (tm y) (tm x)))
        let NP = pred_of y (!!P.[tm y])
        let inst = lemma th (M ==> (Rm.[y0] ==> NP.[y0])) [ trade_forall_implies y Rm NP |> at_left ]               // (9.13)
        let f4 = lemma th ((M * Rm.[y0]) ==> NP.[y0]) [ shunt' M Rm.[y0] NP.[y0] |> apply; Taut inst |> apply ]
        let A3 = (E * M) * C0
        let lt_c = lemma th (A3 ==> (X <. c')) [
                    double_negation (X <. c') |> Commute                 |> at_right
                    OrderedDomain.not_lt D X c'                          |> at [right_branch; apply_unary]                // ¬(x < c) → c ≤ x
                    ident_implies_false_not (c' <=. X) |> Commute        |> at_right
                    shunt' A3 (c' <=. X) F |> Commute                    |> apply
                    Establish f1                                         |> apply
                    Establish f2                                         |> apply
                    Establish f3                                         |> apply
                    Establish f4                                         |> apply
                    Deduce (lemma th ((Py0 * !!Py0) ==> F) [ contr Py0 |> at_left ]) |> apply_right ]
        // q, r := k, x.
        let eb = lemma th (E ==> (b' == tv k * c' + X)) [
                    Deduce' (reflex_implies E)                           |> at [right_branch; right_branch; right_branch]  // x → b − k·c
                    Integers.ring th (tv k * c' + (b' - tv k * c')) b'   |> at [right_branch; right_branch]
                    IntegralDomain.refl_true th b'                       |> apply_right ]
        let Bk = pred_of r (b' == tv k * c' + tv r)
        let introR = exists_intro r (Rr * Bk) (tm x)
        let introQ = exists_intro q (pred_of q (ex1 r (Rr * Bq))) (tm k)
        let Rest = (N * M) * C0
        let g = lemma th ((E * Rest) ==> G) [
                    trade_exists_and r Rr Bq                             |> at [right_branch; select_body]                 // (9.19)
                    Establish lt_c                                       |> apply
                    Establish eb                                         |> apply
                    Establish introR                                     |> apply
                    Deduce introQ                                        |> apply_right ]
        // Eliminate the quotient k, then the least element x.
        let Qk = Rest ==> G
        let tk = lemma th ((T * E) ==> Qk) [
                    commute_and T E                                      |> at_left
                    ident_and E                                          |> at_left
                    shunt' E Rest G |> Commute                           |> apply
                    Taut g                                               |> apply ]
        let elimK = exists_elim th k truepred (Pk X) Qk tk
        let PX = Pof X
        let elimK' = lemma th ((PX * Rest) ==> G) [ shunt' PX Rest G |> apply; Taut elimK |> apply ]
        let xbody = lemma th ((Rng.[tm x] * Min.[tm x]) ==> (C0 ==> G)) [
                    shunt' (Rng.[tm x] * Min.[tm x]) C0 G |> Commute     |> apply
                    Deduce elimK'                                        |> apply_right ]
        let elimX = exists_elim th x Rng Min (C0 ==> G) xbody
        let elimX' = lemma th ((LE * C0) ==> G) [ shunt' LE C0 G |> apply; Taut elimX |> apply ]
        lemma th (C0 ==> G) [
            Establish nonneg                                             |> apply
            Establish (wrap Pe)                                          |> apply
            Establish least                                              |> apply
            Deduce elimX'                                                |> apply_right
        ]

    (* Uniqueness of the quotient and remainder (15.89) *)

    /// `b = q·c + r ∧ 0 ≤ r ∧ r < c` — "q and r are a quotient and remainder of b by c".
    let private qr (D: OrderedDomain.OrderedDomain<int>) (b: DomainTerm<int>) (c: DomainTerm<int>) (q: DomainTerm<int>) (r: DomainTerm<int>) : Prop =
        ((b == q * c + r) * (D.Zero <=. r)) * (r <. c)

    /// `(0 < c ∧ (b = q·c + r ∧ 0 ≤ r)) ∧ (b = q'·c + r' ∧ r' < c) ⇒ q ≤ q'`. Otherwise `q' + 1 ≤ q`,
    /// and then `b = q'·c + r' < c·(q' + 1) ≤ c·q ≤ q·c + r = b`.
    let private quot_le_at (th: Theory) (b: Term<int>) (c: Term<int>) (q: Term<int>) (r: Term<int>) (q2: Term<int>) (r2: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let b, c, q, r, q2, r2 = D.Term b, D.Term c, D.Term q, D.Term r, D.Term q2, D.Term r2
        let C0, H1, N, H2, L = (zero <. c), (b == q * c + r), (zero <=. r), (b == q2 * c + r2), (r2 <. c)
        let S = q2 <. q
        let s = q2 + one
        let A = (C0 * (H1 * N)) * (H2 * L)
        let succ_le = lemma th (S ==> (s <=. q)) [
                        Ident (lt_iff_succ_le th q2 q)                   |> at_left
                        Deduce (reflex_implies (s <=. q))                |> apply_right ]
        let below = lemma th ((H2 * L) ==> (b <. c * s)) [
                        Deduce' (reflex_implies H2)                      |> at [right_branch; left_branch]     // b → q'·c + r'
                        Integers.ring th (q2 * c + r2) (r2 + q2 * c)     |> at [right_branch; left_branch]
                        Integers.ring th (c * s) (c + q2 * c)            |> at [right_branch; right_branch]
                        OrderedDomain.lt_translate D r2 c (q2 * c) |> Commute |> at_right                      // → r' < c
                        Deduce (reflex_implies L)                        |> apply_right ]
        let above = lemma th ((H1 * N) ==> ((c * q) <=. b)) [
                        Deduce' (reflex_implies H1)                      |> at [right_branch; right_branch]    // b → q·c + r
                        Integers.ring th (c * q) (zero + c * q)          |> at [right_branch; left_branch]
                        Integers.ring th (q * c + r) (r + c * q)         |> at [right_branch; right_branch]
                        OrderedDomain.le_translate D zero r (c * q) |> Commute |> at_right                     // → 0 ≤ r
                        Deduce (reflex_implies N)                        |> apply_right ]
        let absurd = lemma th ((b <. b) ==> F) [ OrderedDomain.lt_irrefl D b |> at_left ]
        lemma th (A ==> (q <=. q2)) [
            OrderedDomain.not_lt D q2 q |> Commute                       |> at_right                           // q ≤ q' → ¬(q' < q)
            ident_implies_false_not S |> Commute                         |> at_right                           // ¬S → (S ⇒ F)
            shunt' A S F |> Commute                                      |> apply
            Establish succ_le                                            |> apply                              // q' + 1 ≤ q
            Establish (lt_le th zero c)                                  |> apply                              // 0 ≤ c
            Establish (OrderedDomain.le_mul_nonneg D c s q)              |> apply                              // c·(q' + 1) ≤ c·q
            Establish below                                              |> apply                              // b < c·(q' + 1)
            Establish above                                              |> apply                              // c·q ≤ b
            Establish (OrderedDomain.lt_le_trans D b (c * s) (c * q))    |> apply
            Establish (OrderedDomain.lt_le_trans D b (c * q) b)          |> apply                              // b < b
            Deduce absurd                                                |> apply_right
        ]

    /// The hypotheses of (15.89), `(0 < c ∧ qr(q, r)) ∧ qr(q', r')`.
    let private unique_hyps (D: OrderedDomain.OrderedDomain<int>) (b: Term<int>) (c: Term<int>) (q: Term<int>) (r: Term<int>) (q2: Term<int>) (r2: Term<int>) : Prop =
        let b, c, q, r, q2, r2 = D.Term b, D.Term c, D.Term q, D.Term r, D.Term q2, D.Term r2
        ((D.Zero <. c) * qr D b c q r) * qr D b c q2 r2

    let private quot_unique_at (th: Theory) (b: Term<int>) (c: Term<int>) (q: Term<int>) (r: Term<int>) (q2: Term<int>) (r2: Term<int>) : Theorem =
        let D = Integers.order_domain th
        lemma th (unique_hyps D b c q r q2 r2 ==> (D.Term q == D.Term q2)) [
            Establish (quot_le_at th b c q r q2 r2)                      |> apply
            Establish (quot_le_at th b c q2 r2 q r)                      |> apply
            Deduce (OrderedDomain.le_antisym D q q2)                     |> apply_right                        // (15.45)
        ]

    /// With the quotients equal, `r = (q·c + r) − q·c = b − q·c = (q'·c + r') − q'·c = r'`.
    let private rem_unique_at (th: Theory) (quot_unique: Term<int> list -> Theorem) (b: Term<int>) (c: Term<int>) (q: Term<int>) (r: Term<int>) (q2: Term<int>) (r2: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let b', c', q', r', q2', r2' = D.Term b, D.Term c, D.Term q, D.Term r, D.Term q2, D.Term r2
        let H1, H2, E = (b' == q' * c' + r'), (b' == q2' * c' + r2'), (q' == q2')
        let req = lemma th (((H1 * H2) * E) ==> (r' == r2')) [
                    Integers.ring th r' ((q' * c' + r') - q' * c')                 |> at [right_branch; left_branch]
                    Deduce' (OrderedDomain.eq_sym th b' (q' * c' + r'))            |> at [right_branch; left_branch; left_branch]                // q·c + r → b
                    Deduce' (reflex_implies H2)                                    |> at [right_branch; left_branch; left_branch]                // b → q'·c + r'
                    Deduce' (reflex_implies E)                                     |> at [right_branch; left_branch; right_branch; left_branch]  // q → q'
                    Integers.ring th ((q2' * c' + r2') - q2' * c') r2'             |> at [right_branch; left_branch]
                    IntegralDomain.refl_true th r2'                                |> apply_right ]
        lemma th (unique_hyps D b c q r q2 r2 ==> (r' == r2')) [
            Establish (quot_unique [ b; c; q; r; q2; r2 ])               |> apply
            Deduce req                                                   |> apply_right
        ]

    // Derived once per theory at schema metavariables; the statements are binder-free.
    type private UniqueLemmas =
        { QuotUnique: Expr<int> list -> Theorem
          RemUnique: Expr<int> list -> Theorem }

    let private unique_lemmas = System.Runtime.CompilerServices.ConditionalWeakTable<Theory, UniqueLemmas>()

    let rec private unique_of (th: Theory) : UniqueLemmas =
        unique_lemmas.GetValue(th, fun _ ->
            let terms (xs: Expr<int> list) = xs |> List.map (fun e -> IntTerm e :> Term<int>)
            let six name f =
                Tactics.Schema.thn name 6 (fun xs ->
                    match terms xs with
                    | [ b; c; q; r; q2; r2 ] -> f b c q r q2 r2
                    | _ -> failwith "unreachable")
            let qu (xs: Term<int> list) = (unique_of th).QuotUnique (xs |> List.map (fun x -> x.Expr))
            { QuotUnique = six "quot_unique" (quot_unique_at th)
              RemUnique = six "rem_unique" (rem_unique_at th qu) })

    /// `(0 < c ∧ qr(q, r)) ∧ qr(q', r') ⇒ q = q'`, where `qr(q, r)` is `b = q·c + r ∧ 0 ≤ r ∧ r < c`.
    let quot_unique (th: Theory) (b: Term<int>) (c: Term<int>) (q: Term<int>) (r: Term<int>) (q2: Term<int>) (r2: Term<int>) : Theorem =
        (unique_of th).QuotUnique [ b.Expr; c.Expr; q.Expr; r.Expr; q2.Expr; r2.Expr ]

    /// `(0 < c ∧ qr(q, r)) ∧ qr(q', r') ⇒ r = r'`.
    let rem_unique (th: Theory) (b: Term<int>) (c: Term<int>) (q: Term<int>) (r: Term<int>) (q2: Term<int>) (r2: Term<int>) : Theorem =
        (unique_of th).RemUnique [ b.Expr; c.Expr; q.Expr; r.Expr; q2.Expr; r2.Expr ]

    /// **(15.89)** The quotient and remainder of (15.87) are unique:
    /// `(0 < c ∧ qr(q, r)) ∧ qr(q', r') ⇒ q = q' ∧ r = r'`. If `q < q'`, then
    /// `b = q·c + r < (q + 1)·c ≤ q'·c ≤ b` — so `q ≤ q'`, and by symmetry `q = q'` (15.45); then
    /// `r = b − q·c = r'`.
    let div_unique (th: Theory) (b: Term<int>) (c: Term<int>) (q: Term<int>) (r: Term<int>) (q2: Term<int>) (r2: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let R = (D.Term q == D.Term q2) * (D.Term r == D.Term r2)
        lemma th (unique_hyps D b c q r q2 r2 ==> R) [
            Establish (quot_unique th b c q r q2 r2)                     |> apply
            Establish (rem_unique th b c q r q2 r2)                      |> apply
            Deduce (reflex_implies R)                                    |> apply_right
        ]

    (* ÷ and mod (15.90)–(15.91) *)

    /// (a ⇒ x ∧ y) ⇒ (a ⇒ x)
    let private glue_proj_l = OrderedDomain.taut3 "dv_proj_l" (fun a x y -> (a ==> (x * y)) ==> (a ==> x))
    /// (a ⇒ x ∧ y) ⇒ (a ⇒ y)
    let private glue_proj_r = OrderedDomain.taut3 "dv_proj_r" (fun a x y -> (a ==> (x * y)) ==> (a ==> y))

    /// The three parts of the axiom (15.90) at `b`, `c`, each under `0 < c`.
    let private spec_parts (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem * Theorem * Theorem =
        let D = Integers.order_domain th
        let b', c' = D.Term b, D.Term c
        let q, m = D.Term (div_of b c), D.Term (mod_of b c)
        let C0, S1, S2, S3 = (D.Zero <. c'), (b' == q * c' + m), (D.Zero <=. m), (m <. c')
        let ax = axiom th (div_mod_spec b c)
        let proj glue a x y (t: Theorem) = mp (OrderedDomain.in_theory th (glue a x y)) t
        let s12 = proj glue_proj_l C0 (S1 * S2) S3 ax
        proj glue_proj_l C0 S1 S2 s12, proj glue_proj_r C0 S1 S2 s12, proj glue_proj_r C0 (S1 * S2) S3 ax

    /// **(15.90)**, the defining property: `0 < c ⇒ b = (b ÷ c)·c + b mod c`.
    let div_mod_def (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = let s, _, _ = spec_parts th b c in s

    /// **(15.90)**, the defining property: `0 < c ⇒ 0 ≤ b mod c`.
    let mod_nonneg (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = let _, s, _ = spec_parts th b c in s

    /// **(15.90)**, the defining property: `0 < c ⇒ b mod c < c`.
    let mod_lt (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = let _, _, s = spec_parts th b c in s

    /// **(15.91)** `0 < c ⇒ b = c·(b ÷ c) + b mod c`.
    let div_mod (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let b', c' = D.Term b, D.Term c
        let q, m = D.Term (div_of b c), D.Term (mod_of b c)
        lemma th ((D.Zero <. c') ==> (b' == c' * q + m)) [
            Integers.ring th (c' * q + m) (q * c' + m)                   |> at [right_branch; right_branch]
            Deduce (div_mod_def th b c)                                  |> apply_right
        ]

    let private by_uniqueness (th: Theory) (pick: Theory -> Term<int> -> Term<int> -> Term<int> -> Term<int> -> Term<int> -> Term<int> -> Theorem)
                              (lhs: Term<int>) (rhs: Term<int>) (b: Term<int>) (c: Term<int>) (q: Term<int>) (r: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let s1, s2, s3 = spec_parts th b c
        lemma th (((D.Zero <. D.Term c) * qr D (D.Term b) (D.Term c) (D.Term q) (D.Term r)) ==> (D.Term lhs == D.Term rhs)) [
            Establish s1                                                 |> apply
            Establish s2                                                 |> apply
            Establish s3                                                 |> apply
            Deduce (pick th b c (div_of b c) (mod_of b c) q r)           |> apply_right
        ]

    /// **(15.90)** `b ÷ c` is THE quotient: `0 < c ∧ (b = q·c + r ∧ 0 ≤ r ∧ r < c) ⇒ b ÷ c = q`.
    let div_eq (th: Theory) (b: Term<int>) (c: Term<int>) (q: Term<int>) (r: Term<int>) : Theorem =
        by_uniqueness th quot_unique (div_of b c) q b c q r

    /// **(15.90)** `b mod c` is THE remainder: `0 < c ∧ (b = q·c + r ∧ 0 ≤ r ∧ r < c) ⇒ b mod c = r`.
    let mod_eq (th: Theory) (b: Term<int>) (c: Term<int>) (q: Term<int>) (r: Term<int>) : Theorem =
        by_uniqueness th rem_unique (mod_of b c) r b c q r

    /// `0 < c ⇒ (c ∣ b = (b mod c = 0))` — divisibility is a zero remainder. `⇐`: `b = (b ÷ c)·c + 0`,
    /// so `b ÷ c` is a witness. `⇒`: with `c·k = b`, `b = k·c + 0`, so `b mod c = 0` (15.90).
    let dvd_iff_mod_zero (th: Theory) (c: Term<int>) (b: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero = D.Zero
        let b', c' = D.Term b, D.Term c
        let q, m = D.Term (div_of b c), D.Term (mod_of b c)
        let C0, M0, X = (zero <. c'), (m == zero), dvd c b
        let S1 = b' == q * c' + m
        // ⇐
        let witness = lemma th ((C0 * M0) ==> (c' * q == b')) [
                        Establish (div_mod_def th b c)                   |> apply                                                  // b = (b ÷ c)·c + b mod c
                        Deduce' (reflex_implies S1)                      |> at [right_branch; right_branch]                        // b → (b ÷ c)·c + b mod c
                        Deduce' (reflex_implies M0)                      |> at [right_branch; right_branch; right_branch]          // b mod c → 0
                        Integers.ring th (q * c' + zero) (c' * q)        |> at [right_branch; right_branch]
                        IntegralDomain.refl_true th (c' * q)             |> apply_right ]
        let d = fresh_int "d" [ c.Expr; b.Expr ]
        let back = lemma th ((C0 * M0) ==> X) [
                        dvd_def_with th d c b                            |> at_right
                        Establish witness                                |> apply
                        Deduce (exists_intro d (pred_of d (body c b d)) q) |> apply_right ]                                   // (9.28)
        // ⇒
        let fwd =
            dvd_elim th c b (C0 ==> M0) (fun k ->
                let k' = D.Term (tm k)
                let Hk = body c b k
                let as_qr = lemma th (Hk ==> (b' == k' * c' + zero)) [
                                Deduce' (OrderedDomain.eq_sym th (c' * k') b') |> at [right_branch; left_branch]               // b → c·k
                                Integers.ring th (k' * c' + zero) (c' * k') |> at [right_branch; right_branch]
                                IntegralDomain.refl_true th (c' * k')     |> apply_right ]
                let refl0 = lemma th (Hk ==> (zero <=. zero)) [ OrderedDomain.le_refl D zero |> apply_right ]
                lemma th (Hk ==> (C0 ==> M0)) [
                    shunt' Hk C0 M0 |> Commute                           |> apply
                    Establish as_qr                                      |> apply
                    Establish refl0                                      |> apply
                    Deduce (mod_eq th b c k' zero)                       |> apply_right ])
        let fwdC = lemma th (C0 ==> (X ==> M0)) [
                        shunt' C0 X M0 |> Commute                        |> apply
                        commute_and C0 X                                 |> at_left
                        shunt' X C0 M0                                   |> apply
                        Taut fwd                                         |> apply ]
        let backC = lemma th (C0 ==> (M0 ==> X)) [
                        shunt' C0 M0 X |> Commute                        |> apply
                        Taut back                                        |> apply ]
        lemma th (C0 ==> (X == M0)) [
            mutual_implication' X M0 |> Commute                          |> at_right
            Establish fwdC                                               |> apply
            Establish backC                                              |> apply
            Deduce (reflex_implies ((X ==> M0) * (M0 ==> X)))            |> apply_right
        ]

    (* ------------------------------------------------------------------------------------------
       The greatest common divisor (15.92)–(15.107).

       The definition is `(|DefGcd|_|)`. Existence is Bézout: for b, c not both 0, the least positive
       u·b + v·c (Well ordering, `least_element`) divides b and c, since otherwise its remainder
       would be a smaller positive combination; and every common divisor divides it. That proof
       (`bezout_least`, `gcd_exists`) never mentions gcd. Everything else goes through `gcd_eq`:
       a non-negative common divisor g with `b gcd c ∣ g` IS `b gcd c`.

       Binder-free theorems are derived once per theory at schema metavariables (`cached1`–`cached4`,
       `Tactics.Schema.thn`) and instantiated per call.
       ------------------------------------------------------------------------------------------ *)

    /// ((a ∧ p) ⇒ r) ⇒ (((a ∧ ¬p) ⇒ r) ⇒ (a ⇒ r))
    let private glue_cases = OrderedDomain.taut3 "dv_cases" (fun a p r -> ((a * p) ==> r) ==> (((a * !!p) ==> r) ==> (a ==> r)))

    /// `R`, from `p ⇒ R` and `¬p ⇒ R` — excluded middle.
    let private by_em (th: Theory) (p: Prop) (R: Prop) (cp: Theorem) (cnp: Theorem) : Theorem =
        let em = lemma th (p + !!p) [ excluded_middle' p |> apply ]
        mp (mp (mp (OrderedDomain.in_theory th (glue_or_cases p (!!p) R)) cp) cnp) em

    /// `R`, from `0 ≤ x ⇒ R` and `x ≤ 0 ⇒ R` — Totality.
    let private by_sign (th: Theory) (x: DomainTerm<int>) (R: Prop) (cpos: Theorem) (cneg: Theorem) : Theorem =
        let D = Integers.order_domain th
        mp (mp (mp (OrderedDomain.in_theory th (glue_or_cases (D.Zero <=. x) (x <=. D.Zero) R)) cpos) cneg) (OrderedDomain.le_total D D.Zero x)

    /// `h ⇒ φ`, from `⊢ φ`.
    let private under (th: Theory) (h: Prop) (t: Theorem) : Theorem = lemma th (h ==> Prop(Expr.Cast<bool> t.Stmt)) [ Taut t |> apply_right ]

    /// `⊢ x` and `⊢ y`, from `⊢ x ∧ y`.
    let private conj_fst (th: Theory) (x: Prop) (y: Prop) (t: Theorem) : Theorem = mp (lemma th ((x * y) ==> x) [ Taut (strengthen_and x y) |> apply ]) t
    let private conj_snd (th: Theory) (x: Prop) (y: Prop) (t: Theorem) : Theorem =
        mp (lemma th ((x * y) ==> y) [ commute_and x y |> at_left; Taut (strengthen_and y x) |> apply ]) t

    /// A binder-free theorem about n integers, derived once per theory at schema metavariables.
    let private cachedn (name: string) (n: int) (f: Theory -> Term<int> list -> Theorem) : Theory -> Term<int> list -> Theorem =
        let table = System.Runtime.CompilerServices.ConditionalWeakTable<Theory, Expr<int> list -> Theorem>()
        let create =
            System.Runtime.CompilerServices.ConditionalWeakTable<Theory, Expr<int> list -> Theorem>.CreateValueCallback(fun th ->
                Tactics.Schema.thn name n (fun xs -> f th (xs |> List.map (fun e -> IntTerm e :> Term<int>))))
        fun th args -> (table.GetValue(th, create)) (args |> List.map (fun a -> a.Expr))
    let private cached1 name f = let g = cachedn name 1 (fun th -> function [ a ] -> f th a | _ -> invalidArg "args" name) in fun th a -> g th [ a ]
    let private cached2 name f = let g = cachedn name 2 (fun th -> function [ a; b ] -> f th a b | _ -> invalidArg "args" name) in fun th a b -> g th [ a; b ]
    let private cached3 name f = let g = cachedn name 3 (fun th -> function [ a; b; c ] -> f th a b c | _ -> invalidArg "args" name) in fun th a b c -> g th [ a; b; c ]
    let private cached4 name f = let g = cachedn name 4 (fun th -> function [ a; b; c; d ] -> f th a b c d | _ -> invalidArg "args" name) in fun th a b c d -> g th [ a; b; c; d ]

    (* Order and divisibility facts the gcd needs *)

    /// `0 ≤ x·x` — `x = 0`, or (15.34).
    let private sq_nonneg =
        cached1 "dv_sq_nonneg" (fun th (x: Term<int>) ->
            let D = Integers.order_domain th
            let zero, x = D.Zero, D.Term x
            let Z, R = (x == zero), (zero <=. x * x)
            let cZ = lemma th (Z ==> R) [
                        Deduce' (reflex_implies Z)                      |> at [right_branch; right_branch; left_branch]
                        Deduce' (reflex_implies Z)                      |> at [right_branch; right_branch; right_branch]
                        Integers.ring th (zero * zero) zero             |> at [right_branch; right_branch]
                        OrderedDomain.le_refl D zero                    |> apply_right ]
            let posl = lemma th (pos_of (x * x) ==> (zero <. x * x)) [ OrderedDomain.pos_lt D (x * x) |> at_left ]   // (15.40)
            let cNZ = lemma th ((!!Z) ==> R) [
                        Establish (OrderedDomain.sq_pos D x)            |> apply                                          // (15.34)
                        Establish posl                                  |> apply
                        Deduce (lt_le th zero (x * x))                  |> apply_right ]
            by_em th Z R cZ cNZ)

    /// `0 < a ∧ 0 ≤ b ⇒ 0 < a + b`
    let private lt_add_nonneg (th: Theory) (a: DomainTerm<int>) (b: DomainTerm<int>) : Theorem =
        let D = Integers.order_domain th
        let zero = D.Zero
        lemma th (((zero <. a) * (zero <=. b)) ==> (zero <. a + b)) [
            Establish (lemma th ((zero <. a) ==> (b <. a + b)) [
                        OrderedDomain.lt_translate D zero a b           |> at_left                                        // (15.42)
                        Integers.ring th (zero + b) b                   |> at [left_branch; left_branch]
                        Deduce (reflex_implies (b <. a + b))            |> apply_right ]) |> apply
            Deduce (OrderedDomain.le_lt_trans D zero b (a + b))         |> apply_right ]

    /// `¬(b = 0 ∧ c = 0) ⇒ 0 < b·b + c·c`
    let private pos_sum_sq =
        cached2 "dv_pos_sum_sq" (fun th (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let zero, b, c = D.Zero, D.Term b, D.Term c
            let Zb, Zc = (b == zero), (c == zero)
            let bb, cc = b * b, c * c
            let R = zero <. bb + cc
            let posl (x: DomainTerm<int>) = lemma th (pos_of x ==> (zero <. x)) [ OrderedDomain.pos_lt D x |> at_left ]
            let cB = lemma th ((!!Zb) ==> R) [
                        Establish (OrderedDomain.sq_pos D b)            |> apply
                        Establish (posl bb)                             |> apply
                        Establish (under th (!!Zb) (sq_nonneg th c))    |> apply
                        Deduce (lt_add_nonneg th bb cc)                 |> apply_right ]
            let cC = lemma th ((!!Zc) ==> R) [
                        Integers.ring th (bb + cc) (cc + bb)            |> at [right_branch; right_branch]
                        Establish (OrderedDomain.sq_pos D c)            |> apply
                        Establish (posl cc)                             |> apply
                        Establish (under th (!!Zc) (sq_nonneg th b))    |> apply
                        Deduce (lt_add_nonneg th cc bb)                 |> apply_right ]
            lemma th ((!!(Zb * Zc)) ==> R) [
                distrib_not_and Zb Zc                                   |> at_left
                Taut (mp (mp (OrderedDomain.in_theory th (glue_or_cases (!!Zb) (!!Zc) R)) cB) cC) |> apply ])

    let private dvd_le_at (th: Theory) (g: Term<int>) (d: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let g', d' = D.Term g, D.Term d
        let G0, R = (zero <. g'), (d' <=. g')
        let inner =
            dvd_elim th d g (G0 ==> R) (fun k ->
                let k' = D.Term (tm k)
                let Hk = body d g k
                let P = zero <. d'
                let A = Hk * G0
                // 0 < d: then 0 < k (15.43), so 1 ≤ k, and d = d·1 ≤ d·k = g.
                let kpos = lemma th ((A * P) ==> (zero <. k')) [
                            Deduce' (OrderedDomain.lt_mul D zero k' d') |> apply_right                                    // 0 < k → 0·d < k·d
                            Integers.ring th (zero * d') zero           |> at [right_branch; left_branch]
                            Integers.ring th (k' * d') (d' * k')        |> at [right_branch; right_branch]
                            Deduce' (reflex_implies Hk)                 |> at [right_branch; right_branch]                // d·k → g
                            Deduce (reflex_implies G0)                  |> apply_right ]
                let cP = lemma th ((A * P) ==> R) [
                            Establish kpos                              |> apply
                            Establish (one_le_of_pos th k')             |> apply
                            Establish (lt_le th zero d')                |> apply
                            Integers.ring th d' (d' * one)              |> at [right_branch; left_branch]
                            Deduce' (OrderedDomain.eq_sym th (d' * k') g') |> at [right_branch; right_branch]             // g → d·k
                            Deduce (OrderedDomain.le_mul_nonneg D d' one k') |> apply_right ]
                // d ≤ 0 < g.
                let cN = lemma th ((A * !!P) ==> R) [
                            OrderedDomain.not_lt D zero d'              |> at [left_branch; right_branch]                 // ¬(0 < d) → d ≤ 0
                            Establish (OrderedDomain.le_lt_trans D d' zero g') |> apply
                            Deduce (lt_le th d' g')                     |> apply_right ]
                let full = mp (mp (OrderedDomain.in_theory th (glue_cases A P R)) cP) cN
                lemma th (Hk ==> (G0 ==> R)) [
                    shunt' Hk G0 R |> Commute                           |> apply
                    Taut full                                           |> apply ])
        lemma th ((G0 * dvd d' g') ==> R) [
            commute_and G0 (dvd d' g')                                  |> at_left
            shunt' (dvd d' g') G0 R                                     |> apply
            Taut inner                                                  |> apply ]

    let private dvd_le_c = cached2 "dv_dvd_le" dvd_le_at
    /// `0 < g ∧ d ∣ g ⇒ d ≤ g` — a divisor of a positive integer is at most it. With `d·k = g`: if
    /// `0 < d` then `0 < k` (15.43), so `1 ≤ k` and `d ≤ d·k`; otherwise `d ≤ 0 < g`.
    let dvd_le (th: Theory) (g: Term<int>) (d: Term<int>) : Theorem = dvd_le_c th g d

    let private zero_dvd_at (th: Theory) (x: Term<int>) (b: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero = D.Zero
        let x', b' = D.Term x, D.Term b
        let Z, R = (x' == zero), (b' == zero)
        let inner =
            dvd_elim th x b (Z ==> R) (fun k ->
                let k' = D.Term (tm k)
                let Hk = body x b k
                lemma th (Hk ==> (Z ==> R)) [
                    shunt' Hk Z R |> Commute                            |> apply
                    Deduce' (OrderedDomain.eq_sym th (x' * k') b')      |> at [right_branch; left_branch]                 // b → x·k
                    Deduce' (reflex_implies Z)                          |> at [right_branch; left_branch; left_branch]    // x → 0
                    Integers.ring th (zero * k') zero                   |> at [right_branch; left_branch]
                    IntegralDomain.refl_true th zero                    |> apply_right ])
        lemma th ((Z * dvd x' b') ==> R) [
            commute_and Z (dvd x' b')                                   |> at_left
            shunt' (dvd x' b') Z R                                      |> apply
            Taut inner                                                  |> apply ]

    let private zero_dvd_c = cached2 "dv_zero_dvd" zero_dvd_at
    /// `x = 0 ∧ x ∣ b ⇒ b = 0` — 0 divides only 0.
    let zero_dvd (th: Theory) (x: Term<int>) (b: Term<int>) : Theorem = zero_dvd_c th x b

    let private dvd_antisym_nonneg_at (th: Theory) (x: Term<int>) (y: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero = D.Zero
        let x', y' = D.Term x, D.Term y
        let N = (zero <=. x') * (zero <=. y')
        let E1, E2 = (x' == y'), (x' == -y')
        let R = x' == y'
        let c1 = lemma th ((N * E1) ==> R) [ Deduce (reflex_implies E1) |> apply_right ]
        // x = −y with both non-negative: 0 ≤ −y, so y ≤ 0, so y = 0 = x.
        let ny = lemma th ((zero <=. -y') ==> (y' <=. zero)) [
                    Integers.ring th y' (-(-y'))                        |> at [right_branch; left_branch]
                    Deduce (OrderedDomain.neg_le_zero D (-y'))          |> apply_right ]
        let nonneg_negy = lemma th ((N * E2) ==> (zero <=. -y')) [
                    Deduce' (OrderedDomain.eq_sym th x' (-y'))          |> at [right_branch; right_branch]                // −y → x
                    Deduce (reflex_implies (zero <=. x'))               |> apply_right ]
        let c2 = lemma th ((N * E2) ==> R) [
                    Deduce' (reflex_implies E2)                         |> at [right_branch; left_branch]                 // x → −y
                    Establish nonneg_negy                               |> apply
                    Establish ny                                        |> apply
                    Establish (OrderedDomain.le_antisym D y' zero)      |> apply                                          // y = 0
                    Deduce' (reflex_implies (y' == zero))               |> at [right_branch; left_branch; apply_unary]
                    Deduce' (reflex_implies (y' == zero))               |> at [right_branch; right_branch]
                    Integers.ring th (-zero) zero                       |> at [right_branch; left_branch]
                    IntegralDomain.refl_true th zero                    |> apply_right ]
        lemma th ((N * (dvd x' y' * dvd y' x')) ==> R) [
            Ident (dvd_antisym th x y)                                  |> at [left_branch; right_branch]                 // (15.83)
            commute_and N (E1 + E2)                                     |> at_left
            Taut (by_cases2 th E1 E2 N R c1 c2)                         |> apply ]

    let private dvd_antisym_nonneg_c = cached2 "dv_antisym_nonneg" dvd_antisym_nonneg_at
    /// `(0 ≤ x ∧ 0 ≤ y) ∧ (x ∣ y ∧ y ∣ x) ⇒ x = y` — (15.83) leaves `x = ±y`, and `x = −y` with both
    /// non-negative makes both 0.
    let dvd_antisym_nonneg (th: Theory) (x: Term<int>) (y: Term<int>) : Theorem = dvd_antisym_nonneg_c th x y

    /// `d ∣ x ⇒ d ∣ −x` — (15.84) with `−1`.
    let private dvd_negate (th: Theory) (d: Term<int>) (x: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let d', x' = D.Term d, D.Term x
        lemma th (dvd d' x' ==> dvd d' (-x')) [
            Integers.ring th (-x') (x' * (-D.One))                      |> at [right_branch; right_branch]
            Deduce (dvd_mul th d' x' (-D.One))                          |> apply_right ]

    let private dvd_neg_eq_c =
        cached2 "dv_neg_eq" (fun th (d: Term<int>) (x: Term<int>) ->
            let D = Integers.order_domain th
            let d', x' = D.Term d, D.Term x
            let L, R = dvd d' (-x'), dvd d' x'
            let fwd = lemma th (L ==> R) [
                        Integers.ring th x' (-(-x'))                    |> at [right_branch; right_branch]
                        Deduce (dvd_negate th d' (-x'))                 |> apply_right ]
            lemma th (L == R) [
                mutual_implication' L R |> Commute                      |> apply
                Taut (conj fwd (dvd_negate th d' x'))                   |> apply ])
    /// `(d ∣ −x) = (d ∣ x)`
    let dvd_neg_eq (th: Theory) (d: Term<int>) (x: Term<int>) : Theorem = dvd_neg_eq_c th d x

    let private abs_dvd_c =
        cached1 "dv_abs_dvd" (fun th (b: Term<int>) ->
            let D = Integers.order_domain th
            let b' = D.Term b
            let R = dvd (abs_of b') b'
            let cpos = lemma th ((D.Zero <=. b') ==> R) [
                        Deduce' (OrderedDomain.abs_of_nonneg D b')      |> at [right_branch; left_branch]                 // abs.b → b
                        Taut (dvd_refl th b')                           |> apply_right ]
            let cneg = lemma th ((b' <=. D.Zero) ==> R) [
                        Deduce' (OrderedDomain.abs_of_nonpos D b')      |> at [right_branch; left_branch]                 // abs.b → −b
                        Taut (dvd_intro th (-b') b' (-D.One) (Integers.ring_ident th ((-b') * (-D.One)) b')) |> apply_right ]
            by_sign th b' R cpos cneg)
    /// `abs.b ∣ b`
    let abs_dvd (th: Theory) (b: Term<int>) : Theorem = abs_dvd_c th b

    let private dvd_abs_eq_c =
        cached2 "dv_dvd_abs_eq" (fun th (d: Term<int>) (b: Term<int>) ->
            let D = Integers.order_domain th
            let d', b' = D.Term d, D.Term b
            let E = dvd d' (abs_of b') == dvd d' b'
            let cpos = lemma th ((D.Zero <=. b') ==> E) [
                        Deduce' (OrderedDomain.abs_of_nonneg D b')      |> at [right_branch; left_branch; right_branch]
                        def_true (dvd d' b') |> Commute                 |> apply_right ]
            let cneg = lemma th ((b' <=. D.Zero) ==> E) [
                        Deduce' (OrderedDomain.abs_of_nonpos D b')      |> at [right_branch; left_branch; right_branch]
                        Ident (dvd_neg_eq th d' b')                     |> at [right_branch; left_branch]
                        def_true (dvd d' b') |> Commute                 |> apply_right ]
            by_sign th b' E cpos cneg)
    /// `(d ∣ abs.b) = (d ∣ b)`
    let dvd_abs_eq (th: Theory) (d: Term<int>) (b: Term<int>) : Theorem = dvd_abs_eq_c th d b

    (* The definition, in parts *)

    type private GcdFacts = { Nonneg: Theorem; DvdL: Theorem; DvdR: Theorem; All: Theorem; Dummy: IntVar }

    let private gcd_facts (th: Theory) (b: Term<int>) (c: Term<int>) : GcdFacts =
        let D = Integers.order_domain th
        let b', c', g = D.Term b, D.Term c, D.Term (gcd_of b c)
        let dv = gcd_dummy b c
        let x = D.Term (tm dv)
        let N, B1, B2 = (D.Zero <=. g), dvd g b', dvd g c'
        let ALL = qall dv (dvd x b' * dvd x c') (dvd x g)
        let ax = axiom th (gcd_spec_with dv b c)
        let left = conj_fst th (N * (B1 * B2)) ALL ax
        let b12 = conj_snd th N (B1 * B2) left
        { Nonneg = conj_fst th N (B1 * B2) left; DvdL = conj_fst th B1 B2 b12; DvdR = conj_snd th B1 B2 b12
          All = conj_snd th (N * (B1 * B2)) ALL ax; Dummy = dv }

    /// `0 ≤ b gcd c` — the definition of gcd.
    let gcd_nonneg (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (gcd_facts th b c).Nonneg
    /// `b gcd c ∣ b` — the definition of gcd.
    let gcd_dvd_l (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (gcd_facts th b c).DvdL
    /// `b gcd c ∣ c` — the definition of gcd.
    let gcd_dvd_r (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (gcd_facts th b c).DvdR

    /// `e ∣ b ∧ e ∣ c ⇒ e ∣ b gcd c` — the definition of gcd, instantiated (9.13) at `e`.
    let dvd_gcd (th: Theory) (e: Term<int>) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let f = gcd_facts th b c
        let dv = f.Dummy
        let b', c', g, x = D.Term b, D.Term c, D.Term (gcd_of b c), D.Term (tm dv)
        let Rp, Pp = pred_of dv (dvd x b' * dvd x c'), pred_of dv (dvd x g)
        let inst = lemma th (qall dv (dvd x b' * dvd x c') (dvd x g) ==> (Rp.[e] ==> Pp.[e])) [ trade_forall_implies dv Rp Pp |> at_left ]
        mp inst f.All

    let private gcd_eq_c =
        cached3 "dv_gcd_eq" (fun th (b: Term<int>) (c: Term<int>) (g: Term<int>) ->
            let D = Integers.order_domain th
            let G, g' = D.Term (gcd_of b c), D.Term g
            let H = ((D.Zero <=. g') * (dvd g' (D.Term b) * dvd g' (D.Term c))) * dvd G g'
            lemma th (H ==> (G == g')) [
                Establish (dvd_gcd th g b c)                            |> apply                                          // g ∣ b gcd c
                Establish (under th H (gcd_nonneg th b c))              |> apply
                Deduce (dvd_antisym_nonneg th G g')                     |> apply_right ])
    /// `(0 ≤ g ∧ (g ∣ b ∧ g ∣ c)) ∧ b gcd c ∣ g ⇒ b gcd c = g` — the uniqueness that every gcd law below
    /// is proved by: the definition gives `g ∣ b gcd c`, and `dvd_antisym_nonneg`.
    let gcd_eq (th: Theory) (b: Term<int>) (c: Term<int>) (g: Term<int>) : Theorem = gcd_eq_c th b c g

    let private gcd_pos_c =
        cached2 "dv_gcd_pos" (fun th (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let zero = D.Zero
            let G, b', c' = D.Term (gcd_of b c), D.Term b, D.Term c
            let Zb, Zc = (b' == zero), (c' == zero)
            let Z = G <=. zero
            let both = lemma th (Z ==> (Zb * Zc)) [
                        Establish (under th Z (gcd_nonneg th b c))      |> apply
                        Establish (OrderedDomain.le_antisym D G zero)   |> apply                                          // b gcd c = 0
                        Establish (under th Z (gcd_dvd_l th b c))       |> apply
                        Establish (under th Z (gcd_dvd_r th b c))       |> apply
                        Establish (zero_dvd th G b)                     |> apply
                        Establish (zero_dvd th G c)                     |> apply
                        Deduce (reflex_implies (Zb * Zc))               |> apply_right ]
            lemma th ((!!(Zb * Zc)) ==> (zero <. G)) [
                def_implies_contr (!!(Zb * Zc)) (zero <. G)             |> apply
                double_negation (Zb * Zc)                               |> at_right
                OrderedDomain.not_lt D zero G                           |> at_left
                Taut both                                               |> apply ])
    /// `¬(b = 0 ∧ c = 0) ⇒ 0 < b gcd c` — if it were 0, it would divide only 0 (`zero_dvd`).
    let gcd_pos (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = gcd_pos_c th b c

    let private gcd_greatest_c =
        cached3 "dv_gcd_greatest" (fun th (d: Term<int>) (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let zero = D.Zero
            let G, b', c', d' = D.Term (gcd_of b c), D.Term b, D.Term c, D.Term d
            let NZ = !!((b' == zero) * (c' == zero))
            lemma th ((NZ * (dvd d' b' * dvd d' c')) ==> (d' <=. G)) [
                Establish (gcd_pos th b c)                              |> apply
                Establish (dvd_gcd th d b c)                            |> apply
                Deduce (dvd_le th G d)                                  |> apply_right ])
    /// **(15.92)**, Gries' definition, as a theorem: `¬(b = 0 ∧ c = 0) ∧ (d ∣ b ∧ d ∣ c) ⇒ d ≤ b gcd c`
    /// — `b gcd c` is a common divisor (`gcd_dvd_l`/`gcd_dvd_r`) and the greatest one. A common divisor
    /// divides `b gcd c` (the definition), which is positive (`gcd_pos`), so `dvd_le`.
    let gcd_greatest (th: Theory) (d: Term<int>) (b: Term<int>) (c: Term<int>) : Theorem = gcd_greatest_c th d b c

    /// **(15.92)** `0 gcd 0 = 0` — `0` is a non-negative common divisor of 0 and 0, and divisible by
    /// `0 gcd 0` (15.79).
    let gcd_zero_zero (th: Theory) : Theorem =
        let D = Integers.order_domain th
        let zero = D.Zero
        let G = D.Term (gcd_of zero zero)
        let prem = conj (conj (lemma th (zero <=. zero) [ OrderedDomain.le_refl D zero |> apply ])
                              (conj (dvd_zero th zero) (dvd_zero th zero))) (dvd_zero th G)
        mp (gcd_eq th zero zero zero) prem

    (* Bézout *)

    type private BezoutCore = { Thm: Theorem; X: IntVar; U: IntVar; V: IntVar; HE: Prop; Rbz: IntPred; Bbz: IntPred; NZ: Prop }

    /// See `bezout_least`. Returns the dummies, for `from_bezout_core`.
    let private bezout_core (th: Theory) (b: Term<int>) (c: Term<int>) : BezoutCore =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let b', c' = D.Term b, D.Term c
        let avoid = [ b.Expr.Raw; c.Expr.Raw ]
        let x = fresh_int "x" avoid
        let y = fresh_int "y" (x.Expr.Raw :: avoid)
        let u = fresh_int "u" (x.Expr.Raw :: y.Expr.Raw :: avoid)
        let v = fresh_int "v" (u.Expr.Raw :: x.Expr.Raw :: y.Expr.Raw :: avoid)
        let tv (w: IntVar) = D.Term (tm w)
        let X, U, V = tv x, tv u, tv v
        let lin (p: DomainTerm<int>) (q: DomainTerm<int>) = p * b' + q * c'
        let EXf (w: DomainTerm<int>) = qex u T (qex v T (w == lin U V))
        let P = pred_of x ((zero <. X) * EXf X)
        let Rbz = pred_of x ((zero <. X) * (dvd X b' * dvd X c'))
        let Bbz = pred_of x (EXf X)
        let Bz = exists (x, Rbz, Bbz)
        let NZ = !!((b' == zero) * (c' == zero))
        // S = {x | 0 < x ∧ (∃u,v |: x = u·b + v·c)} contains e = b·b + c·c.
        let e = b' * b' + c' * c'
        let ex_e =
            let inner = exists_intro v (pred_of v (e == lin b' V)) c'
            let outer = exists_intro u (pred_of u (qex v T (e == lin U V))) b'
            mp (OrderedDomain.in_theory th outer) (mp (OrderedDomain.in_theory th inner) (lemma th (e == lin b' c') [ IntegralDomain.refl_true th e |> apply ]))
        let minimal (w: Term<int>) = qall y ((leq 0 (tm y)) * (lt (tm y) w)) (!!P.[tm y])
        let M = minimal (tm x)
        let Rng = pred_of x ((leq 0 (tm x)) * P.[tm x])
        let Min = pred_of x (minimal (tm x))
        let least = least_element th x y P e
        // The least element x, with x = u·b + v·c, divides b and c: otherwise r = t mod x is in S, and r < x.
        let HE, H0 = (X == lin U V), (zero <. X)
        let K = ((leq 0 (tm x)) * H0) * M
        let A = HE * K
        let x_dvd (tt: DomainTerm<int>) (cu: DomainTerm<int> -> DomainTerm<int>) (cv: DomainTerm<int> -> DomainTerm<int>) =
            let q, r = D.Term (div_of tt X), D.Term (mod_of tt X)
            let Cu, Cv = cu q, cv q
            let Lr = lemma th ((H0 * HE) ==> (r == lin Cu Cv)) [
                        Establish (div_mod_def th tt X)                 |> apply                                                       // t = q·x + r
                        Integers.ring th r ((q * X + r) - q * X)        |> at [right_branch; left_branch]
                        Deduce' (OrderedDomain.eq_sym th tt (q * X + r)) |> at [right_branch; left_branch; left_branch]                // q·x + r → t
                        Deduce' (reflex_implies HE)                     |> at [right_branch; left_branch; right_branch; right_branch]  // x → u·b + v·c
                        Integers.ring th (tt - q * lin U V) (lin Cu Cv) |> at [right_branch; left_branch]
                        IntegralDomain.refl_true th (lin Cu Cv)         |> apply_right ]
            let ex_r = lemma th ((H0 * HE) ==> EXf r) [
                        Establish Lr                                    |> apply
                        Establish (exists_intro v (pred_of v (r == lin Cu V)) Cv) |> apply                                             // (9.28)
                        Deduce (exists_intro u (pred_of u (qex v T (r == lin U V))) Cu) |> apply_right ]
            let Pr = lemma th (((H0 * HE) * (zero <. r)) ==> P.[r]) [
                        Establish ex_r                                  |> apply
                        Deduce (reflex_implies P.[r])                   |> apply_right ]
            let Rm = pred_of y ((leq 0 (tm y)) * (lt (tm y) (tm x)))
            let NP = pred_of y (!!P.[tm y])
            let inst = lemma th (M ==> (Rm.[r] ==> NP.[r])) [ trade_forall_implies y Rm NP |> at_left ]                              // (9.13)
            let mi = lemma th ((M * Rm.[r]) ==> NP.[r]) [ shunt' M Rm.[r] NP.[r] |> apply; Taut inst |> apply ]
            let le0 = lemma th (A ==> (r <=. zero)) [
                        OrderedDomain.not_lt D zero r |> Commute        |> at_right                                                    // r ≤ 0 → ¬(0 < r)
                        ident_implies_false_not (zero <. r) |> Commute  |> at_right                                                    // → (0 < r ⇒ F)
                        shunt' A (zero <. r) F |> Commute               |> apply
                        Establish (mod_nonneg th tt X)                  |> apply
                        Establish (mod_lt th tt X)                      |> apply
                        Establish ex_r                                  |> apply
                        Establish Pr                                    |> apply
                        Establish mi                                    |> apply
                        Deduce (lemma th ((P.[r] * !!P.[r]) ==> F) [ contr P.[r] |> at_left ]) |> apply_right ]
            lemma th (A ==> dvd X tt) [
                Deduce' (dvd_iff_mod_zero th X tt)                      |> apply_right                                                 // x ∣ t → t mod x = 0
                Establish le0                                           |> apply
                Establish (mod_nonneg th tt X)                          |> apply
                Deduce (OrderedDomain.le_antisym D r zero)              |> apply_right ]
        let dvb = x_dvd b' (fun q -> one - q * U) (fun q -> -(q * V))
        let dvc = x_dvd c' (fun q -> -(q * U)) (fun q -> one - q * V)
        let core = lemma th (A ==> Bz) [
                    trade_exists_and x Rbz Bbz                          |> at_right                                                    // (9.19)
                    Establish dvb                                       |> apply
                    Establish dvc                                       |> apply
                    Establish (exists_intro v (pred_of v HE) V)         |> apply
                    Establish (exists_intro u (pred_of u (qex v T HE)) U) |> apply
                    Deduce (exists_intro x (Rbz * Bbz) X)               |> apply_right ]
        // Eliminate v, u, then the least element x.
        let Q2 = K ==> Bz
        let EXv = qex v T HE
        let tv_ = lemma th ((T * HE) ==> Q2) [
                    commute_and T HE                                    |> at_left
                    ident_and HE                                        |> at_left
                    shunt' HE K Bz |> Commute                           |> apply
                    Taut core                                           |> apply ]
        let elimV = exists_elim th v truepred (pred_of v HE) Q2 tv_
        let tu_ = lemma th ((T * EXv) ==> Q2) [
                    commute_and T EXv                                   |> at_left
                    ident_and EXv                                       |> at_left
                    Taut elimV                                          |> apply ]
        let elimU = exists_elim th u truepred (pred_of u EXv) Q2 tu_
        let elimU' = lemma th ((EXf X * K) ==> Bz) [ shunt' (EXf X) K Bz |> apply; Taut elimU |> apply ]
        let xbody = lemma th ((Rng.[tm x] * Min.[tm x]) ==> Bz) [ Deduce elimU' |> apply_right ]
        let elimX = exists_elim th x Rng Min Bz xbody
        let thm =
            lemma th (NZ ==> Bz) [
                Establish (pos_sum_sq th b c)                           |> apply
                Establish (lt_le th zero e)                             |> apply
                Establish (under th NZ ex_e)                            |> apply
                Establish least                                         |> apply                                                       // (15.49)
                Deduce elimX                                            |> apply_right ]
        { Thm = thm; X = x; U = u; V = v; HE = HE; Rbz = Rbz; Bbz = Bbz; NZ = NZ }

    /// **Bézout's lemma**, in the form gcd's existence needs — it does not mention gcd:
    ///
    /// ```
    /// ¬(b = 0 ∧ c = 0) ⇒ (∃x | 0 < x ∧ (x ∣ b ∧ x ∣ c) : (∃u |: (∃v |: x = u·b + v·c)))
    /// ```
    ///
    /// S = `{x | 0 < x ∧ (∃u,v |: x = u·b + v·c)}` contains `b·b + c·c`, so it has a least element x
    /// (`least_element`, (15.49)). With `q, r := t ÷ x, t mod x` for `t` either of `b`, `c`, the remainder
    /// `r = t − q·x` is again a combination; `0 ≤ r < x` (15.90), so `r` is not in S, so `r = 0`, so
    /// `x ∣ t` (`dvd_iff_mod_zero`).
    let bezout_least (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (bezout_core th b c).Thm

    /// From `inner : (x = u·b + v·c ∧ (0 < x ∧ (x ∣ b ∧ x ∣ c))) ⇒ Q`, proved about the core's own
    /// dummies, conclude `¬(b = 0 ∧ c = 0) ⇒ Q` — three ∃-eliminations. `Q` must not mention them free.
    let private from_bezout_core (th: Theory) (core: BezoutCore) (Q: Prop) (inner: Theorem) : Theorem =
        let HE = core.HE
        let Rx = core.Rbz.[tm core.X]
        let EXv = qex core.V T HE
        let Qx = Rx ==> Q
        let tv_ = lemma th ((T * HE) ==> Qx) [
                    commute_and T HE                                    |> at_left
                    ident_and HE                                        |> at_left
                    shunt' HE Rx Q |> Commute                           |> apply
                    Taut inner                                          |> apply ]
        let elimV = exists_elim th core.V truepred (pred_of core.V HE) Qx tv_
        let tu_ = lemma th ((T * EXv) ==> Qx) [
                    commute_and T EXv                                   |> at_left
                    ident_and EXv                                       |> at_left
                    Taut elimV                                          |> apply ]
        let elimU = exists_elim th core.U truepred (pred_of core.U EXv) Qx tu_
        let EXx = core.Bbz.[tm core.X]
        let elimU' = lemma th ((EXx * Rx) ==> Q) [ shunt' EXx Rx Q |> apply; Taut elimU |> apply ]
        let xbody = lemma th ((Rx * EXx) ==> Q) [ Deduce elimU' |> apply_right ]
        let elimX = exists_elim th core.X core.Rbz core.Bbz Q xbody
        lemma th (core.NZ ==> Q) [
            Establish core.Thm                                          |> apply
            Deduce elimX                                                |> apply_right ]

    /// **Existence of gcd** — proved WITHOUT the definition of gcd, which this justifies:
    ///
    /// ```
    /// (∃g | 0 ≤ g : (g ∣ b ∧ g ∣ c) ∧ (∀d | d ∣ b ∧ d ∣ c : d ∣ g))
    /// ```
    ///
    /// For b, c not both 0, `bezout_least`'s x: a common divisor d divides `u·b + v·c = x` (15.84),
    /// Gries' ex.15.65), so (∀d …) by generalization and (9.5). For `b = c = 0`, `g := 0`.
    let gcd_exists (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero = D.Zero
        let b', c' = D.Term b, D.Term c
        let g = fresh_int "g" [ b.Expr.Raw; c.Expr.Raw ]
        let d = fresh_int "d" [ g.Expr.Raw; b.Expr.Raw; c.Expr.Raw ]
        let Dd = D.Term (tm d)
        let Rd = pred_of d (dvd Dd b' * dvd Dd c')
        let Phi (w: DomainTerm<int>) = (dvd w b' * dvd w c') * qall d (dvd Dd b' * dvd Dd c') (dvd Dd w)
        let Rg = pred_of g (zero <=. D.Term (tm g))
        let Bg = pred_of g (Phi (D.Term (tm g)))
        let goal = exists (g, Rg, Bg)
        // b, c not both 0: bezout_least's x.
        let core = bezout_core th b c
        let X, U0, V0 = D.Term (tm core.X), D.Term (tm core.U), D.Term (tm core.V)
        let HE0 = core.HE
        let Rx = core.Rbz.[tm core.X]
        let body = lemma th ((Rd.[tm d] * HE0) ==> dvd Dd X) [
                    Deduce' (reflex_implies HE0)                        |> at [right_branch; right_branch]                 // x → u·b + v·c
                    Integers.ring th (U0 * b' + V0 * c') (b' * U0 + c' * V0) |> at [right_branch; right_branch]
                    Establish (dvd_mul th Dd b' U0)                     |> apply                                           // (15.84)
                    Establish (dvd_mul th Dd c' V0)                     |> apply
                    Deduce (dvd_add th Dd (b' * U0) (c' * V0))          |> apply_right ]
        let body' = lemma th (Rd.[tm d] ==> (HE0 ==> dvd Dd X)) [ shunt' Rd.[tm d] HE0 (dvd Dd X) |> Commute |> apply; Taut body |> apply ]
        let gen = lemma th (qall d Rd.[tm d] (HE0 ==> dvd Dd X)) [
                    trade_forall_implies d Rd (pred_of d (HE0 ==> dvd Dd X)) |> apply
                    Taut body'                                          |> at [select_body]
                    ident_forall_true' d                                |> apply ]
        let allx = lemma th (HE0 ==> qall d Rd.[tm d] (dvd Dd X)) [
                    ident_implies_not_or HE0 (qall d Rd.[tm d] (dvd Dd X)) |> apply
                    distrib_or_forall' d Rd (!!HE0) (pred_of d (dvd Dd X)) |> apply                                        // (9.5)
                    ident_implies_not_or HE0 (dvd Dd X) |> Commute      |> at [select_body]
                    Taut gen                                            |> apply ]
        let inner = lemma th ((HE0 * Rx) ==> goal) [
                    trade_exists_and g Rg Bg                            |> at_right
                    Establish (lt_le th zero X)                         |> apply
                    Establish allx                                      |> apply
                    Deduce (exists_intro g (Rg * Bg) X)                 |> apply_right ]
        let nz = from_bezout_core th core goal inner
        // b = c = 0: g := 0.
        let Zb, Zc = (b' == zero), (c' == zero)
        let Z2 = Zb * Zc
        let all0 = lemma th (qall d Rd.[tm d] (dvd Dd zero)) [
                    Taut (dvd_zero th Dd)                               |> at [select_body]
                    ident_forall_true d Rd                              |> apply ]
        let z0 (t: DomainTerm<int>) (Zt: Prop) = lemma th (Z2 ==> dvd zero t) [
                    Deduce' (reflex_implies Zt)                         |> at [right_branch; right_branch]
                    Taut (dvd_zero th zero)                             |> apply_right ]
        let z = lemma th (Z2 ==> goal) [
                    trade_exists_and g Rg Bg                            |> at_right
                    Establish (z0 b' Zb)                                |> apply
                    Establish (z0 c' Zc)                                |> apply
                    Establish (under th Z2 all0)                        |> apply
                    Establish (under th Z2 (lemma th (zero <=. zero) [ OrderedDomain.le_refl D zero |> apply ])) |> apply
                    Deduce (exists_intro g (Rg * Bg) zero)              |> apply_right ]
        by_em th Z2 goal z nz

    /// Bézout with the given dummies.
    let private gcd_bezout_with (th: Theory) (u: IntVar) (v: IntVar) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero = D.Zero
        let b', c', G = D.Term b, D.Term c, D.Term (gcd_of b c)
        let U, V = D.Term (tm u), D.Term (tm v)
        let lin (p: DomainTerm<int>) (q: DomainTerm<int>) = p * b' + q * c'
        let goal = qex u T (qex v T (G == lin U V))
        let intro (p: DomainTerm<int>) (q: DomainTerm<int>) =
            [ Establish (exists_intro v (pred_of v (G == lin p V)) q) |> apply                                             // (9.28)
              Deduce (exists_intro u (pred_of u (qex v T (G == lin U V))) p) |> apply_right ]
        // b, c not both 0: the least positive combination x IS b gcd c.
        let core = bezout_core th b c
        let X, U0, V0 = D.Term (tm core.X), D.Term (tm core.U), D.Term (tm core.V)
        let HE0 = core.HE
        let Rx = core.Rbz.[tm core.X]
        let tdvd = mp (dvd_add th G (b' * U0) (c' * V0))
                      (conj (mp (dvd_mul th G b' U0) (gcd_dvd_l th b c)) (mp (dvd_mul th G c' V0) (gcd_dvd_r th b c)))
        let gx = lemma th ((HE0 * Rx) ==> dvd G X) [
                    Deduce' (reflex_implies HE0)                        |> at [right_branch; right_branch]                 // x → u·b + v·c
                    Integers.ring th (lin U0 V0) (b' * U0 + c' * V0)    |> at [right_branch; right_branch]
                    Taut tdvd                                           |> apply_right ]
        let geq = lemma th ((HE0 * Rx) ==> (G == lin U0 V0)) [
                    Establish gx                                        |> apply
                    Establish (lt_le th zero X)                         |> apply
                    Deduce' (gcd_eq th b c X)                           |> at [right_branch; left_branch]                  // b gcd c → x
                    Deduce (reflex_implies HE0)                         |> apply_right ]
        let inner = lemma th ((HE0 * Rx) ==> goal) ([ Establish geq |> apply ] @ intro U0 V0)
        let nz = from_bezout_core th core goal inner
        // b = c = 0: b gcd c = 0 = 0·b + 0·c.
        let Zb, Zc = (b' == zero), (c' == zero)
        let Z2 = Zb * Zc
        let gz = lemma th (Z2 ==> (G == lin zero zero)) [
                    Integers.ring th (lin zero zero) zero               |> at [right_branch; right_branch]
                    Deduce' (reflex_implies Zb)                         |> at [right_branch; left_branch; left_branch]
                    Deduce' (reflex_implies Zc)                         |> at [right_branch; left_branch; right_branch]
                    Taut (gcd_zero_zero th)                             |> apply_right ]
        let z = lemma th (Z2 ==> goal) ([ Establish gz |> apply ] @ intro zero zero)
        by_em th Z2 goal z nz

    /// **(15.108) Bézout's identity**: `(∃u |: (∃v |: b gcd c = u·b + v·c))` — for all integers; Gries
    /// states it for naturals and proves it by induction on `c`. The least positive combination of
    /// `bezout_least` is a non-negative common divisor that `b gcd c` divides (15.84), so it is
    /// `b gcd c` (`gcd_eq`); and `0 gcd 0 = 0 = 0·0 + 0·0`.
    let gcd_bezout (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem =
        let u = fresh_int "u" [ b.Expr.Raw; c.Expr.Raw ]
        let v = fresh_int "v" [ u.Expr.Raw; b.Expr.Raw; c.Expr.Raw ]
        gcd_bezout_with th u v b c

    (* Properties of gcd (15.96)–(15.107) *)

    /// `b gcd c = g`, from `0 ≤ g`, `g ∣ b`, `g ∣ c` and `b gcd c ∣ g`.
    let private gcd_is (th: Theory) (b: Term<int>) (c: Term<int>) (g: Term<int>) (nonneg: Theorem) (gb: Theorem) (gc: Theorem) (Gg: Theorem) : Theorem =
        mp (gcd_eq th b c g) (conj (conj nonneg (conj gb gc)) Gg)
    let private dtrans (th: Theory) (x: Term<int>) (y: Term<int>) (z: Term<int>) (xy: Theorem) (yz: Theorem) : Theorem =
        mp (dvd_trans th x y z) (conj xy yz)
    /// `g ∣ abs.x`, from `g ∣ x`.
    let private to_abs (th: Theory) (g: Term<int>) (x: Term<int>) (t: Theorem) : Theorem =
        let D = Integers.order_domain th
        lemma th (dvd (D.Term g) (abs_of (D.Term x))) [ Ident (dvd_abs_eq th g x) |> apply; Taut t |> apply ]
    /// `g ∣ x`, from `g ∣ abs.x`.
    let private from_abs (th: Theory) (g: Term<int>) (x: Term<int>) (t: Theorem) : Theorem =
        let D = Integers.order_domain th
        lemma th (dvd (D.Term g) (D.Term x)) [ Ident (dvd_abs_eq th g x) |> Commute |> apply; Taut t |> apply ]

    let private gcd_sym_c =
        cached2 "dv_gcd_sym" (fun th (b: Term<int>) (c: Term<int>) ->
            gcd_is th b c (gcd_of c b) (gcd_nonneg th c b) (gcd_dvd_r th c b) (gcd_dvd_l th c b)
                (mp (dvd_gcd th (gcd_of b c) c b) (conj (gcd_dvd_r th b c) (gcd_dvd_l th b c))))
    /// **(15.96) Symmetry**: `b gcd c = c gcd b`
    let gcd_sym (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = gcd_sym_c th b c

    let private gcd_assoc_c =
        cached3 "dv_gcd_assoc" (fun th (b: Term<int>) (c: Term<int>) (d: Term<int>) ->
            let bc, cd = gcd_of b c, gcd_of c d
            let G1, G2 = gcd_of bc d, gcd_of b cd
            let g1b = dtrans th G1 bc b (gcd_dvd_l th bc d) (gcd_dvd_l th b c)
            let g1c = dtrans th G1 bc c (gcd_dvd_l th bc d) (gcd_dvd_r th b c)
            let g1cd = mp (dvd_gcd th G1 c d) (conj g1c (gcd_dvd_r th bc d))
            let g2c = dtrans th G2 cd c (gcd_dvd_r th b cd) (gcd_dvd_l th c d)
            let g2d = dtrans th G2 cd d (gcd_dvd_r th b cd) (gcd_dvd_r th c d)
            let g2bc = mp (dvd_gcd th G2 b c) (conj (gcd_dvd_l th b cd) g2c)
            let g2g1 = mp (dvd_gcd th G2 bc d) (conj g2bc g2d)
            mp (OrderedDomain.eq_sym th G2 G1) (gcd_is th b cd G1 (gcd_nonneg th bc d) g1b g1cd g2g1))
    /// **(15.97) Associativity**: `(b gcd c) gcd d = b gcd (c gcd d)` — each side is a non-negative common
    /// divisor of b, c, d that the other divides.
    let gcd_assoc (th: Theory) (b: Term<int>) (c: Term<int>) (d: Term<int>) : Theorem = gcd_assoc_c th b c d

    let private gcd_self_c =
        cached1 "dv_gcd_self" (fun th (b: Term<int>) ->
            let D = Integers.order_domain th
            gcd_is th b b (abs_of (D.Term b)) (OrderedDomain.abs_nonneg D b) (abs_dvd th b) (abs_dvd th b) (to_abs th (gcd_of b b) b (gcd_dvd_l th b b)))
    /// **(15.98)** `b gcd b = abs.b`
    let gcd_self (th: Theory) (b: Term<int>) : Theorem = gcd_self_c th b

    let private gcd_one_c =
        cached1 "dv_gcd_one" (fun th (b: Term<int>) ->
            let D = Integers.order_domain th
            let one = D.One
            gcd_is th one b one (mp (lt_le th D.Zero one) (zero_lt_one th)) (dvd_refl th one) (one_dvd th b) (gcd_dvd_l th one b))
    /// **(15.99) Zero**: `1 gcd b = 1` — 1 is the zero of gcd in the divisibility order.
    let gcd_one (th: Theory) (b: Term<int>) : Theorem = gcd_one_c th b

    let private gcd_zero_c =
        cached1 "dv_gcd_zero" (fun th (b: Term<int>) ->
            let D = Integers.order_domain th
            let A = abs_of (D.Term b)
            gcd_is th D.Zero b A (OrderedDomain.abs_nonneg D b) (dvd_zero th A) (abs_dvd th b) (to_abs th (gcd_of D.Zero b) b (gcd_dvd_r th D.Zero b)))
    /// **(15.100) Identity**: `0 gcd b = abs.b`
    let gcd_zero (th: Theory) (b: Term<int>) : Theorem = gcd_zero_c th b

    let private gcd_abs_c =
        cached2 "dv_gcd_abs" (fun th (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let ab, ac = abs_of (D.Term b), abs_of (D.Term c)
            let G, H = gcd_of b c, gcd_of ab ac
            let GH = mp (dvd_gcd th G ab ac) (conj (to_abs th G b (gcd_dvd_l th b c)) (to_abs th G c (gcd_dvd_r th b c)))
            gcd_is th b c H (gcd_nonneg th ab ac) (from_abs th H b (gcd_dvd_l th ab ac)) (from_abs th H c (gcd_dvd_r th ab ac)) GH)
    /// **(15.101)** `b gcd c = (abs.b) gcd (abs.c)` — `d ∣ x` and `d ∣ abs.x` are the same (`dvd_abs_eq`).
    let gcd_abs (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = gcd_abs_c th b c

    let private gcd_add_c =
        cached2 "dv_gcd_add" (fun th (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let b', c' = D.Term b, D.Term c
            let G, H = gcd_of b c, gcd_of b' (b' + c')
            let Hb, Hbc = gcd_dvd_l th b' (b' + c'), gcd_dvd_r th b' (b' + c')
            let Hc = lemma th (dvd (D.Term H) c') [
                        Integers.ring th c' ((b' + c') - b')            |> at [right_branch]
                        Taut (mp (dvd_sub th H (b' + c') b') (conj Hbc Hb)) |> apply ]
            let Gbc = mp (dvd_add th G b' c') (conj (gcd_dvd_l th b c) (gcd_dvd_r th b c))
            gcd_is th b c H (gcd_nonneg th b' (b' + c')) Hb Hc (mp (dvd_gcd th G b' (b' + c')) (conj (gcd_dvd_l th b c) Gbc)))
    /// **(15.102)** `b gcd c = b gcd (b + c)` — the two pairs have the same common divisors (ex.15.65).
    let gcd_add (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = gcd_add_c th b c

    let private gcd_sub_c =
        cached2 "dv_gcd_sub" (fun th (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let b', c' = D.Term b, D.Term c
            let G, H = gcd_of b c, gcd_of b' (b' - c')
            let Hb, Hbc = gcd_dvd_l th b' (b' - c'), gcd_dvd_r th b' (b' - c')
            let Hc = lemma th (dvd (D.Term H) c') [
                        Integers.ring th c' (b' - (b' - c'))            |> at [right_branch]
                        Taut (mp (dvd_sub th H b' (b' - c')) (conj Hb Hbc)) |> apply ]
            let Gbc = mp (dvd_sub th G b' c') (conj (gcd_dvd_l th b c) (gcd_dvd_r th b c))
            gcd_is th b c H (gcd_nonneg th b' (b' - c')) Hb Hc (mp (dvd_gcd th G b' (b' - c')) (conj (gcd_dvd_l th b c) Gbc)))
    /// **(15.102)** `b gcd c = b gcd (b − c)`
    let gcd_sub (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = gcd_sub_c th b c

    let private gcd_lin_c =
        cached4 "dv_gcd_lin" (fun th (a: Term<int>) (b: Term<int>) (c: Term<int>) (d: Term<int>) ->
            let D = Integers.order_domain th
            let a', b', c', d' = D.Term a, D.Term b, D.Term c, D.Term d
            let G, H = D.Term (gcd_of b c), D.Term (gcd_of c d)
            let E = b' == a' * c' + d'
            let Hcad = mp (dvd_add th H (c' * a') d') (conj (mp (dvd_mul th H c' a') (gcd_dvd_l th c d)) (gcd_dvd_r th c d))
            let Hb = lemma th (E ==> dvd H b') [
                        Deduce' (reflex_implies E)                      |> at [right_branch; right_branch]                 // b → a·c + d
                        Integers.ring th (a' * c' + d') (c' * a' + d')  |> at [right_branch; right_branch]
                        Taut Hcad                                       |> apply_right ]
            let Gbca = mp (dvd_sub th G b' (c' * a')) (conj (gcd_dvd_l th b c) (mp (dvd_mul th G c' a') (gcd_dvd_r th b c)))
            let Gd = lemma th (E ==> dvd G d') [
                        Integers.ring th d' ((a' * c' + d') - c' * a')  |> at [right_branch; right_branch]
                        Deduce' (OrderedDomain.eq_sym th b' (a' * c' + d')) |> at [right_branch; right_branch; left_branch] // a·c + d → b
                        Taut Gbca                                       |> apply_right ]
            let GH = lemma th (E ==> dvd G H) [
                        Establish Gd                                    |> apply
                        Establish (under th E (gcd_dvd_r th b c))       |> apply
                        Deduce (dvd_gcd th G c d)                       |> apply_right ]
            lemma th (E ==> (G == H)) [
                Establish (under th E (gcd_nonneg th c d))              |> apply
                Establish Hb                                            |> apply
                Establish (under th E (gcd_dvd_l th c d))               |> apply
                Establish GH                                            |> apply
                Deduce (gcd_eq th b c H)                                |> apply_right ])
    /// **(15.103)** `b = a·c + d ⇒ b gcd c = c gcd d` — under the antecedent, a divisor of two of b, c, d
    /// divides the third.
    let gcd_lin (th: Theory) (a: Term<int>) (b: Term<int>) (c: Term<int>) (d: Term<int>) : Theorem = gcd_lin_c th a b c d

    let private gcd_mul_c =
        cached3 "dv_gcd_mul" (fun th (d: Term<int>) (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let zero = D.Zero
            let d', b', c' = D.Term d, D.Term b, D.Term c
            let G = D.Term (gcd_of b c)
            let db, dc = d' * b', d' * c'
            let K = D.Term (gcd_of db dc)
            let N = zero <=. d'
            let dG = d' * G
            let nonneg = lemma th (N ==> (zero <=. dG)) [
                            Integers.ring th zero (d' * zero)           |> at [right_branch; left_branch]
                            Establish (under th N (gcd_nonneg th b c))  |> apply
                            Deduce (OrderedDomain.le_mul_nonneg D d' zero G) |> apply_right ]
            let side (x: DomainTerm<int>) (t: Theorem) =                                                                   // d·G ∣ d·x, from G ∣ x (15.85)
                lemma th (dvd dG (d' * x)) [
                    Integers.ring th dG (G * d')                        |> at [left_branch]
                    Integers.ring th (d' * x) (x * d')                  |> at [right_branch]
                    Taut (mp (dvd_mul_both th G x d') t)                |> apply ]
            // K ∣ d·G: by Bézout, d·G = u·(d·b) + v·(d·c).
            let avoid = [ d'.Expr.Raw; b'.Expr.Raw; c'.Expr.Raw ]
            let u = fresh_int "u" avoid
            let v = fresh_int "v" (u.Expr.Raw :: avoid)
            let U, V = D.Term (tm u), D.Term (tm v)
            let HB = G == U * b' + V * c'
            let Q = dvd K dG
            let tK = mp (dvd_add th K (db * U) (dc * V)) (conj (mp (dvd_mul th K db U) (gcd_dvd_l th db dc)) (mp (dvd_mul th K dc V) (gcd_dvd_r th db dc)))
            let inner = lemma th (HB ==> Q) [
                            Deduce' (reflex_implies HB)                 |> at [right_branch; right_branch; right_branch]   // b gcd c → u·b + v·c
                            Integers.ring th (d' * (U * b' + V * c')) (db * U + dc * V) |> at [right_branch; right_branch]
                            Taut tK                                     |> apply_right ]
            let EXv = qex v T HB
            let tv_ = lemma th ((T * HB) ==> Q) [ commute_and T HB |> at_left; ident_and HB |> at_left; Taut inner |> apply ]
            let elimV = exists_elim th v truepred (pred_of v HB) Q tv_
            let tu_ = lemma th ((T * EXv) ==> Q) [ commute_and T EXv |> at_left; ident_and EXv |> at_left; Taut elimV |> apply ]
            let elimU = exists_elim th u truepred (pred_of u EXv) Q tu_
            let KdG = mp elimU (gcd_bezout_with th u v b c)
            let KeqdG = lemma th (N ==> (K == dG)) [
                            Establish nonneg                            |> apply
                            Establish (under th N (side b' (gcd_dvd_l th b c))) |> apply
                            Establish (under th N (side c' (gcd_dvd_r th b c))) |> apply
                            Establish (under th N KdG)                  |> apply
                            Deduce (gcd_eq th db dc dG)                 |> apply_right ]
            lemma th (N ==> (dG == K)) [
                Establish KeqdG                                         |> apply
                Deduce (OrderedDomain.eq_sym th K dG)                   |> apply_right ])
    /// **(15.104) Distributivity**: `0 ≤ d ⇒ d·(b gcd c) = (d·b) gcd (d·c)` — `d·(b gcd c)` divides both
    /// (15.85); and by Bézout it is `u·(d·b) + v·(d·c)`, which `(d·b) gcd (d·c)` divides.
    let gcd_mul (th: Theory) (d: Term<int>) (b: Term<int>) (c: Term<int>) : Theorem = gcd_mul_c th d b c

    let private gcd_mod_c =
        cached2 "dv_gcd_mod" (fun th (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let c' = D.Term c
            let q, r = D.Term (div_of b c), D.Term (mod_of b c)
            lemma th ((D.Zero <. c') ==> (D.Term (gcd_of b c) == D.Term (gcd_of c' r))) [
                Establish (div_mod_def th b c)                          |> apply                                           // (15.90)
                Deduce (gcd_lin th q b c r)                             |> apply_right ])
    /// **(15.106)** `0 < c ⇒ b gcd c = c gcd (b mod c)` — Euclid's algorithm's step: (15.103) at
    /// `a, d := b ÷ c, b mod c`, whose antecedent is (15.90).
    let gcd_mod (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = gcd_mod_c th b c

    let private gcd_zero_right_c =
        cached1 "dv_gcd_zero_right" (fun th (b: Term<int>) ->
            let D = Integers.order_domain th
            let zero, b' = D.Zero, D.Term b
            lemma th (D.Term (gcd_of b' zero) == abs_of b') [
                Ident (gcd_sym th b' zero)                              |> at_left
                Ident (gcd_zero th b')                                  |> at_left
                IntegralDomain.refl_true th (abs_of b')                 |> apply ])
    /// **(15.107)** `b gcd 0 = abs.b` — Euclid's base case; Gries states it for natural `b`, as `b gcd 0 = b`.
    let gcd_zero_right (th: Theory) (b: Term<int>) : Theorem = gcd_zero_right_c th b

    (* ------------------------------------------------------------------------------------------
       (15.111)–(15.113): what Bézout gives — common divisors, relatively prime, Euclid's lemma.
       ------------------------------------------------------------------------------------------ *)

    /// (z ⇒ x) ⇒ ((¬x ∧ z) ⇒ r)
    let private glue_absurd' = OrderedDomain.taut3 "dv_absurd2" (fun x z r -> (z ==> x) ==> (((!!x) * z) ==> r))
    /// (a ⇒ x) ⇒ ((a ⇒ ¬x) ⇒ (a ⇒ r))
    let private glue_contra = OrderedDomain.taut3 "dv_contra" (fun x a r -> (a ==> x) ==> ((a ==> !!x) ==> (a ==> r)))
    /// (r ⇒ x ∨ e) ⇒ ((r ∧ ¬e) ⇒ x)
    let private glue_drop = OrderedDomain.taut3 "dv_drop" (fun x e r -> (r ==> (x + e)) ==> ((r * !!e) ==> x))

    /// `∀`-generalization with a hypothesis pulled out: from `body : (R.x ∧ H) ⇒ P.x`, proved about
    /// the dummy `x` itself (not free in `H`), conclude `H ⇒ (∀x | R : P)` — Trading (9.2), the body
    /// as a theorem, (9.8), then (9.5).
    let private gen_under (th: Theory) (x: IntVar) (R: Prop) (H: Prop) (P: Prop) (body: Theorem) : Theorem =
        let Rx, Px = pred_of x R, pred_of x P
        let body' = lemma th (R ==> (H ==> P)) [ shunt' R H P |> Commute |> apply; Taut body |> apply ]
        let gen = lemma th (qall x R (H ==> P)) [
                    trade_forall_implies x Rx (pred_of x (H ==> P))     |> apply
                    Taut body'                                          |> at [select_body]
                    ident_forall_true' x                                |> apply ]
        lemma th (H ==> qall x R P) [
            ident_implies_not_or H (qall x R P)                         |> apply
            distrib_or_forall' x Rx (!!H) Px                            |> apply                                           // (9.5)
            ident_implies_not_or H P |> Commute                         |> at [select_body]
            Taut gen                                                    |> apply ]

    let private dvd_gcd_iff_c =
        cached3 "dv_dvd_gcd_iff" (fun th (k: Term<int>) (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let k', b', c', G = D.Term k, D.Term b, D.Term c, D.Term (gcd_of b c)
            let L, R = dvd k' b' * dvd k' c', dvd k' G
            let bwd = lemma th (R ==> L) [
                        Establish (under th R (gcd_dvd_l th b c))       |> apply
                        Establish (under th R (gcd_dvd_r th b c))       |> apply
                        Establish (dvd_trans th k' G b')                |> apply                                           // (15.82)
                        Establish (dvd_trans th k' G c')                |> apply
                        Deduce (reflex_implies L)                       |> apply_right ]
            lemma th (L == R) [
                mutual_implication' L R |> Commute                      |> apply
                Taut (conj (dvd_gcd th k b c) bwd)                      |> apply ])
    /// **(15.111)** `k ∣ b ∧ k ∣ c ≡ k ∣ (b gcd c)` — `⇒` is the definition of gcd (Gries derives it from
    /// Bézout (15.108)); `⇐` is Transitivity (15.82).
    let dvd_gcd_iff (th: Theory) (k: Term<int>) (b: Term<int>) (c: Term<int>) : Theorem = dvd_gcd_iff_c th k b c

    let private coprime_dvd_c =
        cached3 "dv_coprime_dvd" (fun th (b: Term<int>) (c: Term<int>) (d: Term<int>) ->
            let D = Integers.order_domain th
            let one = D.One
            let b', c', d' = D.Term b, D.Term c, D.Term d
            let G = D.Term (gcd_of b c)
            let A = (G == one) * dvd c' (b' * d')
            let R = dvd c' d'
            let avoid = [ b'.Expr.Raw; c'.Expr.Raw; d'.Expr.Raw ]
            let u = fresh_int "u" avoid
            let v = fresh_int "v" (u.Expr.Raw :: avoid)
            let U, V = D.Term (tm u), D.Term (tm v)
            let HB = G == U * b' + V * c'
            let bdu, cdv = (b' * d') * U, (c' * d') * V
            let ccdv = mp (dvd_mul th c' (c' * d') V) (mp (dvd_mul th c' c' d') (dvd_refl th c'))   // c ∣ (c·d)·v
            let lin = lemma th (A ==> dvd c' (bdu + cdv)) [
                        Establish (dvd_mul th c' (b' * d') U)           |> apply                                           // c ∣ (b·d)·u
                        Establish (under th A ccdv)                     |> apply
                        Deduce (dvd_add th c' bdu cdv)                  |> apply_right ]
            // d = d·1 = d·(b gcd c) = d·(u·b + v·c) = (b·d)·u + (c·d)·v
            let inner = lemma th ((HB * A) ==> R) [
                        Integers.ring th d' (d' * one)                  |> at [right_branch; right_branch]
                        Deduce' (OrderedDomain.eq_sym th G one)         |> at [right_branch; right_branch; right_branch]   // 1 → b gcd c
                        Deduce' (reflex_implies HB)                     |> at [right_branch; right_branch; right_branch]   // → u·b + v·c
                        Integers.ring th (d' * (U * b' + V * c')) (bdu + cdv) |> at [right_branch; right_branch]
                        Deduce lin                                      |> apply_right ]
            let Q = A ==> R
            let inner' = lemma th (HB ==> Q) [ shunt' HB A R |> Commute |> apply; Taut inner |> apply ]
            let EXv = qex v T HB
            let tv_ = lemma th ((T * HB) ==> Q) [ commute_and T HB |> at_left; ident_and HB |> at_left; Taut inner' |> apply ]
            let elimV = exists_elim th v truepred (pred_of v HB) Q tv_
            let tu_ = lemma th ((T * EXv) ==> Q) [ commute_and T EXv |> at_left; ident_and EXv |> at_left; Taut elimV |> apply ]
            let elimU = exists_elim th u truepred (pred_of u EXv) Q tu_
            mp elimU (gcd_bezout_with th u v b c))
    /// **(15.113)** `b ⊥ c ∧ c ∣ (b·d) ⇒ c ∣ d` — by Bézout (15.108), `1 = u·b + v·c`, so
    /// `d = (b·d)·u + (c·d)·v`, and `c` divides both terms.
    let coprime_dvd (th: Theory) (b: Term<int>) (c: Term<int>) (d: Term<int>) : Theorem = coprime_dvd_c th b c d

    (* Primes — the definition, in parts *)

    /// The definition of prime as a rewrite, `prime p → 1 < p ∧ (∀d | 0 < d ∧ d ∣ p : d = 1 ∨ d = p)`,
    /// with the given dummy.
    let private prime_def_with (th: Theory) (d: IntVar) (p: Term<int>) : Rule = ax_ident th (prime_spec_with d p)

    /// The definition of prime as a rewrite, with a dummy `d` fresh for `p`.
    let prime_def (th: Theory) (p: Term<int>) : Rule = prime_def_with th (fresh_int "d" [ p.Expr ]) p

    /// `prime p ⇒ 1 < p`
    let prime_gt_one (th: Theory) (p: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let d = fresh_int "d" [ p.Expr ]
        let G, ALL = (D.One <. D.Term p), prime_all d p
        lemma th (prime p ==> G) [
            prime_def_with th d p                                       |> at_left
            Taut (strengthen_and G ALL)                                 |> apply ]

    /// `prime p ⇒ 0 < p`
    let prime_pos (th: Theory) (p: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one, p' = D.Zero, D.One, D.Term p
        lemma th (prime p ==> (zero <. p')) [
            Establish (prime_gt_one th p)                               |> apply
            Establish (under th (prime p) (zero_lt_one th))             |> apply
            Deduce (OrderedDomain.lt_trans D zero one p')               |> apply_right ]

    /// `prime p ∧ (0 < e ∧ e ∣ p) ⇒ e = 1 ∨ e = p` — the definition, instantiated (9.13) at `e`.
    let prime_dvd (th: Theory) (p: Term<int>) (e: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let d = fresh_int "d" [ p.Expr ]
        let x = tm d
        let Rp, Pp = pred_of d (prime_range x p), pred_of d (prime_body x p)
        let ALL = prime_all d p
        let all_of = lemma th (prime p ==> ALL) [
                        prime_def_with th d p                           |> at_left
                        commute_and (D.One <. D.Term p) ALL             |> at_left
                        Taut (strengthen_and ALL (D.One <. D.Term p))   |> apply ]
        let inst = lemma th (ALL ==> (Rp.[e] ==> Pp.[e])) [ trade_forall_implies d Rp Pp |> at_left ]                    // (9.13)
        lemma th ((prime p * Rp.[e]) ==> Pp.[e]) [
            Establish all_of                                            |> apply
            Deduce (lemma th ((ALL * Rp.[e]) ==> Pp.[e]) [ shunt' ALL Rp.[e] Pp.[e] |> apply; Taut inst |> apply ]) |> apply_right ]

    /// `0 < p ⇒ ¬(b = 0 ∧ p = 0)`
    let private pos_not_both_zero (th: Theory) (b: DomainTerm<int>) (p: DomainTerm<int>) : Theorem =
        let D = Integers.order_domain th
        let zero = D.Zero
        let Z2 = (b == zero) * (p == zero)
        lemma th ((zero <. p) ==> !!Z2) [
            def_implies_contr (zero <. p) (!!Z2)                        |> apply                                           // ¬¬Z ⇒ ¬(0 < p)
            double_negation Z2                                          |> at_left
            Deduce' (reflex_implies (p == zero))                        |> at [right_branch; apply_unary; right_branch]    // p → 0
            OrderedDomain.lt_irrefl D zero                              |> at [right_branch; apply_unary]
            not_false                                                   |> apply_right ]

    let private prime_coprime_c =
        cached2 "dv_prime_coprime" (fun th (p: Term<int>) (b: Term<int>) ->
            let D = Integers.order_domain th
            let zero, one = D.Zero, D.One
            let p', b' = D.Term p, D.Term b
            let g = D.Term (gcd_of b p)
            let NP = !!(dvd p' b')
            let H = prime p' * NP
            let E1, E2 = (g == one), (g == p')
            let gpos = lemma th (prime p' ==> (zero <. g)) [
                        Establish (prime_pos th p)                      |> apply
                        Establish (pos_not_both_zero th b' p')          |> apply
                        Deduce (gcd_pos th b p)                         |> apply_right ]
            // g = p would make p ∣ b.
            let notp = lemma th (E2 ==> dvd p' b') [
                        Deduce' (OrderedDomain.eq_sym th g p')          |> at [right_branch; left_branch]                  // p → b gcd p
                        Taut (gcd_dvd_l th b p)                         |> apply_right ]
            let cA = lemma th ((NP * E1) ==> E1) [ Deduce (reflex_implies E1) |> apply_right ]
            let cB = mp (OrderedDomain.in_theory th (glue_absurd' (dvd p' b') E2 E1)) notp
            lemma th (H ==> E1) [
                Establish gpos                                          |> apply
                Establish (under th H (gcd_dvd_r th b p))               |> apply
                Establish (prime_dvd th p' g)                           |> apply                                           // g = 1 ∨ g = p
                Deduce (by_cases2 th E1 E2 NP E1 cA cB)                 |> apply_right ])
    /// `prime p ∧ ¬(p ∣ b) ⇒ b ⊥ p` — `b gcd p` is a positive divisor of `p`, so 1 or `p`, and `p` would
    /// divide `b`.
    let prime_coprime (th: Theory) (p: Term<int>) (b: Term<int>) : Theorem = prime_coprime_c th p b

    let private euclid_lemma_c =
        cached3 "dv_euclid_lemma" (fun th (p: Term<int>) (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let p', b', c' = D.Term p, D.Term b, D.Term c
            let PB, PC = dvd p' b', dvd p' c'
            let H = prime p' * dvd p' (b' * c')
            let R = PB + PC
            let cP = lemma th ((H * PB) ==> R) [ Deduce (weaken_or PB PC) |> apply_right ]
            let cN = lemma th ((H * !!PB) ==> R) [
                        Establish (prime_coprime th p b)                |> apply                                           // b gcd p = 1
                        Establish (coprime_dvd th b p c)                |> apply                                           // (15.113)
                        commute_or PB PC                                |> apply_right
                        Deduce (weaken_or PC PB)                        |> apply_right ]
            mp (mp (OrderedDomain.in_theory th (glue_cases H PB R)) cP) cN)
    /// **(15.112) Theorem** (Euclid's lemma): `prime p ∧ p ∣ b·c ⇒ p ∣ b ∨ p ∣ c` — if `p ∤ b` then
    /// `b ⊥ p` (`prime_coprime`), and (15.113).
    let euclid_lemma (th: Theory) (p: Term<int>) (b: Term<int>) (c: Term<int>) : Theorem = euclid_lemma_c th p b c

    (* A prime divisor, and infinitely many primes *)

    /// `1 < n ⇒ (∃p | prime p : p ∣ n)`, with the given dummy `p`.
    let private prime_divisor_with (th: Theory) (p: IntVar) (n: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let n' = D.Term n
        let avoid = [ n.Expr.Raw; p.Expr.Raw ]
        let x = fresh_int "x" avoid
        let y = fresh_int "y" (x.Expr.Raw :: avoid)
        let d = fresh_int "d" (x.Expr.Raw :: y.Expr.Raw :: avoid)
        let X, Dd = D.Term (tm x), D.Term (tm d)
        let P = pred_of x ((one <. X) * dvd X n')
        let minimal (w: Term<int>) = qall y ((leq 0 (tm y)) * (lt (tm y) w)) (!!P.[tm y])
        let M = minimal (tm x)
        let Rng = pred_of x ((leq 0 (tm x)) * P.[tm x])
        let Min = pred_of x M
        let A = Rng.[tm x] * Min.[tm x]
        let goal = exists (p, pred_of p (prime (tm p)), pred_of p (dvd (D.Term (tm p)) n'))
        let xpos = lemma th ((one <. X) ==> (zero <. X)) [
                    Establish (under th (one <. X) (zero_lt_one th))    |> apply
                    Deduce (OrderedDomain.lt_trans D zero one X)        |> apply_right ]
        // The least x > 1 dividing n is prime: a positive divisor d ≠ 1 of x is > 1 and divides n, so
        // d < x would contradict minimality; and d ≤ x (`dvd_le`).
        let E1, E2 = (Dd == one), (Dd == X)
        let RD = prime_range Dd X
        let K = RD * A
        let Rb = E1 + E2
        let c1 = lemma th ((K * E1) ==> Rb) [ Deduce (weaken_or E1 E2) |> apply_right ]
        let le_split = lemma th ((one <=. Dd) ==> ((one <. Dd) + E1)) [
                        ax_ident th ((one <=. Dd) == ((one <. Dd) + (one == Dd))) |> at_left                             // (15.38)
                        OrderedDomain.eq_comm th one Dd                 |> at [left_branch; right_branch]
                        Taut (reflex_implies ((one <. Dd) + E1))        |> apply ]
        let gt1 = mp (OrderedDomain.in_theory th (glue_drop (one <. Dd) E1 (one <=. Dd))) le_split                        // 1 ≤ d ∧ ¬(d = 1) ⇒ 1 < d
        let Rm = pred_of y ((leq 0 (tm y)) * (lt (tm y) (tm x)))
        let NPy = pred_of y (!!P.[tm y])
        let inst = lemma th (M ==> (Rm.[Dd] ==> NPy.[Dd])) [ trade_forall_implies y Rm NPy |> at_left ]                   // (9.13)
        let mi = lemma th ((M * Rm.[Dd]) ==> NPy.[Dd]) [ shunt' M Rm.[Dd] NPy.[Dd] |> apply; Taut inst |> apply ]
        let A2 = K * !!E1
        let ge = lemma th (A2 ==> (X <=. Dd)) [
                    OrderedDomain.not_lt D Dd X |> Commute              |> at_right                                        // x ≤ d → ¬(d < x)
                    ident_implies_false_not (Dd <. X) |> Commute        |> at_right
                    shunt' A2 (Dd <. X) F |> Commute                    |> apply
                    Establish (one_le_of_pos th Dd)                     |> apply
                    Establish gt1                                       |> apply
                    Establish (lt_le th zero Dd)                        |> apply
                    Establish (dvd_trans th Dd X n')                    |> apply                                           // d ∣ n
                    Establish (lemma th (((one <. Dd) * dvd Dd n') ==> P.[Dd]) [ Deduce (reflex_implies P.[Dd]) |> apply_right ]) |> apply
                    Establish mi                                        |> apply
                    Deduce (lemma th ((P.[Dd] * !!P.[Dd]) ==> F) [ contr P.[Dd] |> at_left ]) |> apply_right ]
        let c2 = lemma th (A2 ==> Rb) [
                    Establish ge                                        |> apply
                    Establish xpos                                      |> apply
                    Establish (dvd_le th X Dd)                          |> apply                                           // d ≤ x
                    Establish (OrderedDomain.le_antisym D Dd X)         |> apply                                           // d = x
                    commute_or E1 E2                                    |> apply_right
                    Deduce (weaken_or E2 E1)                            |> apply_right ]
        let body = mp (mp (OrderedDomain.in_theory th (glue_cases K E1 Rb)) c1) c2
        let allA = gen_under th d RD A Rb body
        let primeX = lemma th (A ==> prime X) [
                        prime_def_with th d X                           |> at_right
                        Establish allA                                  |> apply
                        Deduce (reflex_implies ((one <. X) * prime_all d X)) |> apply_right ]
        let Rp, Bp = pred_of p (prime (tm p)), pred_of p (dvd (D.Term (tm p)) n')
        let xbody = lemma th (A ==> goal) [
                        trade_exists_and p Rp Bp                        |> at_right                                        // (9.19)
                        Establish primeX                                |> apply
                        Deduce (exists_intro p (Rp * Bp) X)             |> apply_right ]
        let elimX = exists_elim th x Rng Min goal xbody
        let least = least_element th x y P n
        lemma th ((one <. n') ==> goal) [
            Establish (lemma th ((one <. n') ==> (zero <=. n')) [
                        Establish (under th (one <. n') (zero_lt_one th)) |> apply
                        Establish (OrderedDomain.lt_trans D zero one n') |> apply
                        Deduce (lt_le th zero n')                       |> apply_right ]) |> apply
            Establish (under th (one <. n') (dvd_refl th n'))           |> apply
            Establish least                                             |> apply                                           // (15.49)
            Deduce elimX                                                |> apply_right ]

    /// **A prime divisor**: `1 < n ⇒ (∃p | prime p : p ∣ n)` — the step of (15.114)'s existence proof.
    /// The least `x > 1` dividing `n` (Well ordering, `least_element`) is prime: a positive divisor
    /// `d ≠ 1` of it is `> 1` and divides `n`, so it is not below `x`, and it divides `x`, so it is
    /// not above.
    let prime_divisor (th: Theory) (n: Term<int>) : Theorem =
        prime_divisor_with th (fresh_int "p" [ n.Expr ]) n

    // The factorial lemmas below reason about terms that contain a binder (the Π). Each step that
    // touches `n!` is a schema lemma, derived once at metavariables (`cached1`–`cached3`), then
    // instantiated at the Π term (`Tactics.InstantiateTerms` allows a binder-containing value when
    // the statement has none). This was first a workaround: Substitution (3.84a) used to refuse any
    // e/f containing a binder, counting the Π's own dummy in its capture check. The kernel now checks
    // only FREE variables, so `eq_sym`/`Deduce'` work at `n!` directly; the schemas stay because they
    // are cheaper than re-deriving each step.

    /// `b ∣ c ⇒ b ∣ (c·d)` — (15.84), as a schema.
    let private dvd_mul_s = cached3 "dv_dvd_mul_s" (fun th (b: Term<int>) (c: Term<int>) (d: Term<int>) -> dvd_mul th b c d)

    /// `k ∣ a·k`
    let private dvd_mul_self =
        cached2 "dv_dvd_mul_self" (fun th (k: Term<int>) (a: Term<int>) ->
            let D = Integers.order_domain th
            let k', a' = D.Term k, D.Term a
            lemma th (dvd k' (a' * k')) [
                Integers.ring th (a' * k') (k' * a')                    |> at [right_branch]
                Taut (mp (dvd_mul th k' k' a') (dvd_refl th k'))        |> apply ])

    /// `1 ≤ a ∧ 1 ≤ b ⇒ 1 ≤ a·b` — `0 < a`, so `1·a ≤ b·a` (15.43), and Transitivity.
    let private one_le_mul =
        cached2 "dv_one_le_mul" (fun th (a: Term<int>) (b: Term<int>) ->
            let D = Integers.order_domain th
            let zero, one, a', b' = D.Zero, D.One, D.Term a, D.Term b
            let grow = lemma th (((zero <. a') * (one <=. b')) ==> (a' <=. a' * b')) [
                        Integers.ring th a' (one * a')                  |> at [right_branch; left_branch]
                        Integers.ring th (a' * b') (b' * a')            |> at [right_branch; right_branch]
                        Deduce (OrderedDomain.le_mul_pos D one b' a')   |> apply_right ]
            let H = (one <=. a') * (one <=. b')
            lemma th (H ==> (one <=. a' * b')) [
                Establish (under th H (zero_lt_one th))                 |> apply
                Establish (OrderedDomain.lt_le_trans D zero one a')     |> apply                                           // 0 < a
                Establish grow                                          |> apply
                Deduce (OrderedDomain.le_trans D one a' (a' * b'))      |> apply_right ])

    /// `1 ≤ a ⇒ 1 < a + 1`
    let private one_lt_succ =
        cached1 "dv_one_lt_succ" (fun th (a: Term<int>) ->
            let D = Integers.order_domain th
            let one, a' = D.One, D.Term a
            lemma th ((one <=. a') ==> (one <. a' + one)) [
                Ident (lt_succ_le th one a')                            |> at_right                                        // 1 < a+1 → 1 ≤ a
                Taut (reflex_implies (one <=. a'))                      |> apply ])

    /// `(Πj | 1 ≤ j ∧ j ≤ n : j)` — `n!` for `0 ≤ n`, as a product quantification.
    let private factorial_with (j: IntVar) (n: Term<int>) : IntTerm =
        Sums.qprod j ((leq 1 (tm j)) * (leq (tm j) n)) (tm j)

    /// The dummies a factorial lemma uses: `m` (the induction variable), `i` (its hypothesis), `j`
    /// (the product's), fresh for `avoid`.
    let private fact_dummies (avoid: Expr list) =
        let m = fresh_int "m" avoid
        let i = fresh_int "i" (m.Expr.Raw :: avoid)
        let j = fresh_int "j" (m.Expr.Raw :: i.Expr.Raw :: avoid)
        m, i, j

    /// `0 ≤ n ⇒ 1 ≤ n!`, by induction (12.8) — Split off factor for Π in the step.
    let private fact_pos (th: Theory) (n: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let m, i, j = fact_dummies [ n.Expr.Raw ]
        let F (w: Term<int>) = D.Term (factorial_with j w)
        let P = pred_of m (one <=. F (tm m))
        let bse = lemma th (base_case 0 P) [
                    Ident (empty_prod th 1 0 j (tm j))                 |> at_right                                         // Π(1..0) → 1
                    OrderedDomain.le_refl D one                         |> apply ]
        let a, ih = leq 0 (tm m), hypothesis 0 m i P
        let m1 = succ m
        let Fm, M1 = F (tm m), D.Term m1
        let body = lemma th (a ==> (ih ==> P.[m1])) [
                    (shunt' a ih P.[m1] |> Commute)                     |> apply
                    Establish (shift_bound th 0 (tm m))                 |> apply                                           // 1 ≤ m+1
                    Deduce' (split_off_factor th 1 (tm m) j (tm j))     |> apply_right                                     // Π(1..m+1) → Π(1..m)·(m+1)
                    Establish (hypothesis_at th 0 m i P)                |> apply                                           // 1 ≤ m!
                    Deduce (one_le_mul th Fm M1)                        |> apply_right ]
        let all = by_induction th 0 m i P bse (generalize_step th 0 m i P body)
        let Rm = pred_of m a
        mp (lemma th (Prop(Expr.Cast<bool> all.Stmt) ==> (Rm.[n] ==> P.[n])) [ trade_forall_implies m Rm P |> at_left ]) all

    /// `0 ≤ n ∧ (1 ≤ k ∧ k ≤ n) ⇒ k ∣ n!`, by induction (12.8) on `n`: the new factor `n+1` is either
    /// above `k` (the hypothesis, (15.84)) or `k` itself.
    let private fact_dvd (th: Theory) (k: Term<int>) (n: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let K = D.Term k
        let m, i, j = fact_dummies [ k.Expr.Raw; n.Expr.Raw ]
        let Fac (w: Term<int>) = D.Term (factorial_with j w)
        let B (w: Term<int>) = (one <=. K) * (K <=. D.Term w)
        let P = pred_of m (B (tm m) ==> dvd K (Fac (tm m)))
        // Base: 1 ≤ k ≤ 0 is impossible.
        let h, z = (one <=. K), (K <=. zero)
        let zn = lemma th (z ==> !!h) [
                    ident_implies_false_not h |> Commute                |> at_right                                        // ¬h → (h ⇒ F)
                    shunt' z h F |> Commute                             |> apply
                    ax_ident th ((leq 1 0) == F) |> Commute             |> at_right                                        // F → 1 ≤ 0, literal evaluation
                    Deduce (OrderedDomain.le_trans D one K zero)        |> apply_right ]
        let bse = mp (OrderedDomain.in_theory th (glue_absurd h z (dvd K (Fac (IntTerm 0))))) zn
        // Step.
        let a, ih = leq 0 (tm m), hypothesis 0 m i P
        let m1 = succ m
        let Mt, M1 = D.Term (tm m), D.Term m1
        let Fm = Fac (tm m)
        let Bm1 = B m1
        let H = (a * ih) * Bm1
        let R = dvd K (Fm * M1)
        let hm = lemma th (((a * ih) * B (tm m)) ==> dvd K Fm) [
                    shunt' (a * ih) (B (tm m)) (dvd K Fm)               |> apply
                    Taut (hypothesis_at th 0 m i P)                     |> apply ]
        let L1, E1 = (K <=. Mt), (K == M1)
        let cA = lemma th ((H * L1) ==> R) [
                    Establish hm                                        |> apply                                           // k ∣ m!
                    Deduce (dvd_mul_s th K Fm M1)                       |> apply_right ]                                   // (15.84)
        let cB = lemma th ((H * E1) ==> R) [
                    Deduce' (OrderedDomain.eq_sym th K M1)              |> at [right_branch; right_branch; right_branch]   // m+1 → k
                    Taut (dvd_mul_self th K Fm)                         |> apply_right ]
        let split = lemma th ((K <=. M1) ==> (L1 + E1)) [
                    discrete th K (tm m)                                |> at_left                                         // Discreteness
                    Taut (reflex_implies (L1 + E1))                     |> apply ]
        let full = lemma th (H ==> R) [
                    Establish split                                     |> apply
                    Deduce (by_cases2 th L1 E1 H R cA cB)               |> apply_right ]
        let body = lemma th (a ==> (ih ==> P.[m1])) [
                    (shunt' a ih P.[m1] |> Commute)                     |> apply
                    shunt' (a * ih) Bm1 (dvd K (Fac m1)) |> Commute     |> apply
                    Establish (shift_bound th 0 (tm m))                 |> apply
                    Deduce' (split_off_factor th 1 (tm m) j (tm j))     |> apply_right                                     // Π(1..m+1) → Π(1..m)·(m+1)
                    Deduce full                                         |> apply_right ]
        let all = by_induction th 0 m i P bse (generalize_step th 0 m i P body)
        let Rm = pred_of m a
        let pn = mp (lemma th (Prop(Expr.Cast<bool> all.Stmt) ==> (Rm.[n] ==> P.[n])) [ trade_forall_implies m Rm P |> at_left ]) all
        lemma th ((Rm.[n] * B n) ==> dvd K (Fac n)) [ shunt' Rm.[n] (B n) (dvd K (Fac n)) |> apply; Taut pn |> apply ]

    /// **(15.116) Theorem**: there are infinitely many primes — `0 ≤ n ⇒ (∃p | prime p : n < p)`.
    ///
    /// Euclid's argument, with `n! + 1` for Gries' `p₀·…·p_{k−1} + 1` (15.117) — the product of all of
    /// `1..n`, not just the primes among them, which needs no enumeration of the primes. `n!` is
    /// `(Πj | 1 ≤ j ∧ j ≤ n : j)`; `1 ≤ n!` and `k ∣ n!` for `1 ≤ k ≤ n` are proved by induction. So
    /// `n! + 1 > 1` has a prime divisor `p` (`prime_divisor`), and `p ≤ n` would make `p` divide both
    /// `n!` and `n! + 1` (15.86).
    let primes_unbounded (th: Theory) (n: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, one = D.Zero, D.One
        let n' = D.Term n
        let avoid = [ n.Expr.Raw ]
        let q = fresh_int "p" avoid
        let p = fresh_int "q" (q.Expr.Raw :: avoid)
        let _, _, j = fact_dummies avoid
        let F = D.Term (factorial_with j n)
        let Mm = F + one
        let N0 = zero <=. n'
        let goal = exists (q, pred_of q (prime (tm q)), pred_of q (n' <. D.Term (tm q)))
        let fpos = fact_pos th n                                                                                           // 0 ≤ n ⇒ 1 ≤ n!
        let mgt1 = lemma th (N0 ==> (one <. Mm)) [
                    Establish fpos                                      |> apply
                    Deduce (one_lt_succ th F)                           |> apply_right ]
        let P = D.Term (tm p)
        let PR, PM, NL = prime P, dvd P Mm, (n' <. P)
        let Kb = (PR * PM) * N0
        let Rq, Bq = pred_of q (prime (tm q)), pred_of q (n' <. D.Term (tm q))
        let cP = lemma th ((Kb * NL) ==> goal) [
                    trade_exists_and q Rq Bq                            |> at_right
                    Deduce (exists_intro q (Rq * Bq) P)                 |> apply_right ]
        // p ≤ n: then 1 ≤ p ≤ n, so p ∣ n!, and p ∤ n! + 1 (15.86).
        let Kn = Kb * !!NL
        let notpm = lemma th (Kn ==> !!PM) [
                    OrderedDomain.not_lt D n' P                         |> at [left_branch; right_branch]                  // ¬(n < p) → p ≤ n
                    Establish (prime_gt_one th P)                       |> apply
                    Establish (lt_le th one P)                          |> apply
                    Establish (fact_dvd th P n)                         |> apply                                           // p ∣ n!
                    Deduce (not_dvd_succ th P F)                        |> apply_right ]                                   // (15.86)
        let pmK = lemma th (Kn ==> PM) [ Deduce (reflex_implies PM) |> apply_right ]
        let cN = mp (mp (OrderedDomain.in_theory th (glue_contra PM Kn goal)) pmK) notpm
        let body = mp (mp (OrderedDomain.in_theory th (glue_cases Kb NL goal)) cP) cN
        let Q = N0 ==> goal
        let pbody = lemma th ((PR * PM) ==> Q) [ shunt' (PR * PM) N0 goal |> Commute |> apply; Taut body |> apply ]
        let elimP = exists_elim th p (pred_of p (prime (tm p))) (pred_of p (dvd (D.Term (tm p)) Mm)) Q pbody
        let EX = exists (p, pred_of p (prime (tm p)), pred_of p (dvd (D.Term (tm p)) Mm))
        let elimP' = lemma th ((EX * N0) ==> goal) [ shunt' EX N0 goal |> apply; Taut elimP |> apply ]
        // `prime_divisor` at n! + 1, through a ∀ (generalization, then (9.13)): its proof rewrites
        // under hypotheses about its argument, which a term containing the Π cannot be (see above).
        let w = fresh_int "w" (p.Expr.Raw :: q.Expr.Raw :: j.Expr.Raw :: avoid)
        let W = D.Term (tm w)
        let Rw = pred_of w (one <. W)
        let Pw = pred_of w (exists (p, pred_of p (prime (tm p)), pred_of p (dvd (D.Term (tm p)) W)))
        let allw = lemma th (forall (w, Rw, Pw)) [
                    trade_forall_implies w Rw Pw                        |> apply
                    Taut (prime_divisor_with th p W)                    |> at [select_body]
                    ident_forall_true' w                                |> apply ]
        let pdM = lemma th (forall (w, Rw, Pw) ==> (Rw.[Mm] ==> Pw.[Mm])) [ trade_forall_implies w Rw Pw |> at_left ]   // (9.13)
        lemma th (N0 ==> goal) [
            Establish mgt1                                              |> apply
            Establish (mp pdM allw)                                     |> apply
            Deduce elimP'                                               |> apply_right ]

    (* ------------------------------------------------------------------------------------------
       The least common multiple (15.94), the dual of gcd.
       ------------------------------------------------------------------------------------------ *)

    /// **Existence of lcm** — proved WITHOUT the definition of lcm, which this justifies:
    ///
    /// ```
    /// (∃l | 0 ≤ l : (b ∣ l ∧ c ∣ l) ∧ (∀m | b ∣ m ∧ c ∣ m : l ∣ m))
    /// ```
    ///
    /// For `b, c ≠ 0`, the least positive common multiple `x` (Well ordering, `least_element`; the set
    /// contains `(b·b)·(c·c)`): a common multiple `m` has remainder `r = m mod x`, again a common
    /// multiple, with `0 ≤ r < x`, so `r = 0` and `x ∣ m`. If `b = 0` or `c = 0`, `l := 0`: a multiple
    /// of 0 is 0.
    let lcm_exists (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero = D.Zero
        let b', c' = D.Term b, D.Term c
        let avoid = [ b.Expr.Raw; c.Expr.Raw ]
        let l = fresh_int "l" avoid
        let m = fresh_int "m" (l.Expr.Raw :: avoid)
        let x = fresh_int "x" (m.Expr.Raw :: l.Expr.Raw :: avoid)
        let y = fresh_int "y" (x.Expr.Raw :: m.Expr.Raw :: l.Expr.Raw :: avoid)
        let Mv = D.Term (tm m)
        let Rmm = dvd b' Mv * dvd c' Mv
        let Phi (w: DomainTerm<int>) = (dvd b' w * dvd c' w) * qall m Rmm (dvd w Mv)
        let Rl, Bl = pred_of l (zero <=. D.Term (tm l)), pred_of l (Phi (D.Term (tm l)))
        let goal = exists (l, Rl, Bl)
        // b = 0 or c = 0: l := 0.
        let Zb, Zc = (b' == zero), (c' == zero)
        let Z = Zb + Zc
        let R0 = dvd zero Mv
        let to0 (t: DomainTerm<int>) (Zt: Prop) =
            lemma th ((Rmm * Zt) ==> R0) [
                Establish (zero_dvd th t Mv)                            |> apply                                           // m = 0
                Deduce' (reflex_implies (Mv == zero))                   |> at [right_branch; right_branch]
                Taut (dvd_zero th zero)                                 |> apply_right ]
        let body0 = lemma th ((Rmm * Z) ==> R0) [
                    commute_and Rmm Z                                   |> at_left
                    Taut (by_cases2 th Zb Zc Rmm R0 (to0 b' Zb) (to0 c' Zc)) |> apply ]
        let all0 = gen_under th m Rmm Z R0 body0
        let z = lemma th (Z ==> goal) [
                    trade_exists_and l Rl Bl                            |> at_right                                        // (9.19)
                    Establish (under th Z (dvd_zero th b'))             |> apply
                    Establish (under th Z (dvd_zero th c'))             |> apply
                    Establish all0                                      |> apply
                    Establish (under th Z (lemma th (zero <=. zero) [ OrderedDomain.le_refl D zero |> apply ])) |> apply
                    Deduce (exists_intro l (Rl * Bl) zero)              |> apply_right ]
        // b, c ≠ 0: the least positive common multiple.
        let X = D.Term (tm x)
        let P = pred_of x ((zero <. X) * (dvd b' X * dvd c' X))
        let minimal (w: Term<int>) = qall y ((leq 0 (tm y)) * (lt (tm y) w)) (!!P.[tm y])
        let M = minimal (tm x)
        let Rng = pred_of x ((leq 0 (tm x)) * P.[tm x])
        let Min = pred_of x M
        let A = Rng.[tm x] * Min.[tm x]
        let H0 = zero <. X
        let qq, r = D.Term (div_of Mv X), D.Term (mod_of Mv X)
        // t ∣ r, for t a divisor of both m and x: r = m − x·q.
        let rdvd (t: DomainTerm<int>) =
            lemma th (((H0 * dvd t X) * dvd t Mv) ==> dvd t r) [
                Establish (div_mod_def th Mv X)                         |> apply                                           // m = q·x + r
                Integers.ring th r ((qq * X + r) - X * qq)              |> at [right_branch; right_branch]
                Deduce' (OrderedDomain.eq_sym th Mv (qq * X + r))       |> at [right_branch; right_branch; left_branch]    // q·x + r → m
                Establish (dvd_mul th t X qq)                           |> apply                                           // t ∣ x·q
                Deduce (dvd_sub th t Mv (X * qq))                       |> apply_right ]
        let Rm = pred_of y ((leq 0 (tm y)) * (lt (tm y) (tm x)))
        let NPy = pred_of y (!!P.[tm y])
        let inst = lemma th (M ==> (Rm.[r] ==> NPy.[r])) [ trade_forall_implies y Rm NPy |> at_left ]                     // (9.13)
        let mi = lemma th ((M * Rm.[r]) ==> NPy.[r]) [ shunt' M Rm.[r] NPy.[r] |> apply; Taut inst |> apply ]
        let A2 = Rmm * A
        let le0 = lemma th (A2 ==> (r <=. zero)) [
                    OrderedDomain.not_lt D zero r |> Commute            |> at_right                                        // r ≤ 0 → ¬(0 < r)
                    ident_implies_false_not (zero <. r) |> Commute      |> at_right
                    shunt' A2 (zero <. r) F |> Commute                  |> apply
                    Establish (mod_nonneg th Mv X)                      |> apply
                    Establish (mod_lt th Mv X)                          |> apply
                    Establish (rdvd b')                                 |> apply
                    Establish (rdvd c')                                 |> apply
                    Establish (lemma th (((zero <. r) * (dvd b' r * dvd c' r)) ==> P.[r]) [ Deduce (reflex_implies P.[r]) |> apply_right ]) |> apply
                    Establish mi                                        |> apply
                    Deduce (lemma th ((P.[r] * !!P.[r]) ==> F) [ contr P.[r] |> at_left ]) |> apply_right ]
        let xm = lemma th (A2 ==> dvd X Mv) [
                    Deduce' (dvd_iff_mod_zero th X Mv)                  |> apply_right                                     // x ∣ m → m mod x = 0
                    Establish le0                                       |> apply
                    Establish (mod_nonneg th Mv X)                      |> apply
                    Deduce (OrderedDomain.le_antisym D r zero)          |> apply_right ]
        let allA = gen_under th m Rmm A (dvd X Mv) xm
        let xbody = lemma th (A ==> goal) [
                    trade_exists_and l Rl Bl                            |> at_right
                    Establish allA                                      |> apply
                    Deduce (exists_intro l (Rl * Bl) X)                 |> apply_right ]
        let elimX = exists_elim th x Rng Min goal xbody
        // The set contains e = (b·b)·(c·c).
        let bb, cc = b' * b', c' * c'
        let e = bb * cc
        let posl (w: DomainTerm<int>) = lemma th (pos_of w ==> (zero <. w)) [ OrderedDomain.pos_lt D w |> at_left ]        // (15.40)
        let NZ2 = (!!Zb) * (!!Zc)
        let epos = lemma th (NZ2 ==> (zero <. e)) [
                    Establish (OrderedDomain.sq_pos D b')               |> apply                                           // (15.34)
                    Establish (posl bb)                                 |> apply
                    Establish (OrderedDomain.sq_pos D c')               |> apply
                    Establish (posl cc)                                 |> apply
                    Integers.ring th zero (zero * cc)                   |> at [right_branch; left_branch]
                    Deduce (OrderedDomain.lt_mul_pos D zero bb cc)      |> apply_right ]
        let be = dvd_intro th b' e (b' * cc) (Integers.ring_ident th (b' * (b' * cc)) e)
        let ce = dvd_intro th c' e (c' * bb) (Integers.ring_ident th (c' * (c' * bb)) e)
        let least = least_element th x y P e
        let nz = lemma th ((!!Z) ==> goal) [
                    distrib_not_or Zb Zc                                |> at_left
                    Establish epos                                      |> apply
                    Establish (lt_le th zero e)                         |> apply
                    Establish (under th NZ2 be)                         |> apply
                    Establish (under th NZ2 ce)                         |> apply
                    Establish least                                     |> apply                                           // (15.49)
                    Deduce elimX                                        |> apply_right ]
        by_em th Z goal z nz

    (* The definition, in parts *)

    type private LcmFacts = { Nonneg: Theorem; DvdL: Theorem; DvdR: Theorem; All: Theorem; Dummy: IntVar }

    let private lcm_facts (th: Theory) (b: Term<int>) (c: Term<int>) : LcmFacts =
        let D = Integers.order_domain th
        let b', c', L = D.Term b, D.Term c, D.Term (lcm_of b c)
        let dv = lcm_dummy b c
        let x = D.Term (tm dv)
        let N, B1, B2 = (D.Zero <=. L), dvd b' L, dvd c' L
        let ALL = qall dv (dvd b' x * dvd c' x) (dvd L x)
        let ax = axiom th (lcm_spec_with dv b c)
        let left = conj_fst th (N * (B1 * B2)) ALL ax
        let b12 = conj_snd th N (B1 * B2) left
        { Nonneg = conj_fst th N (B1 * B2) left; DvdL = conj_fst th B1 B2 b12; DvdR = conj_snd th B1 B2 b12
          All = conj_snd th (N * (B1 * B2)) ALL ax; Dummy = dv }

    /// `0 ≤ b lcm c` — the definition of lcm.
    let lcm_nonneg (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (lcm_facts th b c).Nonneg
    /// `b ∣ b lcm c` — the definition of lcm.
    let dvd_lcm_l (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (lcm_facts th b c).DvdL
    /// `c ∣ b lcm c` — the definition of lcm.
    let dvd_lcm_r (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = (lcm_facts th b c).DvdR

    /// `b ∣ e ∧ c ∣ e ⇒ b lcm c ∣ e` — the definition of lcm, instantiated (9.13) at `e`.
    let lcm_dvd (th: Theory) (e: Term<int>) (b: Term<int>) (c: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let f = lcm_facts th b c
        let dv = f.Dummy
        let b', c', L, x = D.Term b, D.Term c, D.Term (lcm_of b c), D.Term (tm dv)
        let Rp, Pp = pred_of dv (dvd b' x * dvd c' x), pred_of dv (dvd L x)
        let inst = lemma th (qall dv (dvd b' x * dvd c' x) (dvd L x) ==> (Rp.[e] ==> Pp.[e])) [ trade_forall_implies dv Rp Pp |> at_left ]
        mp inst f.All

    let private lcm_eq_c =
        cached3 "dv_lcm_eq" (fun th (b: Term<int>) (c: Term<int>) (l: Term<int>) ->
            let D = Integers.order_domain th
            let L, l' = D.Term (lcm_of b c), D.Term l
            let H = ((D.Zero <=. l') * (dvd (D.Term b) l' * dvd (D.Term c) l')) * dvd l' L
            lemma th (H ==> (L == l')) [
                Establish (lcm_dvd th l b c)                            |> apply                                           // b lcm c ∣ l
                Establish (under th H (lcm_nonneg th b c))              |> apply
                Deduce (dvd_antisym_nonneg th L l')                     |> apply_right ])
    /// `(0 ≤ l ∧ (b ∣ l ∧ c ∣ l)) ∧ l ∣ b lcm c ⇒ b lcm c = l` — the uniqueness the lcm laws are proved by.
    let lcm_eq (th: Theory) (b: Term<int>) (c: Term<int>) (l: Term<int>) : Theorem = lcm_eq_c th b c l

    let private lcm_least_c =
        cached3 "dv_lcm_least" (fun th (k: Term<int>) (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let L, k' = D.Term (lcm_of b c), D.Term k
            lemma th (((D.Zero <. k') * (dvd (D.Term b) k' * dvd (D.Term c) k')) ==> (L <=. k')) [
                Establish (lcm_dvd th k b c)                            |> apply
                Deduce (dvd_le th k L)                                  |> apply_right ])
    /// **(15.94)**, Gries' definition, as a theorem: `0 < k ∧ (b ∣ k ∧ c ∣ k) ⇒ b lcm c ≤ k` — `b lcm c`
    /// is below every positive common multiple, since it divides it (`dvd_le`).
    let lcm_least (th: Theory) (k: Term<int>) (b: Term<int>) (c: Term<int>) : Theorem = lcm_least_c th k b c

    let private lcm_zero_c =
        cached2 "dv_lcm_zero" (fun th (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let zero = D.Zero
            let b', c', L = D.Term b, D.Term c, D.Term (lcm_of b c)
            let Zb, Zc = (b' == zero), (c' == zero)
            let R = L == zero
            let cb = lemma th ((T * Zb) ==> R) [
                        Establish (under th Zb (dvd_lcm_l th b c))      |> apply
                        Deduce (zero_dvd th b' L)                       |> apply_right ]
            let cc = lemma th ((T * Zc) ==> R) [
                        Establish (under th Zc (dvd_lcm_r th b c))      |> apply
                        Deduce (zero_dvd th c' L)                       |> apply_right ]
            lemma th ((Zb + Zc) ==> R) [
                ident_and (Zb + Zc) |> Commute                          |> at_left
                Taut (by_cases2 th Zb Zc T R cb cc)                     |> apply ])
    /// **(15.94)** `b = 0 ∨ c = 0 ⇒ b lcm c = 0` — 0 divides only 0.
    let lcm_zero (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = lcm_zero_c th b c

    let private lcm_pos_c =
        cached2 "dv_lcm_pos" (fun th (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let zero = D.Zero
            let b', c', L = D.Term b, D.Term c, D.Term (lcm_of b c)
            let Z = (b' == zero) + (c' == zero)
            let bc = b' * c'
            let bbc = mp (dvd_mul th b' b' c') (dvd_refl th b')
            let cbc = lemma th (dvd c' bc) [ Integers.ring th bc (c' * b') |> at [right_branch]; Taut (mp (dvd_mul th c' c' b') (dvd_refl th c')) |> apply ]
            let Lbc = mp (lcm_dvd th bc b c) (conj bbc cbc)
            let le0 = lemma th ((L <=. zero) ==> Z) [
                        IntegralDomain.zero_divisors D b' c' |> Commute |> at_right                                        // → b·c = 0 (15.11)
                        Establish (under th (L <=. zero) (lcm_nonneg th b c)) |> apply
                        Establish (OrderedDomain.le_antisym D L zero)   |> apply                                           // b lcm c = 0
                        Establish (under th (L <=. zero) Lbc)           |> apply
                        Deduce (zero_dvd th L bc)                       |> apply_right ]
            lemma th ((!!Z) ==> (zero <. L)) [
                def_implies_contr (!!Z) (zero <. L)                     |> apply
                double_negation Z                                       |> at_right
                OrderedDomain.not_lt D zero L                           |> at_left
                Taut le0                                                |> apply ])
    /// **(15.94)** `¬(b = 0 ∨ c = 0) ⇒ 0 < b lcm c` — it divides `b·c`, which is not 0 (15.11). With
    /// `lcm_least` and `dvd_lcm_l`/`dvd_lcm_r`, `b lcm c` is Gries' least positive common multiple.
    let lcm_pos (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = lcm_pos_c th b c

    let private lcm_sym_c =
        cached2 "dv_lcm_sym" (fun th (b: Term<int>) (c: Term<int>) ->
            let L, L' = lcm_of b c, lcm_of c b
            mp (lcm_eq th b c L')
               (conj (conj (lcm_nonneg th c b) (conj (dvd_lcm_r th c b) (dvd_lcm_l th c b)))
                     (mp (lcm_dvd th L c b) (conj (dvd_lcm_r th b c) (dvd_lcm_l th b c)))))
    /// `b lcm c = c lcm b`
    let lcm_sym (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = lcm_sym_c th b c

    let private gcd_lcm_c =
        cached2 "dv_gcd_lcm" (fun th (b: Term<int>) (c: Term<int>) ->
            let D = Integers.order_domain th
            let zero = D.Zero
            let b', c' = D.Term b, D.Term c
            let G, L = D.Term (gcd_of b c), D.Term (lcm_of b c)
            let bc = b' * c'
            let LG = L * G
            let A = abs_of bc
            // b·c ∣ L·G: L·G = (L·b) gcd (L·c) (15.104), and b·c divides both (15.85).
            let bcLb = lemma th (dvd bc (L * b')) [
                        Integers.ring th bc (c' * b')                   |> at [left_branch]
                        Taut (mp (dvd_mul_both th c' L b') (dvd_lcm_r th b c)) |> apply ]
            let bcLc = mp (dvd_mul_both th b' L c') (dvd_lcm_l th b c)
            let bcK = mp (dvd_gcd th bc (L * b') (L * c')) (conj bcLb bcLc)
            let dist = mp (gcd_mul th L b c) (lcm_nonneg th b c)                                                           // L·G = (L·b) gcd (L·c)
            let bcLG = lemma th (dvd bc LG) [ Ident dist |> at [right_branch]; Taut bcK |> apply ]
            // L·G ∣ b·c: with G·j = b and G·k = c, b·k is a common multiple, so L ∣ b·k, and (b·k)·G = b·c.
            let R = dvd LG bc
            let core (j: IntVar) (k: IntVar) =
                let J, Kk = D.Term (tm j), D.Term (tm k)
                let Hj, Hk = body G b' j, body G c' k
                let bk = b' * Kk
                let ck = lemma th ((Hj * Hk) ==> dvd c' bk) [
                            Deduce' (OrderedDomain.eq_sym th (G * J) b') |> at [right_branch; right_branch; left_branch]   // b → G·j
                            Integers.ring th ((G * J) * Kk) ((G * Kk) * J) |> at [right_branch; right_branch]
                            Deduce' (reflex_implies Hk)                 |> at [right_branch; right_branch; left_branch]    // G·k → c
                            Taut (mp (dvd_mul th c' c' J) (dvd_refl th c')) |> apply_right ]
                lemma th ((Hj * Hk) ==> R) [
                    Deduce' (OrderedDomain.eq_sym th (G * Kk) c')       |> at [right_branch; right_branch; right_branch]   // c → G·k
                    Integers.ring th (b' * (G * Kk)) (bk * G)           |> at [right_branch; right_branch]
                    Establish ck                                        |> apply
                    Establish (under th (Hj * Hk) (mp (dvd_mul th b' b' Kk) (dvd_refl th b'))) |> apply              // b ∣ b·k
                    Establish (lcm_dvd th bk b c)                       |> apply                                           // L ∣ b·k
                    Deduce (dvd_mul_both th L bk G)                     |> apply_right ]                                   // (15.85)
            let outer =
                dvd_elim th G b' (dvd G c' ==> R) (fun j ->
                    let Hj = body G b' j
                    let inner = dvd_elim th G c' (Hj ==> R) (fun k ->
                                    let Hk = body G c' k
                                    lemma th (Hk ==> (Hj ==> R)) [
                                        shunt' Hk Hj R |> Commute       |> apply
                                        commute_and Hk Hj               |> at_left
                                        Taut (core j k)                 |> apply ])
                    lemma th (Hj ==> (dvd G c' ==> R)) [
                        shunt' Hj (dvd G c') R |> Commute               |> apply
                        commute_and Hj (dvd G c')                       |> at_left
                        shunt' (dvd G c') Hj R                          |> apply
                        Taut inner                                      |> apply ])
            let LGbc = mp (mp outer (gcd_dvd_l th b c)) (gcd_dvd_r th b c)
            let nonneg = lemma th (zero <=. LG) [
                            Integers.ring th zero (L * zero)            |> at [left_branch]
                            Taut (mp (OrderedDomain.le_mul_nonneg D L zero G) (conj (lcm_nonneg th b c) (gcd_nonneg th b c))) |> apply ]
            let eq = mp (dvd_antisym_nonneg th LG A)
                        (conj (conj nonneg (OrderedDomain.abs_nonneg D bc)) (conj (to_abs th LG bc LGbc) (dtrans th A bc LG (abs_dvd th bc) bcLG)))
            lemma th ((G * L) == A) [ Integers.ring th (G * L) LG |> at_left; Taut eq |> apply ])
    /// **`(b gcd c)·(b lcm c) = abs(b·c)`** — the identity Gries announces with (15.94) ("we prove later
    /// … that `b·c = (b gcd c)·(b lcm c)`", for natural `b`, `c`). Each side divides the other and both
    /// are non-negative: `b·c` divides `(b lcm c)·(b gcd c) = (L·b) gcd (L·c)` (15.104); and with
    /// `b = g·j`, `c = g·k`, `b·k` is a common multiple, so `L ∣ b·k` and `L·g ∣ b·k·g = b·c`.
    let gcd_lcm (th: Theory) (b: Term<int>) (c: Term<int>) : Theorem = gcd_lcm_c th b c

    (* ------------------------------------------------------------------------------------------
       Congruences (15.118)–(15.127), Gries pp. 325–327.
       ------------------------------------------------------------------------------------------ *)

    /// The definition (15.118) as a rewrite: `b ≡ c (mod m) → m ∣ (c − b)`.
    let cong_def (th: Theory) (b: Term<int>) (c: Term<int>) (m: Term<int>) : Rule = ax_ident th (cong_spec b c m)

    /// (x ⇒ (a ⇒ y)) ⇒ ((a ⇒ (y ⇒ x)) ⇒ (a ⇒ (x = y)))
    let private glue_iff = OrderedDomain.taut3 "dv_iff_under" (fun a x y -> (x ==> (a ==> y)) ==> ((a ==> (y ==> x)) ==> (a ==> (x == y))))

    /// `a ⇒ (x = y)`, from `x ⇒ (a ⇒ y)` and `a ⇒ (y ⇒ x)` — mutual implication under a hypothesis.
    let private iff_under (th: Theory) (a: Prop) (x: Prop) (y: Prop) (fwd: Theorem) (bwd: Theorem) : Theorem =
        mp (mp (OrderedDomain.in_theory th (glue_iff a x y)) fwd) bwd

    /// `x ⇒ (a ⇒ y)`, from `(a ∧ x) ⇒ y`.
    let private shunt_swap (th: Theory) (a: Prop) (x: Prop) (y: Prop) (t: Theorem) : Theorem =
        lemma th (x ==> (a ==> y)) [ shunt' x a y |> Commute |> apply; commute_and x a |> at_left; Taut t |> apply ]

    /// `a ⇒ (x ⇒ y)`, from `(a ∧ x) ⇒ y`.
    let private shunt_t (th: Theory) (a: Prop) (x: Prop) (y: Prop) (t: Theorem) : Theorem =
        lemma th (a ==> (x ==> y)) [ shunt' a x y |> Commute |> apply; Taut t |> apply ]

    /// `H ⇒ c ∣ b`, from `H ⇒ c·k = b` — ∃-introduction (9.28) at the witness `k`, under a hypothesis.
    let private dvd_intro_under (th: Theory) (H: Prop) (c: Term<int>) (b: Term<int>) (k: Term<int>) (h: Theorem) : Theorem =
        let d = fresh_int "d" [ c.Expr; b.Expr; k.Expr ]
        let P = pred_of d (body c b d)
        let ex = lemma th (P.[k] ==> dvd c b) [
                    dvd_def_with th d c b                               |> at_right
                    Taut (OrderedDomain.in_theory th (exists_intro d P k)) |> apply ]
        lemma th (H ==> dvd c b) [ Establish h |> apply; Deduce ex |> apply_right ]

    /// `0 < x ⇒ ¬(x = 0)`
    let private pos_ne_zero (th: Theory) (x: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let zero, x' = D.Zero, D.Term x
        let P, Z = (zero <. x'), (x' == zero)
        lemma th (P ==> !!Z) [
            ident_implies_false_not Z |> Commute                        |> at_right                                        // ¬Z → (Z ⇒ F)
            shunt' P Z F |> Commute                                     |> apply
            OrderedDomain.lt_irrefl D zero |> Commute                   |> at_right                                        // F → 0 < 0
            Deduce' (OrderedDomain.eq_sym th x' zero)                   |> at [right_branch; right_branch]                 // 0 → x
            Deduce (reflex_implies P)                                   |> apply_right ]

    let private cong_refl_c =
        cached2 "dv_cong_refl" (fun th (b: Term<int>) (m: Term<int>) ->
            let D = Integers.order_domain th
            let b', m' = D.Term b, D.Term m
            lemma th (cong b' b' m') [
                cong_def th b' b' m'                                    |> apply
                Integers.ring th (b' - b') D.Zero                       |> at [right_branch]
                Taut (dvd_zero th m')                                   |> apply ])
    /// **(15.118) Reflexivity**: `b ≡ b (mod m)`.
    let cong_refl (th: Theory) (b: Term<int>) (m: Term<int>) : Theorem = cong_refl_c th b m

    let private cong_sym_c =
        cached3 "dv_cong_sym" (fun th (b: Term<int>) (c: Term<int>) (m: Term<int>) ->
            let D = Integers.order_domain th
            let b', c', m' = D.Term b, D.Term c, D.Term m
            lemma th (cong b' c' m' == cong c' b' m') [
                cong_def th b' c' m'                                    |> at_left
                cong_def th c' b' m'                                    |> at_right
                Integers.ring th (c' - b') (-(b' - c'))                 |> at [left_branch; right_branch]
                Ident (dvd_neg_eq th m' (b' - c'))                      |> at_left ])
    /// **(15.118) Symmetry**: `b ≡ c (mod m) = c ≡ b (mod m)` — `c − b = −(b − c)`.
    let cong_sym (th: Theory) (b: Term<int>) (c: Term<int>) (m: Term<int>) : Theorem = cong_sym_c th b c m

    let private cong_trans_c =
        cached4 "dv_cong_trans" (fun th (b: Term<int>) (c: Term<int>) (e: Term<int>) (m: Term<int>) ->
            let D = Integers.order_domain th
            let b', c', e', m' = D.Term b, D.Term c, D.Term e, D.Term m
            lemma th ((cong b' c' m' * cong c' e' m') ==> cong b' e' m') [
                cong_def th b' c' m'                                    |> at [left_branch; left_branch]
                cong_def th c' e' m'                                    |> at [left_branch; right_branch]
                cong_def th b' e' m'                                    |> at_right
                Integers.ring th (e' - b') ((c' - b') + (e' - c'))      |> at [right_branch; right_branch]
                Taut (dvd_add th m' (c' - b') (e' - c'))                |> apply ])
    /// **(15.118) Transitivity**: `b ≡ c (mod m) ∧ c ≡ e (mod m) ⇒ b ≡ e (mod m)` — `e − b` is
    /// `(c − b) + (e − c)` (Gries' exercise 15.65).
    let cong_trans (th: Theory) (b: Term<int>) (c: Term<int>) (e: Term<int>) (m: Term<int>) : Theorem = cong_trans_c th b c e m

    let private cong_mod_eq_c =
        cached3 "dv_cong_mod_eq" (fun th (b: Term<int>) (c: Term<int>) (m: Term<int>) ->
            let D = Integers.order_domain th
            let zero = D.Zero
            let b', c', m' = D.Term b, D.Term c, D.Term m
            let qb, rb = D.Term (div_of b' m'), D.Term (mod_of b' m')
            let qc, rc = D.Term (div_of c' m'), D.Term (mod_of c' m')
            let C0, E = (zero <. m'), (rb == rc)
            let Sb, Sc = (b' == qb * m' + rb), (c' == qc * m' + rc)
            let X = dvd m' (c' - b')
            // ⇐ : c − b = (qc·m + rc) − (qb·m + rb) = m·(qc − qb) when rb = rc.
            let w = qc - qb
            let wit = lemma th ((C0 * E) ==> (m' * w == c' - b')) [
                        Establish (div_mod_def th b' m')                    |> apply
                        Establish (div_mod_def th c' m')                    |> apply
                        Deduce' (reflex_implies Sc)                         |> at [right_branch; right_branch; left_branch]                  // c → qc·m + rc
                        Deduce' (reflex_implies Sb)                         |> at [right_branch; right_branch; right_branch]                 // b → qb·m + rb
                        Deduce' (reflex_implies E)                          |> at [right_branch; right_branch; right_branch; right_branch]   // rb → rc
                        Integers.ring th ((qc * m' + rc) - (qb * m' + rc)) (m' * w) |> at [right_branch; right_branch]
                        IntegralDomain.refl_true th (m' * w)                |> apply_right ]
            let bwd = shunt_t th C0 E X (dvd_intro_under th (C0 * E) m' (c' - b') w wit)
            // ⇒ : with m·k = c − b, c = (qb + k)·m + rb with 0 ≤ rb < m, so c mod m = rb (15.90).
            let fwd =
                dvd_elim th m' (c' - b') (C0 ==> E) (fun k ->
                    let K = D.Term (tm k)
                    let Hk = body m' (c' - b') k
                    let A = Hk * C0
                    let qk = qb + K
                    let ec = lemma th (A ==> (c' == qk * m' + rb)) [
                                Establish (div_mod_def th b' m')            |> apply
                                Integers.ring th (qk * m' + rb) (m' * K + (qb * m' + rb)) |> at [right_branch; right_branch]
                                Deduce' (reflex_implies Hk)                 |> at [right_branch; right_branch; left_branch]                  // m·k → c − b
                                Deduce' (OrderedDomain.eq_sym th b' (qb * m' + rb)) |> at [right_branch; right_branch; right_branch]     // qb·m + rb → b
                                Integers.ring th ((c' - b') + b') c'        |> at [right_branch; right_branch]
                                IntegralDomain.refl_true th c'              |> apply_right ]
                    let inner = lemma th (A ==> E) [
                                Establish ec                                |> apply
                                Establish (mod_nonneg th b' m')             |> apply
                                Establish (mod_lt th b' m')                 |> apply
                                Establish (mod_eq th c' m' qk rb)           |> apply                                                         // c mod m = rb
                                Deduce (OrderedDomain.eq_sym th rc rb)      |> apply_right ]
                    shunt_t th Hk C0 E inner)
            lemma th (C0 ==> (cong b' c' m' == E)) [
                cong_def th b' c' m'                                    |> at [right_branch; left_branch]
                Taut (iff_under th C0 X E fwd bwd)                      |> apply ])
    /// **(15.119) Alternative definition of congruence**: `0 < m ⇒ (b ≡ c (mod m) = (b mod m = c mod m))`.
    /// Gries omits `0 < m`, but `mod` is defined only for a positive modulus (15.90). Gries' proof:
    /// `⇐` subtracts the two division equations; `⇒` writes `c = (b ÷ m + k)·m + b mod m` and uses
    /// the uniqueness of the remainder.
    let cong_mod_eq (th: Theory) (b: Term<int>) (c: Term<int>) (m: Term<int>) : Theorem = cong_mod_eq_c th b c m

    let private cong_add_c =
        cached4 "dv_cong_add" (fun th (b: Term<int>) (c: Term<int>) (d: Term<int>) (m: Term<int>) ->
            let D = Integers.order_domain th
            let b', c', d', m' = D.Term b, D.Term c, D.Term d, D.Term m
            lemma th (cong b' c' m' == cong (b' + d') (c' + d') m') [
                cong_def th b' c' m'                                    |> at_left
                cong_def th (b' + d') (c' + d') m'                      |> at_right
                Integers.ring th ((c' + d') - (b' + d')) (c' - b')      |> at [right_branch; right_branch] ])
    /// **(15.120) Addition**: `b ≡ c (mod m) = b + d ≡ c + d (mod m)`.
    let cong_add (th: Theory) (b: Term<int>) (c: Term<int>) (d: Term<int>) (m: Term<int>) : Theorem = cong_add_c th b c d m

    let private cong_neg_c =
        cached3 "dv_cong_neg" (fun th (b: Term<int>) (c: Term<int>) (m: Term<int>) ->
            let D = Integers.order_domain th
            let b', c', m' = D.Term b, D.Term c, D.Term m
            lemma th (cong b' c' m' == cong (-b') (-c') m') [
                cong_def th b' c' m'                                    |> at_left
                cong_def th (-b') (-c') m'                              |> at_right
                Integers.ring th ((-c') - (-b')) (-(c' - b'))           |> at [right_branch; right_branch]
                Ident (dvd_neg_eq th m' (c' - b'))                      |> at_right ])
    /// **(15.121) Negation**: `b ≡ c (mod m) = −b ≡ −c (mod m)`.
    let cong_neg (th: Theory) (b: Term<int>) (c: Term<int>) (m: Term<int>) : Theorem = cong_neg_c th b c m

    let private cong_mul_c =
        cached4 "dv_cong_mul" (fun th (b: Term<int>) (c: Term<int>) (d: Term<int>) (m: Term<int>) ->
            let D = Integers.order_domain th
            let b', c', d', m' = D.Term b, D.Term c, D.Term d, D.Term m
            lemma th (cong b' c' m' ==> cong (b' * d') (c' * d') m') [
                cong_def th b' c' m'                                    |> at_left
                cong_def th (b' * d') (c' * d') m'                      |> at_right
                Integers.ring th ((c' * d') - (b' * d')) ((c' - b') * d') |> at [right_branch; right_branch]
                Taut (dvd_mul th m' (c' - b') d')                       |> apply ])
    /// **(15.122) Multiplication**: `b ≡ c (mod m) ⇒ b·d ≡ c·d (mod m)` — (15.84). Not an equality:
    /// `4·2 ≡ 1·2 (mod 6)` but not `4 ≡ 1 (mod 6)`.
    let cong_mul (th: Theory) (b: Term<int>) (c: Term<int>) (d: Term<int>) (m: Term<int>) : Theorem = cong_mul_c th b c d m

    /// `x ≡ y (mod m) ∧ b ≡ c (mod m) ⇒ x·b ≡ y·c (mod m)` — `x·b ≡ y·b ≡ y·c`.
    let private cong_mul2 =
        cachedn "dv_cong_mul2" 5 (fun th args ->
            match args with
            | [ x; y; b; c; m ] ->
                let D = Integers.order_domain th
                let x', y', b', c', m' = D.Term x, D.Term y, D.Term b, D.Term c, D.Term m
                let yb = lemma th (cong b' c' m' ==> cong (y' * b') (y' * c') m') [
                            cong_def th b' c' m'                        |> at_left
                            cong_def th (y' * b') (y' * c') m'          |> at_right
                            Integers.ring th ((y' * c') - (y' * b')) ((c' - b') * y') |> at [right_branch; right_branch]
                            Taut (dvd_mul th m' (c' - b') y')           |> apply ]
                lemma th ((cong x' y' m' * cong b' c' m') ==> cong (x' * b') (y' * c') m') [
                    Establish (cong_mul th x' y' b' m')                 |> apply
                    Establish yb                                        |> apply
                    Deduce (cong_trans th (x' * b') (y' * b') (y' * c') m') |> apply_right ]
            | _ -> invalidArg "args" "dv_cong_mul2")

    /// `(Πj | 1 ≤ j ∧ j ≤ n : b)` — the power `bⁿ` for `0 ≤ n`, as a product quantification, with
    /// the given dummy.
    let private power_with (j: IntVar) (b: Term<int>) (n: Term<int>) : IntTerm =
        Sums.qprod j ((leq 1 (tm j)) * (leq (tm j) n)) b

    /// **The power `bⁿ`** — `(Πj | 1 ≤ j ∧ j ≤ n : b)`, meaningful for `0 ≤ n` (`b⁰ = 1` is the empty
    /// product). Notation, not a symbol: it adds nothing to the theory. The dummy is `j`, or the first
    /// `j#i` fresh for `b` and `n`.
    let power (b: Term<int>) (n: Term<int>) : IntTerm = power_with (fresh_int "j" [ b.Expr; n.Expr ]) b n

    /// **(15.123) Powers**: `0 ≤ n ⇒ (b ≡ c (mod m) ⇒ bⁿ ≡ cⁿ (mod m))`, by induction (12.8) on `n`:
    /// `b⁰ = 1 = c⁰`, and `b^(k+1) = bᵏ·b ≡ cᵏ·c = c^(k+1)` (Split off factor, then `cong_mul2`). The
    /// products' dummy is `j`, or the first `j#i` fresh for `b`, `c`, `n` and `m` — the dummy of
    /// `power b n` whenever neither `c` nor `m` mentions `j`.
    let cong_pow (th: Theory) (b: Term<int>) (c: Term<int>) (n: Term<int>) (m: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let b', c', m' = D.Term b, D.Term c, D.Term m
        let avoid = [ b.Expr.Raw; c.Expr.Raw; n.Expr.Raw; m.Expr.Raw ]
        let j = fresh_int "j" avoid
        let kv = fresh_int "k" (j.Expr.Raw :: avoid)
        let i = fresh_int "i" (kv.Expr.Raw :: j.Expr.Raw :: avoid)
        let Pw (x: DomainTerm<int>) (w: Term<int>) = D.Term (power_with j x w)
        let H = cong b' c' m'
        let C (w: Term<int>) = cong (Pw b' w) (Pw c' w) m'
        let P = pred_of kv (H ==> C (tm kv))
        // Base: b⁰ = 1 = c⁰.
        let bse = lemma th (base_case 0 P) [
                    cong_def th (Pw b' (IntTerm 0)) (Pw c' (IntTerm 0)) m'  |> at_right
                    Ident (empty_prod th 1 0 j b')                      |> at [right_branch; right_branch; right_branch]   // b⁰ → 1
                    Ident (empty_prod th 1 0 j c')                      |> at [right_branch; right_branch; left_branch]    // c⁰ → 1
                    Integers.ring th (D.One - D.One) D.Zero             |> at [right_branch; right_branch]
                    Taut (dvd_zero th m')                               |> apply_right ]
        // Step: b^(k+1) = bᵏ·b, c^(k+1) = cᵏ·c.
        let a, ih = leq 0 (tm kv), hypothesis 0 kv i P
        let k1 = succ kv
        let Ck1 = C k1
        let hk = lemma th (((a * ih) * H) ==> C (tm kv)) [
                    shunt' (a * ih) H (C (tm kv))                       |> apply
                    Taut (hypothesis_at th 0 kv i P)                    |> apply ]
        let body = lemma th (a ==> (ih ==> P.[k1])) [
                    (shunt' a ih P.[k1] |> Commute)                     |> apply
                    shunt' (a * ih) H Ck1 |> Commute                    |> apply
                    Establish (shift_bound th 0 (tm kv))                |> apply                                           // 1 ≤ k+1
                    Deduce' (split_off_factor th 1 (tm kv) j b')        |> apply_right                                     // b^(k+1) → bᵏ·b
                    Deduce' (split_off_factor th 1 (tm kv) j c')        |> apply_right                                     // c^(k+1) → cᵏ·c
                    Establish hk                                        |> apply                                           // bᵏ ≡ cᵏ
                    Deduce (cong_mul2 th [ Pw b' (tm kv); Pw c' (tm kv); b'; c'; m' ]) |> apply_right ]
        let all = by_induction th 0 kv i P bse (generalize_step th 0 kv i P body)
        let Rk = pred_of kv a
        mp (lemma th (Prop(Expr.Cast<bool> all.Stmt) ==> (Rk.[n] ==> P.[n])) [ trade_forall_implies kv Rk P |> at_left ]) all

    let private cong_cancel_c =
        cached4 "dv_cong_cancel" (fun th (b: Term<int>) (c: Term<int>) (d: Term<int>) (m: Term<int>) ->
            let D = Integers.order_domain th
            let b', c', d', m' = D.Term b, D.Term c, D.Term d, D.Term m
            let H = rel_prime d' m'
            let X, Y = cong (b' * d') (c' * d') m', cong b' c' m'
            // ⇒ : m ∣ d·(c − b) and d ⊥ m, so m ∣ c − b (15.113).
            let fwd = lemma th ((H * X) ==> Y) [
                        cong_def th (b' * d') (c' * d') m'              |> at [left_branch; right_branch]
                        cong_def th b' c' m'                            |> at_right
                        Integers.ring th ((c' * d') - (b' * d')) (d' * (c' - b')) |> at [left_branch; right_branch; right_branch]
                        Taut (coprime_dvd th d' m' (c' - b'))           |> apply ]
            iff_under th H X Y (shunt_swap th H X Y fwd) (under th H (cong_mul th b' c' d' m')))
    /// **(15.124) Cancellation**: `d ⊥ m ⇒ (b·d ≡ c·d (mod m) = b ≡ c (mod m))` — `⇒` is (15.113),
    /// `⇐` is (15.122).
    let cong_cancel (th: Theory) (b: Term<int>) (c: Term<int>) (d: Term<int>) (m: Term<int>) : Theorem = cong_cancel_c th b c d m

    let private cong_cancel_mod_c =
        cached4 "dv_cong_cancel_mod" (fun th (b: Term<int>) (c: Term<int>) (d: Term<int>) (m: Term<int>) ->
            let D = Integers.order_domain th
            let zero = D.Zero
            let b', c', d', m' = D.Term b, D.Term c, D.Term d, D.Term m
            let P0 = zero <. d'
            let md, cdbd = m' * d', (c' * d') - (b' * d')
            let X, Y = cong (b' * d') (c' * d') md, cong b' c' m'
            let R = dvd m' (c' - b')
            // ⇐ : (15.85).
            let bwd = lemma th (Y ==> X) [
                        cong_def th b' c' m'                            |> at_left
                        cong_def th (b' * d') (c' * d') md              |> at_right
                        Integers.ring th cdbd ((c' - b') * d')          |> at [right_branch; right_branch]
                        Taut (dvd_mul_both th m' (c' - b') d')          |> apply ]
            // ⇒ : with (m·d)·k = c·d − b·d, d·(m·k) = d·(c − b), and d ≠ 0 cancels (15.7).
            let fwd =
                dvd_elim th md cdbd (P0 ==> R) (fun k ->
                    let K = D.Term (tm k)
                    let Hk = body md cdbd k
                    let A = Hk * P0
                    let NZ = !!(d' == zero)
                    let Xe, Ye = (d' * (m' * K) == d' * (c' - b')), (m' * K == c' - b')
                    let xe = lemma th (A ==> Xe) [
                                Integers.ring th (d' * (m' * K)) (md * K)   |> at [right_branch; left_branch]
                                Integers.ring th (d' * (c' - b')) cdbd      |> at [right_branch; right_branch]
                                Deduce' (reflex_implies Hk)                 |> at [right_branch; left_branch]                                // (m·d)·k → c·d − b·d
                                IntegralDomain.refl_true th cdbd            |> apply_right ]
                    let cancel = axiom th (NZ ==> (Xe == Ye))                                                                                // (15.7)
                    let detach = mp (OrderedDomain.in_theory th (glue_detach NZ Xe Ye)) cancel
                    let ye = lemma th (A ==> Ye) [
                                Establish (pos_ne_zero th d')               |> apply
                                Establish xe                                |> apply
                                Deduce detach                               |> apply_right ]
                    shunt_t th Hk P0 R (dvd_intro_under th A m' (c' - b') K ye))
            let fwd' = lemma th (X ==> (P0 ==> Y)) [
                        cong_def th (b' * d') (c' * d') md              |> at_left
                        cong_def th b' c' m'                            |> at [right_branch; right_branch]
                        Taut fwd                                        |> apply ]
            iff_under th P0 X Y fwd' (under th P0 bwd))
    /// **(15.125) Cancellation**: `0 < d ⇒ (b·d ≡ c·d (mod m·d) = b ≡ c (mod m))` — cancelling in the
    /// modulus too. `⇐` is (15.85); `⇒` cancels `d ≠ 0` from `d·(m·k) = d·(c − b)` (15.7).
    let cong_cancel_mod (th: Theory) (b: Term<int>) (c: Term<int>) (d: Term<int>) (m: Term<int>) : Theorem = cong_cancel_mod_c th b c d m

    /// **(15.126)** `d ⊥ m ⇒ (∃x |: d·x ≡ b (mod m))` — by Bézout (15.108), `1 = u·d + v·m`, so
    /// `b − d·(u·b) = m·(v·b)`: `x = u·b` is a solution. The dummy is `x`, or the first `x#i` fresh
    /// for `d`, `b` and `m`.
    let cong_solvable (th: Theory) (d: Term<int>) (b: Term<int>) (m: Term<int>) : Theorem =
        let D = Integers.order_domain th
        let one = D.One
        let d', b', m' = D.Term d, D.Term b, D.Term m
        let G = D.Term (gcd_of d' m')
        let H = G == one
        let avoid = [ d'.Expr.Raw; b'.Expr.Raw; m'.Expr.Raw ]
        let x = fresh_int "x" avoid
        let u = fresh_int "u" (x.Expr.Raw :: avoid)
        let v = fresh_int "v" (u.Expr.Raw :: x.Expr.Raw :: avoid)
        let U, V = D.Term (tm u), D.Term (tm v)
        let HB = G == U * d' + V * m'
        let P = pred_of x (cong (d' * D.Term (tm x)) b' m')
        let EX = qex x T P.[tm x]
        let ub = U * b'
        let A = HB * H
        // m·(v·b) = b − d·(u·b): b − d·(u·b) = b·1 − b·(u·d) = b·G − b·(u·d) = b·(u·d + v·m) − b·(u·d).
        let wit = lemma th (A ==> (m' * (V * b') == b' - d' * ub)) [
                    Integers.ring th (b' - d' * ub) (b' * one - b' * (U * d')) |> at [right_branch; right_branch]
                    Deduce' (OrderedDomain.eq_sym th G one)             |> at [right_branch; right_branch; left_branch; right_branch]   // 1 → d gcd m
                    Deduce' (reflex_implies HB)                         |> at [right_branch; right_branch; left_branch; right_branch]   // → u·d + v·m
                    Integers.ring th (b' * (U * d' + V * m') - b' * (U * d')) (m' * (V * b')) |> at [right_branch; right_branch]
                    IntegralDomain.refl_true th (m' * (V * b'))         |> apply_right ]
        let sol = lemma th (A ==> P.[ub]) [
                    cong_def th (d' * ub) b' m'                         |> at_right
                    Taut (dvd_intro_under th A m' (b' - d' * ub) (V * b') wit) |> apply ]
        let core = lemma th (A ==> EX) [
                    Establish sol                                       |> apply
                    Deduce (OrderedDomain.in_theory th (exists_intro x P ub)) |> apply_right ]
        // ∃-eliminate v, then u, from Bézout.
        let Q = H ==> EX
        let inner' = shunt_t th HB H EX core
        let EXv = qex v T HB
        let tv_ = lemma th ((T * HB) ==> Q) [ commute_and T HB |> at_left; ident_and HB |> at_left; Taut inner' |> apply ]
        let elimV = exists_elim th v truepred (pred_of v HB) Q tv_
        let tu_ = lemma th ((T * EXv) ==> Q) [ commute_and T EXv |> at_left; ident_and EXv |> at_left; Taut elimV |> apply ]
        let elimU = exists_elim th u truepred (pred_of u EXv) Q tu_
        mp elimU (gcd_bezout_with th u v d' m')

    let private cong_unique_c =
        cachedn "dv_cong_unique" 5 (fun th args ->
            match args with
            | [ d; x; y; b; m ] ->
                let D = Integers.order_domain th
                let d', x', y', b', m' = D.Term d, D.Term x, D.Term y, D.Term b, D.Term m
                let H = rel_prime d' m'
                let dx, dy = d' * x', d' * y'
                let A1, A2, R = cong dx b' m', cong dy b' m', cong x' y' m'
                // d·x ≡ d·y is x·d ≡ y·d, which cancels (15.124).
                let cf = lemma th ((H * cong dx dy m') ==> R) [
                            cong_def th dx dy m'                        |> at [left_branch; right_branch]
                            Integers.ring th (dy - dx) ((y' * d') - (x' * d')) |> at [left_branch; right_branch; right_branch]
                            cong_def th (x' * d') (y' * d') m' |> Commute |> at [left_branch; right_branch]
                            Taut (mp (OrderedDomain.in_theory th (glue_detach H (cong (x' * d') (y' * d') m') R)) (cong_cancel th x' y' d' m')) |> apply ]
                let core = lemma th ((H * (A1 * A2)) ==> R) [
                            Ident (cong_sym th dy b' m')                |> at [left_branch; right_branch; right_branch]                  // b ≡ d·y
                            Establish (cong_trans th dx b' dy m')       |> apply
                            Deduce cf                                   |> apply_right ]
                shunt_t th H (A1 * A2) R core
            | _ -> invalidArg "args" "dv_cong_unique")
    /// **(15.127)** `d ⊥ m ⇒ (d·x ≡ b (mod m) ∧ d·y ≡ b (mod m) ⇒ x ≡ y (mod m))` — the solution of
    /// (15.126) is unique mod `m`: Symmetry and Transitivity give `d·x ≡ d·y`, and (15.124) cancels `d`.
    let cong_unique (th: Theory) (d: Term<int>) (x: Term<int>) (y: Term<int>) (b: Term<int>) (m: Term<int>) : Theorem =
        cong_unique_c th [ d; x; y; b; m ]

    (* Module information members *)

    type private IModuleTypeLocator = interface end

    let Type = match typeof<IModuleTypeLocator>.DeclaringType with | NonNull m -> m | _ -> failwith "Failed to locate module type."
