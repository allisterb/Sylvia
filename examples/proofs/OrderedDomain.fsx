#load "Include.fsx"

// Ordered domains (Gries & Schneider, "A Logical Approach to Discrete Math", ch. 15, §15.1, pp. 307–309).
//
// What this script verifies:
//   (A) the axioms of an ordered domain — (15.30)–(15.33) on `pos`, and the definitions (15.36)–(15.39)
//       of <, >, ≤, ≥ — are recognized, each beside a refusal of a near miss;
//   (B) the order laws are THEOREMS of them: (15.34), (15.40), (15.41a), (15.41d), (15.42), (15.46),
//       and the successor and non-negative-sum facts — at `int` and, the theory being generic, at `int64`;
//   (C) `Integers` IS an ordered domain, and four of its former order axioms — Reflexivity,
//       Transitivity, Translation, "a successor is not ≤" — are now theorems, not axioms. The fifth,
//       discreteness, which no ordered domain gives, is a theorem of `Induction` (from 12.8). What
//       stays an axiom is literal evaluation;
//   (D) the rest of §15.1 used below: (15.45) antisymmetry, totality, `a < a` false, and (15.47)
//       indirect equality as a derived rule;
//   (E) §15.2 minimum ↓ and maximum ↑ from Definition (15.53) alone: (15.54)–(15.60);
//   (F) §15.3 absolute value from (15.71) abs.x = x↑−x: (15.72), (15.74), and the triangle
//       inequality (15.73);
//   (G) the rest of §15.1–15.3 — (15.35), (15.41b/c), (15.43) Monotonicity of ·, (15.44) Trichotomy,
//       (15.45) as an equivalence, (15.61)/(15.62) ↓ and ↑ under ·, (15.75) and (15.76).
//
// Run:  dotnet fsi examples/proofs/OrderedDomain.fsx

open Sylvia
open FSharp.Quotations
open PropCalculus
open IntegralDomain
open OrderedDomain
open DomainTerms
open Induction

fsi.PrintWidth <- 300
Proof.LogLevel <- 0

let mutable failures = 0
let ok label cond =
    if not cond then failures <- failures + 1
    printfn "  %s  %s" (if cond then "✓" else "✗") label
let why what (e: exn) = printfn "     [%s] %s" what (e.Message.Split('\n').[0]); false
let thm (label: string) (f: unit -> Theorem) = ok label (try (f ()).Proof.Complete with e -> why label e)
let rule (label: string) (f: unit -> Rule) =
    ok label (try (match f () with Derive(_, p, _) -> p.Complete | _ -> false) with e -> why label e)

// Terms are written in the domain's own DSL (`DomainTerm.fs`): `+ * -` are the domain's operators,
// `== !=` are = and ≠, `<. <=. >. >=.` are < ≤ > ≥, and `pos_of`, `min_of`, `max_of`, `abs_of` the rest.
let d = Integers.integers :> OrderedDomain<int>
let th = d :> Theory
// `d.Var` makes a variable that is also a term. Bound as a `DomainTerm` so unary minus applies
// (F#'s `~-` is `^a -> ^a`, and a variable's negation is not a variable).
let var (n: string) : DomainTerm<int> = upcast d.Var n
let a, b, c = var "a", var "b", var "c"
let zero, one = d.Zero, d.One
let ax (p: Prop) = th.AxEquiv p.Expr

printfn "\n===== (A) The axioms of an ordered domain ====="
ok "(15.30) Addition         pos.a ∧ pos.b ⇒ pos(a + b)"       (ax (((pos_of a) * (pos_of b)) ==> (pos_of (a + b))))
ok "  ... not  pos.a ∧ pos.b ⇒ pos(a − b)"                     (not (ax (((pos_of a) * (pos_of b)) ==> (pos_of (a - b)))))
ok "(15.31) Multiplication   pos.a ∧ pos.b ⇒ pos(a·b)"         (ax (((pos_of a) * (pos_of b)) ==> (pos_of (a * b))))
ok "  ... not  pos.a ∧ pos.b ⇒ pos(a·c)"                       (not (ax (((pos_of a) * (pos_of b)) ==> (pos_of (a * c)))))
ok "(15.32)                  ¬pos.0"                            (ax (!!(pos_of zero)))
ok "  ... not  ¬pos.1"                                          (not (ax (!!(pos_of one))))
ok "(15.33)                  b ≠ 0 ⇒ (pos.b = ¬pos(−b))"        (ax ((b != zero) ==> ((pos_of b) == !!(pos_of (-b)))))
ok "  ... not without b ≠ 0"                                    (not (ax ((pos_of b) == !!(pos_of (-b)))))
ok "(15.36) Less             a < b = pos(b − a)"                (ax ((a <. b) == (pos_of (b - a))))
ok "  ... not  a < b = pos(a − b)"                              (not (ax ((a <. b) == (pos_of (a - b)))))
ok "(15.37) Greater          a > b = pos(a − b)"                (ax ((a >. b) == (pos_of (a - b))))
ok "(15.38) At most          a ≤ b = a < b ∨ a = b"             (ax ((a <=. b) == ((a <. b) + (a == b))))
ok "  ... not  a ≤ b = a < b ∨ b = c"                           (not (ax ((a <=. b) == ((a <. b) + (b == c)))))
ok "(15.39) At least         a ≥ b = a > b ∨ a = b"             (ax ((a >=. b) == ((a >. b) + (a == b))))

printfn "\n===== (B) The order laws are theorems ====="
thm  "(15.34)  b ≠ 0 ⇒ pos(b·b)"                                 (fun () -> sq_pos d b)
thm  "         1 ≠ 0 ⇒ pos.1        (Gries' corollary: 1 = 1·1)" (fun () -> one_pos d)
thm  "         1 ≠ 0 ⇒ (pos(−1) = false)"                         (fun () -> neg_one_not_pos d)
rule "(15.40)  pos.b = 0 < b"                                     (fun () -> pos_lt d b)
thm  "(15.41a) a < b ∧ b < c ⇒ a < c   (Gries' proof)"            (fun () -> lt_trans d a b c)
thm  "(15.41d) a ≤ b ∧ b ≤ c ⇒ a ≤ c   (four cases)"              (fun () -> le_trans d a b c)
rule "(15.42)  (a < b) = (a + c < b + c)"                         (fun () -> lt_translate d a b c)
rule "         (a ≤ b) = (a + c ≤ b + c)"                         (fun () -> le_translate d a b c)
rule "(15.46)  (a ≤ a) = true"                                    (fun () -> le_refl d a)
thm  "         1 ≠ 0 ⇒ ((a + 1 ≤ a) = false)"                     (fun () -> succ_not_le d a)
thm  "         0 ≤ a ∧ 0 ≤ b ⇒ 0 ≤ a + b"                         (fun () -> nonneg_add d a b)
thm  "         (a = b) ⇒ (b = a)       (3.84a + reflexivity)"     (fun () -> OrderedDomain.eq_sym th a b)
rule "arith:   (b − a) + (c − b) = c − a   (Exercise 15.22)"      (fun () -> sub_chain d a b c)
// A compound argument, to catch a mis-targeted rewrite inside a derivation.
let ab = a + b
thm  "(15.41d) at compound terms"                                 (fun () -> le_trans d ab (a * b) (-c))
rule "(15.42)  at compound terms"                                 (fun () -> le_translate d (a * b) ab (-c))
// Refusals: the theorems prove exactly their statements.
ok "(15.41d) does not prove a ≤ b ∧ c ≤ b ⇒ a ≤ c"
   (not (try (theorem th (((a <=. b) * (c <=. b)) ==> (a <=. c)) [ Taut (le_trans d a b c) |> apply ]).Proof.Complete with _ -> false))
ok "(15.42) does not translate by different amounts"
   (not (try (theorem th ((a <. b) == (a + c <. b + a)) [ lt_translate d a b c |> at_left ]).Proof.Complete with _ -> false))
// The DSL builds EXACTLY the trees raw quotations do, so the axiom patterns see no difference.
ok "DSL terms build the same trees as quotations"
   (sequal (a + -b * c).Expr <@ %a.Expr + -(%b.Expr) * %c.Expr @> && sequal (a <=. b).Expr <@ %a.Expr <= %b.Expr @>)
ok "terms of two different domains do not mix"
   (try (a + (IntegralDomain.IntegralDomain<int>({ d.Ops with Name = "Other" })).One) |> ignore; false with _ -> true)

// The theory is GENERIC: the same theorems at a second carrier.
let ops64 : DomainOps<int64> =
    { Name = "Int64"; Add = <@ (+) @>; Mul = <@ (*) @>; Zero = <@ 0L @>; One = <@ 1L @>; Neg = <@ (~-) @>; Sub = <@ (-) @> }
let ord64 : OrderOps<int64> = { Pos = <@ pos<int64> @>; Lt = <@ (<) @>; Gt = <@ (>) @>; Le = <@ (<=) @>; Ge = <@ (>=) @>; Min = <@ min @>; Max = <@ max @>; Abs = <@ abs @> }
let d64 = OrderedDomain<int64>(ops64, ord64)
let p64, q64, s64 = d64.Var "p", d64.Var "q", d64.Var "s"
thm  "(15.41d) at int64"                                          (fun () -> le_trans d64 p64 q64 s64)
thm  "(15.34)  at int64"                                          (fun () -> sq_pos d64 p64)
thm  "succ_not_le at int64"                                       (fun () -> succ_not_le d64 p64)

printfn "\n===== (C) Integers is an ordered domain; four order axioms are now theorems ====="
let n, m, k = IntVar "n", IntVar "m", IntVar "k"
let ind = Induction.induction :> Theory
ok "Integers is an OrderedDomain<int>"                            ((Integers.integers :> obj) :? OrderedDomain<int>)
ok "... and so is every Induction theory"                         ((ind :> obj) :? OrderedDomain<int>)
let gone = [ "Reflexivity",     (leq n n) == T
             "Transitivity",    ((leq n m) * (leq m k)) ==> (leq n k)
             "Translation",     (leq n m) == (leq (n + 1) (m + 1))
             "Successor",       (leq (n + 1) n) == F ]
for (name, stmt) in gone do
    ok (sprintf "%-13s is no longer an axiom of Integers" name)   (not (ind.AxEquiv stmt.Expr))
thm  "Reflexivity   — a theorem"   (fun () -> theorem ind ((leq n n) == T) [ Integers.le_refl ind n |> apply ])
thm  "Transitivity  — a theorem"   (fun () -> Integers.le_trans ind n m k)
thm  "Translation   — a theorem"   (fun () -> theorem ind ((leq n m) == (leq (n + 1) (m + 1))) [ Integers.le_translate ind n m |> at_left ])
thm  "Successor     — a theorem"   (fun () -> theorem ind ((leq (n + 1) n) == F) [ Integers.succ_not_le ind n |> at_left ])
thm  "1 ≠ 0, by literal evaluation" (fun () -> Integers.one_ne_zero ind)
ok "(1 = 0) = false is an axiom (literal evaluation), (1 = 1) = false is not"
   (ind.AxEquiv ((IntTerm 1 :> Term<int>) == (IntTerm 0 :> Term<int>) == F).Expr
    && not (ind.AxEquiv ((IntTerm 1 :> Term<int>) == (IntTerm 1 :> Term<int>) == F).Expr))
// Discreteness: no ordered-domain axiom gives it (ℚ is one), and it is no axiom at all any more —
// `Induction` derives it from (12.8). See Induction.fsx (G).
ok "Discreteness is not an axiom either — Induction derives it from (12.8)"
   (not (ind.IsAxiom ((leq n (m + 1)) == ((leq n m) + ((n :> Term<int>) == ((m + 1) :> Term<int>))))))
thm  "Discreteness — a theorem of Induction"
     (fun () -> theorem ind ((leq n (m + 1)) == ((leq n m) + ((n :> Term<int>) == ((m + 1) :> Term<int>)))) [ discrete ind n m |> at_left ])

printfn "\n===== (D) The rest of §15.1: irreflexivity, antisymmetry, totality, indirect equality ====="
rule "         (a < a) = false"                                   (fun () -> lt_irrefl d a)
rule "         (a < b ∧ b < a) = false"                           (fun () -> lt_asym d a b)
thm  "(15.45)  a ≤ b ∧ b ≤ a ⇒ a = b   Antisymmetry"            (fun () -> le_antisym d a b)
thm  "(15.44)  a ≤ b ∨ b ≤ a          Totality (of Trichotomy)" (fun () -> le_total d a b)
rule "         (a ≥ b) = (b ≤ a)"                                 (fun () -> ge_le d a b)
rule "         (a = b) = (b = a)"                                 (fun () -> eq_comm th a b)
// REGRESSION: le_trans's case `b = c ∧ a < b` rewrote the first `c` in `a < c` — inside `a` when `a`
// contains `c`. It failed at a := −c (found by abs(abs x) = abs x, below).
thm  "(15.41d) at a := −c, b := 0   (a contains c)"           (fun () -> le_trans d (-c) zero c)
// (15.47) as a derived rule: prove the body at a metavariable z, get a = b.
rule "(15.47)  indirect equality yields x↓y = y↓x"               (fun () -> min_sym d a b)
ok "(15.47)  refuses a body that proves something else"
   (try below_eq d a b (fun z -> ident th ((z <=. a) == (z <=. a)) []) |> ignore; false with _ -> true)

printfn "\n===== (E) §15.2 Minimum ↓ and maximum ↑ — from Definition (15.53) only ====="
ok "(15.53)  z ≤ x↓y = z ≤ x ∧ z ≤ y   is an axiom"             (ax ((c <=. min_of a b) == ((c <=. a) * (c <=. b))))
ok "  ... not  z ≤ x↓y = z ≤ x ∨ z ≤ y"                          (not (ax ((c <=. min_of a b) == ((c <=. a) + (c <=. b)))))
ok "(15.53)  z ≥ x↑y = z ≥ x ∧ z ≥ y   is an axiom"             (ax ((c >=. max_of a b) == ((c >=. a) * (c >=. b))))
ok "  ... not  z ≤ x↑y = z ≤ x ∧ z ≤ y"                          (not (ax ((c <=. max_of a b) == ((c <=. a) * (c <=. b)))))
rule "         (x↑y ≤ z) = (x ≤ z ∧ y ≤ z)"                       (fun () -> max_le d a b c)
rule "(15.54)  x↓y = y↓x"                                         (fun () -> min_sym d a b)
rule "(15.54)  x↑y = y↑x"                                         (fun () -> max_sym d a b)
rule "(15.55)  (x↓y)↓z = x↓(y↓z)"                                 (fun () -> min_assoc d a b c)
rule "(15.55)  (x↑y)↑z = x↑(y↑z)"                                 (fun () -> max_assoc d a b c)
rule "(15.56)  x↓x = x"                                           (fun () -> min_idem d a)
rule "(15.56)  x↑x = x"                                           (fun () -> max_idem d a)
thm  "(15.57)  x↓y ≤ x ∧ x↓y ≤ y"                                 (fun () -> min_le d a b)
thm  "(15.57)  x ≤ x↑y ∧ y ≤ x↑y"                                 (fun () -> max_ge d a b)
rule "(15.58)  (x ≤ y) = (x↓y = x)"                               (fun () -> min_eq d a b)
rule "(15.58)  (y ≤ x) = (x↑y = x)"                               (fun () -> max_eq d a b)
thm  "(15.59)  x↓y = x ∨ x↓y = y"                                 (fun () -> min_cases d a b)
thm  "(15.59)  x↑y = x ∨ x↑y = y"                                 (fun () -> max_cases d a b)
rule "(15.60)  c + (x↓y) = (c + x)↓(c + y)"                       (fun () -> min_add d c a b)
rule "(15.60)  c + (x↑y) = (c + x)↑(c + y)"                       (fun () -> max_add d c a b)
rule "(15.54)  at compound terms  (a+b)↓(−c) = (−c)↓(a+b)"        (fun () -> min_sym d (a + b) (-c))
rule "(15.58)  at compound terms, x inside y"                     (fun () -> min_eq d a (a + b))
ok "(15.58)  does not prove (x ≤ y) = (x↓y = y)"
   (not (try (theorem th ((a <=. b) == (min_of a b == b)) [ min_eq d a b |> at_left ]).Proof.Complete with _ -> false))

printfn "\n===== (F) §15.3 Absolute value — (15.71) abs.x = x↑−x ====="
ok "(15.71)  abs.x = x↑−x   is an axiom"                          (ax (abs_of a == max_of a (-a)))
ok "  ... not  abs.x = x↑x"                                       (not (ax (abs_of a == max_of a a)))
rule "(15.72)  abs.x = abs(−x)"                                   (fun () -> abs_neg d a)
thm  "         x ≤ abs.x ∧ −x ≤ abs.x"                            (fun () -> abs_ge d a)
thm  "         0 ≤ abs.x"                                         (fun () -> abs_nonneg d a)
rule "(15.74)  abs(abs.x) = abs.x"                                (fun () -> abs_abs d a)
thm  "         a ≤ b ∧ c ≤ e ⇒ a + c ≤ b + e"                     (fun () -> le_add d a b c (-a))
thm  "(15.73)  abs(x + y) ≤ abs.x + abs.y   Triangle inequality"  (fun () -> triangle d a b)
// And at the second carrier.
thm  "(15.73)  at int64"                                          (fun () -> triangle d64 p64 q64)
rule "(15.60)  at int64"                                          (fun () -> min_add d64 s64 p64 q64)

printfn "\n===== (G) The rest of §15.1 — (15.35), (15.41b/c), (15.43)–(15.45) — and ↓/↑ with · and − ====="
// Several of these are assembled by modus ponens from propositional glue rather than proved as a
// stated `lemma`, so check the STATEMENT each one proves, not only that its proof is complete.
let same (s: Expr) (p: Prop) = sequal (expand s) (expand p.Expr)
let proves (label: string) (expected: Prop) (f: unit -> Theorem) =
    ok label (try (let t = f () in t.Proof.Complete && same t.Stmt expected) with e -> why label e)
let provesR (label: string) (expected: Prop) (f: unit -> Rule) =
    ok label (try (match f () with Derive(_, p, _) -> p.Complete && same p.Stmt expected | _ -> false) with e -> why label e)
let X, Y, Z = (a <. b), (a == b), (a >. b)
provesR "         (a > b) = (b < a)"                            ((a >. b) == (b <. a))                    (fun () -> gt_lt d a b)
proves  "(15.41b) a ≤ b ∧ b < c ⇒ a < c"                        (((a <=. b) * (b <. c)) ==> (a <. c))     (fun () -> le_lt_trans d a b c)
proves  "(15.41c) a < b ∧ b ≤ c ⇒ a < c"                        (((a <. b) * (b <=. c)) ==> (a <. c))     (fun () -> lt_le_trans d a b c)
provesR "         ¬(a < b) = (b ≤ a)"                           (!!(a <. b) == (b <=. a))                 (fun () -> not_lt d a b)
proves  "(15.44)  (a<b ≡ a=b ≡ a>b) ∧ ¬(a<b ∧ a=b ∧ a>b)  Trichotomy"
                                                                (((X == Y) == Z) * !!((X * Y) * Z))       (fun () -> trichotomy d a b)
provesR "(15.45)  (a ≤ b ∧ b ≤ a) = (a = b)   Antisymmetry"     ((((a <=. b) * (b <=. a))) == (a == b))   (fun () -> le_antisym_eq d a b)
proves  "         0 < c ∧ a < b ⇒ a·c < b·c"                    (((zero <. c) * (a <. b)) ==> (a * c <. b * c))   (fun () -> lt_mul_pos d a b c)
proves  "         0 < c ∧ a·c < b·c ⇒ a < b"                    (((zero <. c) * (a * c <. b * c)) ==> (a <. b))   (fun () -> lt_mul_cancel d a b c)
proves  "(15.43)  0 < c ⇒ (a < b ≡ a·c < b·c)   Monotonicity"   ((zero <. c) ==> ((a <. b) == (a * c <. b * c)))  (fun () -> lt_mul d a b c)
proves  "(15.35)  pos.a ⇒ (pos.b ≡ pos(a·b))"                   ((pos_of a) ==> ((pos_of b) == (pos_of (a * b))))  (fun () -> pos_mul d a b)
proves  "         0 ≤ c ∧ a ≤ b ⇒ c·a ≤ c·b"                    (((zero <=. c) * (a <=. b)) ==> (c * a <=. c * b)) (fun () -> le_mul_nonneg d c a b)
provesR "         (a ≤ b) = (−b ≤ −a)"                          ((a <=. b) == (-b <=. -a))                (fun () -> le_neg d a b)
proves  "(15.61)  0 ≤ c ⇒ c·(x↓y) = (c·x)↓(c·y)"                ((zero <=. c) ==> (c * min_of a b == min_of (c * a) (c * b)))  (fun () -> mul_min d c a b)
proves  "(15.61)  0 ≤ c ⇒ c·(x↑y) = (c·x)↑(c·y)"                ((zero <=. c) ==> (c * max_of a b == max_of (c * a) (c * b)))  (fun () -> mul_max d c a b)
proves  "         c ≤ 0 ∧ a ≤ b ⇒ c·b ≤ c·a"                    (((c <=. zero) * (a <=. b)) ==> (c * b <=. c * a)) (fun () -> le_mul_nonpos d c a b)
proves  "(15.62)  c ≤ 0 ⇒ c·(x↑y) = (c·x)↓(c·y)"                ((c <=. zero) ==> (c * max_of a b == min_of (c * a) (c * b)))  (fun () -> mul_max_nonpos d c a b)
proves  "(15.62)  c ≤ 0 ⇒ c·(x↓y) = (c·x)↑(c·y)"                ((c <=. zero) ==> (c * min_of a b == max_of (c * a) (c * b)))  (fun () -> mul_min_nonpos d c a b)
provesR "         −(x↓y) = (−x)↑(−y)   ((15.62) at c := −1)"    (-(min_of a b) == max_of (-a) (-b))       (fun () -> neg_min d a b)
provesR "         −(x↑y) = (−x)↓(−y)"                           (-(max_of a b) == min_of (-a) (-b))       (fun () -> neg_max d a b)
proves  "         0 ≤ x ⇒ abs.x = x"                            ((zero <=. a) ==> (abs_of a == a))        (fun () -> abs_of_nonneg d a)
proves  "         x ≤ 0 ⇒ abs.x = −x"                           ((a <=. zero) ==> (abs_of a == -a))       (fun () -> abs_of_nonpos d a)
provesR "(15.75)  abs(x·y) = abs.x · abs.y"                     (abs_of (a * b) == abs_of a * abs_of b)   (fun () -> abs_mul d a b)
proves  "(15.76)  −(abs.x + abs.y) ≤ x + y ≤ abs.x + abs.y"
                                                                ((-(abs_of a + abs_of b) <=. a + b) * (a + b <=. abs_of a + abs_of b))  (fun () -> abs_bounds d a b)
// Compound arguments where one term contains another — the MIS-TARGETED SUBSTITUTION class.
thm  "(15.41b) at a := −c, b := 0   (a contains c)"           (fun () -> le_lt_trans d (-c) zero c)
thm  "(15.41c) at a := −c, b := 0   (a contains c)"           (fun () -> lt_le_trans d (-c) zero c)
rule "         ¬(a < b) = (b ≤ a)  at a := a + b, b := a"     (fun () -> not_lt d (a + b) a)
thm  "(15.44)  at a := a, b := a + b"                         (fun () -> trichotomy d a (a + b))
thm  "(15.44)  at a := a + b, b := b"                         (fun () -> trichotomy d (a + b) b)
thm  "(15.43)  at a := a + c   (a contains the multiplier)"   (fun () -> lt_mul d (a + c) b c)
thm  "         0 ≤ c ∧ a ≤ b ⇒ c·a ≤ c·b  at a := c + a"      (fun () -> le_mul_nonneg d c (c + a) b)
thm  "(15.61)  at x := c·a   (x contains c)"                  (fun () -> mul_min d c (c * a) b)
thm  "(15.62)  at x := c·a   (x contains c)"                  (fun () -> mul_max_nonpos d c (c * a) b)
rule "         −(x↓y) = (−x)↑(−y)  at x := a + b, y := a"     (fun () -> neg_min d (a + b) a)
rule "(15.75)  at x := a + b, y := a   (y inside x)"          (fun () -> abs_mul d (a + b) a)
thm  "(15.76)  at x := −a, y := a"                            (fun () -> abs_bounds d (-a) a)
// Refusals: no hypothesis is dropped.
ok "(15.43) does not prove a < b ≡ a·c < b·c without 0 < c"
   (not (try (theorem th ((a <. b) == (a * c <. b * c)) [ Taut (lt_mul d a b c) |> apply ]).Proof.Complete with _ -> false))
ok "(15.61) does not prove c·(x↓y) = (c·x)↓(c·y) without 0 ≤ c"
   (not (try (theorem th (c * min_of a b == min_of (c * a) (c * b)) [ Taut (mul_min d c a b) |> apply ]).Proof.Complete with _ -> false))
// And at the second carrier.
proves  "(15.43)  at int64"  ((d64.Zero <. s64) ==> ((p64 <. q64) == (p64 * s64 <. q64 * s64)))  (fun () -> lt_mul d64 p64 q64 s64)
thm  "(15.44)  at int64"                                      (fun () -> trichotomy d64 p64 q64)
thm  "(15.61)  at int64"                                      (fun () -> mul_min d64 s64 p64 q64)
rule "(15.75)  at int64"                                      (fun () -> abs_mul d64 p64 q64)
ok "(15.62) does not prove c·(x↑y) = (c·x)↓(c·y) from 0 ≤ c"
   (let wrong = (zero <=. c) ==> (c * max_of a b == min_of (c * a) (c * b))
    not (try (theorem th wrong [ Taut (mul_max_nonpos d c a b) |> apply ]).Proof.Complete with _ -> false))

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
