#load "Include.fsx"

// Ordered domains (Gries & Schneider, "A Logical Approach to Discrete Math", ch. 15, §15.1, pp. 307–309).
//
// What this script verifies:
//   (A) the axioms of an ordered domain — (15.30)–(15.33) on `pos`, and the definitions (15.36)–(15.39)
//       of <, >, ≤, ≥ — are recognized, each beside a refusal of a near miss;
//   (B) the order laws are THEOREMS of them: (15.34), (15.40), (15.41a), (15.41d), (15.42), (15.46),
//       and the successor and non-negative-sum facts — at `int` and, the theory being generic, at `int64`;
//   (C) `Integers` IS an ordered domain, and four of its former order axioms — Reflexivity,
//       Transitivity, Translation, "a successor is not ≤" — are now theorems, not axioms. What stays
//       an axiom is what an ordered domain does not give: discreteness, and literal evaluation.
//
// Run:  dotnet fsi examples/proofs/OrderedDomain.fsx

open Sylvia
open FSharp.Quotations
open PropCalculus
open IntegralDomain
open OrderedDomain
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

let d = Integers.integers :> OrderedDomain<int>
let th = d :> Theory
let r = d.Order
let v (n: string) = Expr.Var(Var(n, typeof<int>)) |> Expr.Cast<int>
let a, b, c = v "a", v "b", v "c"
let P = pos_of r
let lt, gt, le, ge = rel r.Lt, rel r.Gt, rel r.Le, rel r.Ge
let ax (p: Prop) = th.AxEquiv p.Expr
let add (x: Expr<int>) (y: Expr<int>) : Expr<int> = <@ %x + %y @>
let mul (x: Expr<int>) (y: Expr<int>) : Expr<int> = <@ %x * %y @>
let sub (x: Expr<int>) (y: Expr<int>) : Expr<int> = <@ %x - %y @>
let neg (x: Expr<int>) : Expr<int> = <@ - %x @>
let zero, one : Expr<int> * Expr<int> = <@ 0 @>, <@ 1 @>

printfn "\n===== (A) The axioms of an ordered domain ====="
ok "(15.30) Addition         pos.a ∧ pos.b ⇒ pos(a + b)"       (ax (((P a) * (P b)) ==> (P (add a b))))
ok "  ... not  pos.a ∧ pos.b ⇒ pos(a − b)"                     (not (ax (((P a) * (P b)) ==> (P (sub a b)))))
ok "(15.31) Multiplication   pos.a ∧ pos.b ⇒ pos(a·b)"         (ax (((P a) * (P b)) ==> (P (mul a b))))
ok "  ... not  pos.a ∧ pos.b ⇒ pos(a·c)"                       (not (ax (((P a) * (P b)) ==> (P (mul a c)))))
ok "(15.32)                  ¬pos.0"                            (ax (!!(P zero)))
ok "  ... not  ¬pos.1"                                          (not (ax (!!(P one))))
ok "(15.33)                  b ≠ 0 ⇒ (pos.b = ¬pos(−b))"        (ax ((!!(eq_t b zero)) ==> ((P b) == !!(P (neg b)))))
ok "  ... not without b ≠ 0"                                    (not (ax ((P b) == !!(P (neg b)))))
ok "(15.36) Less             a < b = pos(b − a)"                (ax ((lt a b) == (P (sub b a))))
ok "  ... not  a < b = pos(a − b)"                              (not (ax ((lt a b) == (P (sub a b)))))
ok "(15.37) Greater          a > b = pos(a − b)"                (ax ((gt a b) == (P (sub a b))))
ok "(15.38) At most          a ≤ b = a < b ∨ a = b"             (ax ((le a b) == ((lt a b) + (eq_t a b))))
ok "  ... not  a ≤ b = a < b ∨ b = c"                           (not (ax ((le a b) == ((lt a b) + (eq_t b c)))))
ok "(15.39) At least         a ≥ b = a > b ∨ a = b"             (ax ((ge a b) == ((gt a b) + (eq_t a b))))

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
let ab = add a b
thm  "(15.41d) at compound terms"                                 (fun () -> le_trans d ab (mul a b) (neg c))
rule "(15.42)  at compound terms"                                 (fun () -> le_translate d (mul a b) ab (neg c))
// Refusals: the theorems prove exactly their statements.
ok "(15.41d) does not prove a ≤ b ∧ c ≤ b ⇒ a ≤ c"
   (not (try (theorem th (((le a b) * (le c b)) ==> (le a c)) [ Taut (le_trans d a b c) |> apply ]).Proof.Complete with _ -> false))
ok "(15.42) does not translate by different amounts"
   (not (try (theorem th ((lt a b) == (lt (add a c) (add b a))) [ lt_translate d a b c |> at_left ]).Proof.Complete with _ -> false))

// The theory is GENERIC: the same theorems at a second carrier.
let ops64 : DomainOps<int64> =
    { Name = "Int64"; Add = <@ (+) @>; Mul = <@ (*) @>; Zero = <@ 0L @>; One = <@ 1L @>; Neg = <@ (~-) @>; Sub = <@ (-) @> }
let ord64 : OrderOps<int64> = { Pos = <@ pos<int64> @>; Lt = <@ (<) @>; Gt = <@ (>) @>; Le = <@ (<=) @>; Ge = <@ (>=) @> }
let d64 = OrderedDomain<int64>(ops64, ord64)
let p64, q64, s64 = Expr.Var(Var("p", typeof<int64>)) |> Expr.Cast<int64>, Expr.Var(Var("q", typeof<int64>)) |> Expr.Cast<int64>, Expr.Var(Var("s", typeof<int64>)) |> Expr.Cast<int64>
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
ok "Discreteness is still an axiom — an ordered domain does not give it (ℚ is one)"
   (ind.AxEquiv ((leq n (m + 1)) == ((leq n m) + ((n :> Term<int>) == ((m + 1) :> Term<int>)))).Expr)

printfn "\n%s (%d failure(s))" (if failures = 0 then "ALL PASS" else "FAILURES") failures
if failures > 0 then exit 1
