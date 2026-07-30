namespace Sylvia

open System
open System.Reflection

open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open FSharp.Reflection

open Formula
open Patterns
    
module Display = 
   
    let private symbolMap = Map [
            "not", "¬"
            "&&", "∧"
            "||", "∨"
            "===>", "⇒"
            // The truth constants are named "True"/"False" internally; show them compactly
            // in the decompiled-expression fallback below.
            "True", "T"
            "False", "F"
        ]
    
    let (|SymbolDisplay|_|):obj -> string option = 
        function
        | :? MethodInfo as info when (Seq.length (info.GetCustomAttributes(typeof<SymbolAttribute>, true))) > 0 ->
            let a =  info.GetCustomAttributes(typeof<SymbolAttribute>, true) in
            let u = (a.[0] :?> SymbolAttribute) in u.Symbol |> Some 
        | :? MethodInfo as info when Symbols.BulitIn.ContainsKey info.Name -> Symbols.BulitIn.[info.Name] |> Some
        | :? PropertyInfo as info when Symbols.BulitIn.ContainsKey info.Name -> Symbols.BulitIn.[info.Name] |> Some
        | :? UnionCaseInfo as info when Symbols.BulitIn.ContainsKey info.Name -> Symbols.BulitIn.[info.Name] |> Some
        | :? Type as t ->
            let a = t.GetCustomAttributes(typeof<SymbolAttribute>, true) in
            if a.Length = 0 then None else let u = (a.[0] :?> SymbolAttribute) in u.Symbol |> Some 
        | :? string as s when Symbols.TransliterateGreek && Symbols.GreekUnicode.ContainsKey s -> Symbols.GreekUnicode.[s] |> Some
        | :? string as s -> s |> Some
        | _ -> None

    let (|VarDisplay|_|):obj -> string option =
        function
        | :? Var as v -> v.Name |> Some
        | :? (Var list) as vars -> vars.Tail |> List.fold (fun s v -> sprintf "%s,%s" s v.Name) vars.Head.Name |> Some
        | _ -> None

    /// Decompile a (sub)expression with the propositional symbol map. LAST-RESORT
    /// fallback only: Unquote decompilation is extremely expensive (it dominated
    /// whole-proof profiles when every propositional formula went through it \u2014 see
    /// docs/expressions-perf.md). It remains the rendering for terms with no
    /// structural case below, e.g. custom operators like the set-algebra `|+|`/`|?|`.
    let private print_src expr =
        let mutable e = src expr
        for kv in symbolMap
            do if e.Contains kv.Key then e <- e.Replace(kv.Key, kv.Value)
        e

    // Rendering caches, keyed by expression REFERENCE.
    //
    // Caching here pays off for the same reason the `Theory.AxEquiv` cache does: top-level
    // statements are all distinct, so a cache consulted only at the entry point would never hit —
    // but the recursion descends into shared subterms, and in a SAT reconstruction every statement
    // contains the SAME clause conjunction `A`. One hit on `A` skips decompiling hundreds of nodes
    // and building the string for all of them. Measured on the pigeonhole payload: -48% wall clock
    // and -68% allocations in steady state.
    //
    // Weak keys, so an entry dies with its expression. The values are strings proportional to the
    // subtree they render, so fully printing one large term costs O(size²) across the cache — and
    // because `Memo` retains theorems strongly, cached strings live as long as its entries do. Call
    // `clear_caches` to release them.
    let private formula_cache = System.Runtime.CompilerServices.ConditionalWeakTable<Expr, string>()


    /// Drop everything the renderers have memoized. Safe at any time — the caches only ever hold
    /// values they would recompute identically — so this is also the way to release their memory.
    let clear_caches () =
        formula_cache.Clear()


    // The one ambient input to rendering is `Symbols.TransliterateGreek`, which decides whether
    // Greek names render as Unicode. Strings computed under one setting are wrong under the other.
    //
    // `Symbols` lives in Sylvia.Expressions, which compiles BEFORE this assembly and so cannot name
    // these caches; and `Symbols` is a module, so its `let mutable` cannot carry a setter to hook.
    // Rather than change that value into a function and update its six read sites, the caches check
    // the flag themselves: a stale epoch clears them on the next render. That cannot be bypassed by
    // assigning the flag directly, which a registered-callback setter could be.
    //
    // Note `Symbolic`'s own renderers read the same flag, so `print_src` below is affected too —
    // which is exactly why the check guards the cache rather than just the structural cases.
    let mutable private rendered_under = Symbols.TransliterateGreek

    /// Drop the caches if the display settings changed since they were populated. Run ONCE per
    /// top-level render, not per node: the recursion below goes through `print_formula_memo`, which
    /// skips it. Checking at every node instead cost a measurable 2-3%.
    let private check_display_settings () =
        if rendered_under <> Symbols.TransliterateGreek then
            clear_caches ()
            rendered_under <- Symbols.TransliterateGreek

    let rec private print_formula_memo (e: Expr) : string =
        match formula_cache.TryGetValue e with
        | true, s -> s
        | _ ->
            let s = print_formula_uncached e
            formula_cache.GetValue(
                e, System.Runtime.CompilerServices.ConditionalWeakTable<Expr, string>.CreateValueCallback(fun _ -> s))
            |> ignore
            s

    and private print_formula_uncached =
        function
        (* Primitive terms *)
        // The truth constants are named "True"/"False" internally; display them compactly.
        | True -> "T"
        | False -> "F"
        | Const(NonNull(SymbolDisplay symbol)) -> symbol
        | Var(VarDisplay v) -> v
        | Atom a -> sprinte a
        | Index(l, r) -> sprintf "here"

        (* Quantifier terms *)
        | ForAll(_, VarDisplay v, Bool true, body) -> sprintf "(\u2200 %s |: %s)" v (print_formula_memo body)
        | ForAll(_, VarDisplay v, range, body) -> sprintf "(\u2200 %s | %s : %s)" v (print_formula_memo range) (print_formula_memo body)
        | Exists(_, VarDisplay v, Bool true, body) -> sprintf "(\u2203 %s | %s)" v (print_formula_memo body)
        | Exists(_, VarDisplay v, range, body) -> sprintf "(\u2203 %s | %s : %s)" v (print_formula_memo range) (print_formula_memo body)
        | SumTerm(_, SymbolDisplay symbol, VarDisplay bound, range, body)
        | ProductTerm(_, SymbolDisplay symbol, VarDisplay bound, range, body) -> sprintf "%s %s %s" symbol (bound) (print_formula_memo body)

        (* Boolean connectives: recurse structurally \u2014 propositional formulas bottom out
           at the Var/Const/True/False cases above with NO decompilation. *)
        | Not e -> sprintf "\u00ac%s" (print_atom_memo e)
        | Equals(l, r) -> sprintf "%s = %s" (print_atom_memo l) (print_atom_memo r)
        | NotEquals(l, r) -> sprintf "%s \u2260 %s" (print_atom_memo l) (print_atom_memo r)
        | Implies(l, r) -> sprintf "%s \u21d2 %s" (print_atom_memo l) (print_atom_memo r)
        | Conseq(l, r) -> sprintf "%s \u21d0 %s" (print_atom_memo l) (print_atom_memo r)
        | And(l, r) -> sprintf "%s \u2227 %s" (print_atom_memo l) (print_atom_memo r)
        | Or(l, r) -> sprintf "%s \u2228 %s" (print_atom_memo l) (print_atom_memo r)

        (* All other terms *)
        | expr -> print_src expr

    /// Print an operand, parenthesizing it when it is itself a binary boolean connective so
    /// the nesting is unambiguous. Quantifiers and quantifier-free operands already carry
    /// their own delimiters, so they are printed as-is.
    ///
    /// NOT given a cache of its own, deliberately — measured. Once `print_formula` is memoized this
    /// is one pattern match and one concatenation, and a `ConditionalWeakTable` probe costs more
    /// than that: adding an `atom_cache` here moved the pigeonhole payload from 262 ms to 295 ms.
    and private print_atom_memo (expr: Expr) : string =
        match expr with
        | Equals _ | NotEquals _ | Implies _ | Conseq _ | And _ | Or _ -> "(" + print_formula_memo expr + ")"
        | _ -> print_formula_memo expr

    /// Render a formula. Checks the display settings once, then renders through the memo.
    let print_formula (e: Expr) : string =
        check_display_settings ()
        print_formula_memo e

    /// Render an operand (see `print_atom_memo`).
    let print_atom (e: Expr) : string =
        check_display_settings ()
        print_atom_memo e