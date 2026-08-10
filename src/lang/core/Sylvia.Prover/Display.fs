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

    /// A one-argument operator that declared a display symbol — via `[<Symbol>]` on the method, or a
    /// `Symbols.BulitIn` entry under its name. Lets a theory's PREFIX operator render structurally
    /// instead of being decompiled back to its F# spelling, without this assembly knowing what the
    /// operator is. (The set complement is `Set.(~-)`, so Unquote decompiles it to `-S`; Gries writes
    /// `~S`, and a substring rewrite cannot fix that without also hitting subtraction.)
    ///
    let private (|SymbolicUnary|_|) (e: Expr) =
        match e with
        | Patterns.Call(None, mi, [x]) ->
            match box mi with
            | SymbolDisplay symbol -> Some(symbol, x)
            | _ -> None
        | _ -> None

    /// The same for an INFIX operator. Both cases exist so the recursion CONTINUES through a
    /// theory's operators instead of stopping at them: a term with no structural case is decompiled
    /// whole by `print_src`, so a nested prefix operator inside one is never reached and reverts to
    /// its F# spelling (`S ∪ -T` rather than `S ∪ ~T`). Rendering the infix operator structurally is
    /// what lets the unary case above apply at depth.
    let private (|SymbolicBinary|_|) (e: Expr) =
        match e with
        | Patterns.Call(None, mi, [l; r]) ->
            match box mi with
            | SymbolDisplay symbol -> Some(symbol, l, r)
            | _ -> None
        | _ -> None

    /// Whether an operand can be written without parentheses: a leaf (variable or nullary constant)
    /// or a prefix application, which binds tighter than any infix operator. Everything else is
    /// bracketed — `~(S ∪ T)` and `(S − T) ⊆ ~T`, never `~S ∪ T`.
    ///
    /// Structural rather than a test on the rendered string, so it does not depend on how the
    /// operand happened to print. Deliberately conservative: these symbols carry no precedence of
    /// their own here, so the only alternative to a parenthesis is an ambiguity.
    let rec private is_tight_operand =
        function
        | Patterns.Var _
        | Patterns.Value _
        | Patterns.ValueWithName _
        | Patterns.NewUnionCase(_, [])
        | Patterns.PropertyGet(None, _, []) -> true
        | SymbolicUnary _ -> true
        | _ -> false

    /// `Symbols.BulitIn` flattened for the textual fallback below, so a THEORY can teach the
    /// renderer its own notation without this assembly knowing anything about that theory. (The set
    /// algebra registers `\u222a`/`\u2229`/`\u2212`/`\u2208`/`\u2286` this way; nothing here mentions sets.)
    ///
    /// Sorted LONGEST KEY FIRST, because these are substring replacements: a short token must not be
    /// allowed to corrupt a longer operator that contains it (` |+| ` inside LinearAlgebra's
    /// ` ||+|| `; registering the surrounding spaces already prevents that particular case, and the
    /// ordering makes the property hold for keys nobody thought about).
    ///
    /// Rebuilt only when the table's size changes. It has to be rechecked rather than read once:
    /// F# runs a file's `do` bindings on first access to that file, so a theory's registrations
    /// land whenever its module is first touched, which can be long after the first render. But
    /// re-flattening a Dictionary per call would put an allocation on the one path already paying
    /// for Unquote decompilation, which is the last path that needs more work.
    let mutable private builtin_subst : (string * string)[] = [||]
    let mutable private builtin_subst_size = -1
    let private builtin_substitutions () =
        if builtin_subst_size <> Symbols.BulitIn.Count then
            builtin_subst <-
                Symbols.BulitIn
                |> Seq.map (fun kv -> kv.Key, kv.Value)
                |> Seq.sortByDescending (fun (k, _) -> k.Length)
                |> Seq.toArray
            builtin_subst_size <- Symbols.BulitIn.Count
        builtin_subst

    /// Decompile a (sub)expression with the propositional symbol map, then with the theory-supplied
    /// one. LAST-RESORT fallback only: Unquote decompilation is extremely expensive (it dominated
    /// whole-proof profiles when every propositional formula went through it \u2014 see
    /// docs/expressions-perf.md). It remains the rendering for terms with no
    /// structural case below, e.g. custom operators like the set-algebra `|+|`/`|?|`.
    let private print_src expr =
        let mutable e = src expr
        for kv in symbolMap
            do if e.Contains kv.Key then e <- e.Replace(kv.Key, kv.Value)
        for (k, v) in builtin_substitutions ()
            do if e.Contains k then e <- e.Replace(k, v)
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

    // `Symbols.BulitIn` is the second ambient input, and unlike the flag it genuinely CHANGES
    // mid-session: a theory's symbol registrations run in its file's `do` bindings, which F#
    // executes on first access to that file, so anything rendered before a theory was first touched
    // was rendered without that theory's notation. Cheap to track here — one `Count` read per
    // top-level render — and without it those earlier strings stay cached and wrong.
    let mutable private rendered_symbols = Symbols.BulitIn.Count

    /// Drop the caches if the display settings changed since they were populated. Run ONCE per
    /// top-level render, not per node: the recursion below goes through `print_formula_memo`, which
    /// skips it. Checking at every node instead cost a measurable 2-3%.
    let private check_display_settings () =
        if rendered_under <> Symbols.TransliterateGreek || rendered_symbols <> Symbols.BulitIn.Count then
            clear_caches ()
            rendered_under <- Symbols.TransliterateGreek
            rendered_symbols <- Symbols.BulitIn.Count

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

        (* Operators that declared a display symbol. Theory-agnostic: this assembly does not know
           which operators exist, only that one said how it wants to be written. Rendering these
           structurally also skips the Unquote decompilation `print_src` would otherwise pay. *)
        | SymbolicUnary(symbol, x) -> sprintf "%s%s" symbol (print_symbolic_operand x)
        | SymbolicBinary(symbol, l, r) ->
            sprintf "%s %s %s" (print_symbolic_operand l) symbol (print_symbolic_operand r)

        (* All other terms *)
        | expr -> print_src expr

    /// An operand of a symbolic operator, bracketed unless it is tight (see `is_tight_operand`).
    and private print_symbolic_operand (e: Expr) : string =
        if is_tight_operand e then print_formula_memo e else "(" + print_formula_memo e + ")"

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