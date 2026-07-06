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

    /// Decompile a quantifier-free (sub)expression with the propositional symbol map.
    /// This is the long-standing rendering for pure propositional formulas; it is kept
    /// intact so propositional output is unchanged.
    let private print_src expr =
        let mutable e = src expr
        for kv in symbolMap
            do if e.Contains kv.Key then e <- e.Replace(kv.Key, kv.Value)
        e

    let rec print_formula =
        function
        (* Primitive terms *)
        // The truth constants are named "True"/"False" internally; display them compactly.
        | True -> "T"
        | False -> "F"
        | Const(NonNull(SymbolDisplay symbol)) -> symbol
        | Var(VarDisplay v) -> v
        | Atom a -> src a
        | Index(l, r) -> sprintf "here"

        (* Quantifier terms *)
        | ForAll(_, VarDisplay v, Bool true, body) -> sprintf "(\u2200 %s |: %s)" v (print_formula body)
        | ForAll(_, VarDisplay v, range, body) -> sprintf "(\u2200 %s | %s : %s)" v (print_formula range) (print_formula body)
        | Exists(_, VarDisplay v, Bool true, body) -> sprintf "(\u2203 %s | %s)" v (print_formula body)
        | Exists(_, VarDisplay v, range, body) -> sprintf "(\u2203 %s | %s : %s)" v (print_formula range) (print_formula body)
        | SumTerm(_, SymbolDisplay symbol, VarDisplay bound, range, body)
        | ProductTerm(_, SymbolDisplay symbol, VarDisplay bound, range, body) -> sprintf "%s %s %s" symbol (bound) (print_formula body)

        // Quantifier-free (sub)expressions keep the original decompiler-based rendering
        // (this preserves all propositional output). Only formulas that CONTAIN a quantifier
        // fall through to the structural connective cases below, so a quantifier nested inside
        // =, \u2228, \u2227, \u21d2, \u00ac, ... is pretty-printed instead of leaking as `forall_expr ...`.
        | expr when (get_quantifiers expr) |> List.isEmpty -> print_src expr

        (* Boolean connectives around a quantifier: recurse structurally *)
        | Not e -> sprintf "\u00ac%s" (print_atom e)
        | Equals(l, r) -> sprintf "%s = %s" (print_atom l) (print_atom r)
        | NotEquals(l, r) -> sprintf "%s \u2260 %s" (print_atom l) (print_atom r)
        | Implies(l, r) -> sprintf "%s \u21d2 %s" (print_atom l) (print_atom r)
        | Conseq(l, r) -> sprintf "%s \u21d0 %s" (print_atom l) (print_atom r)
        | And(l, r) -> sprintf "%s \u2227 %s" (print_atom l) (print_atom r)
        | Or(l, r) -> sprintf "%s \u2228 %s" (print_atom l) (print_atom r)

        (* All other terms *)
        | expr -> print_src expr

    /// Print an operand, parenthesizing it when it is itself a binary boolean connective so
    /// the nesting is unambiguous. Quantifiers and quantifier-free operands already carry
    /// their own delimiters, so they are printed as-is.
    and print_atom expr =
        match expr with
        | Equals _ | NotEquals _ | Implies _ | Conseq _ | And _ | Or _ -> sprintf "(%s)" (print_formula expr)
        | _ -> print_formula expr