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

    let rec print_formula =         
        function
        (* Primitive terms *)
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
             
        (* All other terms *)
        | expr -> 
            let mutable e = src expr
            for kv in symbolMap 
                do if e.Contains kv.Key then e <- e.Replace(kv.Key, kv.Value)
            e