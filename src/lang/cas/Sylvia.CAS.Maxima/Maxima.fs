namespace Sylvia.CAS

open System
open System.Runtime.InteropServices
open System.Text
open System.Text.RegularExpressions
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvia
open ExpectNet
open Expect

type Maxima(?maximaCmd:string) =
    inherit Runtime()
    let cmd = defaultArg maximaCmd "maxima"
    let args = if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then [|"--disable-readline"|] else Array.empty
    let p = new ConsoleProcess(cmd, args, false)
    // No-op handler for the Expect.NET matcher API (results are consumed directly).
    let noop = Action<IResult>(ignore)
    let output, session =
        if p.Initialized then
            let _s = new ProcessSpawnable(p.Process)
            do p.Start ()
            new StringBuilder(), Expect.Spawn(_s, Environment.NewLine) |> Ok
        else
            null, exn "Maxima console process did not initialize." |> Error
    let initialized =
        match session with
        | Ok s ->
            if s.Expect.Contains("(%i1)", noop, Nullable(2000)).IsMatch then
                s.Send.String "ratprint:false;"
                s.Send.String "display2d:false;"
                s.Send.String "linel:500;"
                s.Send.String "load(simplify_sum);"
                s.Send.String "load(diag);"
                if s.Expect.Contains("diag.mac", noop, Nullable(2000)).IsMatch then
                    true
                else
                    err' "Could not set Maxima default options."
                    false
            else
                err' "Did not receive expected response from Maxima console process."
                false
        | Error f -> 
            err "Could not initialize Maxima." [f]
            false

    let failIfNotInitialized x = if not initialized then failwith "The Maxima process is not started." else x

    override x.Initialized = initialized
    
    member x.ConsoleProcess = failIfNotInitialized p
    
    member x.ConsoleSession = session |> success |> failIfNotInitialized 

    member x.Output = failIfNotInitialized output

    member val Input = new StringBuilder()

    
    member val ProcessTimeOut = 2000 with get, set

    member val CurrentInputLine = 1 with get, set
 
module Maxima =
    let private (||||) l r = Microsoft.FSharp.Core.Operators.(|||) l r
    let private outputPattern = """\(%o(\d)+\)\s+(.+)\s+\(%i(\d)+\)\s?"""
    let private outputRegex = new Regex(outputPattern, RegexOptions.Compiled |||| RegexOptions.Multiline)

    let sanitize_symbol (symbol:string) =
        match symbol with
        | s when s.EndsWith("'") -> s.Replace("'", "__dash__")
        | s -> s

    let rec sprint (x:Expr) = 
        match x with
        | List list -> "[" + (list |>  List.map sprint |> List.reduce (fun l r -> l + ", " + r)) + "]"
        
        | SpecificCall <@@ (<) @@> (_, _, [l; r]) -> sprintf("(%s) < (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (<=) @@> (_, _, [l; r]) -> sprintf("(%s) <= (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (>) @@> (_, _, [l; r]) -> sprintf("(%s) > (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (>=) @@> (_, _, [l; r]) -> sprintf("(%s) >= (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (=) @@> (_, _, [l; r]) -> sprintf("(%s) = (%s)") (sprint l) (sprint r)
        
        | SpecificCall <@@ (~-) @@> (_, _, [l]) -> sprintf("-(%s)") (sprint l)
        | SpecificCall <@@ (~+) @@> (_, _, [l]) -> sprintf("+(%s)") (sprint l)
        | SpecificCall <@@ (/) @@> (_, _, [l; r]) -> sprintf("(%s) / (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (*) @@> (_, _, [l; r]) -> sprintf("(%s) * (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (+) @@> (_, _, [l; r]) -> sprintf("(%s) + (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ (-) @@> (_, _, [l; r]) -> sprintf("(%s) - (%s)") (sprint l) (sprint r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [l; r]) -> sprintf("(%s)^(%s)") (sprint l) (sprint r)
        
        | SpecificCall <@@ Microsoft.FSharp.Core.Operators.sin @@> (_, _, [l]) -> sprintf("sin(%s)") (sprint l) 
        | SpecificCall <@@ Microsoft.FSharp.Core.Operators.cos @@> (_, _, [l]) -> sprintf("cos(%s)") (sprint l)
        | SpecificCall <@@ Microsoft.FSharp.Core.Operators.tan @@> (_, _, [l]) -> sprintf("tan(%s)") (sprint l)

        | SpecificCall <@@ Microsoft.FSharp.Core.Operators.log @@> (_, _, [l]) -> sprintf("log(%s)") (sprint l)
        | SpecificCall <@@ Microsoft.FSharp.Core.Operators.min @@> (_, _, [l;r]) -> sprintf("min(%s,%s)") (sprint l) (sprint r)
    
        | PropertyGet(None, Prop "pi", []) -> "%pi"
        | PropertyGet(None, Prop "e", []) -> "%e"

        | Var v -> sanitize_symbol v.Name
  
        | ValueWithName(_,_,n) -> sanitize_symbol n
        | Lambda(x, e) -> sprintf("%A = %s") x (sprint e)
        | Double (Double.MaxValue) -> "inf"
        | Int32 (Int32.MaxValue) -> "inf"
        | Double (Double.MinValue) -> "minf"
        | Int32 (Int32.MinValue) -> "minf"
        | Int32 n -> n.ToString()
        | Double n -> n.ToString()
        | SpecificCall <@@ Numbers.real_frac @@> (_, _, [l;r]) -> sprintf "%s/%s" (sprint l) (sprint r)
        | _ -> x |> expand |> MathNetExpr.fromQuotation |> MathNet.Symbolics.Infix.format

    let sprintl (exprs: Expr<'t> list) =
        exprs 
        |> List.toArray
        |> Array.skip 1 
        |> Array.fold(fun s e -> sprintf "%s, %s" s (sprint e)) (sprint exprs.[0]) 
        |> sprintf "[%s]"

    let sprintm(expr:Expr<_>[][]) =
        expr |> Array.map (Array.map sprinte) |> Array.map(Array.reduce (sprintf "%s, %s")) |> Array.map(sprintf "[%s]")
        |> Array.reduce (sprintf "%s, %s") |> sprintf "matrix(%s)"

    let extract_output text =
        let m = outputRegex.Match text 
        if m.Success then 
            ((m.Groups.Item 1).Value, (m.Groups.Item 2).Value, (m.Groups.Item 3).Value) |> Ok 
        else 
            sprintf "Could not extract Maxima output from process response: %s" text |> exn |> Error
        
    let start path = new Maxima(path)
    
    let stop (m:Maxima) = m.ConsoleProcess.Stop()
    
    let private noop = Action<IResult>(ignore)

    let send (m:Maxima) (input:string) =
        m.Input.AppendLine input |> ignore
        !> m.ConsoleSession.Send.String input
        >>|> (m.ConsoleSession.Expect.Regex(outputPattern, noop, Nullable(m.ProcessTimeOut)) |> wrap_result)
        >>>= extract_output
        >>= fun (_, r, n) -> 
            do m.CurrentInputLine <- Int32.Parse n
            Ok r
       

    let mutable defaultInt:Maxima option = None

    let init (s:string) =
        let m = start s
        match m.Initialized with
        | true -> defaultInt <- Some m
        | _ -> failwithf "Failed to initialize the default Maxima interpreter at %s." s

    let send' s = 
        match defaultInt with
        | Some m ->
            match send (defaultInt.Value) s with
            | Ok o -> Ok o
            | Error e -> Error e
        | None -> failwith "The default Maxima interpreter is not initialized."

    // The current Expect.NET Session exposes a single accumulated Output string
    // (no LastOutput/LastInput); return the last n lines of the given text.
    let private lastLines n (s:string) =
        let lines = (if isNull s then "" else s).Replace("\r\n", "\n").Split('\n')
        lines |> Array.skip (max 0 (lines.Length - n)) |> String.concat "\n"

    let last_output n =
        match defaultInt with
        | Some m -> lastLines n m.ConsoleSession.Output
        | None -> failwith "The default Maxima interpreter is not initialzed."

    let last_input n =
        match defaultInt with
        | Some m -> lastLines n (m.Input.ToString())
        | None -> failwith "The default Maxima interpreter is not initialzed."

    let set_stardisp()  =
        match send' "stardisp:true;" with
        | Ok (r:string) -> if r.Trim() <> "true" then failwithf "Could not set stardisp variable. Maxima returned %s." r
        | Error e -> failwithf "Could not set stardisp variable. Maxima returned %s." e.Message


