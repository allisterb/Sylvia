// Smoke test for bin/sylvia_cadical.dll (built from src/native/Sylvia.CaDiCaL.Native).
//
// Deliberately dependency-free — no Sylvia references — so it can be run the moment the DLL exists,
// before anything else is rebuilt:
//
//     dotnet fsi examples/sat/NativeSmoke.fsx
//
// It checks the three things the native layer exists for: that the captured in-memory proof is
// identical to what `cadical --plain --lrat` writes, that API misuse returns a status instead of
// aborting the process, and that assumptions/cores/incremental reset behave.

open System
open System.Runtime.InteropServices

[<Literal>]
let DLL = __SOURCE_DIRECTORY__ + @"\..\..\bin\sylvia_cadical.dll"

[<Literal>]
let CLI = __SOURCE_DIRECTORY__ + @"\..\..\bin\cadical.exe"

// Status codes from sylvia_cadical.h
let SC_OK, SC_ERR_STATE, SC_ERR_ARG = 0, 2, 3
let SC_SAT, SC_UNSAT = 10, 20
let SC_STEP_ORIGINAL, SC_STEP_DERIVED, SC_STEP_DELETED = 0, 1, 2

module N =
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr sc_signature()
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr sc_create()
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern void sc_destroy(IntPtr s)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr sc_last_error(IntPtr s)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_set_option(IntPtr s, string name, int value)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_get_option(IntPtr s, string name, int& value)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_set_plain(IntPtr s)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_capture_proof(IntPtr s)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_set_timeout_ms(IntPtr s, int64 ms)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_add_clause(IntPtr s, int[] lits, unativeint n)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_assume(IntPtr s, int lit)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_solve(IntPtr s, int& status)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_val(IntPtr s, int lit, int& value)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_failed(IntPtr s, int lit, int& failed)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_proof_num_steps(IntPtr s, int64& n)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_proof_num_lits(IntPtr s, int64& n)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_proof_num_antes(IntPtr s, int64& n)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_proof_first_derived_id(IntPtr s, int64& id)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_proof_export(IntPtr s, int[] kinds, int64[] ids, int[] redundant, int[] witness,
                               int[] lits, int64[] litOff, int64[] antes, int64[] anteOff,
                               int64 stepsCap, int64 litsCap, int64 antesCap)
    [<DllImport(DLL, CallingConvention = CallingConvention.Cdecl)>]
    extern int sc_proof_reset(IntPtr s)

let err (s: IntPtr) = Marshal.PtrToStringAnsi(N.sc_last_error s)

let mutable passed = 0
let mutable failed = 0
let check name cond =
    if cond then passed <- passed + 1; printfn "  PASS  %s" name
    else failed <- failed + 1; printfn "  FAIL  %s" name

/// One captured proof, read back into managed arrays.
type Step = { Kind: int; Id: int64; Witness: int; Lits: int[]; Antes: int64[] }

let readProof (s: IntPtr) : Step[] =
    let mutable nSteps = 0L
    let mutable nLits = 0L
    let mutable nAntes = 0L
    N.sc_proof_num_steps(s, &nSteps) |> ignore
    N.sc_proof_num_lits(s, &nLits) |> ignore
    N.sc_proof_num_antes(s, &nAntes) |> ignore
    let n = int nSteps
    let kinds, ids = Array.zeroCreate n, Array.zeroCreate<int64> n
    let red, wit = Array.zeroCreate n, Array.zeroCreate n
    let lits = Array.zeroCreate<int> (int nLits)
    let antes = Array.zeroCreate<int64> (int nAntes)
    let litOff, anteOff = Array.zeroCreate<int64> (n + 1), Array.zeroCreate<int64> (n + 1)
    let rc =
        N.sc_proof_export(s, kinds, ids, red, wit, lits, litOff, antes, anteOff,
                          nSteps, nLits, nAntes)
    if rc <> SC_OK then failwithf "sc_proof_export failed: %s" (err s)
    [| for i in 0 .. n - 1 ->
        { Kind = kinds.[i]
          Id = ids.[i]
          Witness = wit.[i]
          Lits = lits.[int litOff.[i] .. int litOff.[i + 1] - 1]
          Antes = antes.[int anteOff.[i] .. int anteOff.[i + 1] - 1] } |]

// pigeonhole 4 -> 3: four pigeons, three holes, no two sharing. UNSAT.
let v p h = (p - 1) * 3 + h
let pigeonhole =
    [ for p in 1 .. 4 -> [| for h in 1 .. 3 -> v p h |] ]
    @ [ for h in 1 .. 3 do
          for p1 in 1 .. 4 do
            for p2 in p1 + 1 .. 4 -> [| -(v p1 h); -(v p2 h) |] ]

printfn "== 1. load =="
printfn "signature: %s" (Marshal.PtrToStringAnsi(N.sc_signature ()))

printfn "\n== 2. capture a proof of pigeonhole 4->3 =="
let s = N.sc_create ()
check "create" (s <> IntPtr.Zero)
check "set_plain" (N.sc_set_plain s = SC_OK)
check "capture_proof" (N.sc_capture_proof s = SC_OK)
for c in pigeonhole do
    N.sc_add_clause(s, c, unativeint c.Length) |> ignore
let mutable status = 0
check "solve returns OK" (N.sc_solve(s, &status) = SC_OK)
check "UNSAT" (status = SC_UNSAT)

let proof = readProof s
let originals = proof |> Array.filter (fun x -> x.Kind = SC_STEP_ORIGINAL)
let derived = proof |> Array.filter (fun x -> x.Kind = SC_STEP_DERIVED)
printfn "  %d steps: %d original, %d derived, %d deleted"
    proof.Length originals.Length derived.Length
    (proof |> Array.filter (fun x -> x.Kind = SC_STEP_DELETED) |> Array.length)

check "every input clause was traced" (originals.Length = pigeonhole.Length)
check "input ids are 1..m" (originals |> Array.mapi (fun i x -> x.Id = int64 (i + 1)) |> Array.forall id)
check "derived steps carry antecedents" (derived |> Array.forall (fun x -> x.Antes.Length > 0))
check "ends in the empty clause" (derived |> Array.exists (fun x -> x.Lits.Length = 0))
check "no RAT steps under --plain" (derived |> Array.forall (fun x -> x.Witness = 0))

let mutable firstDerived = 0L
N.sc_proof_first_derived_id(s, &firstDerived) |> ignore
check "begin_proof id is past the inputs" (firstDerived > int64 pigeonhole.Length)

printfn "\n== 3. parity with the CLI's LRAT =="
let tmp = IO.Path.GetTempPath()
let cnfPath, lratPath = IO.Path.Combine(tmp, "smoke.cnf"), IO.Path.Combine(tmp, "smoke.lrat")
let sb = Text.StringBuilder()
sb.AppendLine(sprintf "p cnf 12 %d" pigeonhole.Length) |> ignore
for c in pigeonhole do
    sb.AppendLine((c |> Array.map string |> String.concat " ") + " 0") |> ignore
IO.File.WriteAllText(cnfPath, sb.ToString())
let psi =
    Diagnostics.ProcessStartInfo(CLI,
        sprintf "-q --lrat --no-binary --plain \"%s\" \"%s\"" cnfPath lratPath,
        RedirectStandardOutput = true, UseShellExecute = false, CreateNoWindow = true)
let p = Diagnostics.Process.Start psi
p.StandardOutput.ReadToEnd() |> ignore
p.WaitForExit()

// Each LRAT addition line is `id lit* 0 hint* 0`; deletions are `id d cid* 0`.
let cliAdds =
    IO.File.ReadAllText(lratPath).Split('\n')
    |> Array.map (fun l -> l.Split([| ' '; '\t'; '\r' |], StringSplitOptions.RemoveEmptyEntries))
    |> Array.filter (fun t -> t.Length > 1 && t.[1] <> "d")
    |> Array.map (fun t ->
        let nums = t |> Array.map int64
        let lits = nums.[1..] |> Array.takeWhile (fun n -> n <> 0L)
        let hints = nums.[int lits.Length + 2 ..] |> Array.takeWhile (fun n -> n <> 0L)
        nums.[0], Array.map int lits, hints)

printfn "  CLI %d additions, native %d derived" cliAdds.Length derived.Length
check "same number of derived clauses" (cliAdds.Length = derived.Length)
check "identical ids, literals and antecedents"
    (cliAdds.Length = derived.Length
     && Array.forall2 (fun (cid, clits, chints) d ->
            cid = d.Id && clits = d.Lits && chints = d.Antes) cliAdds derived)

printfn "\n== 4. API misuse returns a status instead of aborting =="
check "unknown option -> SC_ERR_ARG" (N.sc_set_option(s, "no_such_option", 1) = SC_ERR_ARG)
check "  ... with a message" (err s <> "")
check "'plain' is rejected, not silently ignored" (N.sc_set_option(s, "plain", 1) <> SC_OK)
check "set_option after adding -> SC_ERR_STATE" (N.sc_set_option(s, "elim", 0) = SC_ERR_STATE)
check "capture_proof after adding -> SC_ERR_STATE" (N.sc_capture_proof s = SC_ERR_STATE)
check "zero literal -> SC_ERR_ARG" (N.sc_add_clause(s, [| 1; 0 |], 2un) = SC_ERR_ARG)
let mutable dummy = 0
check "val after UNSAT -> SC_ERR_STATE" (N.sc_val(s, 1, &dummy) = SC_ERR_STATE)
N.sc_destroy s
printfn "  (still running — no process abort)"

printfn "\n== 5. assumptions, cores, incremental reset =="
let s2 = N.sc_create ()
N.sc_set_plain s2 |> ignore
N.sc_capture_proof s2 |> ignore
// (1|2) (-1|3) (-3)
for c in [ [| 1; 2 |]; [| -1; 3 |]; [| -3 |] ] do
    N.sc_add_clause(s2, c, unativeint c.Length) |> ignore
let mutable st2 = 0
N.sc_solve(s2, &st2) |> ignore
check "satisfiable without assumptions" (st2 = SC_SAT)
let mutable v1 = 0
check "val readable when SAT" (N.sc_val(s2, 1, &v1) = SC_OK)

N.sc_proof_reset s2 |> ignore
let mutable afterReset = 0L
N.sc_proof_num_steps(s2, &afterReset) |> ignore
check "proof_reset empties the buffer" (afterReset = 0L)

// -2 alone forces 1, then 3, contradicting (-3): UNSAT with a core of exactly {-2}.
N.sc_assume(s2, -2) |> ignore
N.sc_assume(s2, 1) |> ignore
N.sc_solve(s2, &st2) |> ignore
check "UNSAT under assumptions" (st2 = SC_UNSAT)
let mutable f1, f2 = 0, 0
N.sc_failed(s2, -2, &f1) |> ignore
N.sc_failed(s2, 1, &f2) |> ignore
check "core is minimized to {-2}" (f1 = 1 && f2 = 0)

let mutable stepsRound2 = 0L
N.sc_proof_num_steps(s2, &stepsRound2) |> ignore
check "the round produced its own proof steps" (stepsRound2 > 0L)

N.sc_solve(s2, &st2) |> ignore
check "assumptions cleared; solver reusable" (st2 = SC_SAT)
N.sc_destroy s2

printfn "\n== 6. cooperative timeout =="
let s3 = N.sc_create ()
N.sc_set_plain s3 |> ignore
check "set_timeout_ms" (N.sc_set_timeout_ms(s3, 1L) = SC_OK)
// A 10-hole pigeonhole is far out of reach in 1ms; the point is that it STOPS and stays usable.
let v10 p h = (p - 1) * 10 + h
for p in 1 .. 11 do
    let c = [| for h in 1 .. 10 -> v10 p h |]
    N.sc_add_clause(s3, c, unativeint c.Length) |> ignore
for h in 1 .. 10 do
    for p1 in 1 .. 11 do
        for p2 in p1 + 1 .. 11 do
            N.sc_add_clause(s3, [| -(v10 p1 h); -(v10 p2 h) |], 2un) |> ignore
let mutable st3 = 0
let sw = Diagnostics.Stopwatch.StartNew()
check "solve returns OK" (N.sc_solve(s3, &st3) = SC_OK)
printfn "  status=%d after %dms" st3 sw.ElapsedMilliseconds
check "terminated without killing the process" (st3 = 0 || st3 = SC_UNSAT)
N.sc_destroy s3

printfn "\n%d passed, %d failed" passed failed
if failed > 0 then exit 1
