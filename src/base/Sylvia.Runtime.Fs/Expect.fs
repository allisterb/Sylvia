namespace Sylvia

open System
open ExpectNet

module Expect =

    // No-op handler for the Expect.NET matcher API (we consume the returned IResult directly).
    let private noop = Action<IResult>(ignore)

    /// Send a line of input: String appends the session's line terminator by default.
    let send_line (s:Session) line = s.Send.String(line)

    let private wrap_nullable i =
        match i with
        | Some _i -> Nullable(_i) |> Some
        | None -> None

    let private default_expect_params timeout_ms retries =
           let t = defaultArg timeout_ms (Nullable<int>())
           let r = defaultArg retries (Nullable<int>())
           t, r

    let wrap_result (r:IResult) = if r.IsMatch then Ok(r.Text) else sprintf "The text %s does not match the expected result" r.Text |> exn |> Error

    let wrap_result' (res:Result<IResult, exn>) = res |> Result.bind wrap_result

    let starts_with (s:Session) (q:string) (timeout_ms:int option) (retries: int option) =
        // The current Expect.NET API has no StartsWith; match a regex anchored at the start of input.
        let t, _ = default_expect_params (wrap_nullable timeout_ms) (wrap_nullable retries)
        let pattern = "^" + System.Text.RegularExpressions.Regex.Escape(q)
        s.Expect.Regex(pattern, noop, t) |> wrap_result

    let contains (s:Session) (q:string) (timeout_ms:Nullable<int> option) (retries: Nullable<int> option) =
        let t, r = default_expect_params timeout_ms retries
        s.Expect.Contains(q, noop, t, r) |> wrap_result

    let is_match (m:IResult)  = m.IsMatch
