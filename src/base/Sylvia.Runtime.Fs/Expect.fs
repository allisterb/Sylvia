namespace Sylvia

open System
open ExpectNet

module Expect =

    let send_line (s:Session) line = s.Send.Line line
    
    let private wrap_nullable i = 
        match i with
        | Some _i -> Nullable(_i) |> Some
        | None -> None
    
    let private default_expect_params timeout_ms retries= 
           let t = defaultArg timeout_ms (Nullable<int>())
           let r = defaultArg retries (Nullable<int>())
           t, r

    let wrap_result (r:IResult) = if r.IsMatch then Ok(r.Text) else sprintf "The text %s does not match the expected result" r.Text |> exn |> Error
    
    let wrap_result' (res:Result<IResult, exn>) = res |> Result.bind wrap_result        
        
    let starts_with (s:Session) (q:string) (timeout_ms:int option) (retries: int option) = 
        let p = default_expect_params (wrap_nullable timeout_ms) (wrap_nullable retries)
        s.Expect.StartsWith(q, fst p, snd p) |> wrap_result
        
    let contains (s:Session) (q:string) (timeout_ms:Nullable<int> option) (retries: Nullable<int> option) = 
        let p = default_expect_params timeout_ms retries
        s.Expect.Contains(q, fst p, snd p) |> wrap_result

    let is_match (m:IResult)  = m.IsMatch