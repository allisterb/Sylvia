open Sylvia
open PropCalculus

let p,q,r = boolvar3 "p" "q" "r"

trans_implies p q r |> ignore