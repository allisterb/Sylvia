#load "Include.fsx"

open Sylvia
open Integers

let a = intconst "a"
let x = intvar "x"

let p = proof integers (a * x == x * a) []