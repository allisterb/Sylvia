#load "Include.fsx"

open Sylvia
open Integers

let a = intconst "a"
let x = intvar "x"

Proof.LogLevel <- 0
/// Prove integer multiplp
let p = proof integers (a * x == x * a) []