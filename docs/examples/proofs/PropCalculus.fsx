#load "Include.fsx"

// Set print width to 120 characters
fsi.PrintWidth <- 420

// Set print length for collections (e.g., to 1000 items)
fsi.PrintLength <- 1000

open Sylvia
open PropCalculus


let p,q,r = boolvar3 "p" "q" "r"



proof prop_calculus ((p * (q * r)) == ((p * q) * (p * r))) [
    Auto
]

