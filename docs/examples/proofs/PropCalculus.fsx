#load "Include.fsx"

// Set print width to 120 characters
fsi.PrintWidth <- 420

// Set print length for collections (e.g., to 1000 items)
fsi.PrintLength <- 1000

open Sylvia
open PropCalculus
open Calc

let p,q,r = boolvar3 "p" "q" "r"


calc (p * q ==> q) {
    
    eq (apply <| commute_and p q )
    imp (strengthen_and q p)
}

