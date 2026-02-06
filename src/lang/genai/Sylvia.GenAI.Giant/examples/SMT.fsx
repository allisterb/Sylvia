#load "Include.fsx"

open Sylvia
open Sylvia.Z3

// Check if the integr constraints x > 4 and x < 5 are satisfiable.
check_int_sat ["x > 4"; "x < 5"]

