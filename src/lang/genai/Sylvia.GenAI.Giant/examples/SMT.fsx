#load "Include.fsx"

open Sylvia.Z3

// Check if the integer inequalities x > 4 and x < 5 are satisfiable.
check_real_sat ["x > 4"; "x < 5"]

check_bool_sat ["p && not p"]