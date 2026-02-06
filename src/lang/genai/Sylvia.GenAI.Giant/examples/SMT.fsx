#load "Include.fsx"
open Sylvia.Z3

(* Function calling examples begine her *)

// Check if the integer inequalities x > 4 and x < 5 are satisfiable.
check_int_sat ["x > 4"; "x < 5"]

// Check if the real inequalities x > 4 and x < 5 are satisfiable.
check_real_sat ["x > 4"; "x < 5"]

// Check if the boolean formulas p && not p.
check_bool_sat ["p && not p"]