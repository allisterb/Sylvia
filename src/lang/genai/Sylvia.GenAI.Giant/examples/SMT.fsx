fsi.PrintWidth <- 500
#load "Include.fsx"
open Sylvia.Z3

(* GIANT function calling examples begin here *)

// Check if the integer inequalities x > 4 and x < 5 are satisfiable.
check_int_sat ["x > 4"; "x < 5"]

// Check if the real inequalities x > 4 and x < 5 are satisfiable.
check_real_sat ["x > 4"; "x < 5"]

// Check if the propositional formula p && not p is satisfiable.
check_bool_sat ["p && not p"]

// Check if the real equality x^2 = -2 is satisfiable.
check_real_sat ["x^2 = -2"]

// Check if the FOL formula x && not x is satisfiable.
check_bool_sat ["forall(x, x && not x)"]

// Check if the FOL formula x || not x is satisfiable.
check_bool_sat ["forall(x, x || not x)"]


check_int_sat ["exists(y, y > 7 && y < 9)"]