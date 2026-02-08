#load "Include.fsx"

open Sylvia.GenAI.Giant.Examples

(* GIANT function calling examples begin here *)

(*
// Check if the propositional formula p && not p is satisfiable.
check_bool_sat ["p && not p"]

// Check if the integer inequalities x > 4 and x < 5 are satisfiable.
check_int_sat ["x > 4"; "x < 5"]

// Check if the real inequalities x > 4 and x < 5 are satisfiable.
check_real_sat ["x > 4"; "x < 5"]

// Check if the real equality x^2 = -2 is satisfiable.
check_real_sat ["x^2 = -2"]

// Check if the boolean FOL formula x && not x is satisfiable.
check_bool_sat ["forall(x, x && not x)"]

// Check if the boolean FOL formula x || not x is satisfiable.
check_bool_sat ["forall(x, x && not x)"]

// Check if the integer FOL formula x || not x is satisfiable.
check_int_sat ["exists(y, y > 7 && y < 9)"]

// Get a propositional model for 
get_prop_model ["Tie || Shirt"; "not Tie || Shirt"; "not Tie || not Shirt"]

// Get an integer model for x > 5 and x <> 6 and x < 8
get_int_model ["x > 5"; "x <> 6"; "x < 8"]
*)
check_bool_sat [
    "forall(islander, IsKnight(islander) || IsKnave(islander))"
    "IsKnight(islander) ==> TellsTruth(islander) && IsKnave(islander) ==> not TellsTruth(islander)"
    "exists(islander, IsKanve(islander) && TellsTruth(islander))"
]