#load "Include.fsx"

open Sylvia.GenAI.Giant.Examples

(* GIANT SMT function calling examples begin here *)

// Check if the propositional formula p ∧ ¬p is satisfiable.
check_bool_sat ["p && not p"]

// Check if the integer inequalities x > 4 and x < 5 are satisfiable.
check_int_sat ["x > 4"; "x < 5"]

// Check if the real inequalities x > 4 and x < 5 are satisfiable.
check_real_sat ["x > 4"; "x < 5"]

// Check if the real equality x^2 = -2 is satisfiable.
check_real_sat ["x^2 = -2"]

// Check if the boolean FOL formula forall (x, x ∧ ¬x) is satisfiable.
check_bool_sat ["forall(x, x && not x)"]

// Check if the boolean FOL formula forall(x, x ∨ ¬x) is satisfiable.
check_bool_sat ["forall(x, x && not x)"]

// Check if the integer FOL formula exists(y, y > 7 && y < 9) is satisfiable.
check_int_sat ["exists(y, y > 7 && y < 9)"]

// Get a propositional model for Tie || Shirt and not Tie || Shirt and not Tie || not Shirt
get_bool_model ["Tie || Shirt"; "not Tie || Shirt"; "not Tie || not Shirt"]

// Get an integer model for x > 5 and x <> 6 and x < 8
get_int_model ["x > 5"; "x <> 6"; "x < 8"]

(*
    Formalize the following puzzle and check if it is satisfiable. If it is, get a model for the puzzle.
    "On an island, there are only two types of people: knights who always tell the truth and knaves who always lie."
    "There is at least one knave on the island who tells the truth."
*)
get_int_model ["x > 5"; "x <> 6"; "x < 8"]
get_bool_model [
    "forall(islander, IsKnight(islander) || IsKnave(islander))"
    "forall(islander, IsKnight(islander) ==> TellsTruth(islander))"
    "forall(islander, IsKnave(islander) ==> not TellsTruth(islander))"
    "exists(islander, IsKnave(islander) && not TellsTruth(islander))"
]