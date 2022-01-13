(* cnf *)
type lit = { var : int }
type clause = { lits : lit list }
type cnf = { num_vars : int; clauses : clause list }

(* output *)
type t = L_True | L_False | L_Undef
type ret = t list
