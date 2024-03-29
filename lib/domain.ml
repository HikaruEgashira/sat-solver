(* cnf *)
type lit = { var : int }
type clause = { lits : lit list }
type cnf = { num_vars : int; clauses : clause list }

(* output *)
type t = bool option
type ts = t list
type output = ts option

let get_label lit = Int.abs lit.var - 1
let get_bool lit = lit.var > 0
