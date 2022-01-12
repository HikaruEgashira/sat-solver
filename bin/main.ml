exception Parse_error of string

type lit = {
  var: int (* 1, -2 *)
}
type clause = {
  lits: lit list (* [1, 2] *)
}
type cnf = {
  num_vars: int;  (* 2 *)
  clauses: clause list; (* [[1, 2], [-1, 2], [-1, -2]] *)
}

let file = "./cnf/lesson1.dimacs" (* (P1∨P2)∧(￢P1∨P2)∧(￢P1∨￢P2) *)

(* parser *)
let parse_clause string_list =
  let lits = string_list
  |> List.filter_map int_of_string_opt
  |> List.filter (fun x -> x != 0)
  |> List.map (fun x -> { var=x }) in
  { lits=lits }

let rec parse_clause_list channel =
  match input_line channel with
  | exception End_of_file -> [] (* 終了条件 *)
  | line ->
    let clauses = line 
    |> Str.split (Str.regexp " ")
    |> parse_clause in
    clauses :: parse_clause_list channel

let parse_cnf channel =
  let header = input_line channel
  |> Str.split (Str.regexp " ") in

  let num_vars = match header with (* 2番目の値を取りたいだけ *)
  | _::_::num::_ -> int_of_string num
  | _ -> raise (Parse_error "DIMACS header mismatch\n") in

  let clauses = parse_clause_list channel in

  {num_vars=num_vars; clauses=clauses}

(* main *)
let () =
  let ic = open_in file in
  try
    let cnf = parse_cnf ic in
    print_int cnf.num_vars; print_endline "";
    close_in ic;
  with e ->
    close_in_noerr ic;
    raise e
