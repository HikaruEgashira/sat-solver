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

(* debug *)
let rec print_file channel =
  match input_line channel with
  | line -> print_endline line; print_file channel
  | exception End_of_file -> ()

(* parser *)
let parse_clause stringlist =
  let int_list = List.map int_of_string stringlist in
  let lit_list = match List.rev int_list with
  | 0::t -> List.rev t (* 末尾の0をいい感じに取りたかった *)
  | _ -> raise (Parse_error "literal") in
  let lits = List.map (fun x -> {var=x}) lit_list in
  {lits=lits}

let rec parse_clause_list channel =
  match input_line channel with
  | line -> 
    let clauses = parse_clause (Str.split (Str.regexp " ") line) in
    let acc = parse_clause_list channel in
    clauses :: acc
  | exception End_of_file -> []

let parse_cnf channel =
  let line = input_line channel in
  match Str.split (Str.regexp " ") line with
  | _::_::num::_ ->
    let num_vars = int_of_string num in
    let clauses = parse_clause_list channel in
    {num_vars=num_vars; clauses=clauses}
  | _ -> raise (Parse_error "DIMACS header mismatch\n")

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
