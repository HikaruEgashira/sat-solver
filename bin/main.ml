exception Parse_error of string

let file = "./cnf/lesson1.dimacs" (* (P1∨P2)∧(￢P1∨P2)∧(￢P1∨￢P2) *)

let rec print_file channel =
  match input_line channel with
  | line -> print_endline line; print_file channel
  | exception End_of_file -> ()

let () =
  let ic = open_in file in
  try 
    let line = input_line ic in (* line == p cnf <num_bers> <clauses_length> *)
    match Str.split (Str.regexp " ") line with
    | _::_::num::clo::_ ->
      let num_vers = int_of_string num in
      let num_clauses = int_of_string clo in
      print_int (num_vers + num_clauses); print_endline ""; print_file ic; close_in ic
    | _ -> raise (Parse_error "DIMACS header mismatch\n")
  with e ->
    close_in_noerr ic;
    raise e
