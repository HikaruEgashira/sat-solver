type lit = { var : int }
type clause = { lits : lit list }
type cnf = { num_vars : int; clauses : clause list }

exception Invalid_format

let file = "./cnf/lesson1.dimacs"

(* parser *)
let get_num_vars channel =
  let list = input_line channel |> Str.split (Str.regexp " ") in
  List.nth list 2 |> int_of_string

let parse_clause str =
  str
  |> Str.split (Str.regexp " ")
  |> List.filter_map int_of_string_opt
  |> List.filter (fun x -> x != 0) (* 末尾の0を除く *)
  |> List.map (fun var -> { var })
  |> fun lits -> { lits }

let rec get_clause_list channel =
  match input_line channel with
  | exception End_of_file -> [] (* 終了条件 *)
  | line ->
      let clauses = parse_clause line in
      clauses :: get_clause_list channel

let parse_cnf channel =
  let num_vars = get_num_vars channel in
  let clauses = get_clause_list channel in
  { num_vars; clauses }

(* main *)
let () =
  let channel = open_in file in
  let cnf = parse_cnf channel in
  print_endline (string_of_int cnf.num_vars);
  close_in channel
