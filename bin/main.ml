exception Invalid_format

type lit = { var : int }
type clause = { lits : lit list }
type cnf = { num_vars : int; clauses : clause list }

let file = "./cnf/lesson1.dimacs"

(* parser *)
let parse_clause str =
  let lits =
    str
    |> Str.split (Str.regexp " ")
    |> List.filter_map int_of_string_opt
    |> List.filter (fun x -> x != 0) (* 末尾の0を除く *)
    |> List.map (fun x -> { var = x })
  in
  { lits }

let rec parse_clause_list channel =
  match input_line channel with
  | exception End_of_file -> [] (* 終了条件 *)
  | line ->
      let clauses = parse_clause line in
      clauses :: parse_clause_list channel

let parse_cnf channel =
  let header = input_line channel |> Str.split (Str.regexp " ") in
  let num_vars =
    match header with
    (* 2番目の値を取りたいだけ *)
    | _ :: _ :: num :: _ -> int_of_string num
    | _ -> raise Invalid_format
  in

  let clauses = parse_clause_list channel in

  { num_vars; clauses }

(* main *)
let () =
  let ic = open_in file in
  try
    let cnf = parse_cnf ic in
    print_int cnf.num_vars;
    print_endline "";
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e
