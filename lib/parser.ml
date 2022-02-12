open Domain

let get_num_vars channel =
  let list = input_line channel |> Str.split (Str.regexp " ") in
  List.nth list 2 |> int_of_string

let parse_clause str =
  str
  |> Str.split (Str.regexp " ")
  |> List.map int_of_string
  |> List.filter (fun x -> x <> 0)
  |> List.map (fun var -> { var })
  |> fun lits -> { lits }

let rec get_clauses channel =
  match input_line channel with
  | exception End_of_file -> [] (* 終了条件 *)
  | line -> parse_clause line :: get_clauses channel

let parse_cnf channel =
  let num_vars = get_num_vars channel in
  let clauses = get_clauses channel in
  { num_vars; clauses }
