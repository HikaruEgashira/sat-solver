open Domain

let get_num_vars channel =
  (* ここがやばい2022 *)
  let list = input_line channel |> Str.split (Str.regexp " ") in
  List.nth list 2 |> int_of_string

let parse_clause str =
  str
  |> Str.split (Str.regexp " ")
  |> List.map int_of_string
  |> List.filter (fun x -> x != 0) (* 末尾の0を除く、上と関数合成したら逆に分かりにくくなるのでしない *)
  |> List.map (fun var -> { var })
  |> fun lits -> { lits }

let rec get_clause_list channel =
  match input_line channel with
  | exception End_of_file -> [] (* 終了条件 *)
  | line -> parse_clause line :: get_clause_list channel

let parse_cnf channel =
  let num_vars = get_num_vars channel in
  let clauses = get_clause_list channel in
  { num_vars; clauses }