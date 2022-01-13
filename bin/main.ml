(* cnf *)
type lit = { var : int }
type clause = { lits : lit list }
type cnf = { num_vars : int; clauses : clause list }

(* output *)
type t = L_True | L_False | L_Undef
type ret = t list

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

(* printer *)
let print_ret ret =
  match ret with
  | [] -> print_endline "UNSAT"
  | l ->
      let out_str =
        l
        |> List.map (fun x ->
               match x with
               | L_True -> "True"
               | L_False -> "False"
               | L_Undef -> "Undef")
        |> List.fold_right (fun s x -> s ^ " " ^ x)
        |> fun x -> x " "
      in
      print_endline ("SAT\n" ^ out_str)

(* solver *)
let solve _cnf =
  let out = [ L_False; L_True ] in
  out

(* main *)
let () =
  let channel = open_in file in
  let cnf = parse_cnf channel in

  print_endline (string_of_int cnf.num_vars);
  let out = solve cnf in

  print_ret out;
  close_in channel
