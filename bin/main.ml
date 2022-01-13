(* #domain *)

(* cnf *)
type lit = { var : int }
type clause = { lits : lit list }
type cnf = { num_vars : int; clauses : clause list }

(* output *)
type t = L_True | L_False | L_Undef
type ret = t list

exception Invalid_format

let file = "./cnf/lesson1.dimacs"

(* #parser *)
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

(* #printer *)

(* Util: separator で結合 *)
let list_join separator list =
  list |> List.fold_right (fun s x -> s ^ " " ^ x) |> fun x -> x separator

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
        |> list_join " "
      in
      print_endline ("SAT\n" ^ out_str)

(* #solver *)
let solve _cnf =
  (* WIP *)
  let out = [ L_False; L_True ] in
  out

(* #main *)
let () =
  (* ファイルからCNFを生成 *)
  let channel = open_in file in
  let cnf = parse_cnf channel in

  (* solve *)
  let out = solve cnf in

  (* 出力 *)
  print_ret out;
  close_in channel
