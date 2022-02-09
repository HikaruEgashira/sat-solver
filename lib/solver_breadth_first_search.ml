(* 幅優先探索 *)
open Domain
open Util

let is_clause_sat clause ret =
  List.exists (fun lit -> is_literal_sat lit ret) clause.lits

let is_cnf_sat cnf_clauses ret =
  List.for_all (fun clause -> is_clause_sat clause ret) cnf_clauses

let rec solve_rec cnf ret_left ret_right : ret =
  match ret_right with
  | _ :: rest ->
      let case_true = solve_rec cnf (ret_left @ [ Some true ]) rest in
      let case_false = solve_rec cnf (ret_left @ [ Some false ]) rest in
      if case_true != [] then case_true
      else if case_false != [] then case_false
      else []
  | [] -> if is_cnf_sat cnf ret_left then ret_left else []

let solve cnf : ret =
  let t_base = List.init cnf.num_vars (fun _ -> None) in
  solve_rec cnf.clauses [] t_base

let%test _ =
  let input_file = "../cnf/lesson1.dimacs" in
  let channel = open_in input_file in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let ret = solve cnf in
  List.combine ret [ Some false; Some true ]
  |> List.for_all (fun (x, y) -> x == y)