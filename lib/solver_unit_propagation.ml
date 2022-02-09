(* 単位伝搬 *)
open Solver_breadth_first_search
open Domain
open Util

(* 必然的な真偽値割り当てを行う *)
let rec rec_assign_clause old_lits new_lits ret =
  match old_lits with
  | [] -> new_lits
  | lit :: rest_lits ->
      if t_of_lit lit ret == None then
        (* Undefだから節を残す *)
        rec_assign_clause rest_lits (lit :: new_lits) ret
      else if (* すでにTrueだから節ごと消す *) is_literal_sat lit ret then []
      else (* すでにFalseだからリテラルを消す *) rec_assign_clause rest_lits new_lits ret

let assign_clause clause_lits ret : clause =
  { lits = rec_assign_clause clause_lits [] ret }

let assign_cnf cnf_clauses ret : clause list =
  List.map (fun clause -> assign_clause clause.lits ret) cnf_clauses
  |> List.filter (fun clause -> clause.lits != [])

let rec solve_rec cnf_clauses ret_left ret_right : ret =
  match ret_right with
  | _ :: rest ->
      let applied_cnf = assign_cnf cnf_clauses (ret_left @ ret_right) in
      let case_true = solve_rec applied_cnf (ret_left @ [ Some true ]) rest in
      let case_false = solve_rec applied_cnf (ret_left @ [ Some false ]) rest in
      if case_true != [] then case_true
      else if case_false != [] then case_false
      else []
  | [] -> if is_cnf_sat cnf_clauses ret_left then ret_left else []

let solve cnf : ret =
  let t_base = List.init cnf.num_vars (fun _ -> None) in
  solve_rec cnf.clauses [] t_base

let%test _ =
  let input_file = "../cnf/lesson1.dimacs" in
  let channel = open_in input_file in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let ret1 = Solver_breadth_first_search.solve cnf in
  let ret2 = solve cnf in
  List.combine ret1 ret2
  |> List.for_all (fun (x, y) -> Option.get x = Option.get y)