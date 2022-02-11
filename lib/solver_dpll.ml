(* 単位伝搬 *)
open Solver_dfs
open Domain
open Util

(* 必然的な真偽値割り当てを行う *)
let rec rec_assign_clause old_lits new_lits ret =
  match old_lits with
  | [] -> new_lits
  | lit :: rest_lits ->
      if t_of_lit lit ret == None then
        (* Noneだからリテラルを残す *)
        rec_assign_clause rest_lits (lit :: new_lits) ret
      else if (* すでにTrueだから節ごと消す *) is_literal_sat lit ret then []
      else
        (* すでにFalseだからとりあえずリテラルを残す *)
        rec_assign_clause rest_lits (lit :: new_lits) ret

let assign_clause clause_lits ret : clause =
  { lits = rec_assign_clause clause_lits [] ret }

let assign_cnf cnf_clauses ret : clause list =
  cnf_clauses
  |> List.map (fun clause -> assign_clause clause.lits ret)
  |> List.filter (fun clause -> clause.lits != [])

let rec solve_rec cnf_clauses ret_left ret_right : ret =
  match ret_right with
  | _ :: rest ->
      let new_cnf = assign_cnf cnf_clauses (ret_left @ ret_right) in
      let case_true = solve_rec new_cnf (ret_left @ [ Some true ]) rest in
      let case_false = solve_rec new_cnf (ret_left @ [ Some false ]) rest in
      if case_true != [] then case_true
      else if case_false != [] then case_false
      else []
  | [] -> if is_cnf_sat cnf_clauses ret_left then ret_left else []

let solve cnf : ret =
  let t_base = List.init cnf.num_vars (fun _ -> None) in
  solve_rec cnf.clauses [] t_base
