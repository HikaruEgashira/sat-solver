(* 単位伝搬 *)
open Domain
open Util

(* 必然的な真偽値割り当てを行う *)
let rec rec_apply_clause old_lits new_lits t =
  match old_lits with
  | [] -> new_lits
  | lit :: rest_lits ->
      if t_of_lit lit t == L_Undef then
        (* Undefだから残る節 *)
        rec_apply_clause rest_lits (lit :: new_lits) t
      else if (* すでにTrueだから節ごと消す *) check_literal lit t then []
      else (* すでにFalseだからリテラルを消す *) rec_apply_clause rest_lits new_lits t

let apply_clause lits t = { lits = rec_apply_clause lits [] t }

let apply_cnf cnf_clauses t : clause list =
  List.map (fun clause -> apply_clause clause.lits t) cnf_clauses
  |> List.filter (fun clause -> clause.lits != [])

let check_clause clause t =
  List.exists (fun lit -> check_literal lit t) clause.lits

let check_cnf cnf_clauses t =
  List.for_all (fun clause -> check_clause clause t) cnf_clauses

let rec solve_rec cnf_clauses ret_left ret_right : ret =
  match ret_right with
  | _ :: rest ->
      let applied_cnf = apply_cnf cnf_clauses (ret_left @ ret_right) in
      let case_true = solve_rec applied_cnf (ret_left @ [ L_True ]) rest in
      let case_false = solve_rec applied_cnf (ret_left @ [ L_False ]) rest in
      if case_true != [] then case_true
      else if case_false != [] then case_false
      else []
  | [] -> if check_cnf cnf_clauses ret_left then ret_left else []

let solve cnf : ret =
  let t_base = List.init cnf.num_vars (fun _ -> L_Undef) in
  solve_rec cnf.clauses [] t_base
