(* 幅優先探索 *)
open Domain
open Util

let check_clause clause t =
  List.find_opt (fun lit -> check_literal lit t) clause.lits |> Option.is_some

let check_cnf cnf t =
  List.for_all (fun clause -> check_clause clause t) cnf.clauses

let rec solve_rec cnf ret_left ret_right : ret =
  match ret_right with
  | _ :: rest ->
      let case_true = solve_rec cnf (ret_left @ [ L_True ]) rest in
      let case_false = solve_rec cnf (ret_left @ [ L_False ]) rest in
      if case_true != [] then case_true
      else if case_false != [] then case_false
      else []
  | [] -> if check_cnf cnf ret_left then ret_left else []

let solve cnf : ret =
  let t_base = List.init cnf.num_vars (fun _ -> L_Undef) in
  solve_rec cnf [] t_base
