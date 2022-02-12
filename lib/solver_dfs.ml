(* 幅優先探索 *)
open Domain
open Util

let is_literal_sat lit (ret : ret) =
  let lit_label = get_label lit |> List.nth ret in
  let lit_bool_a = get_bool lit in
  let lit_bool_b = bool_of_t lit_label in
  not (xor lit_bool_a lit_bool_b)

let is_clause_sat clause ret =
  List.exists (fun lit -> is_literal_sat lit ret) clause.lits

let is_cnf_sat cnf_clauses ret =
  List.for_all (fun clause -> is_clause_sat clause ret) cnf_clauses

let rec solve_rec cnf ret_left ret_right : ret option =
  match ret_right with
  | _ :: rest ->
      let case_true = solve_rec cnf (ret_left @ [ Some true ]) rest in
      let case_false = solve_rec cnf (ret_left @ [ Some false ]) rest in
      if Option.is_some case_true then case_true
      else if Option.is_some case_false then case_false
      else None
  | [] -> if is_cnf_sat cnf ret_left then Some ret_left else None

let solve cnf : ret option =
  let t_base = List.init cnf.num_vars (fun _ -> None) in
  solve_rec cnf.clauses [] t_base
