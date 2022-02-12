(* 幅優先探索 *)
open Domain
open Util

let is_literal_sat lit (ts : ts) =
  let lit_label = get_label lit |> List.nth ts in
  let lit_bool_a = get_bool lit in
  let lit_bool_b = bool_of_t lit_label in
  not (xor lit_bool_a lit_bool_b)

let is_clause_sat clause output =
  List.exists (fun lit -> is_literal_sat lit output) clause.lits

let is_cnf_sat cnf_clauses output =
  List.for_all (fun clause -> is_clause_sat clause output) cnf_clauses

let rec solve_rec cnf (ts_left : ts) (ts_right : ts) : output =
  match ts_right with
  | _ :: rest ->
      (* Node *)
      let case_true = solve_rec cnf (ts_left @ [ Some true ]) rest in
      let case_false = solve_rec cnf (ts_left @ [ Some false ]) rest in
      if Option.is_some case_true then case_true
      else if Option.is_some case_false then case_false
      else None
  | [] -> (* Leaf *) if is_cnf_sat cnf ts_left then Some ts_left else None

let solve cnf : output =
  let t_base : ts = List.init cnf.num_vars (fun _ -> None) in
  solve_rec cnf.clauses [] t_base
