(* 単位伝搬 *)
open Solver_dfs
open Domain
open Util

(* 必然的な真偽値割り当てを行う *)
let rec rec_assign_clause old_lits new_lits ret =
  match old_lits with
  | [] -> Some new_lits
  | lit :: rest_lits ->
      if t_of_lit lit ret == None then
        rec_assign_clause rest_lits (lit :: new_lits) ret (* Noneだからリテラルを残す *)
      else if is_literal_sat lit ret then Some [] (* すでにTrueだから節ごと消す *)
      else None

let assign_clause clause_lits ret =
  rec_assign_clause clause_lits [] ret |> Option.map (fun lits -> { lits })

let assign_cnf cnf_clauses ret =
  cnf_clauses
  |> List.map (fun clause -> assign_clause clause.lits ret)
  |> funcA
  |> Option.map (List.filter (fun l -> l.lits != []))

let rec solve_rec cnf_clauses ret_left ret_right : ret option =
  match ret_right with
  | _ :: rest -> (
      (* ↓ 割り当てが追加 *)
      let new_cnf = assign_cnf cnf_clauses (ret_left @ ret_right) in
      match new_cnf with
      | None -> None
      | Some new_cnf ->
          let case_true = solve_rec new_cnf (ret_left @ [ Some true ]) rest in
          let case_false = solve_rec new_cnf (ret_left @ [ Some false ]) rest in
          if Option.is_some case_true then case_true
          else if Option.is_some case_false then case_false
          else None)
  | [] -> if is_cnf_sat cnf_clauses ret_left then Some ret_left else None

let solve cnf : ret option =
  let t_base = List.init cnf.num_vars (fun _ -> None) in
  solve_rec cnf.clauses [] t_base
