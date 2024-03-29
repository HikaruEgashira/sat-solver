(* 単位伝搬 *)
open Solver_dfs
open Domain
open Util

(* 必然的な真偽値割り当てを行う *)
let rec rec_assign_clause old_lits new_lits ts =
  match old_lits with
  | [] -> Some new_lits
  | lit :: rest_lits ->
      if t_of_lit lit ts |> Option.is_none then
        rec_assign_clause rest_lits (lit :: new_lits) ts (* Noneだからリテラルを残す *)
      else if is_literal_sat lit ts then Some [] (* すでにTrueだから節ごと消す *)
      else rec_assign_clause rest_lits (lit :: new_lits) ts
(* UNSAT因子は消さない *)

let assign_clause clause ts =
  rec_assign_clause clause.lits [] ts |> Option.map (fun lits -> { lits })

let assign_cnf cnf_clauses ts =
  cnf_clauses
  |> List.map (fun clause -> assign_clause clause ts)
  |> funcA
  |> Option.map (List.filter (fun l -> l.lits <> []))

let rec solve_rec cnf_clauses (ts_left : ts) (ts_right : ts) : output =
  match ts_right with
  | _ :: rest -> (
      (* ↓ 割り当てが追加 *)
      let maybe_cnf_clauses = assign_cnf cnf_clauses (ts_left @ ts_right) in
      match maybe_cnf_clauses with
      | None -> None
      | Some new_cnf_clauses ->
          let case_true =
            solve_rec new_cnf_clauses (ts_left @ [ Some true ]) rest
          in
          let case_false =
            solve_rec new_cnf_clauses (ts_left @ [ Some false ]) rest
          in
          (* TODO: 早期リターンしたかった *)
          if Option.is_some case_true then case_true else case_false)
  | [] -> if is_cnf_sat cnf_clauses ts_left then Some ts_left else None

let solve cnf : output =
  let t_base : ts = List.init cnf.num_vars (fun _ -> None) in
  solve_rec cnf.clauses [] t_base
