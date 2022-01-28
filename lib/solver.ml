open Domain
open Util

let check_literal lit t =
  let lit_label = get_label lit |> List.nth t in
  let lit_bool_a = get_bool lit in
  let lit_bool_b =
    match lit_label with L_False -> false | L_True -> true | L_Undef -> false
  in
  not (xor lit_bool_a lit_bool_b)

let rec check_clause lits t =
  match lits with
  | [] -> false
  | lit :: rest_lits ->
      let valid_lit = check_literal lit t in
      let valid_clause = check_clause rest_lits t in
      valid_lit || valid_clause

let rec check_cnf cnf_clauses t =
  match cnf_clauses with
  | [] -> true
  | clause :: rest_clauses ->
      let valid_clause = check_clause clause.lits t in
      let valid_cnf = check_cnf rest_clauses t in
      valid_clause && valid_cnf

let rec rec_solve cnf ta tb : ret =
  match tb with
  | L_Undef :: rest ->
      let case_true = rec_solve cnf ta (L_True :: rest) in
      let case_false = rec_solve cnf ta (L_False :: rest) in
      if case_true != [] then case_true
      else if case_false != [] then case_false
      else []
  | hd :: rest ->
      let h : ret = ta @ [ hd ] in
      if List.exists (fun v -> v == L_Undef) rest then rec_solve cnf h rest
      else if check_cnf cnf.clauses h then h
      else []
  | [] -> []

let rec solve_rec cnf ret_left ret_right : ret =
  match ret_right with
  | L_Undef :: rest ->
      let case_true = solve_rec cnf ret_left (L_True :: rest) in
      let case_false = solve_rec cnf ret_left (L_False :: rest) in
      if case_true != [] then case_true
      else if case_false != [] then case_false
      else []
  | hd :: rest ->
      let h : ret = ret_left @ [ hd ] in
      if include_undef rest then solve_rec cnf h rest
      else if check_cnf cnf.clauses h then h
      else []
  | [] -> []

let solve cnf : ret =
  let t_base = List.init cnf.num_vars (fun _ -> L_Undef) in
  solve_rec cnf [] t_base
(* let t = [ L_False; L_True ] in
   let r = check_cnf cnf.clauses t in
   if r then t else [] *)
