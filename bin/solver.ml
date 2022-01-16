open Domain
open Util

let is_valid_literal lit t =
  let lit_label = get_label lit |> List.nth t in
  let lit_bool_a = get_bool lit in
  let lit_bool_b =
    match lit_label with L_False -> false | L_True -> true | L_Undef -> false
  in
  xor lit_bool_a lit_bool_b

(* TODO: List.for_all にする *)

let rec is_valid_clause lits t =
  match lits with
  | [] -> true
  | lit :: rest_lits ->
      let valid_lit = is_valid_literal lit t in
      let valid_clause = is_valid_clause rest_lits t in
      valid_lit || valid_clause

let rec is_valid_cnf cnf_clauses t =
  match cnf_clauses with
  | [] -> true
  | clause :: rest_clauses ->
      let valid_clause = is_valid_clause clause.lits t in
      let valid_cnf = is_valid_cnf rest_clauses t in
      valid_clause && valid_cnf

let solve cnf : ret =
  let t = [ L_False; L_True ] in
  if is_valid_cnf cnf.clauses t then t else []
