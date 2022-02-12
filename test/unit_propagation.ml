open Sat_solver

let _ =
  let channel = open_in "../cnf/lesson1.dimacs" in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  (* cnf に retを割り当ててみる *)
  let ret = [ Some false; Some false ] in
  let new_clause_list = Solver_dpll.assign_cnf cnf.clauses ret in

  (* Trueなので空配列に *)
  new_clause_list = Some []
