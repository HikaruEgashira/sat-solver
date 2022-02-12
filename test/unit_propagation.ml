open Sat_solver

let _ =
  let channel = open_in "../cnf/lesson1.dimacs" in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  (* cnf に outputを割り当ててみる *)
  let output = [ Some false; Some false ] in
  let new_clause_list = Solver_dpll.assign_cnf cnf.clauses output in

  (* Trueなので空配列に *)
  new_clause_list = Some []
