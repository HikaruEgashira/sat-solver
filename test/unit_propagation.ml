open Sat_solver

let _ =
  let channel = open_in "../cnf/lesson1.dimacs" in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let ret = [ Some false; Some false ] in
  let new_clause_list = Solver_unit_propagation.assign_cnf cnf.clauses ret in
  new_clause_list = []
