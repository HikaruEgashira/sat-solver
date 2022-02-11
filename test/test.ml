open Sat_solver

let _ =
  let input_file = "../cnf/lesson1.dimacs" in
  let channel = open_in input_file in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let ret = Solver_breadth_first_search.solve cnf in
  ret = [ Some false; Some true ]

let _ =
  let input_file = "../cnf/lesson1.dimacs" in
  let channel = open_in input_file in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let ret1 = Solver_breadth_first_search.solve cnf in
  let ret2 = Solver_unit_propagation.solve cnf in
  ret1 = ret2

let test1 =
  QCheck.Test.make ~name:"unit_propagation should same as breadth_first_search"
    QCheck.(triple (1 -- 20) (1 -- 50) (1 -- 50))
    (fun (v, l, c) ->
      let cnf = Generator.generate_cnf v l c in
      let ret1 = Solver_breadth_first_search.solve cnf in
      let ret2 = Solver_unit_propagation.solve cnf in
      if ret1 = ret2 then true (* SAT or UNSAT *)
      else
        (* SAT but not res1 = res2 *)
        let res1 = Solver_unit_propagation.assign_cnf cnf.clauses ret1 in
        let res2 = Solver_unit_propagation.assign_cnf cnf.clauses ret2 in
        res1 = res2)

let _ = QCheck_runner.run_tests [ test1 ]
