open Sat_solver

let test1 =
  QCheck.Test.make ~name:"unit_propagation should same as breadth_first_search"
    QCheck.(triple (1 -- 20) (1 -- 50) (1 -- 50))
    (fun (v, l, c) ->
      let cnf = Generator.generate_cnf v l c in
      let ret1 = Solver_breadth_first_search.solve cnf in
      let ret2 = Solver_unit_propagation.solve cnf in
      ret1 = ret1 && ret2 = ret2)

let _ = QCheck_runner.run_tests [ test1 ]
