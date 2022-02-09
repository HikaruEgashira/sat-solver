open Sat_solver

let test =
  let cnf = Generator.generate_cnf 10 10 10 |> Util.debug_cnf in
  let ret = Solver_breadth_first_search.solve cnf in
  Printer.print_ret ret
