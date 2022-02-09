open Sat_solver

let test =
  let input_file = "../cnf/lesson1.dimacs" in
  let channel = open_in input_file in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let ret = Solver_unit_propagation.solve cnf in
  Printer.print_ret ret
