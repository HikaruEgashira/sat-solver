open Sat_solver.Parser
open Sat_solver.Printer

let input_file = "../cnf/lesson1.dimacs"

let () =
  (* ファイルからCNFを生成 *)
  let channel = open_in input_file in
  let cnf = parse_cnf channel in
  close_in channel;

  (* solve *)
  let ret1 = Sat_solver.Solver1.solve cnf in
  let ret2 = Sat_solver.Solver2.solve cnf in

  (* 出力 *)
  print_ret ret1;
  print_ret ret2
