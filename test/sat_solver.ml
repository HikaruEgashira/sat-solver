open Sat_solver.Solver
open Sat_solver.Parser
open Sat_solver.Printer

let input_file = ref "../cnf/lesson1.dimacs"

let () =
  (* ファイルからCNFを生成 *)
  let channel = open_in input_file.contents in
  let cnf = parse_cnf channel in
  close_in channel;

  (* solve *)
  let ret = solve cnf in

  (* 出力 *)
  print_ret ret
