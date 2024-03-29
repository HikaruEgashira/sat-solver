open Sat_solver.Solver
open Sat_solver.Parser
open Sat_solver.Printer

let usage_msg = "sat_solver <cnf-file>"
let input_file = ref ""
let anon_fun filename = input_file := filename
let speclist = []

let () =
  Arg.parse speclist anon_fun usage_msg;

  (* ファイルからCNFを生成 *)
  let channel = open_in input_file.contents in
  let cnf = parse_cnf channel in
  close_in channel;

  (* solve *)
  let output = DPLL.solve cnf in

  (* 出力 *)
  print_output output
