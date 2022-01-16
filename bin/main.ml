open Parser
open Printer
open Solver

exception Invalid_format

let file = "./cnf/lesson1.dimacs"

let () =
  (* ファイルからCNFを生成 *)
  let channel = open_in file in
  let cnf = parse_cnf channel in
  close_in channel;

  (* solve *)
  let ret = solve cnf in

  (* 出力 *)
  print_ret ret
