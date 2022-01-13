open Parser
open Printer
open Solver

exception Invalid_format

let file = "./cnf/lesson1.dimacs"

(* #main *)
let () =
  (* ファイルからCNFを生成 *)
  let channel = open_in file in
  let cnf = parse_cnf channel in

  (* solve *)
  let ret = solve cnf in

  (* 出力 *)
  print_ret ret;
  close_in channel
