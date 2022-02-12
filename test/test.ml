open Sat_solver

let _ =
  let channel = open_in "../cnf/lesson1.dimacs" in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let ret = Solver.DFS.solve cnf in
  ret = Some [ Some false; Some true ]

let _ =
  let channel = open_in "../cnf/lesson1.dimacs" in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let ret1 = Solver.DFS.solve cnf in
  let ret2 = Solver.DPLL.solve cnf in
  ret1 = ret2

(* debug *)
let _ =
  print_endline "CNF";
  let cnf = Generator.generate_cnf 0 2 1 1 |> Util.debug_cnf in

  print_endline "\nRET";
  print_endline "1";
  let _ret1 = Solver.DFS.solve cnf |> Option.map Util.debug_ret in
  print_endline "2";
  let _ret2 = Solver.DPLL.solve cnf |> Option.map Util.debug_ret in

  print_endline "\nASSIGN"
(* Solver.DPLL.assign_cnf cnf.clauses ret1
   |> List.map Util.string_of_caluse
   |> Util.join_list_by " " |> print_endline;
   print_endline "2";
   Solver.DPLL.assign_cnf cnf.clauses ret2
   |> List.map Util.string_of_caluse
   |> Util.join_list_by " " |> print_endline *)

let test1 =
  (*  *)
  QCheck.Test.make ~name:"dpll should be same as dfs"
    QCheck.(quad int (1 -- 20) (1 -- 50) (1 -- 50))
    (fun (seed, v, l, c) ->
      let cnf = Generator.generate_cnf seed v l c in
      let ret1 = Solver.DFS.solve cnf in
      let ret2 = Solver.DPLL.solve cnf in

      if Option.is_none ret1 && Option.is_none ret2 then true
      else
        let res1 = Solver_dpll.assign_cnf cnf.clauses (Option.get ret1) in
        let res2 = Solver_dpll.assign_cnf cnf.clauses (Option.get ret2) in
        res1 = res2)

let _ = QCheck_runner.run_tests [ test1 ]
