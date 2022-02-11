open Sat_solver

let _ =
  let channel = open_in "../cnf/lesson1.dimacs" in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let ret = Solver_breadth_first_search.solve cnf in
  ret = [ Some false; Some true ]

let _ =
  let channel = open_in "../cnf/lesson1.dimacs" in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let ret1 = Solver_breadth_first_search.solve cnf in
  let ret2 = Solver_unit_propagation.solve cnf in
  ret1 = ret2

(* debug *)
(* let _ =
   print_endline "CNF";
   let cnf = Generator.generate_cnf 0 4 2 6 |> Util.debug_cnf in

   print_endline "\nRET";
   let ret1 = Solver_breadth_first_search.solve cnf |> Util.debug_ret in
   let ret2 = Solver_unit_propagation.solve cnf |> Util.debug_ret in

   print_endline "\nASSIGN\n1";
   Solver_unit_propagation.assign_cnf cnf.clauses ret1
   |> List.map Util.string_of_caluse
   |> Util.join_list_by " " |> print_endline;
   print_endline "2";
   Solver_unit_propagation.assign_cnf cnf.clauses ret2
   |> List.map Util.string_of_caluse
   |> Util.join_list_by " " |> print_endline *)

let test1 =
  QCheck.Test.make ~name:"unit_propagation should same as breadth_first_search"
    QCheck.(quad int (1 -- 20) (1 -- 50) (1 -- 50))
    (fun (seed, v, l, c) ->
      let cnf = Generator.generate_cnf seed v l c in
      let ret1 = Solver_breadth_first_search.solve cnf in
      let ret2 = Solver_unit_propagation.solve cnf in
      if ret1 = ret2 then true (* SAT or UNSAT *)
      else
        let res1 = Solver_unit_propagation.assign_cnf cnf.clauses ret1 in
        let res2 = Solver_unit_propagation.assign_cnf cnf.clauses ret2 in
        res1 = res2)

let _ = QCheck_runner.run_tests [ test1 ]
