open Sat_solver

let _ =
  let channel = open_in "../cnf/lesson1.dimacs" in
  let cnf = Parser.parse_cnf channel in
  close_in channel;

  let output = Solver.DFS.solve cnf in
  output = Some [ Some false; Some true ]

let test1 =
  QCheck.Test.make ~name:"dpll should be same as dfs"
    QCheck.(triple (1 -- 20) (1 -- 50) (1 -- 50))
    (fun (v, l, c) ->
      let cnf = Generator.generate_cnf v l c in
      let output1 = Solver.DFS.solve cnf in
      let output2 = Solver.DPLL.solve cnf in

      if Option.is_none output1 && Option.is_none output2 then true
      else
        let res1 = Solver_dpll.assign_cnf cnf.clauses (Option.get output1) in
        let res2 = Solver_dpll.assign_cnf cnf.clauses (Option.get output2) in
        res1 = res2)

let _ = QCheck_runner.run_tests [ test1 ]

(* debug *)
(* let _ =
   print_endline "CNF";
   let cnf = Generator.generate_cnf 76 3 4 25 |> Util.debug_cnf in

   print_endline "\noutput";
   print_endline "1";
   let output1 = Solver.DFS.solve cnf |> Util.debug_output in
   print_endline "2";
   let output2 = Solver.DPLL.solve cnf |> Util.debug_output in

   print_endline "\nASSIGN";
   print_endline "1";
   (match output1 with
   | None -> print_endline "UNSAT"
   | Some ts ->
       Option.get (Solver_dpll.assign_cnf cnf.clauses ts)
       |> List.map Util.string_of_caluse
       |> Util.join_list_by " " |> print_endline);
   print_endline "2";
   match output2 with
   | None -> print_endline "UNSAT"
   | Some ts ->
       Option.get (Solver_dpll.assign_cnf cnf.clauses ts)
       |> List.map Util.string_of_caluse
       |> Util.join_list_by " " |> print_endline *)