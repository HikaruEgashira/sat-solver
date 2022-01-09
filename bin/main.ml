let file = "./cnf/lesson1.dimacs" (* (P1∨P2)∧(￢P1∨P2)∧(￢P1∨￢P2) *)

let rec print_file channel =
  match input_line channel with
  | line -> print_endline line; print_file channel
  | exception End_of_file -> () 

let () =
  let ic = open_in file in
  try 
    print_file ic;               (* print the file *)
    close_in ic                  (* 入力チャネルを閉じる *)
  with e ->                      (* 期待しない例外が起こったとき *)
    close_in_noerr ic;           (* 緊急にチャネルを閉じる *)
    raise e                      (* エラー終了: ファイルは閉じられるが
                                    チャネルはフラッシュされない *)

  (* 通常終了: チャネルは全てフラッシュされて閉じられる *)
