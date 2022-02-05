open Domain

(* Util: separator で結合 *)
let list_join list =
  list |> List.fold_right (fun s x -> s ^ " " ^ x) |> fun x -> x " "

let string_of_ret ret =
  ret
  |> List.map (fun x ->
         match x with
         | L_True -> "True"
         | L_False -> "False"
         | L_Undef -> "Undef")
  |> list_join

let print_ret ret =
  let is_unsat = List.exists (fun x -> x = L_Undef) ret || ret == [] in
  match is_unsat with
  | true -> print_endline "UNSAT"
  | false ->
      print_endline "SAT";
      print_endline (string_of_ret ret)
