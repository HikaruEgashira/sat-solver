open Domain

(* Util: separator で結合 *)
let list_join separator list =
  list |> List.fold_right (fun s x -> s ^ " " ^ x) |> fun x -> x separator

let print_ret ret =
  let is_unsat = List.exists (fun x -> x = L_Undef) ret in

  match is_unsat with
  | true -> print_endline "UNSAT"
  | false ->
      let ret_str =
        ret
        |> List.map (fun x ->
               match x with
               | L_True -> "True"
               | L_False -> "False"
               | L_Undef -> "Undef")
        |> list_join " "
      in
      print_endline ("SAT\n" ^ ret_str)
