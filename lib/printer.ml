(* Util: separator で結合 *)
let join_list list =
  list |> List.fold_left (fun s x -> s ^ " " ^ x) "" |> String.trim

let string_of_t t =
  match t with Some true -> "True" | Some false -> "False" | None -> "Undef"

let string_of_ret ret = ret |> List.map string_of_t |> join_list

let print_ret ret =
  let is_unsat = List.exists (fun x -> x == None) ret || ret == [] in
  match is_unsat with
  | true -> print_endline "UNSAT"
  | false ->
      print_endline "SAT";
      print_endline (string_of_ret ret)
