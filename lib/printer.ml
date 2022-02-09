open Util

let string_of_t t =
  match t with Some true -> "True" | Some false -> "False" | None -> "Undef"

let string_of_ret ret = ret |> List.map string_of_t |> join_list_by " "

let print_ret ret =
  let is_unsat = List.exists (fun x -> x == None) ret || ret == [] in
  match is_unsat with
  | true -> print_endline "UNSAT"
  | false ->
      print_endline "SAT";
      print_endline (string_of_ret ret)
