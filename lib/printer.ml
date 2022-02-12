open Domain
open Util

let string_of_t (t : t) =
  match t with Some true -> "True" | Some false -> "False" | None -> "Undef"

let string_of_output (output : output) =
  match output with
  | None -> ""
  | Some v -> v |> List.map string_of_t |> join_list_by " "

let print_output output =
  let is_unsat =
    match output with None -> true | Some v -> v |> List.exists Option.is_none
  in
  match is_unsat with
  | true -> print_endline "UNSAT"
  | false ->
      print_endline "SAT";
      print_endline (string_of_output output)
