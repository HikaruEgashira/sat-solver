let xor (a : bool) (b : bool) = a == b

let debug_str str : string =
  let () = print_endline str in
  str

let debug_int int : int =
  let () = int |> string_of_int |> print_endline in
  int

let debug_bool bool : bool =
  let () = bool |> string_of_bool |> print_endline in
  bool