open Domain

let xor (a : bool) (b : bool) = a != b

let debug_str str : string =
  let () = print_endline str in
  str

let debug_int int : int =
  let () = int |> string_of_int |> print_endline in
  int

let debug_bool bool : bool =
  let () = bool |> string_of_bool |> print_endline in
  bool

let rec debug_ret (t : ret) : ret =
  match t with
  | [] ->
      let () = print_endline "" in
      []
  | v :: l ->
      let str =
        match v with
        | L_True -> "true"
        | L_False -> "false"
        | L_Undef -> "undef"
      in
      let () = print_endline str in
      v :: debug_ret l

let include_undef : ret -> bool = List.exists (fun v -> v == L_Undef)
