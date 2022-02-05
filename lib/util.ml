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

let bool_of_t t =
  match t with L_True -> true | L_False -> false | L_Undef -> false

let t_of_bool b = match b with true -> L_True | false -> L_False

let check_literal lit t =
  let lit_label = get_label lit |> List.nth t in
  let lit_bool_a = get_bool lit in
  let lit_bool_b = bool_of_t lit_label in
  not (xor lit_bool_a lit_bool_b)

let t_of_lit lit t =
  let lit_label = get_label lit |> List.nth t in
  let lit_bool_a = get_bool lit in
  match lit_label with
  | L_Undef -> L_Undef
  | t ->
      let lit_bool_b = bool_of_t t in
      (not (xor lit_bool_a lit_bool_b)) |> t_of_bool
