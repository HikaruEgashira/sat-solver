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
        | Some true -> "True"
        | Some false -> "False"
        | None -> "Undef"
      in
      let () = print_endline str in
      v :: debug_ret l

let include_undef : ret -> bool = List.exists (fun v -> v == None)

let bool_of_t t =
  match t with Some true -> true | Some false -> false | None -> false

let is_literal_sat lit (ret : ret) =
  let lit_label = get_label lit |> List.nth ret in
  let lit_bool_a = get_bool lit in
  let lit_bool_b = bool_of_t lit_label in
  not (xor lit_bool_a lit_bool_b)

let t_of_lit lit (ret : ret) : t =
  let lit_label = get_label lit |> List.nth ret in
  let lit_bool_a = get_bool lit in
  Option.map (fun b -> xor lit_bool_a b) lit_label
