open Domain

let xor a b = if a then not b else b

let join_list_by sep list =
  list |> List.fold_left (fun s x -> s ^ sep ^ x) "" |> String.trim

let debug_str v =
  v |> print_endline;
  v

let debug_int v =
  v |> string_of_int |> print_endline;
  v

let debug_bool v =
  v |> string_of_bool |> print_endline;
  v

let rec debug_ret (ret : ret) : ret =
  match ret with
  | [] ->
      print_endline "";
      []
  | v :: l ->
      let str =
        match v with
        | Some true -> "True"
        | Some false -> "False"
        | None -> "Undef"
      in
      print_endline str;
      v :: debug_ret l

let string_of_lit lit = string_of_int lit.var

let string_of_caluse clause =
  clause.lits |> List.map string_of_lit |> join_list_by " "

let debug_cnf cnf =
  cnf.num_vars |> string_of_int |> print_endline;
  cnf.clauses |> List.map string_of_caluse |> join_list_by "\n" |> print_endline;
  cnf

let bool_of_t t = match t with Some v -> v | None -> false

let t_of_lit lit (ret : ret) : t =
  let lit_label = get_label lit |> List.nth ret in
  let lit_bool_a = get_bool lit in
  Option.map (fun b -> xor lit_bool_a b) lit_label

let is_equal ret1 ret2 =
  List.combine ret1 ret2 |> List.for_all (fun (x, y) -> x == y)
