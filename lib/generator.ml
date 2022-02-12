open Domain

let rand n = Random.int n + 1

let generate_lit num_vars =
  let lit_bool = if Random.bool () then 1 else -1 in
  let lit_label = rand num_vars in
  let var = lit_label * lit_bool in
  { var }

let generate_clauses num_vars lit_length =
  let lits = List.init lit_length (fun _ -> generate_lit num_vars) in
  { lits }

let generate_cnf seed num_vars lit_length clause_length =
  Random.init seed;
  let clauses =
    List.init clause_length (fun _ -> generate_clauses num_vars lit_length)
  in
  { num_vars; clauses }
