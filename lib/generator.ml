open Domain

let rand n = Random.int n + 1

let generate_lit num_vars : lit =
  let lit_bool = if Random.bool () then 1 else -1 in
  let lit_label = rand num_vars in
  let var = lit_label * lit_bool in
  { var }

let generate_clauses num_vars max_length_clauses : clause =
  let clause_length = rand max_length_clauses in
  let lits = List.init clause_length (fun _ -> generate_lit num_vars) in
  { lits }

let generate_cnf max_vars max_length_clause max_length_lit : cnf =
  let num_vars = rand max_vars in
  let clause_length = rand max_length_clause in
  let clauses =
    List.init clause_length (fun _ -> generate_clauses num_vars max_length_lit)
  in
  { num_vars; clauses }
