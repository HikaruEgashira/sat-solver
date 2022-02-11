open Domain

module type Solver = sig
  val solve : cnf -> ret
end

module DFS : Solver = struct
  let solve = Solver_dfs.solve
end

module DPLL : Solver = struct
  let solve = Solver_dpll.solve
end
