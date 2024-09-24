open Path_constraint.PathConstraint

type flag = | OTrivial
            | OLazyDefault
            | OIncrementalSolver
            | OSoftConstraints

let optim_list = [
  "trivial", OTrivial;
  "lazy-default", OLazyDefault;
  "incremental", OIncrementalSolver;
  "soft", OSoftConstraints;
]
let trivial : flag list -> bool = List.mem OTrivial
let lazy_default : flag list -> bool = List.mem OLazyDefault
let incremental_solver : flag list -> bool = List.mem OIncrementalSolver
let soft_constraints : flag list -> bool = List.mem OSoftConstraints

let remove_trivial_constraints opt (pcs : naked_path) : naked_path =
  if not (trivial opt) then pcs
  else begin
    let f pc =
      match pc.expr with Pc_z3 s -> not (Z3.Boolean.is_true s) | _ -> true
    in
    List.filter f pcs
  end

