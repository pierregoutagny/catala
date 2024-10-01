open Path_constraint.PathConstraint

type flag = | OTrivial
            | OLazyDefault
            | OIncrementalSolver
            | OSoftConstraints
            | OTestsVTime
            | OMutationRemove
            | OMutationDuplicate

let optim_list = [
  "trivial", OTrivial;
  "lazy-default", OLazyDefault;
  "incremental", OIncrementalSolver;
  "soft", OSoftConstraints;
  "tests-vs-time", OTestsVTime;
  "mutation-remove", OMutationRemove;
  "mutation-duplicate", OMutationDuplicate;
]
let trivial : flag list -> bool = List.mem OTrivial
let lazy_default : flag list -> bool = List.mem OLazyDefault
let incremental_solver : flag list -> bool = List.mem OIncrementalSolver
let soft_constraints : flag list -> bool = List.mem OSoftConstraints
let tests_vs_time : flag list -> bool = List.mem OTestsVTime
let mutation_remove : flag list -> bool = List.mem OMutationRemove
let mutation_duplicate : flag list -> bool = List.mem OMutationDuplicate

let mutation flags = mutation_remove flags || mutation_duplicate flags

let remove_trivial_constraints opt (pcs : naked_path) : naked_path =
  if not (trivial opt) then pcs
  else begin
    let f pc =
      match pc.expr with Pc_z3 s -> not (Z3.Boolean.is_true s) | _ -> true
    in
    List.filter f pcs
  end

