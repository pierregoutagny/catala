open Path_constraint.PathConstraint

type flag = | OTrivial
            | OLazyDefault
            | OIncrementalSolver
            | OSoftConstraints
            | OTestsVTime
            | OTimeoutRetry
            | OMutationRemove
            | OMutationDuplicate
            | OMutationNegateJusts

let optim_list = [
  "trivial", OTrivial;
  "lazy-default", OLazyDefault;
  "incremental", OIncrementalSolver;
  "soft", OSoftConstraints;
  "tests-vs-time", OTestsVTime;
  "no-timeout-retry", OTimeoutRetry;
  "mutation-remove", OMutationRemove;
  "mutation-duplicate", OMutationDuplicate;
  "mutation-negate-justs", OMutationNegateJusts;
]
let trivial : flag list -> bool = List.mem OTrivial
let lazy_default : flag list -> bool = List.mem OLazyDefault
let incremental_solver : flag list -> bool = List.mem OIncrementalSolver
let soft_constraints : flag list -> bool = List.mem OSoftConstraints
let tests_vs_time : flag list -> bool = List.mem OTestsVTime
let timeout_retry (l: flag list) : bool = not (List.mem OTimeoutRetry l) (* This option is active by default *)
let mutation_remove : flag list -> bool = List.mem OMutationRemove
let mutation_duplicate : flag list -> bool = List.mem OMutationDuplicate
let mutation_negate_justs : flag list -> bool = List.mem OMutationNegateJusts

let mutation flags = mutation_remove flags || mutation_duplicate flags || mutation_negate_justs flags

let remove_trivial_constraints opt (pcs : naked_path) : naked_path =
  if not (trivial opt) then pcs
  else begin
    let f pc =
      match pc.expr with Pc_z3 s -> not (Z3.Boolean.is_true s) | _ -> true
    in
    List.filter f pcs
  end

