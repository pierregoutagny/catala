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
            | OMutationOneConflict

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
  "mutation-one-conflict", OMutationOneConflict;
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
let mutation_one_conflict : flag list -> bool = List.mem OMutationOneConflict

let random_mutations flags = mutation_remove flags || mutation_duplicate flags || mutation_negate_justs flags
let one_mutation flags = mutation_one_conflict flags
let mutation flags = random_mutations flags || one_mutation flags

let check_optims_coherent optims =
  if soft_constraints optims && not @@ incremental_solver optims then Catala_utils.Message.warning "[CONC] Soft constraints are enabled without incremental solver. This won't do anything."

let remove_trivial_constraints opt (pcs : naked_path) : naked_path =
  if not (trivial opt) then pcs
  else begin
    let f pc =
      match pc.expr with Pc_z3 s -> not (Z3.Boolean.is_true s) | _ -> true
    in
    List.filter f pcs
  end

