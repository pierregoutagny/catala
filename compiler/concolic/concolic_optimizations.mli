open Path_constraint

type flag =
  | OTrivial | OLazyDefault
  | OIncrementalSolver
  | OSoftConstraints
  | OTestsVTime
  | OTimeoutRetry
  | OMutationRemove | OMutationDuplicate | OMutationNegateJusts

val optim_list : (string * flag) list
(** Used for command line arguments *)


val lazy_default : flag list -> bool

val incremental_solver : flag list -> bool

val soft_constraints : flag list -> bool

val tests_vs_time : flag list -> bool

val timeout_retry : flag list -> bool


val mutation_remove : flag list -> bool

val mutation_duplicate : flag list -> bool

val mutation_negate_justs : flag list -> bool

val mutation : flag list -> bool


val remove_trivial_constraints :
  flag list -> PathConstraint.naked_path -> PathConstraint.naked_path
