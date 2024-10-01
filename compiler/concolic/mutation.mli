open Shared_ast

type ('e, 'c, 't) mutation_type =
  excepts:((yes, 'e, 'c) interpr_kind, 't) gexpr boxed list
  -> just:((yes, 'e, 'c) interpr_kind, 't) gexpr boxed
  -> cons:((yes, 'e, 'c) interpr_kind, 't) gexpr boxed
  -> 't mark
  -> ((yes, 'e, 'c) interpr_kind, 't) gexpr boxed

val remove_excepts : float -> ('e, 'c, 't) mutation_type

val one_mutation : float -> ('e, 'c, 't) mutation_type -> ((yes, 'e, 'c) interpr_kind, 't) gexpr -> ((yes, 'e, 'c) interpr_kind, 't) gexpr boxed
