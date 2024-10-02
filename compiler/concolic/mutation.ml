open Shared_ast
open Catala_utils

let _ = Random.self_init ()

let choose_in_list l n =
  if l = [] || n = 0 then [] else
  let len = List.length l in
  List.init n (fun _ -> List.nth l (Random.int len))

let random p =
  let f = Random.float 1.0 in
(*   Message.result "random %.2f %.2f" f p; *)
  f < p

type ('e, 'c, 't) mutation_type = ((yes, 'e, 'c) interpr_kind, 't) gexpr boxed -> ((yes, 'e, 'c) interpr_kind, 't) gexpr boxed

let remove_excepts_n = ref 0
let remove_excepts (p: float) : ('e, 'c, 't) mutation_type =
  fun e -> match Expr.unbox e with
    | (EDefault {excepts ; just ; cons }, m) -> begin
        let len = List.length excepts in
        let excepts = List.filter_map (fun e -> if random p then Some (Expr.rebox e) else None) excepts in
        let just = Expr.rebox just in
        let cons = Expr.rebox cons in
        let diff = len - List.length excepts in
        if Global.options.debug then Message.debug "[mutation] Removing %n/%n excepts" diff len;
        remove_excepts_n := !remove_excepts_n + diff;
        Expr.edefault ~excepts ~just ~cons m
      end
    | _ -> e

let duplicate_excepts_n = ref 0
let duplicate_excepts : ('e, 'c, 't) mutation_type =
  fun e -> match Expr.unbox e with
    | (EDefault {excepts ; just ; cons }, m) -> begin
      let excepts = if excepts = [] then excepts else
        let exc = choose_in_list excepts 1 |> List.hd in
          if Global.options.debug then Message.debug "[mutation] Duplicating except %a" (Print.expr ()) exc;
          incr duplicate_excepts_n;
          exc :: excepts
        in
        let excepts = List.map Expr.rebox excepts in
        let just = Expr.rebox just in
        let cons = Expr.rebox cons in
        Expr.edefault ~excepts ~just ~cons m
      end
    | _ -> e

let negate_justs_n = ref 0
let negate_justs : ('e, 'c, 't) mutation_type =
  fun e -> match Expr.unbox e with
    | (EDefault {excepts ; just ; cons }, m) -> begin
        let excepts = List.map Expr.rebox excepts in
        let just = Expr.rebox just in
        let not_just = Expr.eappop ~op:Operator.Not ~args:[just] ~tys:[TLit TBool, Expr.mark_pos m] m in
        let cons = Expr.rebox cons in
        if Global.options.debug then Message.debug "[mutation] Negating just";
        incr negate_justs_n;
        Expr.edefault ~excepts ~just:not_just ~cons m
      end
    | _ -> e

let apply_mutations (type e c) (mutations: ((e, c, 't) mutation_type * float) list) expr =
  let op = Fun.id in
  let rec f : ((yes, e, c) interpr_kind, 't) gexpr -> ((yes, e, c) interpr_kind, 't) gexpr boxed = function
    | (EDefault {excepts ; just ; cons}, m) as e ->
        if Global.options.debug then Message.debug "[mutation] looking at expression %a (%n)" (Print.expr ()) e (List.length excepts);
        let excepts = List.map f excepts in
        let just = f just in
        let cons = f cons in
        List.fold_left (
          fun acc (mutation, p) ->
            if random p then begin
              if Global.options.debug then Message.debug "[mutation] mutating expression %a" (Print.expr ()) e;
              mutation acc end
              else acc
          ) (Expr.edefault ~excepts ~just ~cons m) mutations
    | _ as e -> Expr.map ~op ~f e
  in f expr


type ast_stats_t = {
  mutable defaults : int;
  mutable defaults_with_excepts : int;
  mutable excepts : int;
  (* ifs, nb of except by default, asserts *)
}

let get_stats expr =
  let stats = { defaults = 0 ; defaults_with_excepts = 0 ; excepts = 0 } in
  let op = Fun.id in
  let rec f : ((yes, 'e, 'c) interpr_kind, 't) gexpr -> ((yes, 'e, 'c) interpr_kind, 't) gexpr boxed = function
    | (EDefault {excepts ; _}, _) as e ->
        stats.excepts <- stats.excepts + List.length (excepts);
        stats.defaults <- stats.defaults + 1;
        if excepts <> [] then stats.defaults_with_excepts <- stats.defaults_with_excepts + 1;
        Expr.map ~op ~f e
    | _ as e -> Expr.map ~op ~f e
  in
  let _ = f expr in
  stats

let mutate_default_at_index (type e c)
  (mutation: (e, c, 't) mutation_type)
  (only_with_excepts: bool) (goal: int)
  expr =
    let op = Fun.id in
    let i = ref 0 in
    let rec f : ((yes, e, c) interpr_kind, 't) gexpr -> ((yes, e, c) interpr_kind, 't) gexpr boxed = function
      | (EDefault {excepts ; just ; cons}, m) as e ->
          if excepts <> [] || not only_with_excepts then incr i;
          if !i = goal then begin
            if Global.options.debug then Message.debug "[mutation] looking at expression %a (%n)" (Print.expr ()) e (List.length excepts);
            let excepts = List.map Expr.rebox excepts in
            let just = Expr.rebox just in
            let cons = Expr.rebox cons in
            mutation (Expr.edefault ~excepts ~just ~cons m)
          end
          else Expr.map ~op ~f e
      | _ as e -> Expr.map ~op ~f e
    in f expr

let create_one_conflict (expr: ((yes, 'e, 'c) interpr_kind, 't) gexpr) :
  ((yes, 'e, 'c) interpr_kind, 't) gexpr boxed =
  let stats = get_stats expr in
  let goal = Random.int stats.defaults_with_excepts + 1 in
  mutate_default_at_index duplicate_excepts true goal expr


