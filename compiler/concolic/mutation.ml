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
        let len = List.length excepts in
        let exc = choose_in_list excepts 1 |> List.hd in
          if Global.options.debug then Message.debug "[mutation] Duplicating 1/%n excepts" len;
          incr duplicate_excepts_n;
          exc :: excepts
        in
        let excepts = List.map Expr.rebox excepts in
        let just = Expr.rebox just in
        let cons = Expr.rebox cons in
        Expr.edefault ~excepts ~just ~cons m
      end
    | _ -> e

let total_excepts_n = ref 0
let apply_mutations (type e c) (mutations: ((e, c, 't) mutation_type * float) list) expr =
  let op = Fun.id in
  let rec f : ((yes, e, c) interpr_kind, 't) gexpr -> ((yes, e, c) interpr_kind, 't) gexpr boxed = function
    | (EDefault {excepts ; just ; cons}, m) as e ->
        total_excepts_n := !total_excepts_n + List.length (excepts);
        if Global.options.debug then Message.debug "[mutation] looking at expression %a (%n)" (Print.expr ()) e (List.length excepts);
        let excepts = List.map (Expr.map ~op ~f) excepts in
        let just = Expr.map ~op ~f just in
        let cons = Expr.map ~op ~f cons in
        List.fold_left (
          fun acc (mutation, p) ->
            if random p then begin
              if Global.options.debug then Message.debug "[mutation] mutating expression %a" (Print.expr ()) e;
              mutation acc end
              else acc
          ) (Expr.edefault ~excepts ~just ~cons m) mutations
    | _ as e -> Expr.map ~op ~f e
  in f expr


