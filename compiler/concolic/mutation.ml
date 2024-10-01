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

type ('e, 'c, 't) mutation_type =
  excepts:((yes, 'e, 'c) interpr_kind, 't) gexpr boxed list
  -> just:((yes, 'e, 'c) interpr_kind, 't) gexpr boxed
  -> cons:((yes, 'e, 'c) interpr_kind, 't) gexpr boxed
  -> 't mark
  -> ((yes, 'e, 'c) interpr_kind, 't) gexpr boxed

let remove_excepts (p: float) : ('e, 'c, 't) mutation_type =
  fun ~excepts ~just ~cons m ->
  let len = List.length excepts in
  let excepts = List.filter (fun _ -> random p) excepts in
  if Global.options.debug then Message.debug "[mutation] Removing %n/%n excepts" (len - List.length excepts) len;
  Expr.edefault ~excepts ~just ~cons m

let duplicate_excepts : ('e, 'c, 't) mutation_type =
  fun ~excepts ~just ~cons m ->
  let excepts = if excepts = [] then excepts else
    let len = List.length excepts in
    let exc = choose_in_list excepts 1 |> List.hd in
    if Global.options.debug then Message.debug "[mutation] Duplicating 1/%n excepts" len;
    exc :: excepts
  in
  Expr.edefault ~excepts ~just ~cons m

let one_mutation (type e c) (p: float) mutation expr =
  let op = Fun.id in
  let rec f : ((yes, e, c) interpr_kind, 't) gexpr -> ((yes, e, c) interpr_kind, 't) gexpr boxed = function
    | (EDefault {excepts; just ; cons}, m) as e ->
        if Global.options.debug then Message.debug "[mutation] looking at expression %a (%n)" (Print.expr ()) e (List.length excepts);
        let excepts = List.map (Expr.map ~op ~f) excepts in
        let just = Expr.map ~op ~f just in
        let cons = Expr.map ~op ~f cons in
        if random p || List.length excepts > 2
        then begin
          if Global.options.debug then Message.debug "[mutation] mutating expression %a" (Print.expr ()) e;
(*           Expr.edefault ~excepts:[] ~just ~cons m *)
          mutation ~excepts ~just ~cons m
        end
        else Expr.edefault ~excepts ~just ~cons m
    | _ as e -> Expr.map ~op ~f e
  in f expr


