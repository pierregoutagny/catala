open Catala_utils
open Shared_ast

(* TODO invalid_arg with Format.asprintf inside? *)

module RuntimeError = struct
  type span = string option * Pos.t
  type span_list = span list

  type runtime_error =
    | EmptyError
    | ConflictError of { spans : span_list }
    | DivisionByZeroError of { spans : span_list }  (** TODO factorize? *)
    | AssertionError

  type message = string

  type t = {
    except : runtime_error; (* TODO use actual exceptions from [Runtime]? *)
    message : message; (* TODO use formatted stuff instead *)
  }

  let make (except : runtime_error) (message : message) = { except; message }

  (* TODO use formatter *)
  let string_of_span (s : span) : string =
    let _, pos = s in
    Pos.to_string_short pos

  let string_of_spans (spans : span_list) : string =
    let span_strings = List.map string_of_span spans in
    List.fold_left (fun acc a -> a ^ "," ^ acc) "" span_strings

  let to_string { except; _ } =
    let except_string =
      match except with
      | EmptyError -> "Empty"
      | ConflictError { spans } -> "Conflict(" ^ string_of_spans spans ^ ")"
      | DivisionByZeroError { spans } ->
        "DivisionByZero(" ^ string_of_spans spans ^ ")"
      | AssertionError -> "AssertionError"
    in
    "↯" ^ except_string ^ "↯"
end

module SymbExpr = struct
  type z3_expr = Z3.Expr.expr
  type z3_symbol = Z3.Symbol.symbol
  type reentrant = { name : StructField.t; symbol : z3_expr }

  type t =
    | Symb_z3 of z3_expr
    | Symb_reentrant of reentrant
      (* only for the lambda expression corresponding to a reentrant variable *)
    | Symb_none
    | Symb_incomplete
        (* use for expressions that cannot be encoded symbolically, and depend on input variables *)
    | Symb_fallback
        (* use for expression that cannot be encoded symbolically, but don't depend on input variables, this allows the conc execution to fallback to a concrete value later *)
    | Symb_abs
    | Symb_error of RuntimeError.t
        (** TODO make sure that this can only be used on errors? *)

  (* TODO printing module *)
  let string_of_reentrant { name; _ } =
    "<" ^ Mark.remove (StructField.get_info @@ name) ^ ">"

  let to_string ?(typed : bool = false) e =
    match e with
    | Symb_z3 s ->
      let str = Z3.Expr.to_string s in
      if typed then
        "(" ^ str ^ ":" ^ (Z3.Sort.to_string @@ Z3.Expr.get_sort s) ^ ")"
      else str
    | Symb_reentrant r -> string_of_reentrant r
    | Symb_none -> "None"
    | Symb_incomplete -> "Incomplete"
    | Symb_fallback -> "Fallback"
    | Symb_abs -> "Abs"
    | Symb_error err -> RuntimeError.to_string err

  let formatter (fmt : Format.formatter) (symb_expr : t) : unit =
    Format.pp_print_string fmt (to_string symb_expr)

  let formatter_typed (fmt : Format.formatter) (symb_expr : t) : unit =
    Format.pp_print_string fmt (to_string ~typed:true symb_expr)


  let mk_z3 s = Symb_z3 s
  let mk_reentrant name symbol = Symb_reentrant { name; symbol }
  let none = Symb_none
  let incomplete = Symb_incomplete
  let fallback = Symb_fallback
  let abs = Symb_abs

  let mk_emptyerror message =
    let err = RuntimeError.(make EmptyError message) in
    Symb_error err

  let mk_conflicterror message spans =
    let open RuntimeError in
    let conflict = ConflictError { spans } in
    let err = make conflict message in
    Symb_error err

  let mk_divisionbyzeroerror message spans =
    let open RuntimeError in
    let conflict = DivisionByZeroError { spans } in
    let err = make conflict message in
    Symb_error err

  let mk_assertionerror message =
    let open RuntimeError in
    let conflict = AssertionError in
    let err = make conflict message in
    Symb_error err

  let has_variables (e: t) =
    match e with
    | Symb_z3 s -> Z3_utils.has_constants s
    | Symb_reentrant _ -> true
    | _ -> invalid_arg (Format.asprintf
      "[SymbExpr.has_variables] expected a z3 or reentrant variable, not %a" formatter e)

  let max_incomplete_fallback e1 e2 =
    match e1, e2 with
    | Symb_incomplete, _ | _, Symb_incomplete -> Symb_incomplete
    | Symb_fallback, _ -> Symb_fallback
    | _, _ -> Symb_none
        (* invalid_args (Format.asprintf "[SymbExpr.max_incomplete_fallback] expected incomplete or fallback, not %a and %a" formatter e1 formatter e2) *)

  let mk_incomplete_or_fallback (e: t) =
    if has_variables e then incomplete else fallback

  let mk_incomplete_or_fallback_list (es: t list) =
    let mk_map = List.map mk_incomplete_or_fallback es in
    let max = List.fold_left max_incomplete_fallback Symb_fallback mk_map in
    assert (max = Symb_incomplete || max = Symb_fallback);
    max

  let propagate_incomplete_fallback e (k: t -> t) =
    match e with
    | Symb_incomplete -> Symb_incomplete
    | Symb_fallback -> Symb_fallback
    | _ -> k e

  let propagate_incomplete_fallback2 e1 e2 (k: t -> t -> t) =
    (* this cannot be chained [propagate_incomplete_fallback]s because we want
       to encode the priority of [Symb_incomplete] over [Symb_fallback] *)
    match e1, e2 with
    | Symb_incomplete, _ | _, Symb_incomplete -> Symb_incomplete
    | Symb_fallback, Symb_fallback -> Symb_fallback
    | Symb_fallback, e | e, Symb_fallback -> mk_incomplete_or_fallback e
    | _, _ -> k e1 e2

  let propagate_incomplete_fallback_list l (k: t list -> t) =
    let rec aux acc max = function
      | [] -> if max = none then k (List.rev acc) else max
      | e::r -> aux (e :: acc) (max_incomplete_fallback max e) r
    in
    aux [] Symb_none l

  let apply_fallback (e: t) (e_concrete: t) : t =
    match e with
    | Symb_incomplete -> Symb_incomplete
    | Symb_fallback -> e_concrete
    | _ -> invalid_arg (Format.asprintf
      "[SymbExpr.apply_fallback] expected an incomplete or fallback, not %a" formatter e)


  let map_z3 (f : z3_expr -> z3_expr) = function
    | Symb_z3 e -> Symb_z3 (f e)
    | x -> x

  let app_z3 (f : z3_expr -> z3_expr) e =
    propagate_incomplete_fallback e @@ function
    | Symb_z3 s -> Symb_z3 (f s)
    | _ -> invalid_arg "[SymbExpr.app_z3] expected a z3 expression"

  let app2_z3 (f : z3_expr -> z3_expr -> z3_expr) e1 e2 =
    propagate_incomplete_fallback2 e1 e2 @@ fun e1 e2 ->
    match e1, e2 with
    | Symb_z3 s1, Symb_z3 s2 -> Symb_z3 (f s1 s2)
    | _ -> invalid_arg (Format.asprintf "[SymbExpr.app2_z3] expected two z3 expressions, got %a and %a" formatter e1 formatter e2)

  let applist_z3 (f : z3_expr list -> z3_expr) (l : t list) =
    let extract_z3 = function
      | Symb_z3 e -> e
      | _ -> invalid_arg "[SymbExpr.applist_z3] expected z3 expressions"
    in
    let l_z3 = List.map extract_z3 l in
    Symb_z3 (f l_z3)

  let map_none ~none = function Symb_none -> none | e -> e
  let simplify = map_z3 (fun e -> Z3.Expr.simplify e None)

end
