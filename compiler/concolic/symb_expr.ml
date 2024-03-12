open Catala_utils
open Shared_ast

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

  let emit { except; message } pos =
    match except with
    | EmptyError -> Message.emit_spanned_error pos "%s" message
    | ConflictError { spans } ->
      Message.emit_multispanned_error spans "%s" message
    | DivisionByZeroError { spans } ->
      Message.emit_multispanned_error spans "%s" message
    | AssertionError -> Message.emit_spanned_error pos "%s" message
end

module SymbExpr = struct
  type z3_expr = Z3.Expr.expr
  type reentrant = { name : StructField.t; symbol : z3_expr }

  type t =
    | Symb_z3 of z3_expr
    | Symb_reentrant of reentrant
      (* only for the lambda expression corresponding to a reentrant variable *)
    | Symb_none
    | Symb_error of RuntimeError.t
        (** TODO make sure that this can only be used on errors? *)

  let mk_z3 s = Symb_z3 s
  let mk_reentrant name symbol = Symb_reentrant { name; symbol }
  let none = Symb_none

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

  let map_z3 (f : z3_expr -> z3_expr) = function
    | Symb_z3 e -> Symb_z3 (f e)
    | x -> x

  let app_z3 (f : z3_expr -> z3_expr) = function
    | Symb_z3 e -> Symb_z3 (f e)
    | _ -> invalid_arg "[SymbExpr.app_z3] expected two z3 expressions"

  let app2_z3 (f : z3_expr -> z3_expr -> z3_expr) e1 e2 =
    match e1, e2 with
    | Symb_z3 e1, Symb_z3 e2 -> Symb_z3 (f e1 e2)
    | _ -> invalid_arg "[SymbExpr.app2_z3] expected two z3 expressions"

  let applist_z3 (f : z3_expr list -> z3_expr) (l : t list) =
    let extract_z3 = function
      | Symb_z3 e -> e
      | _ -> invalid_arg "[SymbExpr.applist_z3] expected z3 expressions"
    in
    let l_z3 = List.map extract_z3 l in
    Symb_z3 (f l_z3)

  let map_none ~none = function Symb_none -> none | e -> e
  let simplify = map_z3 (fun e -> Z3.Expr.simplify e None)

  let format_reentrant fmt { name; _ } =
    Format.fprintf fmt "<%s>" (Mark.remove (StructField.get_info @@ name))

  (* TODO formatter *)
  let mk_formatter ~(typed : bool) fmt e =
    match e with
    | Symb_z3 s ->
      let str = Z3.Expr.to_string s in
      if typed then
        Format.fprintf fmt "(%s:%s)" str
          (Z3.Sort.to_string @@ Z3.Expr.get_sort s)
      else Format.pp_print_string fmt str
    | Symb_reentrant r -> format_reentrant fmt r
    | Symb_none -> Format.pp_print_string fmt "None"
    | Symb_error err -> Format.pp_print_string fmt (RuntimeError.to_string err)

  let formatter : Format.formatter -> t -> unit = mk_formatter ~typed:false
  let formatter_typed : Format.formatter -> t -> unit = mk_formatter ~typed:true
end
