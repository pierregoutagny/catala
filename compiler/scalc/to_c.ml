(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Shared_ast
module Runtime = Runtime_ocaml.Runtime
module D = Dcalc.Ast
module L = Lcalc.Ast
open Ast

let avoid_keywords (s : string) : string =
  if
    match s with
    (* list taken from
       https://learn.microsoft.com/en-us/cpp/c-language/c-keywords *)
    | "auto" | "break" | "case" | "char" | "const" | "continue" | "default"
    | "do" | "double" | "else" | "enum" | "extern" | "float" | "for" | "goto"
    | "if" | "inline" | "int" | "long" | "register" | "restrict" | "return"
    | "short" | "signed" | "sizeof" | "static" | "struct" | "switch" | "typedef"
    | "union" | "unsigned" | "void" | "volatile" | "while" ->
      true
    | _ -> false
  then s ^ "_"
  else s

let format_struct_name (fmt : Format.formatter) (v : StructName.t) : unit =
  Format.fprintf fmt "%s"
    (Format.asprintf "%a_struct" StructName.format v
    |> String.to_ascii
    |> String.to_snake_case
    |> avoid_keywords)

let format_struct_field_name (fmt : Format.formatter) (v : StructField.t) : unit
    =
  Format.fprintf fmt "%s"
    (Format.asprintf "%a_field" StructField.format v
    |> String.to_ascii
    |> String.to_snake_case
    |> avoid_keywords)

let format_enum_name (fmt : Format.formatter) (v : EnumName.t) : unit =
  Format.fprintf fmt "%s_enum"
    (Format.asprintf "%a" EnumName.format v
    |> String.to_ascii
    |> String.to_snake_case
    |> avoid_keywords)

let format_enum_cons_name (fmt : Format.formatter) (v : EnumConstructor.t) :
    unit =
  Format.fprintf fmt "%s_cons"
    (Format.asprintf "%a" EnumConstructor.format v
    |> String.to_ascii
    |> String.to_snake_case
    |> avoid_keywords)

let format_name_cleaned (fmt : Format.formatter) (s : string) : unit =
  s
  |> String.to_ascii
  |> String.to_snake_case
  |> Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\\.") ~subst:(fun _ -> "_dot_")
  |> String.to_ascii
  |> avoid_keywords
  |> Format.fprintf fmt "%s"

let format_func_name (fmt : Format.formatter) (v : FuncName.t) : unit =
  let v_str = Mark.remove (FuncName.get_info v) in
  Format.fprintf fmt "%a_func" format_name_cleaned v_str

module StringMap = String.Map

module IntMap = Map.Make (struct
  include Int

  let format ppf i = Format.pp_print_int ppf i
end)

(** For each `VarName.t` defined by its string and then by its hash, we keep
    track of which local integer id we've given it. This is used to keep
    variable naming with low indices rather than one global counter for all
    variables. TODO: should be removed when
    https://github.com/CatalaLang/catala/issues/240 is fixed. *)
let string_counter_map : int IntMap.t StringMap.t ref = ref StringMap.empty

let format_var (fmt : Format.formatter) (v : VarName.t) : unit =
  let v_str = Mark.remove (VarName.get_info v) in
  let hash = VarName.hash v in
  let local_id =
    match StringMap.find_opt v_str !string_counter_map with
    | Some ids -> (
      match IntMap.find_opt hash ids with
      | None ->
        let max_id =
          snd
            (List.hd
               (List.fast_sort
                  (fun (_, x) (_, y) -> Int.compare y x)
                  (IntMap.bindings ids)))
        in
        string_counter_map :=
          StringMap.add v_str
            (IntMap.add hash (max_id + 1) ids)
            !string_counter_map;
        max_id + 1
      | Some local_id -> local_id)
    | None ->
      string_counter_map :=
        StringMap.add v_str (IntMap.singleton hash 0) !string_counter_map;
      0
  in
  if v_str = "_" then Format.fprintf fmt "dummy_var"
    (* special case for the unit pattern TODO escape dummy_var *)
  else if local_id = 0 then format_name_cleaned fmt v_str
  else Format.fprintf fmt "%a_%d" format_name_cleaned v_str local_id

module TypMap = Map.Make (struct
  type t = naked_typ

  let compare x y = Type.compare (x, Pos.no_pos) (y, Pos.no_pos)
  let format fmt x = Print.typ_debug fmt (x, Pos.no_pos)
end)

(* Here, [element_name] is the struct field, union member or function parameter
   of which you're printing the type. *)
let rec format_typ
    (decl_ctx : decl_ctx)
    (element_name : Format.formatter -> unit)
    (fmt : Format.formatter)
    (typ : typ) : unit =
  match Mark.remove typ with
  | TLit TUnit -> Format.fprintf fmt "void* /* unit */ %t" element_name
  | TLit TMoney -> Format.fprintf fmt "int /* money */ %t" element_name
  | TLit TInt -> Format.fprintf fmt "int %t" element_name
  | TLit TRat -> Format.fprintf fmt "double %t" element_name
  | TLit TDate -> Format.fprintf fmt "double %t" element_name
  | TLit TDuration -> Format.fprintf fmt "double %t" element_name
  | TLit TBool -> Format.fprintf fmt "char /* bool */ %t" element_name
  | TTuple ts ->
    Format.fprintf fmt "@[<v 2>struct {@,%a @]@,}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (t, i) ->
           Format.fprintf fmt "%a;"
             (format_typ decl_ctx (fun fmt -> Format.fprintf fmt "arg_%d" i))
             t))
      (List.mapi (fun x y -> y, x) ts)
  | TStruct s -> Format.fprintf fmt "%a %t" format_struct_name s element_name
  | TOption _ ->
    Message.raise_internal_error
      "All option types should have been monomorphized before compilation to C."
  | TDefault t -> format_typ decl_ctx element_name fmt t
  | TEnum e -> Format.fprintf fmt "%a %t" format_enum_name e element_name
  | TArrow (t1, t2) ->
    Format.fprintf fmt "%a(%a)"
      (format_typ decl_ctx (fun fmt -> Format.fprintf fmt "(*%t)" element_name))
      t2
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt (i, t1_arg) ->
           (format_typ decl_ctx (fun fmt -> Format.fprintf fmt "arg_%d_typ" i))
             fmt t1_arg))
      (List.mapi (fun x y -> x, y) t1)
  | TArray t1 ->
    (format_typ decl_ctx (fun fmt -> Format.fprintf fmt "* %t" element_name))
      fmt t1
  | TAny -> Format.fprintf fmt "void * /* any */ %t" element_name
  | TClosureEnv -> Format.fprintf fmt "void * /* closure_env */ %t" element_name

let format_ctx
    (type_ordering : Scopelang.Dependency.TVertex.t list)
    (fmt : Format.formatter)
    (ctx : decl_ctx) : unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    let fields = StructField.Map.bindings struct_fields in
    Format.fprintf fmt "@[<v 2>typedef struct %a {@ %a@]@,} %a;"
      format_struct_name struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (struct_field, struct_field_type) ->
           Format.fprintf fmt "@[<v>%a;@]"
             (format_typ ctx (fun fmt ->
                  format_struct_field_name fmt struct_field))
             struct_field_type))
      fields format_struct_name struct_name
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if EnumConstructor.Map.is_empty enum_cons then
      failwith "no constructors in the enum"
    else
      Format.fprintf fmt "@[<v 2>enum %a_code {@,%a@]@,} %a_code;@\n@\n"
        format_enum_name enum_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (enum_cons, _) ->
             Format.fprintf fmt "%a_%a" format_enum_name enum_name
               format_enum_cons_name enum_cons))
        (EnumConstructor.Map.bindings enum_cons)
        format_enum_name enum_name;
    Format.fprintf fmt
      "@[<v 2>typedef struct %a {@ enum %a_code code;@ @[<v 2>union {@ %a@]@,\
       } payload;@]@,\
       } %a;" format_enum_name enum_name format_enum_name enum_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (enum_cons, typ) ->
           Format.fprintf fmt "%a;"
             (format_typ ctx (fun fmt -> format_enum_cons_name fmt enum_cons))
             typ))
      (EnumConstructor.Map.bindings enum_cons)
      format_enum_name enum_name
  in

  let is_in_type_ordering s =
    List.exists
      (fun struct_or_enum ->
        match struct_or_enum with
        | Scopelang.Dependency.TVertex.Enum _ -> false
        | Scopelang.Dependency.TVertex.Struct s' -> s = s')
      type_ordering
  in
  let scope_structs =
    List.map
      (fun (s, _) -> Scopelang.Dependency.TVertex.Struct s)
      (StructName.Map.bindings
         (StructName.Map.filter
            (fun s _ -> not (is_in_type_ordering s))
            ctx.ctx_structs))
  in
  List.iter
    (fun struct_or_enum ->
      match struct_or_enum with
      | Scopelang.Dependency.TVertex.Struct s ->
        Format.fprintf fmt "%a@\n@\n" format_struct_decl
          (s, StructName.Map.find s ctx.ctx_structs)
      | Scopelang.Dependency.TVertex.Enum e ->
        Format.fprintf fmt "%a@\n@\n" format_enum_decl
          (e, EnumName.Map.find e ctx.ctx_enums))
    (type_ordering @ scope_structs)

let format_lit (fmt : Format.formatter) (l : lit Mark.pos) : unit =
  match Mark.remove l with
  | LBool true -> Format.pp_print_string fmt "1 /* TRUE */"
  | LBool false -> Format.pp_print_string fmt "0 /* FALSE */"
  | LInt i -> Format.fprintf fmt "%d" (Runtime.integer_to_int i)
  | LUnit -> Format.pp_print_string fmt "NULL"
  | LRat i -> Format.fprintf fmt "%F" (Runtime.decimal_to_float i)
  | LMoney e -> Format.fprintf fmt "%F" (Runtime.money_to_float e)
  | LDate d ->
    Format.fprintf fmt "catala_date_from_ymd(%d,%d,%d)"
      (Runtime.integer_to_int (Runtime.year_of_date d))
      (Runtime.integer_to_int (Runtime.month_number_of_date d))
      (Runtime.integer_to_int (Runtime.day_of_month_of_date d))
  | LDuration d ->
    let years, months, days = Runtime.duration_to_years_months_days d in
    Format.fprintf fmt "catala_duration_from_ymd(%d,%d,%d)" years months days

let format_op (fmt : Format.formatter) (op : operator Mark.pos) : unit =
  match Mark.remove op with
  | Log (_entry, _infos) -> assert false
  | Minus_int | Minus_rat | Minus_mon | Minus_dur ->
    Format.pp_print_string fmt "-"
  (* Todo: use the names from [Operator.name] *)
  | Not -> Format.pp_print_string fmt "!"
  | Length -> Format.pp_print_string fmt "catala_list_length"
  | ToRat_int -> Format.pp_print_string fmt "catala_decimal_from_integer"
  | ToRat_mon -> Format.pp_print_string fmt "catala_decimal_from_money"
  | ToMoney_rat -> Format.pp_print_string fmt "catala_money_from_decimal"
  | GetDay -> Format.pp_print_string fmt "catala_day_of_month_of_date"
  | GetMonth -> Format.pp_print_string fmt "catala_month_number_of_date"
  | GetYear -> Format.pp_print_string fmt "catala_year_of_date"
  | FirstDayOfMonth ->
    Format.pp_print_string fmt "catala_date_first_day_of_month"
  | LastDayOfMonth -> Format.pp_print_string fmt "catala_date_last_day_of_month"
  | Round_mon -> Format.pp_print_string fmt "catala_money_round"
  | Round_rat -> Format.pp_print_string fmt "catala_decimal_round"
  | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _ | Add_dur_dur
  | Concat ->
    Format.pp_print_string fmt "+"
  | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur
  | Sub_dur_dur ->
    Format.pp_print_string fmt "-"
  | Mult_int_int | Mult_rat_rat | Mult_mon_rat | Mult_dur_int ->
    Format.pp_print_string fmt "*"
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_rat | Div_dur_dur ->
    Format.pp_print_string fmt "/"
  | And -> Format.pp_print_string fmt "&&"
  | Or -> Format.pp_print_string fmt "||"
  | Eq -> Format.pp_print_string fmt "=="
  | Xor -> Format.pp_print_string fmt "!="
  | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur ->
    Format.pp_print_string fmt "<"
  | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat | Lte_dur_dur ->
    Format.pp_print_string fmt "<="
  | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur ->
    Format.pp_print_string fmt ">"
  | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat | Gte_dur_dur ->
    Format.pp_print_string fmt ">="
  | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dat_dat | Eq_dur_dur ->
    Format.pp_print_string fmt "=="
  | Map -> Format.pp_print_string fmt "catala_list_map"
  | Reduce -> Format.pp_print_string fmt "catala_list_reduce"
  | Filter -> Format.pp_print_string fmt "catala_list_filter"
  | Fold -> Format.pp_print_string fmt "catala_list_fold_left"
  | HandleDefault -> Format.pp_print_string fmt "catala_handle_default"
  | HandleDefaultOpt | FromClosureEnv | ToClosureEnv | Map2 ->
    failwith "unimplemented"

let _format_string_list (fmt : Format.formatter) (uids : string list) : unit =
  let sanitize_quotes = Re.compile (Re.char '"') in
  Format.fprintf fmt "c(%a)"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       (fun fmt info ->
         Format.fprintf fmt "\"%s\""
           (Re.replace sanitize_quotes ~f:(fun _ -> "\\\"") info)))
    uids

let rec format_expression (ctx : decl_ctx) (fmt : Format.formatter) (e : expr) :
    unit =
  match Mark.remove e with
  | EVar v -> format_var fmt v
  | EFunc f -> format_func_name fmt f
  | EStruct { fields = es; _ } ->
    (* These should only appear when initializing a variable definition *)
    Format.fprintf fmt "{ %a }"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (_, e) -> Format.fprintf fmt "%a" (format_expression ctx) e))
      (StructField.Map.bindings es)
  | EStructFieldAccess { e1; field; _ } ->
    Format.fprintf fmt "%a.%a" (format_expression ctx) e1
      format_struct_field_name field
  | EInj { e1; cons; name = enum_name; _ } ->
    Format.fprintf fmt "{%a_%a,@ {%a: %a}}" format_enum_name enum_name
      format_enum_cons_name cons format_enum_cons_name cons
      (format_expression ctx) e1
  | EArray _ ->
    failwith
      "should not happen, array initialization is caught at the statement level"
  | ELit l -> Format.fprintf fmt "%a" format_lit (Mark.copy e l)
  | EAppOp { op = (Map | Filter) as op; args = [arg1; arg2] } ->
    Format.fprintf fmt "%a(%a,@ %a)" format_op (op, Pos.no_pos)
      (format_expression ctx) arg1 (format_expression ctx) arg2
  | EAppOp { op; args = [arg1; arg2] } ->
    Format.fprintf fmt "(%a %a@ %a)" (format_expression ctx) arg1 format_op
      (op, Pos.no_pos) (format_expression ctx) arg2
  | EAppOp { op = Not; args = [arg1] } ->
    Format.fprintf fmt "%a %a" format_op (Not, Pos.no_pos)
      (format_expression ctx) arg1
  | EAppOp
      {
        op = (Minus_int | Minus_rat | Minus_mon | Minus_dur) as op;
        args = [arg1];
      } ->
    Format.fprintf fmt "%a %a" format_op (op, Pos.no_pos)
      (format_expression ctx) arg1
  | EAppOp { op; args = [arg1] } ->
    Format.fprintf fmt "%a(%a)" format_op (op, Pos.no_pos)
      (format_expression ctx) arg1
  | EAppOp { op = HandleDefaultOpt | HandleDefault; args = _ } ->
    failwith "should not happen because of keep_special_ops"
  | EApp { f; args } ->
    Format.fprintf fmt "%a(@[<hov 0>%a)@]" (format_expression ctx) f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | EAppOp { op; args } ->
    Format.fprintf fmt "%a(@[<hov 0>%a)@]" format_op (op, Pos.no_pos)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | ETuple _ | ETupleAccess _ ->
    Message.raise_internal_error "Tuple compilation to R unimplemented!"

let typ_is_array (ctx : decl_ctx) (typ : typ) =
  match Mark.remove typ with
  | TStruct s_name ->
    let fields = StructName.Map.find s_name ctx.ctx_structs in
    StructField.Map.exists
      (fun _ t -> match Mark.remove t with TArray _ -> true | _ -> false)
      fields
  | _ -> false

let rec format_statement
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    (s : stmt Mark.pos) : unit =
  match Mark.remove s with
  | SInnerFuncDef _ ->
    Message.raise_spanned_error (Mark.get s)
      "Internal error: this inner functions should have been hoisted in Scalc"
  | SLocalDecl { name = v; typ = ty } ->
    Format.fprintf fmt "@[<hov 2>%a@];"
      (format_typ ctx (fun fmt -> format_var fmt (Mark.remove v)))
      ty
    (* Below we detect array initializations which have special treatment. *)
  | SLocalInit { name = v; expr = EStruct { fields; name }, _; typ }
    when typ_is_array ctx typ ->
    let array_contents =
      match
        List.find
          (fun (field, _) ->
            String.equal "content" (Mark.remove (StructField.get_info field)))
          (StructField.Map.bindings fields)
      with
      | _, (EArray args, _) -> args
      | _ -> failwith "should not happen"
    in
    Format.fprintf fmt
      "@[<hov 2>%a;@]@\n\
       @[<hov 2>%a.content_field = catala_malloc(sizeof(%a));@]@\n\
       %a"
      (format_typ ctx (fun fmt -> format_var fmt (Mark.remove v)))
      typ format_var (Mark.remove v) format_struct_name name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt (i, arg) ->
           Format.fprintf fmt "@[<hov 2>%a.content_field[%d] =@ %a;@]"
             format_var (Mark.remove v) i (format_expression ctx) arg))
      (List.mapi (fun i a -> i, a) array_contents)
  | SLocalInit { name = v; expr = e; typ } ->
    Format.fprintf fmt "@[<hov 2>%a = %a;@]"
      (format_typ ctx (fun fmt -> format_var fmt (Mark.remove v)))
      typ (format_expression ctx) e
  | SLocalDef { name = v; expr = e; _ } ->
    Format.fprintf fmt "@[<hov 2>%a = %a;@]" format_var (Mark.remove v)
      (format_expression ctx) e
  | STryExcept _ -> failwith "should not happen"
  | SRaise e ->
    let pos = Mark.get s in
    Format.fprintf fmt
      "catala_fatal_error_raised.code = %s;@,\
       catala_fatal_error_raised.position.filename = \"%s\";@,\
       catala_fatal_error_raised.position.start_line = %d;@,\
       catala_fatal_error_raised.position.start_column = %d;@,\
       catala_fatal_error_raised.position.end_line = %d;@,\
       catala_fatal_error_raised.position.end_column = %d;@,\
       longjmp(catala_fatal_error_jump_buffer, 0);"
      (match e with
      | ConflictError -> "catala_conflict"
      | EmptyError -> "catala_empty"
      | NoValueProvided -> "catala_no_value_provided"
      | Crash -> "catala_crash")
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos)
  | SIfThenElse { if_expr = cond; then_block = b1; else_block = b2 } ->
    Format.fprintf fmt
      "@[<hov 2>if (%a) {@\n%a@]@\n@[<hov 2>} else {@\n%a@]@\n}"
      (format_expression ctx) cond (format_block ctx) b1 (format_block ctx) b2
  | SSwitch { switch_expr = e1; enum_name = e_name; switch_cases = cases; _ } ->
    let cases =
      List.map2
        (fun x (cons, _) -> x, cons)
        cases
        (EnumConstructor.Map.bindings (EnumName.Map.find e_name ctx.ctx_enums))
    in
    let tmp_var = VarName.fresh ("match_arg", Pos.no_pos) in
    Format.fprintf fmt "@[<hov 2>%a %a = %a;@]@\n@[<hov 2>if %a@]@\n}"
      format_enum_name e_name format_var tmp_var (format_expression ctx) e1
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@]@\n@[<hov 2>} else if ")
         (fun fmt ({ case_block; payload_var_name; payload_var_typ }, cons_name) ->
           Format.fprintf fmt "(%a.code == %a_%a) {@\n%a = %a.payload.%a;@\n%a"
             format_var tmp_var format_enum_name e_name format_enum_cons_name
             cons_name
             (format_typ ctx (fun fmt -> format_var fmt payload_var_name))
             payload_var_typ format_var tmp_var format_enum_cons_name cons_name
             (format_block ctx) case_block))
      cases
  | SReturn e1 ->
    Format.fprintf fmt "@[<hov 2>return %a;@]" (format_expression ctx)
      (e1, Mark.get s)
  | SAssert e1 ->
    let pos = Mark.get s in
    Format.fprintf fmt
      "@[<hov 2>if (!(%a)) {@\n\
       catala_fatal_error_raised.code = catala_assertion_failure;@,\
       catala_fatal_error_raised.position.filename = \"%s\";@,\
       catala_fatal_error_raised.position.start_line = %d;@,\
       catala_fatal_error_raised.position.start_column = %d;@,\
       catala_fatal_error_raised.position.end_line = %d;@,\
       catala_fatal_error_raised.position.end_column = %d;@,\
       longjmp(catala_fatal_error_jump_buffer, 0);@,\
       }"
      (format_expression ctx)
      (e1, Mark.get s)
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos)
  | SSpecialOp (OHandleDefaultOpt { exceptions; just; cons; return_typ }) ->
    let e_name =
      match Mark.remove return_typ with
      | TEnum t -> t
      | _ -> failwith "should not happen"
    in
    let option_config =
      List.map fst
        (EnumConstructor.Map.bindings (EnumName.Map.find e_name ctx.ctx_enums))
    in
    let none_cons, some_cons =
      match option_config with
      | [none_cons; some_cons] -> none_cons, some_cons
      | _ -> failwith "should not happen"
    in
    let pos = Mark.get s in
    let exception_acc_var = VarName.fresh ("exception_acc", Mark.get s) in
    let exception_current = VarName.fresh ("exception_current", Mark.get s) in
    let exception_conflict = VarName.fresh ("exception_conflict", Mark.get s) in
    let variable_defined_in_cons =
      match List.hd (List.rev cons) with
      | SReturn (EVar v), _ -> v
      | SLocalDef { name; _ }, _ | SLocalInit { name; _ }, _ -> Mark.remove name
      | _ -> failwith "should not happen"
    in
    if exceptions <> [] then begin
      Format.fprintf fmt "@[<hov 2>%a = {%a_%a,@ {%a: NULL}};@]@,"
        (format_typ ctx (fun fmt -> format_var fmt exception_acc_var))
        return_typ format_enum_name e_name format_enum_cons_name none_cons
        format_enum_cons_name none_cons;
      Format.fprintf fmt "%a;@,"
        (format_typ ctx (fun fmt -> format_var fmt exception_current))
        return_typ;
      Format.fprintf fmt "char %a = 0;@," format_var exception_conflict;
      List.iter
        (fun except ->
          Format.fprintf fmt
            "%a = %a;@,\
             @[<v 2>if (%a.code == %a_%a) {@,\
             @[<v 2>if (%a.code == %a_%a) {@,\
             %a = 1;@]@,\
             @[<v 2>} else {@,\
             %a = %a;@]@,\
             }@]@,\
             }@,"
            format_var exception_current (format_expression ctx) except
            format_var exception_current format_enum_name e_name
            format_enum_cons_name some_cons format_var exception_acc_var
            format_enum_name e_name format_enum_cons_name some_cons format_var
            exception_conflict format_var exception_acc_var format_var
            exception_current)
        exceptions;
      Format.fprintf fmt
        "@[<v 2>if (%a) {@,\
         catala_fatal_error_raised.code = catala_conflict;@,\
         catala_fatal_error_raised.position.filename = \"%s\";@,\
         catala_fatal_error_raised.position.start_line = %d;@,\
         catala_fatal_error_raised.position.start_column = %d;@,\
         catala_fatal_error_raised.position.end_line = %d;@,\
         catala_fatal_error_raised.position.end_column = %d;@,\
         longjmp(catala_fatal_error_jump_buffer, 0);@]@,\
         }@,"
        format_var exception_conflict (Pos.get_file pos)
        (Pos.get_start_line pos) (Pos.get_start_column pos)
        (Pos.get_end_line pos) (Pos.get_end_column pos);
      Format.fprintf fmt
        "@[<v 2>if (%a.code == %a_%a) {@,%a = %a;@]@,@[<v 2>} else {@,"
        format_var exception_acc_var format_enum_name e_name
        format_enum_cons_name some_cons format_var variable_defined_in_cons
        format_var exception_acc_var
    end;
    Format.fprintf fmt
      "@[<v 2>if (%a) {@,\
       %a@]@,\
       @[<v 2>} else {@,\
       %a.code = %a_%a;@,\
       %a.payload.%a = NULL;@]@,\
       }"
      (format_expression ctx) just (format_block ctx) cons format_var
      variable_defined_in_cons format_enum_name e_name format_enum_cons_name
      none_cons format_var variable_defined_in_cons format_enum_cons_name
      none_cons;
    if exceptions <> [] then Format.fprintf fmt "@]@,}"

and format_block (ctx : decl_ctx) (fmt : Format.formatter) (b : block) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    (format_statement ctx) fmt b

let format_program
    (fmt : Format.formatter)
    (p : Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) : unit =
  Format.fprintf fmt
    "@[<v>/* This file has been generated by the Catala compiler, do not edit! \
     */@,\
     @,\
     #include <stdio.h>@,\
     #include <stdlib.h>@,\
     #include <runtime.c>@,\
     @,\
     %a@,\
     %a@,\
     @]"
    (format_ctx type_ordering) p.decl_ctx
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline (fun fmt code_item ->
         match code_item with
         | SVar { var; expr; typ } ->
           Format.fprintf fmt "@[<v 2>%a = %a;@]"
             (format_typ p.decl_ctx (fun fmt -> format_var fmt var))
             typ
             (format_expression p.decl_ctx)
             expr
         | SFunc { var; func }
         | SScope { scope_body_var = var; scope_body_func = func; _ } ->
           let { func_params; func_body; func_return_typ } = func in
           Format.fprintf fmt "@[<v 2>%a(%a) {@,%a@]@,}"
             (format_typ p.decl_ctx (fun fmt -> format_func_name fmt var))
             func_return_typ
             (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
                (fun fmt (var, typ) ->
                  (format_typ p.decl_ctx (fun fmt ->
                       format_var fmt (Mark.remove var)))
                    fmt typ))
             func_params (format_block p.decl_ctx) func_body))
    p.code_items
