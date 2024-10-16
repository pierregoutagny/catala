(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
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
open Ast

let needs_parens (_e : expr) : bool = false

let format_var_name (fmt : Format.formatter) (v : VarName.t) : unit =
  Format.fprintf fmt "%a_%s" VarName.format v (string_of_int (VarName.hash v))

let format_func_name (fmt : Format.formatter) (v : FuncName.t) : unit =
  Format.fprintf fmt "%a_%s" FuncName.format v (string_of_int (FuncName.hash v))

let rec format_expr
    (decl_ctx : decl_ctx)
    ?(debug : bool = false)
    (fmt : Format.formatter)
    (e : expr) : unit =
  let format_expr = format_expr decl_ctx ~debug in
  let format_with_parens (fmt : Format.formatter) (e : expr) =
    if needs_parens e then
      Format.fprintf fmt "%a%a%a" Print.punctuation "(" format_expr e
        Print.punctuation ")"
    else Format.fprintf fmt "%a" format_expr e
  in
  match Mark.remove e with
  | EVar v -> Format.fprintf fmt "%a" format_var_name v
  | EFunc v -> Format.fprintf fmt "%a" format_func_name v
  | EStruct { fields = es; name = s } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" StructName.format s
      Print.punctuation "{"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (struct_field, e) ->
           Format.fprintf fmt "%a%a%a%a %a" Print.punctuation "\""
             StructField.format struct_field Print.punctuation "\""
             Print.punctuation ":" format_expr e))
      (StructField.Map.bindings es)
      Print.punctuation "}"
  | ETuple es ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@]" Print.punctuation "()"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt e -> Format.fprintf fmt "%a" format_expr e))
      es Print.punctuation ")"
  | EArray es ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@]" Print.punctuation "["
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt e -> Format.fprintf fmt "%a" format_expr e))
      es Print.punctuation "]"
  | EStructFieldAccess { e1; field; _ } ->
    Format.fprintf fmt "%a%a%a%a%a" format_expr e1 Print.punctuation "."
      Print.punctuation "\"" StructField.format field Print.punctuation "\""
  | ETupleAccess { e1; index } ->
    Format.fprintf fmt "%a%a%a%d%a" format_expr e1 Print.punctuation "."
      Print.punctuation "\"" index Print.punctuation "\""
  | EInj { e1 = e; cons; _ } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" EnumConstructor.format cons
      format_expr e
  | ELit l -> Print.lit fmt l
  | EAppOp { op = (Map | Filter) as op; args = [arg1; arg2] } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" (Print.operator ~debug) op
      format_with_parens arg1 format_with_parens arg2
  | EAppOp { op; args = [arg1; arg2] } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" format_with_parens arg1
      (Print.operator ~debug) op format_with_parens arg2
  | EAppOp { op = Log _; args = [arg1] } when not debug ->
    Format.fprintf fmt "%a" format_with_parens arg1
  | EAppOp { op; args = [arg1] } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" (Print.operator ~debug) op
      format_with_parens arg1
  | EApp { f; args = [] } ->
    Format.fprintf fmt "@[<hov 2>%a@ ()@]" format_expr f
  | EApp { f; args } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_expr f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         format_with_parens)
      args
  | EAppOp { op; args } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" (Print.operator ~debug) op
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         format_with_parens)
      args

let rec format_statement
    (decl_ctx : decl_ctx)
    ?(debug : bool = false)
    (fmt : Format.formatter)
    (stmt : stmt Mark.pos) : unit =
  if debug then () else ();
  match Mark.remove stmt with
  | SInnerFuncDef { name; func } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@]@\n@[<v 2>  %a@]" Print.keyword
      "let func" format_var_name (Mark.remove name)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt ((name, _), typ) ->
           Format.fprintf fmt "%a%a %a@ %a%a" Print.punctuation "("
             format_var_name name Print.punctuation ":" (Print.typ decl_ctx) typ
             Print.punctuation ")"))
      func.func_params Print.punctuation "="
      (format_block decl_ctx ~debug)
      func.func_body
  | SLocalDecl { name; typ } ->
    Format.fprintf fmt "@[<hov 2>%a %a %a@ %a@]" Print.keyword "decl"
      format_var_name (Mark.remove name) Print.punctuation ":"
      (Print.typ decl_ctx) typ
  | SLocalDef { name; expr = naked_expr; _ } ->
    Format.fprintf fmt "@[<hov 2>%a %a@ %a@]" format_var_name (Mark.remove name)
      Print.punctuation "="
      (format_expr decl_ctx ~debug)
      naked_expr
  | SLocalInit { name; typ; expr = naked_expr } ->
    Format.fprintf fmt "@[<hov 2>%a %a %a %a@ %a@]" format_var_name
      (Mark.remove name) Print.punctuation ":" (Print.typ decl_ctx) typ
      Print.punctuation "="
      (format_expr decl_ctx ~debug)
      naked_expr
  | STryExcept { try_block = b_try; except; with_block = b_with } ->
    Format.fprintf fmt "@[<v 2>%a%a@ %a@]@\n@[<v 2>%a %a%a@ %a@]" Print.keyword
      "try" Print.punctuation ":"
      (format_block decl_ctx ~debug)
      b_try Print.keyword "with" Print.except except Print.punctuation ":"
      (format_block decl_ctx ~debug)
      b_with
  | SRaise except ->
    Format.fprintf fmt "@[<hov 2>%a %a@]" Print.keyword "raise" Print.except
      except
  | SIfThenElse { if_expr = e_if; then_block = b_true; else_block = b_false } ->
    Format.fprintf fmt "@[<v 2>%a @[<hov 2>%a@]%a@ %a@ @]@[<v 2>%a%a@ %a@]"
      Print.keyword "if"
      (format_expr decl_ctx ~debug)
      e_if Print.punctuation ":"
      (format_block decl_ctx ~debug)
      b_true Print.keyword "else" Print.punctuation ":"
      (format_block decl_ctx ~debug)
      b_false
  | SReturn ret ->
    Format.fprintf fmt "@[<hov 2>%a %a@]" Print.keyword "return"
      (format_expr decl_ctx ~debug)
      (ret, Mark.get stmt)
  | SAssert naked_expr ->
    Format.fprintf fmt "@[<hov 2>%a %a@]" Print.keyword "assert"
      (format_expr decl_ctx ~debug)
      (naked_expr, Mark.get stmt)
  | SSwitch { switch_expr = e_switch; enum_name = enum; switch_cases = arms; _ }
    ->
    let cons = EnumName.Map.find enum decl_ctx.ctx_enums in
    Format.fprintf fmt "@[<v 0>%a @[<hov 2>%a@]%a@,@]%a" Print.keyword "switch"
      (format_expr decl_ctx ~debug)
      e_switch Print.punctuation ":"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt ((case, _), switch_case_data) ->
           Format.fprintf fmt "@[<v 2>%a %a %a %a@ %a@]" Print.punctuation "|"
             EnumConstructor.format case format_var_name
             switch_case_data.payload_var_name Print.punctuation "→"
             (format_block decl_ctx ~debug)
             switch_case_data.case_block))
      (List.combine (EnumConstructor.Map.bindings cons) arms)
  | SSpecialOp (OHandleDefaultOpt { exceptions; just; cons; _ }) ->
    Format.fprintf fmt "@[<hov 2>%a %a%a%a@]@\n@[<hov 2>%a@ %a %a%a@\n%a@]"
      Print.keyword "handle exceptions" Print.punctuation "["
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt e -> Format.fprintf fmt "%a" (format_expr decl_ctx ~debug) e))
      exceptions Print.punctuation "]" Print.keyword "or if"
      (format_expr decl_ctx ~debug)
      just Print.keyword "then" Print.punctuation ":"
      (format_block decl_ctx ~debug)
      cons

and format_block
    (decl_ctx : decl_ctx)
    ?(debug : bool = false)
    (fmt : Format.formatter)
    (block : block) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " Print.punctuation ";")
    (format_statement decl_ctx ~debug)
    fmt block

let format_item decl_ctx ?debug ppf def =
  Format.pp_open_hvbox ppf 2;
  Format.pp_open_hovbox ppf 4;
  Print.keyword ppf "let ";
  let () =
    match def with
    | SVar { var; expr; typ = _ } ->
      format_var_name ppf var;
      Print.punctuation ppf " =";
      Format.pp_close_box ppf ();
      Format.pp_print_space ppf ();
      format_expr decl_ctx ?debug ppf expr
    | SScope { scope_body_var = var; scope_body_func = func; _ }
    | SFunc { var; func } ->
      format_func_name ppf var;
      Format.pp_print_list
        (fun ppf (arg, ty) ->
          Format.fprintf ppf "@ (%a: %a)" format_var_name (Mark.remove arg)
            (Print.typ decl_ctx) ty)
        ppf func.func_params;
      Print.punctuation ppf " =";
      Format.pp_close_box ppf ();
      Format.pp_print_space ppf ();
      format_block decl_ctx ?debug ppf func.func_body
  in
  Format.pp_close_box ppf ();
  Format.pp_print_cut ppf ()

let format_program decl_ctx ?debug ppf prg =
  let decl_ctx =
    {
      decl_ctx with
      ctx_enums =
        EnumName.Map.add Expr.option_enum Expr.option_enum_config
          decl_ctx.ctx_enums;
    }
  in
  Format.pp_open_vbox ppf 0;
  Format.pp_print_list (format_item decl_ctx ?debug) ppf prg.code_items;
  Format.pp_close_box ppf ()
