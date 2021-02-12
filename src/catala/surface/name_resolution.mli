(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr> Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Builds a context that allows for mapping each name to a precise uid, taking lexical scopes into
    account *)

open Utils

(** {1 Name resolution context} *)

type ident = string

type typ = Scopelang.Ast.typ

type unique_rulename = Ambiguous | Unique of Desugared.Ast.RuleName.t

type scope_context = {
  var_idmap : Scopelang.Ast.ScopeVar.t Desugared.Ast.IdentMap.t;  (** Scope variables *)
  label_idmap : Desugared.Ast.RuleName.t Desugared.Ast.IdentMap.t;
  default_rulemap : unique_rulename Desugared.Ast.ScopeDefMap.t;
      (** What is the default rule to refer to for unnamed exceptions, if any *)
  sub_scopes_idmap : Scopelang.Ast.SubScopeName.t Desugared.Ast.IdentMap.t;
      (** Sub-scopes variables *)
  sub_scopes : Scopelang.Ast.ScopeName.t Scopelang.Ast.SubScopeMap.t;
      (** To what scope sub-scopes refer to? *)
}
(** Inside a scope, we distinguish between the variables and the subscopes. *)

type struct_context = typ Pos.marked Scopelang.Ast.StructFieldMap.t
(** Types of the fields of a struct *)

type enum_context = typ Pos.marked Scopelang.Ast.EnumConstructorMap.t
(** Types of the payloads of the cases of an enum *)

type context = {
  local_var_idmap : Scopelang.Ast.Var.t Desugared.Ast.IdentMap.t;
      (** Inside a definition, local variables can be introduced by functions arguments or pattern
          matching *)
  scope_idmap : Scopelang.Ast.ScopeName.t Desugared.Ast.IdentMap.t;  (** The names of the scopes *)
  struct_idmap : Scopelang.Ast.StructName.t Desugared.Ast.IdentMap.t;
      (** The names of the structs *)
  field_idmap : Scopelang.Ast.StructFieldName.t Scopelang.Ast.StructMap.t Desugared.Ast.IdentMap.t;
      (** The names of the struct fields. Names of fields can be shared between different structs *)
  enum_idmap : Scopelang.Ast.EnumName.t Desugared.Ast.IdentMap.t;  (** The names of the enums *)
  constructor_idmap :
    Scopelang.Ast.EnumConstructor.t Scopelang.Ast.EnumMap.t Desugared.Ast.IdentMap.t;
      (** The names of the enum constructors. Constructor names can be shared between different
          enums *)
  scopes : scope_context Scopelang.Ast.ScopeMap.t;  (** For each scope, its context *)
  structs : struct_context Scopelang.Ast.StructMap.t;  (** For each struct, its context *)
  enums : enum_context Scopelang.Ast.EnumMap.t;  (** For each enum, its context *)
  var_typs : (typ Pos.marked * bool) (* is it a condition? *) Scopelang.Ast.ScopeVarMap.t;
      (** The types of each scope variable declared *)
}
(** Main context used throughout {!module: Surface.Desugaring} *)

(** {1 Helpers} *)

(** Temporary function raising an error message saying that a feature is not supported yet *)
val raise_unsupported_feature : string -> Pos.t -> 'a

(** Function to call whenever an identifier used somewhere has not been declared in the program
    previously *)
val raise_unknown_identifier : string -> ident Pos.marked -> 'a

(** Gets the type associated to an uid *)
val get_var_typ : context -> Scopelang.Ast.ScopeVar.t -> typ Pos.marked

val is_var_cond : context -> Scopelang.Ast.ScopeVar.t -> bool

(** Get the variable uid inside the scope given in argument *)
val get_var_uid :
  Scopelang.Ast.ScopeName.t ->
  context -> ident Pos.marked -> Scopelang.Ast.ScopeVar.t

(** Get the subscope uid inside the scope given in argument *)
val get_subscope_uid :
  Scopelang.Ast.ScopeName.t ->
  context -> ident Pos.marked -> Scopelang.Ast.SubScopeName.t

(** [is_subscope_uid scope_uid ctxt y] returns true if [y] belongs to the subscopes of [scope_uid]. *)
val is_subscope_uid : Scopelang.Ast.ScopeName.t -> context -> ident -> bool

(** Checks if the var_uid belongs to the scope scope_uid *)
val belongs_to :
  context -> Scopelang.Ast.ScopeVar.t -> Scopelang.Ast.ScopeName.t -> bool

(** Retrieves the type of a scope definition from the context *)
val get_def_typ : context -> Desugared.Ast.ScopeDef.t -> typ Pos.marked

val is_def_cond : context -> Desugared.Ast.ScopeDef.t -> bool


(** {1 Declarations pass} *)

(** Process a subscope declaration *)
val process_subscope_decl :
  Scopelang.Ast.ScopeName.t ->
  context -> Ast.scope_decl_context_scope -> context

val is_type_cond : Ast.typ Pos.marked -> bool

(** Process a basic type (all types except function types) *)
val process_base_typ :
  context ->
  Ast.base_typ Pos.marked -> Scopelang.Ast.typ Pos.marked

(** Process a type (function or not) *)
val process_type :
  context ->
  Ast.typ Pos.marked -> Scopelang.Ast.typ Pos.marked

val process_data_decl :
  Scopelang.Ast.ScopeName.t ->
  context -> Ast.scope_decl_context_data -> context

val process_item_decl :
  Scopelang.Ast.ScopeName.t ->
  context -> Ast.scope_decl_context_item -> context

(** Adds a binding to the context *)
val add_def_local_var :
  context -> ident Pos.marked -> context * Scopelang.Ast.Var.t

val process_scope_decl : context -> Ast.scope_decl -> context

val process_struct_decl : context -> Ast.struct_decl -> context

val process_enum_decl : context -> Ast.enum_decl -> context

(** Process a code item that is a declaration *)
val process_decl_item :
  context -> Ast.code_item Pos.marked -> context

val process_code_block :
  context ->
  Ast.code_block ->
  (context -> Ast.code_item Pos.marked -> context) -> context

(** Process a law article item, only considering the code blocks *)
val process_law_article_item :
  context ->
  Ast.law_article_item ->
  (context -> Ast.code_item Pos.marked -> context) -> context

(** Process a law structure, only considering the code blocks *)
val process_law_structure :
  context ->
  Ast.law_structure ->
  (context -> Ast.code_item Pos.marked -> context) -> context

(** Process a program item, only considering the code blocks *)
val process_program_item :
  context ->
  Ast.program_item ->
  (context -> Ast.code_item Pos.marked -> context) -> context


(** {1 Scope uses pass} *)

val get_def_key :
  Ast.qident ->
  Scopelang.Ast.ScopeName.t ->
  context -> Pos.t -> Desugared.Ast.ScopeDef.t

val process_rule :
  context -> Scopelang.Ast.ScopeName.t -> Ast.rule -> context

val process_definition :
  context -> Scopelang.Ast.ScopeName.t -> Ast.definition -> context

val process_scope_use_item :
  Scopelang.Ast.ScopeName.t ->
  context -> Ast.scope_use_item Pos.marked -> context

val process_scope_use : context -> Ast.scope_use -> context

val process_use_item :
  context -> Ast.code_item Pos.marked -> context

(** {1 API} *)

(** Derive the context from metadata, in one pass over the declarations *)
val form_context : Ast.program -> context
