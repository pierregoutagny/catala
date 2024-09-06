(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2021 Inria, contributor:
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

(** Formats a lambda calculus program into a valid Python program *)

open Catala_utils
open Shared_ast
open Ast

val renaming : Renaming.t

val format_program :
  Format.formatter -> Ast.program -> Scopelang.Dependency.TVertex.t list -> unit
(** Usage [format_program fmt p type_dependencies_ordering] *)

val format_name_cleaned : Format.formatter -> string -> unit
val format_lit : Format.formatter -> lit Mark.pos -> unit
val format_enum_name : ctx -> Format.formatter -> EnumName.t -> unit
val format_enum_cons_name : Format.formatter -> EnumConstructor.t -> unit
