(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Alain Delaët <alain.delaet--tixeuil@inria.Fr>, Louis
   Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Reference interpreter for the default calculus *)

open Catala_utils
open Definitions
(* open Op *)
open Interpreter
(* module Runtime = Runtime_ocaml.Runtime *)


(* Trying some stuff *)

let default_of_tlit (ty: typ_lit) : lit =
  match ty with
  | TBool -> LBool true
  | TUnit -> LUnit
  | TInt -> LInt (Z.of_int 42)
  | _ -> failwith "not implemented"


let interpret_program_default_values p s : (Uid.MarkedString.info * ('a, 'm) gexpr) list
    =
  let ctx = p.decl_ctx in
  let e = Expr.unbox (Program.to_expr p s) in
  match evaluate_expr p.decl_ctx p.lang (addcustom e) with
  | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e -> begin
    (* At this point, the interpreter seeks to execute the scope but does not
       have a way to retrieve input values from the command line. [taus] contain
       the types of the scope arguments. For [context] arguments, we can provide
       an empty thunked term. But for [input] arguments of another type, we
       cannot provide anything so we have to fail. *)
    let taus = StructName.Map.find s_in ctx.ctx_structs in
    let application_term =
      StructField.Map.map
        (fun ty ->
          match Mark.remove ty with
          | TArrow (ty_in, ty_out) ->
            Expr.make_abs
              (Array.of_list @@ List.map (fun _ -> Var.make "_") ty_in)
              (Bindlib.box EEmptyError, Expr.with_ty mark_e ty_out)
              ty_in (Expr.mark_pos mark_e)
          | TLit t -> Expr.elit (default_of_tlit t) mark_e
          | _ ->
            Message.raise_spanned_error (Mark.get ty)
              "This scope needs input arguments to be executed. But the Catala \
               built-in interpreter does not have a way to retrieve input \
               values from the command line, so it cannot execute this scope. \
               Please create another scope that provides the input arguments \
               to this one and execute it instead. ")
        taus
    in
    let to_interpret =
      Expr.make_app (Expr.box e)
        [Expr.estruct ~name:s_in ~fields:application_term mark_e]
        (Expr.pos e)
    in
    match Mark.remove (evaluate_expr ctx p.lang (Expr.unbox to_interpret)) with
    | EStruct { fields; _ } ->
      List.map
        (fun (fld, e) -> StructField.get_info fld, e)
        (StructField.Map.bindings fields)
    | _ ->
      Message.raise_spanned_error (Expr.pos e)
        "The interpretation of a program should always yield a struct \
         corresponding to the scope variables"
  end
  | _ ->
    Message.raise_spanned_error (Expr.pos e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"

