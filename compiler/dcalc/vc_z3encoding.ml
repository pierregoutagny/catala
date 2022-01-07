(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2022 Inria, contributor: Aymeric Fromherz
   <aymeric.fromherz@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Ast 
open Z3

(** [translate_lit] returns the Z3 expression as a literal corresponding to [lit] **)
let translate_lit (ctx : context) (l : lit) : Expr.expr =
  match l with
  | LBool b ->
      if b then Boolean.mk_true ctx else Boolean.mk_false ctx

  | LEmptyError -> failwith "[Z3 encoding] LEmptyError literals not supported"
  | LInt _ -> failwith "[Z3 encoding] LInt literals not supported" 
  | LRat _ -> failwith "[Z3 encoding] LRat literals not supported" 
  | LMoney _ -> failwith "[Z3 encoding] LMoney literals not supported" 
  | LUnit -> failwith "[Z3 encoding] LUnit literals not supported"
  | LDate _ -> failwith "[Z3 encoding] LDate literals not supported"
  | LDuration _ -> failwith "[Z3 encoding] LDuration literals not supported"


(** [translate_op] returns the Z3 expression corresponding to the
    application of [op] to the arguments [args] **)
let rec translate_op (ctx : context) (op : operator) (args : expr Pos.marked list) : Expr.expr =
  match op with
  | Ternop _top ->
      (match args with
        | [_; _; _] -> () 
        (* TODO: Print term for error message *)
        | _ -> failwith "[Z3 encoding] Ill-formed ternary operator application"
      );
      
      failwith "[Z3 encoding] ternary operator application not supported"

  | Binop bop ->
      (match args with
        | [_; _] -> ()
        (* TODO: Print term for error message *)
        | _ -> failwith "[Z3 encoding] Ill-formed binary operator application"
      );
      
      (match bop with
      | And -> Boolean.mk_and ctx (List.map (translate_expr ctx) args)

      | Or -> Boolean.mk_or ctx (List.map (translate_expr ctx) args)

      | Xor -> failwith "[Z3 encoding] application of binary operator Xor not supported"
      | Add _ -> failwith "[Z3 encoding] application of binary operator Add not supported"
      | Sub _ -> failwith "[Z3 encoding] application of binary operator Sub not supported"
      | Mult _ -> failwith "[Z3 encoding] application of binary operator Mult not supported"
      | Div _ -> failwith "[Z3 encoding] application of binary operator Div not supported"
      | Lt _ -> failwith "[Z3 encoding] application of binary operator Lt not supported"
      | Lte _ -> failwith "[Z3 encoding] application of binary operator Lte not supported"  
      | Gt _ -> failwith "[Z3 encoding] application of binary operator Gt not supported"
      | Gte _ -> failwith "[Z3 encoding] application of binary operator Gte not supported"
      | Eq -> failwith "[Z3 encoding] application of binary operator Eq not supported"
      | Neq -> failwith "[Z3 encoding] application of binary operator New not supported"
      | Map -> failwith "[Z3 encoding] application of binary operator Map not supported"
      | Concat -> failwith "[Z3 encoding] application of binary operator Concat not supported"
      | Filter -> failwith "[Z3 encoding] application of binary operator Filter not supported"
      )

  | Unop _op -> 
      (match args with
        | [_] -> ()
        (* TODO: Print term for error message *)
        | _ -> failwith "[Z3 encoding] Ill-formed unary operator application"
      );
      
     failwith "[Z3 encoding] unary operator application not supported"

(** [translate_expr] translate the expression [vc] to its corresponding Z3 expression **)
and translate_expr (ctx:context) (vc : expr Pos.marked) : Expr.expr =
  match Pos.unmark vc with
  | EVar _ -> failwith "[Z3 encoding] EVar unsupported"
  | ETuple _ -> failwith "[Z3 encoding] ETuple unsupported"
  | ETupleAccess _ -> failwith "[Z3 encoding] ETupleAccess unsupported"
  | EInj _ -> failwith "[Z3 encoding] EInj unsupported"
  | EMatch _ -> failwith "[Z3 encoding] EMatch unsupported"
  | EArray _ -> failwith "[Z3 encoding] EArray unsupported"

  | ELit l -> translate_lit ctx l

  | EAbs _ -> failwith "[Z3 encoding] EAbs unsupported"

  | EApp (head, args) -> (match Pos.unmark head with
      | EOp op -> translate_op ctx op args
      | _ -> failwith "[Z3 encoding] EApp of a non-operator unsupported"
    )

  | EAssert _ ->  failwith "[Z3 encoding] EAssert unsupported"
  | EOp _ -> failwith "[Z3 encoding] EOp unsupported"
  | EDefault _ -> failwith "[Z3 encoding] EDefault unsupported"

  | EIfThenElse (e_if, e_then, e_else) -> 
      (* Encode this as (e_if ==> e_then) /\ (not e_if ==> e_else) *)
      let z3_if = translate_expr ctx e_if in
      Boolean.mk_and ctx [
        Boolean.mk_implies ctx z3_if (translate_expr ctx e_then);
        Boolean.mk_implies ctx (Boolean.mk_not ctx z3_if) (translate_expr ctx e_else)
      ]

  | ErrorOnEmpty _ -> failwith "[Z3 encoding] ErrorOnEmpty unsupported"

(** [solve_vc] is the main entry point of this module.
    It takes a list of expressions [vcs] corresponding to verification conditions
    that must be discharged by Z3, and attempts to solve them **)
let solve_vc (vcs : expr Pos.marked list) : unit =
  Printf.printf "Running Z3 version %s\n" Version.to_string ;

  let cfg = [("model", "true"); ("proof", "false")] in
	let ctx = (mk_context cfg) in
  
  let solver = Solver.mk_solver ctx None in

  let z3_vcs = List.map (translate_expr ctx) vcs in

  List.iter (fun vc -> Printf.printf "Generated VC: %s\n" (Expr.to_string vc)) z3_vcs;

  Solver.add solver z3_vcs;

  if (Solver.check solver []) = SATISFIABLE then
    Printf.printf "Success: Empty unreachable\n"
  else
    (* TODO: Print model as error message for Catala debugging purposes *) 
    Printf.printf "Failure: Empty reachable\n"

