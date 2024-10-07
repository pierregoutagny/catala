open Path_constraint.PathConstraint

type flag = | OTrivial
            | OLazyDefault
            | OLinearizeMatch
            | OIncrementalSolver
            | OSoftConstraints
            | OTestsVTime
            | OTimeoutRetry
            | OMutationRemove
            | OMutationDuplicate
            | OMutationNegateJusts
            | OMutationOneConflict
            | OASTStats

let optim_list = [
  "trivial", OTrivial;
  "lazy-default", OLazyDefault;
  "linearize-match", OLinearizeMatch;
  "incremental", OIncrementalSolver;
  "soft", OSoftConstraints;
  "tests-vs-time", OTestsVTime;
  "no-timeout-retry", OTimeoutRetry;
  "mutation-remove", OMutationRemove;
  "mutation-duplicate", OMutationDuplicate;
  "mutation-negate-justs", OMutationNegateJusts;
  "mutation-one-conflict", OMutationOneConflict;
  "ast-stats", OASTStats;
]
let trivial : flag list -> bool = List.mem OTrivial
let lazy_default : flag list -> bool = List.mem OLazyDefault
let linearize_match : flag list -> bool = List.mem OLinearizeMatch
let incremental_solver : flag list -> bool = List.mem OIncrementalSolver
let soft_constraints : flag list -> bool = List.mem OSoftConstraints
let tests_vs_time : flag list -> bool = List.mem OTestsVTime
let timeout_retry (l: flag list) : bool = not (List.mem OTimeoutRetry l) (* This option is active by default *)
let mutation_remove : flag list -> bool = List.mem OMutationRemove
let mutation_duplicate : flag list -> bool = List.mem OMutationDuplicate
let mutation_negate_justs : flag list -> bool = List.mem OMutationNegateJusts
let mutation_one_conflict : flag list -> bool = List.mem OMutationOneConflict
let ast_stats : flag list -> bool = List.mem OASTStats

let random_mutations flags = mutation_remove flags || mutation_duplicate flags || mutation_negate_justs flags
let one_mutation flags = mutation_one_conflict flags
let mutation flags = random_mutations flags || one_mutation flags

let check_optims_coherent optims =
  if soft_constraints optims && not @@ incremental_solver optims then Catala_utils.Message.warning "[CONC] Soft constraints are enabled without incremental solver. This won't do anything."

let remove_trivial_constraints opt (pcs : naked_path) : naked_path =
  if not (trivial opt) then pcs
  else begin
    let f pc =
      match pc.expr with Pc_z3 s -> not (Z3.Boolean.is_true s) | _ -> true
    in
    List.filter f pcs
  end

open Catala_utils
open Shared_ast

let all_match_cases_take_unit_and_map_to_boolean_literals cases =
  EnumConstructor.Map.for_all
    (fun _ case ->
      match Mark.remove case with
      | EAbs { binder; tys } -> (
        let _, body = Bindlib.unmbind binder in
        match Mark.remove body with
        | ELit (LBool _) ->
          let ty = List.hd tys in
          (* because of invariant [invariant_match], the arity is always one. *)
          Mark.remove ty = TLit TUnit
        | _ -> false)
      | _ -> assert false)
    cases

let rec optimize_rec :
  type a b c.
    ((a, b, c) interpr_kind , 'm) gexpr ->
    ((a, b, c) interpr_kind , 'm) boxed_gexpr =
 fun e ->
  (* We proceed bottom-up, first apply on the subterms *)
  let e = Expr.map ~f:optimize_rec ~op:Fun.id e in
  let mark = Mark.get e in
  (* Fixme: when removing enclosing expressions, it would be better if we were
     able to keep the inner position (see the division_by_zero test) *)
  (* Then reduce the parent node (this is applied through Box.apply, therefore
     delayed to unbinding time: no need to be concerned about reboxing) *)
  let reduce (e : ((a, b, c) interpr_kind, 'm) gexpr) =
    (* Todo: improve the handling of eapp(log,elit) cases here, it obfuscates
       the matches and the log calls are not preserved, which would be a good
       property *)
    match Mark.remove e with
    | EMatch { e = e'; cases; name = n }
      when all_match_cases_take_unit_and_map_to_boolean_literals cases ->
      (* transform matches whose arms are all of the form [Constructor () ->
         true/false] to a big [or] of all those cases *)
      let cases_true =
        EnumConstructor.Map.filter
          (fun _ case ->
            match Mark.remove case with
            | EAbs { binder; _ } -> (
              let _, body = Bindlib.unmbind binder in
              match Mark.remove body with
              | ELit (LBool b) -> b
              | _ -> failwith "should not happen")
            | _ -> assert false)
          cases
      in
      let boxed_e' = Expr.rebox e' in
      let m = Mark.get e' |> Expr.no_mark in
      let lunit = Expr.elit LUnit m in
      let enum_typ = Mark.add Pos.no_pos (TEnum n) in
      let bool_typ = Mark.add Pos.no_pos (TLit TBool) in
      let make_eq (cons : EnumConstructor.t) =
        let inj = Expr.einj ~name:n ~cons ~e:lunit m in
        Expr.eappop ~op:Eq ~args:[boxed_e'; inj] ~tys:[enum_typ; enum_typ] m
      in
      let make_or e1 e2 =
        Expr.eappop ~op:Or ~args:[e1; e2] ~tys:[bool_typ; bool_typ] m
      in
      let f cons _ acc =
        let eq = make_eq cons in
        make_or acc eq
      in
      let lfalse = Expr.elit (LBool false) m in
      EnumConstructor.Map.fold f cases_true lfalse
      |> Expr.unbox
      |> optimize_rec
      |> Expr.unbox
      |> Mark.remove
    | e -> e
  in
  Expr.Box.app1 e reduce mark

let optimize_expr (type a b c) (optims : flag list) :
      ((a, b, c) interpr_kind, 'm) gexpr ->
      ((a, b, c) interpr_kind, 'm) gexpr =
 fun e ->
   if linearize_match optims then begin
     if Global.options.debug then Message.debug "[CONC] Applying match linearization optim";
    Expr.unbox (optimize_rec e)
  end
  else e



