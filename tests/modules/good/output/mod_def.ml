(** This file has been generated by the Catala compiler, do not edit! *)

open Runtime_ocaml.Runtime

[@@@ocaml.warning "-4-26-27-32-41-42"]

module Enum1 = struct
 type t =
    | Yes of unit
    | No of unit
    | Maybe of unit
  end

module S = struct
  type t = {sr: money; e1: Enum1.t}
end

module Str1 = struct
  type t = {fld1: Enum1.t; fld2: integer}
end

module S_in = struct
  type t = unit
end


let s (s_in: S_in.t) : S.t =
  let sr: money =
    match
      (match
         (handle_exceptions
            [|{filename="tests/modules/good/mod_def.catala_en";
               start_line=16; start_column=10; end_line=16; end_column=12;
               law_headings=["Test modules + inclusions 1"]}|]
            ([|(match
                  (handle_exceptions
                     [|{filename="tests/modules/good/mod_def.catala_en";
                        start_line=29; start_column=24;
                        end_line=29; end_column=30;
                        law_headings=["Test modules + inclusions 1"]}|]
                     ([||]))
                with
                | Eoption.ENone _ ->
                    ( if true then
                       (Eoption.ESome (money_of_cents_string "100000")) else
                       (Eoption.ENone ()))
                | Eoption.ESome x -> (Eoption.ESome x))|]))
       with
       | Eoption.ENone _ ->
           ( if false then (Eoption.ENone ()) else (Eoption.ENone ()))
       | Eoption.ESome x -> (Eoption.ESome x))
    with
    | Eoption.ENone _ -> (raise
        (Runtime_ocaml.Runtime.Error (NoValue, [{filename="tests/modules/good/mod_def.catala_en";
                                                 start_line=16; start_column=10;
                                                 end_line=16; end_column=12;
                                                 law_headings=["Test modules + inclusions 1"]}])))
    | Eoption.ESome arg -> arg in
  let e1: Enum1.t =
    match
      (match
         (handle_exceptions
            [|{filename="tests/modules/good/mod_def.catala_en";
               start_line=17; start_column=10; end_line=17; end_column=12;
               law_headings=["Test modules + inclusions 1"]}|]
            ([|(match
                  (handle_exceptions
                     [|{filename="tests/modules/good/mod_def.catala_en";
                        start_line=30; start_column=24;
                        end_line=30; end_column=29;
                        law_headings=["Test modules + inclusions 1"]}|]
                     ([||]))
                with
                | Eoption.ENone _ ->
                    ( if true then (Eoption.ESome (Enum1.Maybe ())) else
                       (Eoption.ENone ()))
                | Eoption.ESome x -> (Eoption.ESome x))|]))
       with
       | Eoption.ENone _ ->
           ( if false then (Eoption.ENone ()) else (Eoption.ENone ()))
       | Eoption.ESome x -> (Eoption.ESome x))
    with
    | Eoption.ENone _ -> (raise
        (Runtime_ocaml.Runtime.Error (NoValue, [{filename="tests/modules/good/mod_def.catala_en";
                                                 start_line=17; start_column=10;
                                                 end_line=17; end_column=12;
                                                 law_headings=["Test modules + inclusions 1"]}])))
    | Eoption.ESome arg -> arg in
  {S.sr = sr; S.e1 = e1}

let half : integer -> decimal =
  fun (x: integer) ->
    o_div_int_int
      {filename="tests/modules/good/mod_def.catala_en";
       start_line=21; start_column=14; end_line=21; end_column=15;
       law_headings=["Test modules + inclusions 1"]} x (integer_of_string
      "2")

let maybe : Enum1.t -> Enum1.t =
  fun (x: Enum1.t) -> Enum1.Maybe ()

let () =
  Runtime_ocaml.Runtime.register_module "Mod_def"
    [ "S", Obj.repr s;
      "half", Obj.repr half;
      "maybe", Obj.repr maybe ]
    "CMX|XXXXXXXX|XXXXXXXX|XXXXXXXX"
