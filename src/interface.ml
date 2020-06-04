open Types
open Parsetree
open Cmi_format

(* Types *)

type coq_ident = string

type type_desc  = {
  name : Ident.t;
  coq_model : coq_ident option
}

type function_desc = {
  name : Ident.t;
  coq_model : coq_ident option
}

type primitive_desc = {
  name : Ident.t;
}

type entry_desc =
  | Primitive of primitive_desc
  | Function of function_desc
  | Type of type_desc

type interface_desc = {
  module_path : string list;
  primitives : primitive_desc list;
  functions : function_desc list;
  types : type_desc list;
}

(* Functions *)

let empty modname =
  let modpath = Str.split (Str.regexp "__") modname in {
    module_path = modpath;
    primitives = [];
    functions = [];
    types = [];
  }

let has_ffi_pure =
  let is_ffi_pure attr = String.equal attr.attr_name.txt "ffi_pure" in
  List.exists is_ffi_pure

let expr_to_string = function
  | Pexp_constant (Pconst_string (str, _)) -> Some str
  | _ -> None

let struct_to_string = function
  | Pstr_eval (expr, _) -> expr_to_string expr.pexp_desc
  | _ -> None

let find_coq_model =
  let get_model attr = if String.equal attr.attr_name.txt "coq_model"
    then match attr.attr_payload with
      | PStr [ model ] -> struct_to_string model.pstr_desc
      | _ -> None
    else None (* TODO: emit a manual *)
  in List.find_map get_model

let entry_of_signature = function
  | Sig_value (ident, desc, Exported) ->
    if has_ffi_pure desc.val_attributes
    then Function { name = ident; coq_model = find_coq_model desc.val_attributes }
    else Primitive { name = ident }
  | Sig_type (ident, desc, _, Exported) ->
    Type { name = ident; coq_model = find_coq_model desc.type_attributes }
  | _ -> assert false

let input_of_cmi_infos x =
  let add_entry i = function
    | Function f -> { i with functions = f :: i.functions }
    | Primitive p -> { i with primitives = p :: i.primitives }
    | Type t -> {i with types = t :: i.types } in
  List.fold_left (fun i s -> entry_of_signature s |> add_entry i)
    (empty x.cmi_name)
    x.cmi_sign
