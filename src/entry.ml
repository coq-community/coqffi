open Repr
open Parsetree
open Types
open Config

type primitive_entry = {
  prim_name : string;
  prim_type : type_repr;
  prim_may_raise : bool;
}

type function_entry = {
  func_name : string;
  func_type : type_repr;
  func_model : string option;
  func_may_raise : bool;
}

type variant_entry = {
  variant_name : string;
  variant_args: mono_type_repr list;
}

type type_value =
  | Variant of variant_entry list
  | Opaque

type type_entry = {
  type_name : string;
  type_params : string list;
  type_model : string option;
  type_value : type_value
}

type mutually_recursive_types_entry = type_entry list

type exception_entry = {
  exception_name : string;
  exception_args : mono_type_repr list;
}

type entry =
  | EPrim of primitive_entry
  | EFunc of function_entry
  | EType of type_entry
  | EExn of exception_entry

exception UnsupportedOCamlSignature of Types.signature_item
exception UnsupportedGADT
exception Anomaly

let polymorphic_param (t : type_expr) : string =
  match t.desc with
  | Tvar (Some x) -> x
  | Tvar None -> raise UnsupportedGADT
  | _ -> raise Anomaly

let polymorphic_params (decl : type_declaration) : string list =
  List.map polymorphic_param decl.type_params

let has_attr name : attributes -> bool =
  List.exists (fun attr -> attr.attr_name.txt = name)

let get_attr_string name : attributes -> string option =
  let expr_to_string = function
    | Pexp_constant (Pconst_string (str, _, _)) -> Some str
    | _ -> None in

  let struct_to_string = function
    | Pstr_eval (expr, _) -> expr_to_string expr.pexp_desc
    | _ -> None in

  let get_attr attr =
    if attr.attr_name.txt = name
    then match attr.attr_payload with
         | PStr [ model ] -> struct_to_string model.pstr_desc
         | _ -> None
    else None in

  Compat.find_map get_attr

let has_coq_model : attributes -> string option =
  get_attr_string "coq_model"

let args_of_constructor : Types.constructor_arguments -> mono_type_repr list = function
  | Cstr_tuple typs -> List.map mono_type_repr_of_type_expr typs
  | _ -> assert false

let entry_of_value lf ident desc =
  let is_pure attrs model t =
    is_enabled lf PureModule
    || Repr.supposedly_pure t
    || Option.is_some model
    || has_attr "pure" attrs
  in

  let name = Ident.name ident in
  let repr = type_repr_of_type_expr desc.val_type in
  let model = has_coq_model desc.val_attributes in
  let may_raise = has_attr "may_raise" desc.val_attributes in

  if is_pure desc.val_attributes model repr
  then EFunc {
         func_name = name;
         func_type = repr;
         func_model = model;
         func_may_raise = may_raise;
       }
  else EPrim {
         prim_name = name;
         prim_type = repr;
         prim_may_raise = may_raise;
       }

let entry_of_type lf ident decl  =
  let to_variant_entry v = {
    variant_name = Ident.name v.cd_id;
    variant_args = args_of_constructor v.cd_args;
  } in

  let value t =
    if is_enabled lf TransparentTypes
    then match t with
         | Type_variant v -> Variant (List.map to_variant_entry v)
         | _ -> Opaque
    else Opaque in

  EType {
    type_params = polymorphic_params decl;
    type_name = Ident.name ident;
    type_model = has_coq_model decl.type_attributes;
    type_value = value decl.type_kind;
  }

let entry_of_exn ident cst =
  EExn {
    exception_name = Ident.name ident;
    exception_args = args_of_constructor cst.ext_args;
  }

let entry_of_signature lf (s : Types.signature_item) : entry =
  match s with
  | Sig_value (ident, desc, Exported) ->
    entry_of_value lf ident desc
  | Sig_type (ident, decl, _, Exported) ->
    entry_of_type lf ident decl
  | Sig_typext (ident, cst, Text_exception, Exported) ->
    entry_of_exn ident cst
  | _ -> raise (UnsupportedOCamlSignature s)

type state = Unvisited | OnStack | Visited

type node = {
  node_state : state ref;
  node_type : type_entry;
  node_deps : string list;
  node_id : int ref;
  node_low : int ref;
}

let new_node t deps = {
  node_state = ref Unvisited;
  node_type = t;
  node_deps = deps;
  node_id = ref 0;
  node_low = ref 0;
}

let unvisited = function
  | Unvisited -> true
  | _ -> false

let on_stack = function
  | OnStack -> true
  | _ -> false

let dependencies t =
  match t.type_value with
  | Variant l ->
    List.sort_uniq String.compare
      (Compat.concat_map
         (fun e -> Compat.concat_map mono_dependencies e.variant_args) l)
  | Opaque -> []

let find_mutually_recursive_types tl =
  let id : int ref = ref 0 in

  let stack : (node list) ref = ref [] in

  let res : ((type_entry list) list) ref = ref [] in

  let nodes = List.map (fun t -> t.type_name) tl in

  let matrix : (string, node) Hashtbl.t = begin
    let tbl = Hashtbl.create (List.length tl) in
    List.iter
      (fun t ->
         let deps = dependencies t in
         let min_deps = List.filter
             (fun x -> List.exists (String.equal x) deps)
             nodes in
         Hashtbl.add tbl t.type_name (new_node t min_deps))
      tl;
    tbl
  end in

  let rec empty_stack at =
    match !stack with
    | node :: rst ->
      stack := rst;
      node.node_state := Visited;
      node.node_low := !(at.node_id);
      if !(node.node_id) == !(at.node_id) then [node.node_type]
      else node.node_type :: empty_stack at
    | _ -> assert false in

  let rec dfs at =
    stack := at :: !stack;
    at.node_state := OnStack;
    at.node_id := !id;
    at.node_low := !id;
    id := !id + 1;

    List.iter
      (fun t ->
         let dep = Hashtbl.find matrix t in
         if unvisited !(dep.node_state) then dfs dep;
         if on_stack !(dep.node_state)
         then at.node_low := min !(dep.node_low) !(at.node_low))
      at.node_deps;

    if !(at.node_id) = !(at.node_low) then
      res := List.rev (empty_stack at) :: !res in

  Hashtbl.iter (fun _ v ->
      if unvisited !(v.node_state) then dfs v) matrix;

  List.rev !res
