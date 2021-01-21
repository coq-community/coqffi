open Repr
open Parsetree
open Types
open Feature
open Error

type primitive_entry = {
  prim_name : string;
  prim_type : type_repr;
  prim_may_raise : bool;
  prim_loc : Location.t;
}

type function_entry = {
  func_name : string;
  func_type : type_repr;
  func_model : string option;
  func_may_raise : bool;
  func_loc : Location.t;
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
  type_value : type_value;
  type_loc : Location.t;
}

type mutually_recursive_types_entry = type_entry list

type exception_entry = {
  exception_name : string;
  exception_args : mono_type_repr list;
  exception_loc : Location.t;
}

type entry =
  | EPrim of primitive_entry
  | EFunc of function_entry
  | EType of type_entry
  | EExn of exception_entry

let polymorphic_param (t : type_expr) : string =
  match t.desc with
  | Tvar (Some x) -> x
  | Tvar None -> raise_error UnsupportedGADT
  | _ -> failwith "Type parameters should be made with [Tvar] and nothing else"

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

let signature_ident = function
  | Sig_value (ident, _, _)
    | Sig_type (ident, _, _, _)
    | Sig_typext (ident, _, _, _)
    | Sig_module (ident, _, _, _, _)
    | Sig_modtype (ident, _, _)
    | Sig_class (ident, _, _, _)
    | Sig_class_type (ident, _, _, _)
    -> ident

let signature_loc = function
  | Sig_value (_, v, _) -> v.val_loc
  | Sig_type (_, ty, _, _) -> ty.type_loc
  | Sig_typext (_, ext, _, _) -> ext.ext_loc
  | Sig_module (_, _, m, _, _) -> m.md_loc
  | Sig_modtype (_, m, _) -> m.mtd_loc
  | Sig_class (_, c, _, _) -> c.cty_loc
  | Sig_class_type (_, c, _, _) -> c.clty_loc

let args_of_constructor : Types.constructor_arguments -> mono_type_repr list = function
  | Cstr_tuple typs -> List.map mono_type_repr_of_type_expr typs
  | _ -> assert false

let entry_of_value lf ident desc loc =
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
         func_loc = loc;
       }
  else EPrim {
         prim_name = name;
         prim_type = repr;
         prim_may_raise = may_raise;
         prim_loc = loc;
       }

let entry_of_type lf ident decl loc =
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
    type_loc = loc;
  }

let entry_of_exn ident cst loc =
  EExn {
    exception_name = Ident.name ident;
    exception_args = args_of_constructor cst.ext_args;
    exception_loc = loc;
  }

let entry_of_signature lf (s : Types.signature_item) : entry =
  let loc = signature_loc s in
  match s with
  | Sig_value (ident, desc, Exported) ->
    entry_of_value lf ident desc loc
  | Sig_type (ident, decl, _, Exported) ->
    entry_of_type lf ident decl loc
  | Sig_typext (ident, cst, Text_exception, Exported) ->
    entry_of_exn ident cst loc
  | _ -> raise_error (UnsupportedOCamlSignature s)

let error_of_signature s exn : error = {
    error_loc = signature_loc s;
    error_entry = Ident.name (signature_ident s);
    error_exn = error_kind_of_exn exn;
  }

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
