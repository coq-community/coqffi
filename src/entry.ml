open Repr
open Parsetree
open Types

type primitive_entry = {
  prim_name : string;
  prim_type : type_repr;
}

type function_entry = {
  func_name : string;
  func_type : type_repr;
  func_model : string option
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

type entry =
  | EPrim of primitive_entry
  | EFunc of function_entry
  | EType of type_entry

exception UnsupportedOCamlSignature of Types.signature_item
exception UnsupportedOCamlTypeKind of Types.type_kind

let entry_of_signature ?(transparent_types = false) (s : Types.signature_item)
  : entry =
  let is_impure attr =
    attr.attr_name.txt = "impure" in

  let has_impure : attributes -> bool =
    List.exists is_impure in

  let expr_to_string = function
    | Pexp_constant (Pconst_string (str, _, _)) -> Some str
    | _ -> None in

  let struct_to_string = function
    | Pstr_eval (expr, _) -> expr_to_string expr.pexp_desc
    | _ -> None in

  let get_model attr =
    if attr.attr_name.txt = "coq_model"
    then match attr.attr_payload with
      | PStr [ model ] -> struct_to_string model.pstr_desc
      | _ -> None
    else None in

  let find_coq_model = Compat.find_map get_model in

  match s with
  | Sig_value (ident, desc, Exported) ->
    let name = Ident.name ident in
    let repr = type_repr_of_type_expr desc.val_type in

    if has_impure desc.val_attributes
    then EPrim {
        prim_name = name;
        prim_type = repr;
      }
    else EFunc {
        func_name = name;
        func_type = repr;
        func_model = find_coq_model desc.val_attributes;
      }
  | Sig_type (ident, decl, _, Exported) ->
    let get_poly t =
      match t.desc with
      | Tvar (Some x) -> Some x
      | _ -> None in

    let minimize f l = List.sort_uniq String.compare (List.filter_map f l) in

    let polys = minimize get_poly decl.type_params in

    let of_constructor_args = function
      | Cstr_tuple typs -> List.map mono_type_repr_of_type_expr typs
      | _ -> assert false in

    let to_variant_entry v = {
      variant_name = Ident.name v.cd_id;
      variant_args = of_constructor_args v.cd_args;
    } in

    let value t =
      if transparent_types
      then match t with
        | Type_abstract -> Opaque
        | Type_variant v -> Variant (List.map to_variant_entry v)
        | t -> raise (UnsupportedOCamlTypeKind t)
      else Opaque in

    let name = Ident.name ident in

    EType {
      type_params = polys;
      type_name = name;
      type_model = find_coq_model decl.type_attributes;
      type_value = value decl.type_kind;
    }
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
