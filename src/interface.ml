open Types
open Parsetree
open Cmi_format
open Format

(* Types *)

exception UnsupportedOcaml of string

type type_tree =
  | ArrowNode of (type_tree * type_tree)
  | TypeLeaf of type_leaf
and type_leaf = (string * type_tree list)

type type_info = {
  poly_vars : string list;
  domain_types : type_tree;
  codomain_type : type_leaf;
}

type type_entry  = {
  name : Ident.t;
  coq_model : string option
}

type function_entry = {
  name : Ident.t;
  coq_model : string option;
  type_sig : type_info;
}

type primitive_entry = {
  name : Ident.t;
  type_sig : type_info;
}

type entry =
  | Primitive of primitive_entry
  | Function of function_entry
  | Type of type_entry

type interface = {
  module_path : string list;
  primitives : primitive_entry list;
  functions : function_entry list;
  types : type_entry list;
}

(* Functions *)

let rec poly_vars (t : type_expr) : string list =
  let minimize = List.sort_uniq String.compare in
  minimize
    (match t.desc with
     | Tvar (Some x) -> [x]
     | Tarrow (_, t1, t2, _) ->
       List.merge String.compare (poly_vars t1) (poly_vars t2)
     | Tconstr (_, types, _) ->
       List.concat_map (fun x -> poly_vars x) types
     | _ ->
       fprintf str_formatter "Cannot find polymorphic types in: %a"
         Printtyp.raw_type_expr t;
       raise (UnsupportedOcaml (flush_str_formatter ())))

let rec to_type_tree (t : type_expr) : type_tree =
  match t.desc with
  | Tvar (Some x) -> TypeLeaf (x, [])
  | Tarrow (_ , t1, t2, _) ->
    let i1 = to_type_tree t1 in
    let i2 = to_type_tree t2 in
    ArrowNode (i1, i2)
  | Tconstr (name, types, _) ->
    TypeLeaf (Path.name name, (List.map (fun x -> to_type_tree x) types))
  | _ ->
    fprintf str_formatter "Unsupported type construction: %a"
      Printtyp.raw_type_expr t;
    raise (UnsupportedOcaml (flush_str_formatter ()))

let to_type_leaf (t : type_expr) : type_leaf option =
  match t.desc with
  | Tconstr (name, types, _) ->
    Some (Path.name name, (List.map (fun x -> to_type_tree x) types))
  | Tvar (Some x) -> Some (x, [])
  | _ -> None

let to_type_info (t : type_expr) : type_info =
  let rec split_arrow t =
    match t.desc with
    | Tarrow (_, t1, t2, _) -> (match split_arrow t2 with
        | (Some x, r) -> (Some (ArrowNode (to_type_tree t1, x)), r)
        | (None, r) -> (Some (to_type_tree t1), r))
    | _ -> (match to_type_leaf t with
        | Some leaf -> (None, leaf)
        | None -> raise (UnsupportedOcaml "")) in
  let poly = poly_vars t in
  match split_arrow t with
  | (Some x, r) -> { poly_vars = poly; domain_types = x; codomain_type = r }
  | (_, _) -> assert false

let empty (modname : string) : interface =
  let modpath = Str.split (Str.regexp "__") modname in {
    module_path = modpath;
    primitives = [];
    functions = [];
    types = [];
  }

let has_ffi_pure : attributes -> bool =
  let is_ffi_pure attr = String.equal attr.attr_name.txt "ffi_pure" in
  List.exists is_ffi_pure

let expr_to_string = function
  | Pexp_constant (Pconst_string (str, _)) -> Some str
  | _ -> None

let struct_to_string = function
  | Pstr_eval (expr, _) -> expr_to_string expr.pexp_desc
  | _ -> None

let find_coq_model log_fmt =
  let get_model attr = if String.equal attr.attr_name.txt "coq_model"
    then match attr.attr_payload with
      | PStr [ model ] -> struct_to_string model.pstr_desc
      | _ -> begin
          fprintf log_fmt "Missing payload for `coq_model': treated as axiom.\n";
          None
        end
    else
      None in
  List.find_map get_model

let entry_of_signature log_fmt = function
  | Sig_value (ident, desc, Exported) ->
    if has_ffi_pure desc.val_attributes
    then Some (Function {
        name = ident;
        coq_model = find_coq_model log_fmt desc.val_attributes;
        type_sig = to_type_info desc.val_type;
      })
    else Some (Primitive {
        name = ident;
        type_sig = to_type_info desc.val_type;
      })
  | Sig_type (ident, desc, _, Exported) ->
    Some (Type {
      name = ident;
      coq_model = find_coq_model log_fmt desc.type_attributes;
    })
  | _ ->
    fprintf log_fmt "Unsupported entry: ignored.\n";
    None

let input_of_cmi_infos log_fmt x =
  let add_entry i = function
    | Some (Function f) -> { i with functions = f :: i.functions }
    | Some (Primitive p) -> { i with primitives = p :: i.primitives }
    | Some (Type t) -> {i with types = t :: i.types }
    | None -> i in
  List.fold_left (fun i s -> entry_of_signature log_fmt s |> add_entry i)
    (empty x.cmi_name)
    x.cmi_sign
