exception UnsupportedOcaml of string

type type_tree =
  | ArrowNode of (type_tree * type_tree)
  | ProdNode of (type_tree list)
  | TypeLeaf of type_leaf
and type_leaf = (string * type_tree list)

type type_info = {
  poly_vars : string list;
  domain_types : type_tree;
  codomain_type : type_tree;
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

val input_of_cmi_infos : Format.formatter -> Cmi_format.cmi_infos -> interface
val coq_of_ocaml_types : (string, string) Hashtbl.t -> interface -> interface
