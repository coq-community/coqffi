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

val input_of_cmi_infos : Cmi_format.cmi_infos -> interface
