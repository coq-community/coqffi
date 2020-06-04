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

val entry_of_signature : Types.signature_item -> entry_desc
val input_of_cmi_infos : Cmi_format.cmi_infos -> interface_desc
