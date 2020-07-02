open Repr

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
  type_value : type_value;
}

type mutually_recursive_types_entry = type_entry list

type entry =
  | EPrim of primitive_entry
  | EFunc of function_entry
  | EType of type_entry

val entry_of_signature : ?transparent_types:bool -> Types.signature_item -> entry

exception UnsupportedOCamlSignature of Types.signature_item
exception UnsupportedOCamlTypeKind of Types.type_kind

val dependencies : type_entry -> string list

val find_mutually_recursive_types
  : type_entry list -> mutually_recursive_types_entry list
