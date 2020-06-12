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

type type_entry = {
  type_name : string;
  type_params : string list;
  type_model : string option;
}

type entry =
  | EPrim of primitive_entry
  | EFunc of function_entry
  | EType of type_entry

val entry_of_signature : Types.signature_item -> entry

exception UnsupportedOCamlSignature of Types.signature_item

type input_module = {
  module_namespace : string list;
  module_name : string;
  module_types : type_entry list;
  module_functions : function_entry list;
  module_primitives : primitive_entry list;
}

val empty_module : string -> input_module

val input_module_of_cmi_infos : Cmi_format.cmi_infos -> input_module

val translate : Translation.t -> input_module -> input_module

(** * Format *)

val pp_types_decl : Format.formatter -> input_module -> unit
val pp_types_extract_decl : Format.formatter -> input_module -> unit
val pp_interface_decl : Format.formatter -> input_module -> unit
val pp_interface_primitive_helpers_decl : Format.formatter -> input_module -> unit
val pp_interface_semantics_decl : Format.formatter -> input_module -> unit
val pp_interface_handlers_decl : Format.formatter -> input_module -> unit
val pp_interface_handlers_extract_decl : Format.formatter -> input_module -> unit
val pp_input_module : Format.formatter -> input_module -> unit
