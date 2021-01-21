open Entry
open Feature

type t = {
  interface_namespace : string list;
  interface_name : string;
  interface_types : type_entry list;
  interface_functions : function_entry list;
  interface_primitives : primitive_entry list;
  interface_exceptions : exception_entry list;
}

val empty_interface : string -> t

val interface_of_cmi_infos
  : features:features -> Cmi_format.cmi_infos -> t

val translate : Translation.t -> t -> t

val qualified_name : t -> string -> string
