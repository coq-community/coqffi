open Entry
open Feature

type t = {
  mod_namespace : string list;
  mod_name : string;
  mod_types : type_entry list;
  mod_functions : function_entry list;
  mod_primitives : primitive_entry list;
  mod_exceptions : exception_entry list;
}

val of_cmi_infos : features:features -> Cmi_format.cmi_infos -> t

val translate : Translation.t -> t -> t

val qualified_name : t -> string -> string
