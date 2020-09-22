open Entry
open Config

type interface = {
  interface_namespace : string list;
  interface_name : string;
  interface_types : type_entry list;
  interface_functions : function_entry list;
  interface_primitives : primitive_entry list;
}

val empty_interface : string -> interface

val interface_of_cmi_infos
  : features:features -> Cmi_format.cmi_infos -> interface

val translate : Translation.t -> interface -> interface

(** * Format *)

val pp_interface : string list -> features -> Format.formatter -> interface -> unit
