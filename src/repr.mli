type mono_type_repr =
  | TLambda of (mono_type_repr * mono_type_repr)
  | TProd of (mono_type_repr list)
  | TParam of (string * mono_type_repr list)

type type_repr =
  | TMono of mono_type_repr
  | TPoly of (string list * mono_type_repr)

exception UnsupportedOCamlType of Types.type_expr

val type_repr_of_type_expr : Types.type_expr -> type_repr

val map_codomain : (mono_type_repr -> mono_type_repr) -> type_repr -> type_repr

val impure_proj : string -> type_repr -> type_repr

val interface_proj : string -> type_repr -> type_repr

exception UnknownOCamlType of string

val translate_type_repr : Translation.t -> type_repr -> type_repr

(** * Format *)

val pp_type_repr_arrows : Format.formatter -> type_repr -> unit

val pp_type_repr_prototype : Format.formatter -> type_repr -> unit

val pp_type_repr_arg_list : Format.formatter -> type_repr -> unit
