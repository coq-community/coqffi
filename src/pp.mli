open Format

val pp_if_not_empty : (formatter -> unit -> unit) -> formatter
  -> 'a list
  -> unit

val pp_list : ?pp_prefix:(formatter -> unit -> unit)
  -> ?pp_suffix:(formatter -> unit -> unit)
  -> pp_sep:(formatter -> unit -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'a list
  -> unit

val pp_args_list : formatter -> Repr.type_repr list -> unit

val pp_type_args_list : formatter -> string list -> unit
