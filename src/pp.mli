open Format
open Conflict

val pp_if_not_empty :
  (formatter -> unit -> unit) -> formatter -> 'a list -> unit

val pp_list :
  ?enclose:('a list -> bool) ->
  ?pp_prefix:(formatter -> unit -> unit) ->
  ?pp_suffix:(formatter -> unit -> unit) ->
  pp_sep:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) ->
  formatter ->
  'a list ->
  unit

val pp_arg_name : formatter -> Repr.argument -> unit

val pp_arg_call : formatter -> Repr.argument -> unit

val pp_args_list : formatter -> (Repr.argument * Repr.type_repr) list -> unit

val pp_type_args_list : formatter -> string list -> unit

val pp_try_with : (formatter -> unit -> unit) -> formatter -> unit -> unit

val pp_fun_call :
  ?paren:bool -> ocaml_name -> string list -> formatter -> unit -> unit
