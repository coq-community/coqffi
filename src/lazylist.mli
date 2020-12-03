type 'a t

val push : 'a -> 'a t -> 'a t
val push_list : 'a list -> 'a t -> 'a t
val of_list : 'a list -> 'a t
val singleton : 'a -> 'a t

val (|+) : 'a t -> 'a -> 'a t
val (|++) : 'a t -> 'a list -> 'a t

val pp_print_lazylist : pp_sep:(Format.formatter -> unit -> unit)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit
