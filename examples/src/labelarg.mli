val test : ?fallback:string -> string -> string [@@pure]

val value : 'a option -> default:'a -> 'a

val valueexn : 'a option -> default:'a -> 'a [@@may_raise]
