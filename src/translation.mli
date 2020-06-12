type t

val empty : t
val add : string -> string -> t -> t
val find : t -> string -> string option
