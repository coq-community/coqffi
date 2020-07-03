open Config

type t

val add : string -> string -> t -> t
val find : t -> string -> string option

val types_table : extraction_profile -> t
