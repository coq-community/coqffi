open Config

type t

val translate : ocaml:string -> coq:string -> t -> t
val preserve : string -> t -> t
val find : t -> ocaml:string -> string option

val types_table : extraction_profile -> t
