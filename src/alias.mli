type t = {
    alias_coq : string;
    alias_operator : bool;
  }

type table

val default : table

val add_operator : ocaml:string -> coq:string -> table -> table
val add_alias : ocaml:string -> coq:string -> table -> table
val add_keyword : coq:string -> table -> table

val ocaml_name : table -> string -> string
val coq_name : table -> string -> string
