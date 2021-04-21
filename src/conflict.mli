(** {1 Local conflict table} *)

type local
(** A table conflict for a given module. *)

(** {2 Global conflict table} *)

type t

val default : t

val is_keyword : string -> t -> bool

(** {2 Construct conflicts table} *)

val add_keyword : string -> t -> t

val add_operator : string -> coq:string -> t -> t

val add_type : string list -> string -> t -> t

val add_value : string list -> string -> t -> t

val add_field : string list -> owner:string -> string -> t -> t

val add_constructor : string list -> owner:string -> string -> t -> t

val add_helper :
  string list -> ?owner:string -> string -> (string -> string) -> t -> t

(** {2 Get conflict-free names} *)

type ocaml_name

val qualified_name : string list -> ocaml_name -> ocaml_name

val of_ocaml_name : ocaml_name -> string

val pp_ocaml_name : Format.formatter -> ocaml_name -> unit

type coq_name

val of_coq_name : coq_name -> string

val unsafe_coq_name : string -> coq_name

val pp_coq_name : Format.formatter -> coq_name -> unit

(** {3 Types} *)

val get_ocaml_type : string list -> t -> ty:string -> ocaml_name

val get_coq_type : string list -> t -> ty:string -> coq_name

(** {3 Values} *)

val get_ocaml_value : string list -> t -> value:string -> ocaml_name

val get_coq_value : string list -> t -> value:string -> coq_name

(** {3 Fields} *)

val get_ocaml_field :
  string list -> t -> owner:string -> field:string -> ocaml_name

val get_coq_field : string list -> t -> owner:string -> field:string -> coq_name

(** {3 Constructors} *)

val get_ocaml_constructor :
  string list -> t -> owner:string -> cstr:string -> ocaml_name

val get_coq_constructor :
  string list -> t -> owner:string -> cstr:string -> coq_name

(** {4 Helpers} *)

val get_coq_helper :
  string list -> t -> ?owner:string -> string -> (string -> string) -> coq_name

(** {5 Modules} *)

val get_coq_module : t -> m:string -> coq_name
