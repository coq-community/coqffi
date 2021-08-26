type t

(** The [rev_namespace] parameter shared by the functions of this
    module describes a hierarchy of modules, from the deepest up to
    the root. For instance, the [rev_namespace] parameter for the
    hierarchy [M1.M2.M3] is ["M3"; "M2"; "M1"].

    To each module of this hierarchy, we assign a translation
    table. When we add a translation for the ocaml type [t] (with
    [translate] or [preserve]), not only we add [t] to the translation
    table of [M1.M2.M3]. We also add [M3.t] to the table of [M1.M2]
    and [M2.M3.t] to [M1] and [M1.M2.M3.t] to the root namespace.

    When we are trying to find the translation of [t] within a module
    [M1.M2.M3], we first look if a translation is available at this
    module level. If not, we search for [t] in [M1.M2], then [M1],
    etc. *)

val max_tuple_size : int

val translate :
  ?rev_namespace:string list -> ocaml:string -> coq:string -> t -> t

val preserve : ?rev_namespace:string list -> string -> t -> t

val find : ?rev_namespace:string list -> ocaml:string -> t -> string option

val types_table : t
