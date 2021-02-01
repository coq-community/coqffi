open Sexplib

type t
val empty : t
val from_path : string -> t
val feed_aliases : t -> Coqffi.Alias.table -> Coqffi.Alias.table

exception FieldShouldBeString of string * Sexp.t
exception SectionShouldBeList of string * Sexp.t
exception MissingField of string * Sexp.t
exception IllFormedAliasesEntry of Sexp.t
