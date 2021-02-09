open Coqffi
open Sexplib

type t = {
    config_aliases : Alias.table;
  }

val empty : t

type lang

val empty_lang : lang
val from_path : string -> lang
val feed_aliases : lang -> t -> t

exception FieldShouldBeString of string * Sexp.t
exception SectionShouldBeList of string * Sexp.t
exception MissingField of string * Sexp.t
exception IllFormedAliasesEntry of Sexp.t
