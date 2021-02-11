open Coqffi
open Sexplib

type t = {
    config_aliases : Alias.table;
    config_translations : Translation.t;
  }

val empty : t

type lang

val empty_lang : lang

(** [config_from_path aliases includes] reads aliases from [aliases] and
    translation types from [includes] *)
val config_from_path : string option -> string list -> t

exception FieldShouldBeString of string * Sexp.t
exception SectionShouldBeList of string * Sexp.t
exception MissingField of string * Sexp.t
exception IllFormedAliasesEntry of Sexp.t
