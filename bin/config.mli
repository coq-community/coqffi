open Coqffi
open Sexplib

type t = { config_conflicts : Conflict.t; config_translations : Translation.t }

val empty : t

type lang

val empty_lang : lang

val config_from_path : string option -> string list -> t
(** [config_from_path aliases includes] reads aliases from [aliases] and
    translation types from [includes] *)

exception FieldShouldBeString of string * Sexp.t

exception SectionShouldBeList of string * Sexp.t

exception MissingField of string * Sexp.t

exception IllFormedAliasesEntry of Sexp.t
