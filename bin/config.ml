open Coqffi
open Sexplib

type t = { config_conflicts : Conflict.t; config_translations : Translation.t }

type lang = Sexp.t list

type section = Sexp.t

exception MissingField of string * Sexp.t

exception FieldShouldBeString of string * Sexp.t

exception SectionShouldBeList of string * Sexp.t

exception IllFormedAliasesEntry of Sexp.t

let sexpl_fold_left f acc = function
  | Sexp.List l -> List.fold_left f acc l
  | a -> f acc a

let empty =
  {
    config_conflicts = Conflict.default;
    config_translations = Translation.types_table;
  }

let empty_lang = []

let rec get_section name : lang -> section option = function
  | Sexp.List [ Sexp.Atom atom; Sexp.Atom value ] :: _ when atom = name ->
      Some (Sexp.Atom value)
  | Sexp.List (Sexp.Atom atom :: value) :: _ when atom = name ->
      Some (Sexp.List value)
  | _ :: rst -> get_section name rst
  | _ -> None

let get_optional_section name lang =
  Option.value ~default:(Sexp.List []) @@ get_section name lang

let from_path path : lang = open_in path |> Sexp.input_sexps

let feed_aliases (l : lang) (c : t) : t =
  let add_operator aliases = function
    | Sexp.List [ Sexp.Atom op; Sexp.Atom coq ] ->
        Conflict.add_operator op ~coq aliases
    | u -> raise (IllFormedAliasesEntry u)
  in

  let add_keyword aliases = function
    | Sexp.Atom kw -> Conflict.add_keyword kw aliases
    | _ -> assert false
  in

  let feed_section secname f acc =
    sexpl_fold_left f acc (get_optional_section secname l)
  in

  let config_conflicts =
    feed_section "operators" add_operator c.config_conflicts
    |> feed_section "keywords" add_keyword
  in

  { c with config_conflicts }

let feed_translations (c : t) (l : lang) =
  let translations = get_optional_section "translations" l in

  let config_translations =
    sexpl_fold_left
      (fun t -> function
        | Sexp.List [ Sexp.Atom ocaml; Sexp.Atom "."; Sexp.Atom coq ] ->
            Translation.translate ~coq ~ocaml t
        | l -> raise (IllFormedAliasesEntry l))
      c.config_translations translations
  in

  { c with config_translations }

let feed_translations_list (l : lang list) (c : t) =
  List.fold_left feed_translations c l

let config_from_path aliases includes =
  let aliases =
    Option.value ~default:empty_lang (Option.map from_path aliases)
  in

  let includes = List.map from_path includes in

  let config = empty in

  feed_aliases aliases config |> feed_translations_list includes
