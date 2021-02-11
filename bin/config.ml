open Coqffi
open Sexplib

type t = {
    config_aliases : Alias.table;
    config_translations : Translation.t;
  }

type lang = Sexp.t list
type section = Sexp.t

exception MissingField of string * Sexp.t
exception FieldShouldBeString of string * Sexp.t
exception SectionShouldBeList of string * Sexp.t
exception IllFormedAliasesEntry of Sexp.t

let sexpl_fold_left f acc = function
  | Sexp.List l -> List.fold_left f acc l
  | a -> f acc a

let empty = {
    config_aliases = Alias.default;
    config_translations = Translation.types_table;
  }

let empty_lang = []

let rec get_section name : lang -> section option = function
  | Sexp.List [Sexp.Atom atom; Sexp.Atom value] :: _ when atom = name ->
     Some (Sexp.Atom value)
  | Sexp.List (Sexp.Atom atom :: value) :: _ when atom = name ->
     Some (Sexp.List value)
  | _ :: rst ->
     get_section name rst
  | _ -> None

let get_optional_section name lang =
  Option.value ~default:(Sexp.List []) @@ get_section name lang

let from_path path : lang = open_in path |> Sexp.input_sexps

let get_field name fields =
  let rec get_field : section -> section = function
    | Sexp.List (Sexp.Atom atom :: value :: _) when atom = name -> value
    | Sexp.List (Sexp.Atom _ :: _ :: rst) -> get_field (Sexp.List rst)
    | _ -> raise (MissingField (name, fields)) in
  get_field fields

let get_string_field name fields =
  match get_field name fields with
  | Sexp.Atom str -> str
  | _ -> raise (FieldShouldBeString (name, fields))

let feed_aliases (l : lang) (c : t) : t =
  let add_operator aliases = function
    | Sexp.List [Sexp.Atom ocaml; Sexp.Atom coq] ->
       Alias.add_operator ~ocaml ~coq aliases
    | u -> raise (IllFormedAliasesEntry u) in

  let add_alias aliases op_spec =
    let ocaml = get_string_field ":ocaml" op_spec
    and coq = get_string_field ":coq" op_spec in
    Alias.add_alias ~ocaml ~coq aliases in

  let add_keyword aliases = function
    | Sexp.Atom kw -> Alias.add_keyword aliases ~coq:kw
    | _ -> assert false in

  let feed_section secname f acc =
    sexpl_fold_left f acc (get_optional_section secname l) in

  let config_aliases =
    feed_section "operators" add_operator c.config_aliases
    |> feed_section "aliases" add_alias
    |> feed_section "keywords" add_keyword in

  { c with config_aliases }

let feed_translations (c : t) (l : lang) =
  let translations = get_optional_section "translations" l in

  let config_translations = sexpl_fold_left
    (fun t -> function
      | Sexp.List [Sexp.Atom ocaml; Sexp.Atom "."; Sexp.Atom coq] ->
         Translation.translate ~coq ~ocaml t
      | l -> raise (IllFormedAliasesEntry l))
    c.config_translations
    translations in

  { c with config_translations }

let feed_translations_list (l : lang list) (c : t) =
  List.fold_left feed_translations c l

let config_from_path aliases includes =
  let aliases = Option.value ~default:empty_lang
                         (Option.map from_path aliases) in

  let includes = List.map from_path includes in

  let config = empty in

  feed_aliases aliases config
  |> feed_translations_list includes
