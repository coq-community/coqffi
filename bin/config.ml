open Coqffi
open Sexplib

type t = {
    config_aliases : Alias.table;
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
    config_aliases = Alias.default
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

  { config_aliases }
