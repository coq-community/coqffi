open Coqffi
open Sexplib

type t = Sexp.t list

type section = Sexp.t

let empty = []

let rec get_section name : t -> section option = function
  | Sexp.List [Sexp.Atom atom; Sexp.Atom value] :: _ when atom = name ->
     Some (Sexp.Atom value)
  | Sexp.List (Sexp.Atom atom :: value) :: _ when atom = name ->
     Some (Sexp.List value)
  | _ :: rst ->
     get_section name rst
  | _ -> None

let from_path path : t = open_in path |> Sexp.input_sexps

exception MissingField of string * Sexp.t

let get_field name fields =
  let rec get_field : section -> section = function
    | Sexp.List (Sexp.Atom atom :: value :: _) when atom = name -> value
    | Sexp.List (Sexp.Atom _ :: _ :: rst) -> get_field (Sexp.List rst)
    | _ -> raise (MissingField (name, fields)) in
  get_field fields

exception FieldShouldBeString of string * Sexp.t

let get_string_field name fields =
  match get_field name fields with
  | Sexp.Atom str -> str
  | _ -> raise (FieldShouldBeString (name, fields))

exception SectionShouldBeList of string * Sexp.t
exception IllFormedAliasesEntry of Sexp.t

let feed_aliases (c : t) (aliases : Alias.table) : Alias.table =
  let unsafe_to_list = function
    | Sexp.List l -> l
    | a -> raise (SectionShouldBeList ("aliases", a)) in

  let rec add_entries aliases = function
    | Sexp.List [Sexp.Atom "operator"; op_spec] :: rst ->
       let ocaml = get_string_field ":ocaml" op_spec
       and coq = get_string_field ":coq" op_spec in
       add_entries (Alias.add_operator ~ocaml ~coq aliases) rst
    | Sexp.List [Sexp.Atom "alias"; op_spec] :: rst ->
       let ocaml = get_string_field ":ocaml" op_spec
       and coq = get_string_field ":coq" op_spec in
       add_entries (Alias.add_alias ~ocaml ~coq aliases) rst
    | Sexp.List [Sexp.Atom "keyword"; Sexp.Atom keyword] :: rst ->
       add_entries (Alias.add_keyword ~coq:keyword aliases) rst
    | [] -> aliases
    | u :: _ -> raise (IllFormedAliasesEntry u) in

  Option.value ~default:aliases
    (Option.map (fun candidates -> add_entries aliases (unsafe_to_list candidates))
       (get_section "aliases" c))
