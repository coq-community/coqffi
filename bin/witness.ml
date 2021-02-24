open Sexplib
open Coqffi
open Coqffi.Mod
open Coqffi.Entry

let str s = Sexp.Atom s
let list l = Sexp.List l
let ns_expend ns m =
  match ns with
  | "" -> m
  | ns -> ns ^ "." ^ m

let type_to_sexp ocamlns coqns typ =
  let ocamlt = ocamlns ^ "." ^ typ.type_name in
  let coqt = ns_expend coqns typ.type_name in
  list [str ocamlt; str "."; str coqt]

let mutually_recursive_type_to_sexp ocamlns coqns mt =
  List.map (type_to_sexp ocamlns coqns) mt

let ns_to_string = function
  | [base] -> [base]
  | base :: rst ->
     let seps  = ["."; "__"] in
     List.map (fun sep -> base ^ sep ^ String.concat "." rst) seps
  | _ -> assert false

let rec intro_to_sexp ocamlns coqns aliases = function
  | Right mt -> mutually_recursive_type_to_sexp ocamlns coqns mt
  | Left m ->
     let coqns = ns_expend coqns (Alias.coq_name aliases m.mod_name) in
     from_mod ~coqns aliases m

and from_mod ~coqns aliases (m : Mod.t) =
  let ocamlns = ns_to_string m.mod_namespace in

  let aux ns = Compat.concat_map (intro_to_sexp ns coqns aliases) m.mod_intro in

  Compat.concat_map aux ocamlns 

let pp fmt witness =
  let open Format in
  fprintf fmt "@[<v 2>(translations%a)@]"
    (Pp.pp_list
       ~pp_prefix:pp_print_space
       ~pp_sep:pp_print_space Sexp.pp) witness
