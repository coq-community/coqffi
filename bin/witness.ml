open Sexplib
open Coqffi
open Coqffi.Mod
open Coqffi.Entry
open Coqffi.Conflict

let str s = Sexp.Atom s

let list l = Sexp.List l

let ns_to_string = function
  | [ base ] -> [ base ]
  | base :: rst ->
      let seps = [ "."; "__" ] in
      List.map (fun sep -> base ^ sep ^ String.concat "." rst) seps
  | _ -> assert false

let ns_expend ns m =
  let m = of_coq_name m in
  match ns with "" -> m | ns -> ns ^ "." ^ m

let type_to_sexp ~libname ~rev_namespace ~coqns conflicts typ =
  let coqt =
    ns_expend coqns
      (Conflict.get_coq_type rev_namespace conflicts ~ty:typ.type_name)
  in

  List.map
    (fun sep ->
      list
        [
          str
            (libname ^ sep
            ^ String.concat "." (List.rev (typ.type_name :: rev_namespace)));
          str ".";
          str coqt;
        ])
    [ "."; "__" ]

let mutually_recursive_type_to_sexp ~libname ~rev_namespace ~coqns conflicts mt
    =
  Compat.concat_map (type_to_sexp ~libname ~rev_namespace ~coqns conflicts) mt

let rec intro_to_sexp ~libname ~rev_namespace ~coqns conflicts = function
  | Right mt ->
      mutually_recursive_type_to_sexp ~libname ~rev_namespace ~coqns conflicts
        mt
  | Left m ->
      let rev_namespace = m.mod_name :: rev_namespace in
      let coqmod = Conflict.get_coq_module conflicts ~m:m.mod_name in
      let coqns = ns_expend coqns coqmod in
      mod_to_sexp ~libname ~rev_namespace ~coqns conflicts m

and mod_to_sexp ~libname ~rev_namespace ~coqns conflicts m =
  Compat.concat_map
    (intro_to_sexp ~libname ~rev_namespace ~coqns conflicts)
    m.mod_intro

let of_mod ~coqns conflicts m =
  let libname = List.hd m.mod_namespace in
  let rev_namespace = if m.mod_name = libname then [] else [ m.mod_name ] in
  mod_to_sexp ~libname ~rev_namespace ~coqns conflicts m

let pp fmt witness =
  let open Format in
  fprintf fmt "@[<v 2>(translations%a)@]"
    (Pp.pp_list ~pp_prefix:pp_print_space ~pp_sep:pp_print_space Sexp.pp)
    witness
