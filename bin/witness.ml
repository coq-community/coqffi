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

let ns_to_string ~qualid =
  let sep = if qualid then "." else "__" in function
  | base :: rst -> base ^ sep ^ String.concat "." rst
  | _ -> assert false

let rec intro_to_sexp ocamlns coqns = function
  | Right mt -> mutually_recursive_type_to_sexp ocamlns coqns mt
  | Left m ->
     let coqns = ns_expend coqns m.mod_name in
     from_mod ~coqns m

and from_mod ~coqns (m : Mod.t) =
  let ocamlns = ns_to_string ~qualid:true m.mod_namespace in
  let ocamlns' = ns_to_string ~qualid:false m.mod_namespace in

  List.concat [
      Compat.concat_map (intro_to_sexp ocamlns coqns) m.mod_intro;
      Compat.concat_map (intro_to_sexp ocamlns' coqns) m.mod_intro;
    ]

let pp fmt witness =
  let open Format in
  fprintf fmt "@[<v 2>(translations%a)@]"
    (Pp.pp_list
       ~pp_prefix:pp_print_space
       ~pp_sep:pp_print_space Sexp.pp) witness
