open Cmi_format
open Entry
open Error

type t = module_entry

type intro_list =
  | ConsTypes of (mutually_recursive_types_entry) list * intro_list
  | ConsMod of t * intro_list
  | Nil

let rec segment_mod_intro : intro_entry list -> intro_list = function
  | IntroMod m :: rst -> ConsMod (m, segment_mod_intro rst)
  | _ :: _ as l -> segment_mod_intro_aux [] l
  | [] -> Nil

and segment_mod_intro_aux acc = function
  | IntroType t :: rst -> segment_mod_intro_aux (t :: acc) rst
  | l -> match acc with
         | [] -> segment_mod_intro l
         | typs -> ConsTypes (find_mutually_recursive_types typs, segment_mod_intro l)

let compute_intro_list m = segment_mod_intro m.mod_intro

let rec fold_intro_list for_mtyps for_mod acc = function
  | ConsTypes (mtyps, rst) ->
     fold_intro_list for_mtyps for_mod (List.fold_left for_mtyps acc mtyps) rst
  | ConsMod (m, rst) -> fold_intro_list for_mtyps for_mod (for_mod acc m) rst
  | Nil -> acc

let rec map_intro_list f g = function
  | ConsTypes (mtyps, rst) -> List.map f mtyps @ map_intro_list f g rst
  | ConsMod (m, rst) -> g m :: map_intro_list f g rst
  | Nil -> []

let namespace_and_path modname =
  let rec namespace_and_path acc = function
    | [x] -> (List.rev acc, String.capitalize_ascii x)
    | x :: rst -> namespace_and_path (String.capitalize_ascii x :: acc) rst
    | _ -> assert false in

  namespace_and_path [] (Str.split (Str.regexp "__") modname)

let of_cmi_infos ~features (info : cmi_infos) =
  let (namespace, name) = namespace_and_path info.cmi_name in
  module_of_signatures features namespace name info.cmi_sign

let qualified_name m name =
  String.concat "." (m.mod_namespace @ [m.mod_name; name])

let error_function f e = {
    error_loc = f.func_loc;
    error_entry = f.func_name;
    error_exn = error_kind_of_exn e;
  }

let error_primitive p e = {
    error_loc = p.prim_loc;
    error_entry = p.prim_name;
    error_exn = error_kind_of_exn e;
  }

let error_exception exn e = {
    error_loc = exn.exception_loc;
    error_entry = exn.exception_name;
    error_exn = error_kind_of_exn e;
  }

let error_type ty e = {
    error_loc = ty.type_loc;
    error_entry = ty.type_name;
    error_exn = error_kind_of_exn e;
  }

let error_mod m e = {
    error_loc = m.mod_loc;
    error_entry = m.mod_name;
    error_exn = error_kind_of_exn e;
  }

let error_intro i =
  match i with
  | IntroType t -> error_type t
  | IntroMod m -> error_mod m

let rec update_table tbl m =
  let aux tbl = function
    | IntroType t -> Translation.preserve t.type_name tbl
    | IntroMod m -> update_table tbl m in
  List.fold_left aux tbl m.mod_intro

let rec translate tbl m =
  let tbl' = update_table tbl m in

  let safe error translate x =
    try
      Some (translate x)
    with e ->
      pp_error Format.err_formatter (error x e);
      None in

  {
    m with
    mod_intro =
      List.filter_map (safe error_intro @@ translate_intro tbl') m.mod_intro;
    mod_functions =
      List.filter_map (safe error_function @@ translate_function tbl') m.mod_functions;
    mod_primitives =
      List.filter_map (safe error_primitive @@ translate_primitive tbl') m.mod_primitives;
    mod_exceptions =
      List.filter_map (safe error_exception @@ translate_exception tbl') m.mod_exceptions;
  }

and translate_intro tbl = function
  | IntroType t -> IntroType (translate_type tbl t)
  | IntroMod m -> IntroMod (translate tbl m)
