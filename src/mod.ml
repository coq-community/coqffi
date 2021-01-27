open Cmi_format
open Entry
open Error

type t = {
    mod_namespace : string list;
    mod_name : string;
    mod_intro : intro list;
    mod_functions : function_entry list;
    mod_primitives : primitive_entry list;
    mod_exceptions : exception_entry list;
    mod_loc : Location.t;
  }

and intro =
  | Right of mutually_recursive_types_entry
  | Left of t

let rec of_module_entry (m : module_entry) : t =
  let mod_intro = segment_module_intro m.module_intro in {
      mod_intro;
      mod_name = m.module_name;
      mod_functions = m.module_functions;
      mod_primitives = m.module_primitives;
      mod_exceptions = m.module_exceptions;
      mod_loc = m.module_loc;
      mod_namespace = m.module_namespace
    }

and segment_module_intro : intro_entry list -> intro list = function
  | IntroMod m :: rst -> Left (of_module_entry m) :: segment_module_intro rst
  | _ :: _ as l -> segment_module_intro_aux [] l
  | [] -> []

and segment_module_intro_aux acc = function
  | IntroType t :: rst -> segment_module_intro_aux (t :: acc) rst
  | l -> match acc with
         | [] -> segment_module_intro l
         | typs ->
            let typs = List.map (fun x -> Right x) (find_mutually_recursive_types typs) in
            typs @ segment_module_intro l

let dispatch f g = function
  | Right mt -> f mt
  | Left m -> g m

let fold_intro_list for_mtyps for_mod =
  List.fold_left (fun acc -> dispatch (for_mtyps acc) (for_mod acc))

let map_intro_list f g = List.map (dispatch f g)

let namespace_and_path modname =
  let rec namespace_and_path acc = function
    | [x] -> (List.rev acc, String.capitalize_ascii x)
    | x :: rst -> namespace_and_path (String.capitalize_ascii x :: acc) rst
    | _ -> assert false in

  namespace_and_path [] (Str.split (Str.regexp "__") modname)

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
    error_loc = m.module_loc;
    error_entry = m.module_name;
    error_exn = error_kind_of_exn e;
  }

let error_intro i =
  match i with
  | IntroType t -> error_type t
  | IntroMod m -> error_mod m

let rec translate_module ?(rev_namespace=[]) tbl m =
  let safe error translate x =
    try
      Some (translate x)
    with e ->
      pp_error Format.err_formatter (error x e);
      None in

  {
    m with
    module_intro =
      List.filter_map (safe error_intro @@ translate_intro ~rev_namespace tbl) m.module_intro;
    module_functions =
      List.filter_map (safe error_function @@ translate_function ~rev_namespace tbl) m.module_functions;
    module_primitives =
      List.filter_map (safe error_primitive @@ translate_primitive ~rev_namespace tbl) m.module_primitives;
    module_exceptions =
      List.filter_map (safe error_exception @@ translate_exception ~rev_namespace tbl) m.module_exceptions;
  }

and translate_intro ~rev_namespace tbl = function
  | IntroType t -> IntroType (translate_type ~rev_namespace tbl t)
  | IntroMod m -> let rev_namespace = m.module_name :: rev_namespace in
                  IntroMod (translate_module ~rev_namespace tbl m)

let rec update_table rev_namespace tbl m =
  let aux tbl = function
    | IntroType t ->
       Translation.preserve ~rev_namespace t.type_name tbl
    | IntroMod m ->
       update_table (m.module_name :: rev_namespace) tbl m in
  List.fold_left aux tbl m.module_intro

let translate tbl m =
  (* FIXME: To support shadowing, we should update the translation
     table while we are translating, not computing it ahead of
     time. *)
  let tbl' = update_table [] tbl m in
  translate_module ~rev_namespace:[] tbl' m

let of_cmi_infos ~features (info : cmi_infos) =
  let (namespace, name) = namespace_and_path info.cmi_name in
  module_of_signatures features namespace name info.cmi_sign
  |> translate Translation.types_table
  |> of_module_entry
