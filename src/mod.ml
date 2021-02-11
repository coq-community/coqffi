open Cmi_format
open Entry
open Error

type t = {
    mod_namespace : string list;
    mod_name : string;
    mod_intro : intro list;
    mod_functions : function_entry list;
    mod_primitives : primitive_entry list;
    mod_lwt : lwt_entry list;
    mod_exceptions : exception_entry list;
    mod_loc : Location.t;
  }

and intro =
  | Right of mutually_recursive_types_entry
  | Left of t

let translate_mutually_recursive_types ~rev_namespace
      (tbl : Translation.t) (mt : mutually_recursive_types_entry)
    : Translation.t * intro list =
  let update_table tbl t = Translation.preserve ~rev_namespace t.type_name tbl in
  let tbl' = List.fold_left update_table tbl mt in

  let res = try
      [Right (List.map (translate_type ~rev_namespace tbl') mt)]
    with _ ->
      (* Something went wrong when coqffi tried to translate the types
         within a variant.  As a consequence, we treat each type of [mt]
         as if it were opaque. *)
      List.map
        (fun t -> Right [translate_type ~rev_namespace tbl' { t with type_value = Opaque }])
        mt
  in

  (tbl', res)

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
  String.concat "." (m.mod_namespace @ [name])

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

let error_lwt p e = {
    error_loc = p.lwt_loc;
    error_entry = p.lwt_name;
    error_exn = error_kind_of_exn e;
  }

let error_exception exn e = {
    error_loc = exn.exception_loc;
    error_entry = exn.exception_name;
    error_exn = error_kind_of_exn e;
  }

let safe_translate error translate x =
  try
    Some (translate x)
  with e ->
    pp_error Format.err_formatter (error x e);
    None

let rec of_module_entry ?(rev_namespace=[]) tbl (m : module_entry) : Translation.t * t =
  let (tbl, mod_intro) = segment_module_intro ~rev_namespace tbl m.module_intro in
  let mod_functions =
      List.filter_map
        (safe_translate error_function @@ translate_function ~rev_namespace tbl)
        m.module_functions in
  let mod_primitives =
      List.filter_map
        (safe_translate error_primitive @@ translate_primitive ~rev_namespace tbl)
        m.module_primitives in
  let mod_lwt =
      List.filter_map
        (safe_translate error_lwt @@ translate_lwt ~rev_namespace tbl)
        m.module_lwt in
  let mod_exceptions =
      List.filter_map
        (safe_translate error_exception @@ translate_exception ~rev_namespace tbl)
        m.module_exceptions in

  (tbl, {
     mod_intro;
     mod_name = m.module_name;
     mod_functions;
     mod_primitives;
     mod_lwt;
     mod_exceptions;
     mod_loc = m.module_loc;
     mod_namespace = m.module_namespace
   })

and segment_module_intro ~rev_namespace tbl : intro_entry list -> Translation.t * intro list = function
  | [] -> (tbl, [])
  | IntroMod m :: rst ->
     let rev_namespace = m.module_name :: rev_namespace in
     let (tbl, m) = of_module_entry ~rev_namespace tbl m in
     let (tbl, rst) = segment_module_intro ~rev_namespace tbl rst in
     (tbl, Left  m :: rst)
  | l -> segment_module_intro_aux ~rev_namespace tbl [] l

and segment_module_intro_aux ~rev_namespace tbl acc = function
  | IntroType t :: rst -> segment_module_intro_aux ~rev_namespace tbl (t :: acc) rst
  | l -> match acc with
         | [] -> segment_module_intro ~rev_namespace tbl l
         | typs ->
            (* We have constructed the list backward, so we need to
               reverse it before searching for mutually recursive
               type, so that we do not need to perform a complex
               analysis on types’ dependencies to decide their order
               of declaration. *)
            let typs = List.rev typs in
            let (tbl, typs) = Compat.fold_left_map
                         (translate_mutually_recursive_types ~rev_namespace)
                         tbl
                         (find_mutually_recursive_types typs) in
            let (tbl, rst) = segment_module_intro ~rev_namespace tbl l in
            (tbl, List.concat typs @ rst)

let of_cmi_infos ~translations ~features ~lwt_alias (info : cmi_infos) =
  let (namespace, name) = namespace_and_path info.cmi_name in
  module_of_signatures features lwt_alias namespace name info.cmi_sign
  |> of_module_entry translations
  |> snd
