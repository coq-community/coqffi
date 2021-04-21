open Cmi_format
open Entry
open Error
open Types

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

and intro = Right of mutually_recursive_types_entry | Left of t

let translate_mutually_recursive_types ~rev_namespace (tbl : Translation.t)
    (mt : mutually_recursive_types_entry) : Translation.t * intro list =
  let update_table rev_namespace tbl t =
    Translation.preserve ~rev_namespace t.type_name tbl
  in

  (* To perform the translation, we do not use the namespace. This is
     because we want coqffi to find an issue with the following entry:

         module Z : sig
           type t = Z.t
         end

     In OCaml, this is a shadowing of Z.t.  But, from coqffi
     perspective, if we were using [tbl_future], then to translate
     [t], coqffi would first try [t], then [Z.t], something we do not
     want.  So we use two different tables. *)
  let tbl_translate = List.fold_left (update_table []) tbl mt in
  let tbl_future = List.fold_left (update_table rev_namespace) tbl mt in

  let res =
    try [ Right (List.map (translate_type ~rev_namespace tbl_translate) mt) ]
    with _ ->
      (* Something went wrong when coqffi tried to translate the types
         within a variant.  As a consequence, we treat each type of [mt]
         as if it were opaque. *)
      List.map
        (fun t ->
          Right
            [
              translate_type ~rev_namespace tbl_translate
                { t with type_value = Opaque };
            ])
        mt
  in

  (tbl_future, res)

let dispatch f g = function Right mt -> f mt | Left m -> g m

let fold_intro_list for_mtyps for_mod =
  List.fold_left (fun acc -> dispatch (for_mtyps acc) (for_mod acc))

let map_intro_list f g = List.map (dispatch f g)

let namespace_and_path modname =
  let rec namespace_and_path acc = function
    | [ x ] -> (List.rev acc, String.capitalize_ascii x)
    | x :: rst -> namespace_and_path (String.capitalize_ascii x :: acc) rst
    | _ -> assert false
  in

  namespace_and_path [] (Str.split (Str.regexp "__") modname)

let error_function f e =
  {
    error_loc = f.func_loc;
    error_entry = f.func_name;
    error_exn = error_kind_of_exn e;
  }

let error_primitive p e =
  {
    error_loc = p.prim_loc;
    error_entry = p.prim_name;
    error_exn = error_kind_of_exn e;
  }

let error_lwt p e =
  {
    error_loc = p.lwt_loc;
    error_entry = p.lwt_name;
    error_exn = error_kind_of_exn e;
  }

let error_exception exn e =
  {
    error_loc = exn.exception_loc;
    error_entry = exn.exception_name;
    error_exn = error_kind_of_exn e;
  }

let safe_translate error translate x =
  try Some (translate x)
  with e ->
    pp_error Format.err_formatter (error x e);
    None

let rec of_module_entry ?(rev_namespace = []) tbl (m : module_entry) :
    Translation.t * t =
  let tbl, mod_intro = segment_module_intro ~rev_namespace tbl m.module_intro in
  let mod_functions =
    List.filter_map
      (safe_translate error_function @@ translate_function ~rev_namespace tbl)
      m.module_functions
  in
  let mod_primitives =
    List.filter_map
      (safe_translate error_primitive @@ translate_primitive ~rev_namespace tbl)
      m.module_primitives
  in
  let mod_lwt =
    List.filter_map
      (safe_translate error_lwt @@ translate_lwt ~rev_namespace tbl)
      m.module_lwt
  in
  let mod_exceptions =
    List.filter_map
      (safe_translate error_exception @@ translate_exception ~rev_namespace tbl)
      m.module_exceptions
  in

  ( tbl,
    {
      mod_intro;
      mod_name = m.module_name;
      mod_functions;
      mod_primitives;
      mod_lwt;
      mod_exceptions;
      mod_loc = m.module_loc;
      mod_namespace = m.module_namespace;
    } )

and segment_module_intro ~rev_namespace tbl :
    intro_entry list -> Translation.t * intro list = function
  | [] -> (tbl, [])
  | IntroMod m :: rst ->
      let rev_namespace = m.module_name :: rev_namespace in
      let tbl, m = of_module_entry ~rev_namespace tbl m in
      let tbl, rst = segment_module_intro ~rev_namespace tbl rst in
      (tbl, Left m :: rst)
  | l -> segment_module_intro_aux ~rev_namespace tbl [] l

and segment_module_intro_aux ~rev_namespace tbl acc = function
  | IntroType t :: rst ->
      segment_module_intro_aux ~rev_namespace tbl (t :: acc) rst
  | l -> (
      match acc with
      | [] -> segment_module_intro ~rev_namespace tbl l
      | typs ->
          let segment_list fstart =
            let aux acc entry =
              match acc with
              | x :: rst ->
                  if fstart entry then [ entry ] :: List.rev x :: rst
                  else (entry :: x) :: rst
              | [] -> [ [ entry ] ]
            in
            List.fold_left aux []
          in
          let find_recursive_types typs =
            let candidates =
              List.rev
                (segment_list
                   (fun t -> t.type_rec = Trec_first || t.type_rec = Trec_not)
                   typs)
            in
            Compat.concat_map find_mutually_recursive_types candidates
          in
          (* We have constructed the list backward, so we need to
             reverse it before searching for mutually recursive
             type, so that we do not need to perform a complex
             analysis on types’ dependencies to decide their order
             of declaration. *)
          let typs = List.rev typs in
          let tbl, typs =
            Compat.fold_left_map
              (translate_mutually_recursive_types ~rev_namespace)
              tbl
              (find_recursive_types typs)
          in
          let tbl, rst = segment_module_intro ~rev_namespace tbl l in
          (tbl, List.concat typs @ rst))

let of_cmi_infos ~translations ~features ~lwt_module (info : cmi_infos) =
  let namespace, name = namespace_and_path info.cmi_name in
  module_of_signatures features lwt_module namespace name info.cmi_sign
  |> of_module_entry translations
  |> snd

let compute_conflicts =
  (* always call with [rev_namesace], which is never empty *)
  let modname = List.hd in

  let cc_variant ~rev_namespace owner conflicts v =
    Conflict.add_constructor rev_namespace ~owner v.variant_name conflicts
  in

  let cc_field ~rev_namespace owner conflicts f =
    Conflict.add_field rev_namespace ~owner f.field_name conflicts
  in

  let cc_type ~rev_namespace conflicts t =
    let conflicts = Conflict.add_type rev_namespace t.type_name conflicts in

    match t.type_value with
    | Variant vs ->
        List.fold_left (cc_variant ~rev_namespace t.type_name) conflicts vs
    | Record fs ->
        List.fold_left (cc_field ~rev_namespace t.type_name) conflicts fs
    | _ -> conflicts
  in

  let cc_types ~rev_namespace conflicts typs =
    List.fold_left (cc_type ~rev_namespace) conflicts typs
  in

  let cc_functions ~rev_namespace funcs conflicts =
    List.fold_left
      (fun conflicts func ->
        Conflict.add_value rev_namespace func.func_name conflicts)
      conflicts funcs
  in

  let cc_prim ~rev_namespace conflicts prim =
    let modname = modname rev_namespace in
    let owner = prim.prim_name in

    Conflict.add_value rev_namespace owner conflicts
    |> Conflict.add_helper rev_namespace owner Name.io_helper
    |> Conflict.add_helper rev_namespace owner Name.lwt_sync_helper
    |> Conflict.add_helper rev_namespace ~owner:modname owner
         Name.interface_cstr
    |> Conflict.add_helper rev_namespace owner Name.inject_helper
    |> Conflict.add_helper rev_namespace owner Name.semantics_helper
  in

  let cc_prims ~rev_namespace prims conflicts =
    let owner = modname rev_namespace in

    List.fold_left (cc_prim ~rev_namespace) conflicts prims
    |> Conflict.add_helper rev_namespace owner Name.prim_monad
    |> Conflict.add_helper rev_namespace owner Name.io_instance
    |> Conflict.add_helper rev_namespace owner Name.lwt_sync_instance
    |> Conflict.add_helper rev_namespace owner Name.interface_type
    |> Conflict.add_helper rev_namespace owner Name.inject_instance
    |> Conflict.add_helper rev_namespace owner Name.semantics
  in

  let cc_lwt ~rev_namespace conflicts lwt =
    let modname = modname rev_namespace in
    let owner = lwt.lwt_name in

    Conflict.add_value rev_namespace lwt.lwt_name conflicts
    |> Conflict.add_helper rev_namespace owner Name.lwt_async_helper
    |> Conflict.add_helper rev_namespace ~owner:modname owner
         Name.interface_cstr
    |> Conflict.add_helper rev_namespace owner Name.inject_helper
    |> Conflict.add_helper rev_namespace owner Name.inject_instance
  in

  let cc_lwts ~rev_namespace lwts conflicts =
    let owner = modname rev_namespace in

    List.fold_left (cc_lwt ~rev_namespace) conflicts lwts
    |> Conflict.add_helper rev_namespace owner Name.async_monad
    |> Conflict.add_helper rev_namespace owner Name.lwt_async_instance
    |> Conflict.add_helper rev_namespace owner Name.async_interface_type
    |> Conflict.add_helper rev_namespace owner Name.async_inject_instance
  in

  let cc_exn ~rev_namespace conflicts exn =
    let owner = exn.exception_name in
    Conflict.add_helper rev_namespace owner Name.to_exn conflicts
    |> Conflict.add_helper rev_namespace owner Name.of_exn
    |> Conflict.add_helper rev_namespace owner Name.exn_proxy_type
    |> Conflict.add_helper rev_namespace owner Name.exn_proxy_cstr
    |> Conflict.add_helper rev_namespace owner Name.exn_instance
  in

  let cc_exns ~rev_namespace exns conflicts =
    List.fold_left (cc_exn ~rev_namespace) conflicts exns
  in

  let rec cc_module ~rev_namespace conflicts m =
    let rev_namespace = m.mod_name :: rev_namespace in

    fold_intro_list (cc_types ~rev_namespace) (cc_module ~rev_namespace)
      conflicts m.mod_intro
    |> cc_functions ~rev_namespace m.mod_functions
    |> cc_prims ~rev_namespace m.mod_primitives
    |> cc_lwts ~rev_namespace m.mod_lwt
    |> cc_exns ~rev_namespace m.mod_exceptions
  in

  cc_module ~rev_namespace:[]
