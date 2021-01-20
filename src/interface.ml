open Cmi_format
open Entry
open Repr

type t = {
  interface_namespace : string list;
  interface_name : string;
  interface_types : type_entry list;
  interface_functions : function_entry list;
  interface_primitives : primitive_entry list;
  interface_exceptions : exception_entry list;
}

let empty_interface (modname : string) =
  let rec namespace_and_path acc = function
    | [x] -> (List.rev acc, String.capitalize_ascii x)
    | x :: rst -> namespace_and_path (String.capitalize_ascii x :: acc) rst
    | _ -> assert false in

  let (namespace, name) = namespace_and_path [] (Str.split (Str.regexp "__") modname)
  in {
    interface_namespace = namespace;
    interface_name = name;
    interface_types = [];
    interface_functions = [];
    interface_primitives = [];
    interface_exceptions = [];
  }

let interface_of_cmi_infos ~features (info : cmi_infos) =
  let add_primitive_entry (m : t) (pr : primitive_entry) : t = {
    m with
    interface_primitives = m.interface_primitives @ [pr]
  } in

  let add_function_entry (m : t) (f : function_entry) : t = {
    m with
    interface_functions = m.interface_functions @ [f]
  } in

  let add_type_entry (m : t) (t : type_entry) : t = {
    m with
    interface_types = m.interface_types @ [t]
  } in

  let add_exception_entry (m : t) (e : exception_entry) : t = {
    m with
    interface_exceptions = m.interface_exceptions @ [e]
  } in

  let add_entry (m : t) = function
    | EPrim pr -> add_primitive_entry m pr
    | EFunc fn -> add_function_entry m fn
    | EType t -> add_type_entry m t
    | EExn e -> add_exception_entry m e in

  let foldf m s =
    try entry_of_signature features s |> add_entry m
    with e ->
      let err = error_of_signature s e in
      Format.fprintf Format.err_formatter "%a: Unsupported entry %s\n"
        Location.print_loc err.error_loc
        err.error_entry;
      m
  in

  List.fold_left foldf (empty_interface info.cmi_name) info.cmi_sign

let qualified_name m name =
  String.concat "." (m.interface_namespace @ [m.interface_name; name])

let translate tbl m =
  let translate_function tbl f = {
    f with
    func_type = translate_type_repr tbl f.func_type
  } in

  let error_function f e = {
      error_loc = f.func_loc;
      error_entry = f.func_name;
      error_exn = e;
    } in

  let translate_primitive tbl prim = {
    prim with
    prim_type = translate_type_repr tbl prim.prim_type
  } in

  let error_primitive p e = {
      error_loc = p.prim_loc;
      error_entry = p.prim_name;
      error_exn = e;
    } in

  let translate_exception tbl e = {
    e with
    exception_args = List.map (translate_mono_type_repr tbl) e.exception_args
  } in

  let error_exception exn e = {
      error_loc = exn.exception_loc;
      error_entry = exn.exception_name;
      error_exn = e;
    } in

  let translate_type_value tbl = function
    | Variant l ->
      Variant (List.map
                 (fun v -> {
                      v with
                      variant_args = List.map (translate_mono_type_repr tbl) v.variant_args
                    } )
                 l)
    | Opaque -> Opaque in

  let translate_type tbl typ =
    let tbl' = List.fold_left
        (fun tbl t -> Translation.preserve t tbl)
        tbl
        typ.type_params in
    {
      typ with
      type_value = translate_type_value tbl' typ.type_value
    } in

  let error_type ty e = {
      error_loc = ty.type_loc;
      error_entry = ty.type_name;
      error_exn = e;
    } in

  let tbl' = List.fold_left
      (fun tbl t -> Translation.preserve t.type_name tbl)
      tbl
      m.interface_types in

  let safe error translate x =
    try
      Some (translate x)
    with e ->
      let err = error x e in
      Format.fprintf Format.err_formatter "%a: Could not translate the type of entry %s\n"
        Location.print_loc err.error_loc
        err.error_entry;
      None in

  {
    m with
    interface_types =
      List.filter_map (safe error_type @@ translate_type tbl') m.interface_types;
    interface_functions =
      List.filter_map (safe error_function @@ translate_function tbl') m.interface_functions;
    interface_primitives =
      List.filter_map (safe error_primitive @@ translate_primitive tbl') m.interface_primitives;
    interface_exceptions =
      List.filter_map (safe error_exception @@ translate_exception tbl') m.interface_exceptions;
  }
