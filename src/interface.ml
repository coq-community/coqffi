open Cmi_format
open Format
open Entry
open Repr
open Config

type interface = {
  interface_namespace : string list;
  interface_name : string;
  interface_types : type_entry list;
  interface_functions : function_entry list;
  interface_primitives : primitive_entry list;
}

let empty_interface (modname : string) =
  let rec namespace_and_path acc = function
    | [x] -> (List.rev acc, x)
    | x :: rst -> namespace_and_path (x :: acc) rst
    | _ -> assert false in

  let (namespace, name) = namespace_and_path [] (Str.split (Str.regexp "__") modname)
  in {
    interface_namespace = namespace;
    interface_name = name;
    interface_types = [];
    interface_functions = [];
    interface_primitives = [];
  }

let interface_of_cmi_infos ~features (info : cmi_infos) =
  let add_primitive_entry (m : interface) (pr : primitive_entry) : interface = {
    m with
    interface_primitives = m.interface_primitives @ [pr]
  } in

  let add_function_entry (m : interface) (f : function_entry) : interface = {
    m with
    interface_functions = m.interface_functions @ [f]
  } in

  let add_type_entry (m : interface) (t : type_entry) : interface = {
    m with
    interface_types = m.interface_types @ [t]
  } in

  let add_entry (m : interface) = function
    | EPrim pr -> add_primitive_entry m pr
    | EFunc fn -> add_function_entry m fn
    | EType t -> add_type_entry m t in

  List.fold_left
    (fun m s ->
       entry_of_signature ~transparent_types:(is_enabled features TransparentTypes) s
       |> add_entry m)
    (empty_interface info.cmi_name)
    info.cmi_sign

let qualname m name =
  String.concat "." (m.interface_namespace @ [m.interface_name; name])

let translate tbl m =
  let translate_function tbl f = {
    f with
    func_type = translate_type_repr tbl f.func_type
  } in

  let translate_primitive tbl prim = {
    prim with
    prim_type = translate_type_repr tbl prim.prim_type
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

  let tbl' = List.fold_left
      (fun tbl t -> Translation.preserve t.type_name tbl)
      tbl
      m.interface_types in

  {
    m with
    interface_types = List.map (translate_type tbl') m.interface_types;
    interface_functions = List.map (translate_function tbl') m.interface_functions;
    interface_primitives = List.map (translate_primitive tbl') m.interface_primitives;
  }

let pp_interface_decl (fmt : formatter) (m : interface) =
  let interface_name = String.uppercase_ascii m.interface_name in
  let prims = m.interface_primitives in

  let pp_print_primitive fmt prim =
    fprintf fmt "@[<hov 2>| %s@ : %a@]"
      (String.capitalize_ascii prim.prim_name)
      pp_type_repr_arrows (interface_proj interface_name prim.prim_type) in

  fprintf fmt "@[<v>Inductive %s : Type -> Type :=@ %a.@]"
    interface_name
    (pp_print_list ~pp_sep:pp_print_space pp_print_primitive) prims

let pp_functions_decl (fmt : formatter) (m : interface) =
  let pp_function_decl (fmt : formatter) (f : function_entry) =
    match f.func_model with
    | Some model -> fprintf fmt "@[<v 2>@[<hov 2>Definition %s@ : %a :=@]@ %s.@]"
                      f.func_name
                      pp_type_repr_arrows f.func_type
                      model
    | _ -> fprintf fmt "Axiom (%s : %a)."
             f.func_name
             pp_type_repr_arrows f.func_type
  in

  pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ @ ")
    pp_function_decl fmt m.interface_functions

let pp_types_decl (fmt : formatter) (m : interface) =
  let mut_types = find_mutually_recursive_types m.interface_types in

  let rec var_proto ret = function
    | [] -> ret
    | x :: rst -> TLambda (x, var_proto ret rst) in

  let ret_type t =
    TParam (t.type_name, List.map (fun x -> TParam (x, [])) t.type_params) in

  let pp_args fmt = function
    | [] -> ()
    | params ->
      fprintf fmt "@ %a"
        (pp_print_list ~pp_sep:pp_print_newline
           (fun fmt -> fprintf fmt "(%s : Type)")) params in

  let pp_type_param (fmt : formatter) (params : string list) =
    match params with
    | [] -> pp_print_text fmt "Type"
    | _ -> fprintf fmt "@[<hov 2>@[<hv 2>forall %a,@] Type@]"
             (pp_print_list ~pp_sep:pp_print_space
                (fun fmt name -> fprintf fmt "(%s : Type)" name)) params in

  let pp_constructors ret fmt l =
    pp_print_list ~pp_sep:pp_print_space
      (fun fmt e ->
         fprintf fmt "| %s : %a"
           e.variant_name
           pp_mono_type_repr_arrows (var_proto ret e.variant_args)) fmt l in

  let pp_type_decl (fmt : formatter) (t : type_entry) =
    match (t.type_model, t.type_value) with
    | (Some model, _) ->
      fprintf fmt "Definition %s : %a := %s."
        t.type_name
        pp_type_param t.type_params
        model
    | (_, Variant l) ->

      fprintf fmt "@[<v>@[<hv 2>@[<hov 2>Inductive %s%a@]@ : Type :=@]@ %a.@]"
        t.type_name
        pp_args t.type_params
        (pp_constructors (ret_type t)) l
    | _ ->
      fprintf fmt "Axiom (%s : %a)."
        t.type_name
        pp_type_param t.type_params
  in

  let pp_mutually_rectypes_decl (fmt : formatter) (mt : mutually_recursive_types_entry) =
    match mt with
    [t] -> pp_type_decl fmt t
    | _ ->
      fprintf fmt "@[<v>@[<hv 2>@[<hov 2>Inductive %a.@]"
        (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ @[<hv 2>@[<hov 2>with ")
           (fun fmt t ->
              fprintf fmt "%s%a@]@ : Type :=@]@ %a"
                t.type_name
                pp_args t.type_params
                (pp_constructors (ret_type t))
                (match t.type_value with
                 | Variant l -> l
                 | _ -> assert false))) mt
  in

  pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ @ ")
    pp_mutually_rectypes_decl fmt mut_types

let pp_interface_primitive_helpers_decl (fmt : formatter) (m : interface) =
  let interface_name = String.uppercase_ascii m.interface_name in

  pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ @ ")
    (fun fmt prim ->
       let prefix = sprintf "Definition inj_%s `{Inject %s m}"
           prim.prim_name
           interface_name in
       fprintf fmt "@[<hv 2>%a :=@ inject (%s %a)@]."
         (pp_type_repr_prototype prefix) (interface_proj "m" prim.prim_type)
         (String.capitalize_ascii prim.prim_name)
         pp_type_repr_arg_list prim.prim_type)
    fmt m.interface_primitives;

  fprintf fmt "@ @ ";

  fprintf fmt "@[<v 2>Instance Monad%s_Inject `(Inject %s m) : Monad%s m :=@ { %a@ }.@]"
    m.interface_name
    interface_name
    m.interface_name
    (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ ; ")
       (fun fmt prim -> fprintf fmt "%s := inj_%s" prim.prim_name prim.prim_name))
    m.interface_primitives

let pp_types_extract_decl (fmt : formatter) (m : interface) =
  let print_args_list = pp_print_list (fun fmt x -> fprintf fmt " \"'%s\"" x) in

  let print_args_prod fmt = function
    | [] -> ()
    | [x] -> fprintf fmt "'%s " x
    | args -> fprintf fmt "(%a) "
                (pp_print_list ~pp_sep:(fun fmt _ -> pp_print_text fmt ", ")
                   (fun fmt -> fprintf fmt "'%s")) args in

  pp_print_list ~pp_sep:pp_print_space
    (fun fmt t ->
       match (t.type_model, t.type_value) with
       | (None, Variant l) ->
         fprintf fmt "@[<hov 2>Extract Inductive %s =>@ \"%s\"@ [%a].@]"
           t.type_name
           (qualname m t.type_name)
           (pp_print_list ~pp_sep:pp_print_space
              (fun fmt v -> fprintf fmt "\"%s\"" (qualname m v.variant_name))) l
       | _ ->
         fprintf fmt "@[<hov 2>Extract Constant %s%a@ => \"%a%s\".@]"
           t.type_name
           print_args_list t.type_params
           print_args_prod t.type_params
           (qualname m t.type_name)) fmt m.interface_types

let pp_functions_extract_decl (fmt : formatter) (m : interface) =
  pp_print_list ~pp_sep:pp_print_space
    (fun fmt f ->
       fprintf fmt "@[<hov 2>Extract Constant %s@ => \"%s\".@]"
         f.func_name
         (qualname m f.func_name)) fmt m.interface_functions

let pp_io_extract (fmt : formatter) (m : interface) =
  pp_print_list ~pp_sep:pp_print_space
    (fun fmt prim ->
       let k_name = "k__" (* TODO: freshen *) in
       fprintf fmt "@[<hov 2>Extract Constant io_%s =>@ \"(fun %a %s -> %s (%s %a))\".@]"
         prim.prim_name
         pp_type_repr_arg_list prim.prim_type
         k_name
         k_name
         (qualname m prim.prim_name)
         pp_type_repr_arg_list prim.prim_type)
    fmt m.interface_primitives

let pp_impure_decl conf fmt m =
  fprintf fmt "(** * Impure Primitives *)@ @ ";

  fprintf fmt "(** ** Monad *)@ @ ";

  fprintf fmt "@[<v 2>Class Monad%s (m : Type -> Type) : Type :=@ { %a@ }.@]@ @ "
    m.interface_name
    (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ ; ")
       (fun fmt prim -> fprintf fmt "%s : %a"
           prim.prim_name
           pp_type_repr_arrows (interface_proj "m" prim.prim_type)))
    m.interface_primitives;

  fprintf fmt "(** ** [IO] Instance *)@ @ ";

  fprintf fmt "%a@ @ "
    (pp_print_list ~pp_sep:pp_print_space
       (fun fmt prim ->
          fprintf fmt "Axiom (io_%s : %a)."
            prim.prim_name
            pp_type_repr_arrows (interface_proj "IO" prim.prim_type)))
    m.interface_primitives;

  fprintf fmt "%a@ @ " pp_io_extract m;

  fprintf fmt "@[<v 2>Instance Monad%s_IO : Monad%s IO :=@ { %a@ }.@]@ @ "
    m.interface_name
    m.interface_name
    (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ ; ")
       (fun fmt prim -> fprintf fmt "%s := io_%s" prim.prim_name prim.prim_name))
    m.interface_primitives;

  if is_enabled conf Interface
  then begin
    fprintf fmt "(** ** Interface Definition *)@ @ ";

    fprintf fmt "@[<v>%a@]@ @ "
      pp_interface_decl m;

    fprintf fmt "@[<v>%a@]"
      pp_interface_primitive_helpers_decl m
  end

let not_empty = function
  | [] -> false
  | _ -> true

let pp_types fmt m =
  if not_empty m.interface_types then
    begin
      fprintf fmt "(** * Types *)@ @ ";

      fprintf fmt "@[<v>%a@]@ @ "
        pp_types_decl m;

      fprintf fmt "@[<v>%a@]@ @ "
        pp_types_extract_decl m
    end

let pp_funcs fmt m =
  if not_empty m.interface_functions then
    begin
      fprintf fmt "(** * Pure Functions *)@ @ ";

      fprintf fmt "@[<v>%a@]@ @ "
        pp_functions_decl m;

      fprintf fmt "@[<v>%a@]@ @ "
        pp_functions_extract_decl m
    end

let pp_impure conf fmt m =
  if not_empty m.interface_primitives then
    begin
      pp_impure_decl conf fmt m;
    end

let pp_models fmt = function
  | (_ :: _) as models -> begin
      pp_print_list ~pp_sep:pp_print_newline
        (fun fmt -> fprintf fmt "Require %s.")
        fmt models;
      fprintf fmt "@ @ "
    end
  | [] -> ()

let pp_interface (models : string list) (features : Config.features)
    (fmt : formatter) (m : interface) =
  pp_open_vbox fmt 0;
  fprintf fmt "(* This file has been generated by coqffi. *)@ @ ";

  fprintf fmt "Set Implicit Arguments.@ ";
  fprintf fmt "Unset Strict Implicit.@ ";
  fprintf fmt "Set Contextual Implicit.@ ";
  fprintf fmt "Generalizable All Variables.@ @ ";

  fprintf fmt "From CoqFFI Require Export Extraction.@ ";
  fprintf fmt "From SimpleIO Require Import IO_Monad.@ ";
  if is_enabled features Interface
  then fprintf fmt "From CoqFFI Require Import Interface.@ ";
  pp_models fmt models;

  pp_types fmt m;

  pp_funcs fmt m;

  pp_impure features fmt m;

  fprintf fmt "@?";
  pp_close_box fmt ()
