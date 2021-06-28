open Entry
open Conflict
open Format
open Feature
open Mod
open Repr
open Lazylist
open Pp

let qualified_name m name = Conflict.qualified_name m.mod_namespace name

let pp_lwt_return fmt = function
  | Some lwt_module -> fprintf fmt "%s.return" lwt_module
  | _ -> assert false

type from_require_import = { import_from : string; import_module : string }

let pp_from_require_import fmt fri =
  fprintf fmt "From %s Require Import %s." fri.import_from fri.import_module

type from_require_export = { export_from : string; export_module : string }

let pp_from_require_export fmt fre =
  fprintf fmt "From %s Require Export %s." fre.export_from fre.export_module

type require = { require_module : string }

let pp_require fmt req = fprintf fmt "Require %s." req.require_module

type constructor = {
  constructor_name : coq_name;
  constructor_prototype : Repr.prototype_repr;
}

let pp_constructor fmt c =
  fprintf fmt "| @[<hov 2>@[<hov 2>%a%a%a%a%a@]@ : %a@]" pp_coq_name
    c.constructor_name
    (pp_if_not_empty pp_print_space)
    c.constructor_prototype.prototype_type_args pp_type_args_list
    c.constructor_prototype.prototype_type_args
    (pp_if_not_empty pp_print_space)
    c.constructor_prototype.prototype_args pp_args_list
    c.constructor_prototype.prototype_args pp_type_repr
    c.constructor_prototype.prototype_ret_type

type inductive = {
  inductive_name : coq_name;
  inductive_type_args : string list;
  inductive_type : Repr.type_repr;
  inductive_constructors : constructor list;
}

let pp_inductive fmt = function
  | [] -> ()
  | lind ->
      let pp_inductive_aux fmt ind =
        fprintf fmt "%a%a%a@ : %a :=@]@ %a" pp_coq_name ind.inductive_name
          (pp_if_not_empty pp_print_space)
          ind.inductive_type_args pp_type_args_list ind.inductive_type_args
          pp_type_repr ind.inductive_type
          (pp_print_list ~pp_sep:pp_print_space pp_constructor)
          ind.inductive_constructors
      in
      fprintf fmt "@[<v>@[<hov 2>Inductive %a.@]"
        (pp_print_list
           ~pp_sep:(fun fmt _ -> fprintf fmt "@ @[<hov 2>with ")
           pp_inductive_aux)
        lind

type field = { field_name : coq_name; field_type : mono_type_repr }

type record = {
  record_name : coq_name;
  record_type_args : string list;
  record_fields : field list;
}

let pp_record fmt r =
  let pp_field fmt f =
    fprintf fmt "%a : %a" pp_coq_name f.field_name pp_mono_type_repr
      f.field_type
  in

  fprintf fmt "@[<hov 2>Record %a%a%a :=@ @[<hov>{@ %a@ }.@]@]" pp_coq_name
    r.record_name
    (pp_if_not_empty (fun fmt _ -> pp_print_text fmt " "))
    r.record_type_args pp_type_args_list r.record_type_args
    (pp_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ ; ") pp_field)
    r.record_fields

type definition = {
  def_name : coq_name;
  def_typeclass_args : string list;
  def_prototype : Repr.prototype_repr;
  def_body : Format.formatter -> unit -> unit;
}

let pp_definition fmt def =
  fprintf fmt "@[<hov 2>@[<hov 2>Definition %a%a%a%a%a%a@] :@ %a :=@ %a.@]"
    pp_coq_name def.def_name
    (pp_list
       ~pp_prefix:(fun fmt _ -> pp_print_string fmt " `{")
       ~pp_suffix:(fun fmt _ -> pp_print_string fmt "}")
       ~pp_sep:pp_print_space pp_print_string)
    def.def_typeclass_args
    (pp_if_not_empty pp_print_space)
    def.def_prototype.prototype_type_args pp_type_args_list
    def.def_prototype.prototype_type_args
    (pp_if_not_empty pp_print_space)
    def.def_prototype.prototype_args pp_args_list
    def.def_prototype.prototype_args pp_type_repr
    def.def_prototype.prototype_ret_type def.def_body ()

type typeclass = {
  class_name : coq_name;
  class_typeclass_args : string list;
  class_args : (string * Repr.type_repr) list;
  class_type : Repr.type_repr;
  class_members : (coq_name * Repr.type_repr) list;
}

let pp_typeclass fmt cls =
  fprintf fmt "@[<hov 2>@[<hov 2>Class %a%a%a%a%a@] :@ %a :=@ @[<v>{ %a@ @]}.@]"
    pp_coq_name cls.class_name
    (pp_if_not_empty pp_print_space)
    cls.class_typeclass_args
    (pp_print_list ~pp_sep:pp_print_space pp_print_string)
    cls.class_typeclass_args
    (pp_if_not_empty pp_print_space)
    cls.class_args
    (pp_print_list ~pp_sep:pp_print_space (fun fmt (n, t) ->
         fprintf fmt "(%s : %a)" n pp_type_repr t))
    cls.class_args pp_type_repr cls.class_type
    (pp_print_list
       ~pp_sep:(fun fmt _ -> fprintf fmt "@ ; ")
       (fun fmt (m, t) -> fprintf fmt "%a : %a" pp_coq_name m pp_type_repr t))
    cls.class_members

type instance = {
  instance_name : coq_name;
  instance_typeclass_args : string list;
  instance_type : Repr.type_repr;
  instance_members : (coq_name * string) list;
}

let pp_instance fmt inst =
  fprintf fmt "@[<hov 2>Instance %a%a@ : %a :=@ @[<v>{ %a@ }.@]@]" pp_coq_name
    inst.instance_name
    (pp_list
       ~pp_prefix:(fun fmt _ -> pp_print_string fmt "`{")
       ~pp_suffix:(fun fmt _ -> pp_print_string fmt "} ")
       ~pp_sep:(fun fmt _ -> pp_print_string fmt ", ")
       pp_print_string)
    inst.instance_typeclass_args pp_type_repr inst.instance_type
    (pp_print_list
       ~pp_sep:(fun fmt _ -> fprintf fmt "@ ; ")
       (fun fmt (m, v) -> fprintf fmt "%a := %s" pp_coq_name m v))
    inst.instance_members

type axiom = {
  axiom_name : coq_name;
  axiom_typeclass_args : string list;
  axiom_type : Repr.type_repr;
}

let pp_axiom fmt ax =
  fprintf fmt "@[<hov 2>Axiom %a@ : %a%a.@]" pp_coq_name ax.axiom_name
    (pp_list
       ~pp_prefix:(fun fmt _ -> pp_print_text fmt "forall `{")
       ~pp_suffix:(fun fmt _ -> pp_print_text fmt "}, ")
       ~pp_sep:(fun fmt _ -> pp_print_text fmt ",@ ")
       pp_print_text)
    ax.axiom_typeclass_args pp_type_repr ax.axiom_type

type extract_constant = {
  constant_qualid : coq_name;
  constant_type_vars : string list;
  constant_target : string;
}

let pp_extract_constant fmt extr =
  let print_args_prod fmt = function
    | [] -> ()
    | [ x ] -> fprintf fmt "'%s " x
    | args ->
        fprintf fmt "(%a) "
          (pp_print_list
             ~pp_sep:(fun fmt _ -> pp_print_text fmt ", ")
             (fun fmt -> fprintf fmt "'%s"))
          args
  in

  fprintf fmt "@[<hov 2>Extract Constant %a%a@ => \"%a%s\".@]" pp_coq_name
    extr.constant_qualid
    (pp_list
       ~pp_prefix:(fun fmt _ -> pp_print_char fmt ' ')
       ~pp_sep:(fun fmt _ -> pp_print_string fmt " ")
       (fun fmt -> fprintf fmt "\"'%s\""))
    extr.constant_type_vars print_args_prod extr.constant_type_vars
    extr.constant_target

type extract_inductive = {
  inductive_qualid : coq_name;
  inductive_target : string;
  inductive_variants_target : string list;
}

let pp_extract_inductive fmt ind =
  fprintf fmt "@[<hov 2>Extract Inductive %a =>@ \"%s\"@ [@[<hov 2>%a@]].@]"
    pp_coq_name ind.inductive_qualid ind.inductive_target
    (pp_list ~pp_sep:pp_print_space ~pp_prefix:pp_print_space
       ~pp_suffix:pp_print_space (fun fmt t -> fprintf fmt "\"%s\"" t))
    ind.inductive_variants_target

type coq_module = { coqmod_name : coq_name; coqmod_content : t }

and t =
  | Section of string
  | Subsection of string
  | Comment of string
  | Block of t Lazylist.t
  | CompactedBlock of t Lazylist.t
  | CoqModule of coq_module
  | ConfigPrologue
  | FromRequireImport of from_require_import
  | FromRequireExport of from_require_export
  | Require of require
  | Definition of definition
  | Inductive of inductive list
  | Typeclass of typeclass
  | Instance of instance
  | Record of record
  | Axiom of axiom
  | ExtractConstant of extract_constant
  | ExtractInductive of extract_inductive

let exn_t = TParam (CName "exn", [])

(** [monadic_may_raise_t {m t}] returns {m (sum t exn)}. It fails if
    its argument is in a different form. *)
let monadic_may_raise_t may_raise t =
  if may_raise then
    map_codomain
      (function
        | TParam (m, [ t ]) -> TParam (m, [ TParam (CName "sum", [ t; exn_t ]) ])
        | _ -> assert false)
      t
  else t

(** [monadic_may_raise_t {t}] returns {sum t exn} *)
let may_raise_t may_raise t =
  if may_raise then map_codomain (fun t -> TParam (CName "sum", [ t; exn_t ])) t
  else t

let rec pp_vernac fmt = function
  | Block l ->
      fprintf fmt "@[<v>%a@]"
        (pp_print_lazylist ~pp_sep:(fun fmt _ -> fprintf fmt "@ @ ") pp_vernac)
        l
  | CompactedBlock l ->
      fprintf fmt "@[<v>%a@]"
        (pp_print_lazylist ~pp_sep:pp_print_space pp_vernac)
        l
  | Comment str -> fprintf fmt "(* %s *)" str
  | Section str -> fprintf fmt "(** * %s *)" str
  | Subsection str -> fprintf fmt "(** ** %s *)" str
  | ConfigPrologue ->
      pp_print_list ~pp_sep:pp_print_space pp_print_string fmt
        [
          "Set Implicit Arguments.";
          "Unset Strict Implicit.";
          "Set Contextual Implicit.";
          "Generalizable All Variables.";
          "Close Scope nat_scope.";
        ]
  | CoqModule m -> pp_module fmt m
  | FromRequireImport fri -> pp_from_require_import fmt fri
  | FromRequireExport fre -> pp_from_require_export fmt fre
  | Require req -> pp_require fmt req
  | Definition def -> pp_definition fmt def
  | Inductive ind -> pp_inductive fmt ind
  | Record rc -> pp_record fmt rc
  | Typeclass cls -> pp_typeclass fmt cls
  | Instance inst -> pp_instance fmt inst
  | Axiom ax -> pp_axiom fmt ax
  | ExtractConstant extr -> pp_extract_constant fmt extr
  | ExtractInductive ind -> pp_extract_inductive fmt ind

and pp_module fmt m =
  fprintf fmt "@[<v>@[<v 2>Module %a.@ %a@]@ End %a.@]" pp_coq_name
    m.coqmod_name pp_vernac m.coqmod_content pp_coq_name m.coqmod_name

let block_of_list l = Block (of_list l)

let compacted_block_of_list l = CompactedBlock (of_list l)

let empty = function [] -> true | _ -> false

let ( @? ) cond f = if cond then f else fun x -> x

let proto_vars proto =
  List.map (fun (l, _) -> asprintf "%a" pp_arg_name l) proto.prototype_args

let call_vars proto =
  List.map (fun (l, _) -> asprintf "%a" pp_arg_call l) proto.prototype_args

let instance_member_body proto name =
  let tvars = proto.prototype_type_args in
  let vars = proto_vars proto in
  if 0 < List.length tvars then
    asprintf "@[<h 2>fun%a%a@ => %a%a@]"
      (pp_list
         ~pp_prefix:(fun fmt _ -> pp_print_text fmt " ")
         ~pp_sep:(fun fmt _ -> pp_print_text fmt " ")
         (fun fmt _ -> pp_print_text fmt "_"))
      tvars
      (pp_list
         ~pp_prefix:(fun fmt _ -> pp_print_text fmt " ")
         ~pp_sep:(fun fmt _ -> pp_print_text fmt " ")
         pp_print_text)
      vars pp_coq_name name
      (pp_list
         ~pp_prefix:(fun fmt _ -> pp_print_text fmt " ")
         ~pp_sep:(fun fmt _ -> pp_print_text fmt " ")
         pp_print_text)
      vars
  else of_coq_name name

let requires_vernac features models =
  let requires_freespec =
    Lazylist.push
      (FromRequireImport
         { import_from = "FreeSpec.Core"; import_module = "Core" })
  in

  let requires_io =
    Lazylist.push
      (FromRequireImport
         { import_from = "SimpleIO"; import_module = "IO_Monad" })
  in

  let requires_interface =
    Lazylist.push
      (FromRequireImport { import_from = "CoqFFI"; import_module = "Interface" })
  in

  Lazylist.push
  @@ CompactedBlock
       (singleton
          (FromRequireExport
             { export_from = "CoqFFI"; export_module = "Extraction" })
       |> is_enabled features SimpleIO @? requires_io
       |> is_enabled features Interface @? requires_interface
       |> is_enabled features FreeSpec @? requires_freespec
       |++ List.map (fun x -> Require { require_module = x }) models)

let functions_vernac ~rev_namespace conflicts m =
  let to_def f =
    let func_type = may_raise_t f.func_may_raise f.func_type in
    let func_name =
      Conflict.get_coq_value rev_namespace conflicts ~value:f.func_name
    in

    match f.func_model with
    | Some model ->
        Definition
          {
            def_name = func_name;
            def_typeclass_args = [];
            def_prototype =
              {
                prototype_type_args = [];
                prototype_args = [];
                prototype_ret_type = func_type;
              };
            def_body = (fun fmt _ -> pp_print_string fmt model);
          }
    | _ ->
        Axiom
          {
            axiom_name = func_name;
            axiom_typeclass_args = [];
            axiom_type = func_type;
          }
  in

  let to_extr f =
    let target =
      qualified_name m
        (Conflict.get_ocaml_value rev_namespace conflicts ~value:f.func_name)
    in
    let func_name =
      Conflict.get_coq_value rev_namespace conflicts ~value:f.func_name
    in

    let func_extract f =
      if f.func_may_raise then
        let proto = type_repr_to_prototype_repr f.func_type in
        let pargs = proto_vars proto in
        let cargs = call_vars proto in
        asprintf "(fun %a -> %a)"
          (pp_print_list
             ~pp_sep:(fun fmt _ -> pp_print_string fmt " ")
             pp_print_string)
          pargs
          (pp_try_with (pp_fun_call target cargs))
          ()
      else if has_labelled_arg f.func_type then
        let proto = type_repr_to_prototype_repr f.func_type in
        let pargs = proto_vars proto in
        let cargs = call_vars proto in
        asprintf "(fun %a -> %a)"
          (pp_print_list
             ~pp_sep:(fun fmt _ -> pp_print_string fmt " ")
             pp_print_string)
          pargs
          (pp_fun_call ~paren:false target cargs)
          ()
      else asprintf "%a" pp_ocaml_name target
    in

    ExtractConstant
      {
        constant_qualid = func_name;
        constant_type_vars = [];
        constant_target = func_extract f;
      }
  in

  Lazylist.push_list
    [
      Section "Pure functions";
      compacted_block_of_list @@ List.map to_def m.mod_functions;
      compacted_block_of_list @@ List.map to_extr m.mod_functions;
    ]

let variant_entry_to_constructor ~rev_namespace conflicts owner v =
  {
    constructor_name =
      Conflict.get_coq_constructor rev_namespace conflicts ~owner
        ~cstr:v.variant_name;
    constructor_prototype = v.variant_prototype;
  }

let field_entry_to_field ~rev_namespace owner conflicts (r : field_entry) =
  let field_name =
    Conflict.get_coq_field rev_namespace conflicts ~owner ~field:r.field_name
  in
  { field_name; field_type = r.field_type }

let ind_type =
  let rec aux pos arity =
    if arity == 0 then type_sort_mono
    else
      TLambda
        {
          argtype = { position = pos; kind = PositionedArg };
          domain = type_sort_mono;
          codomain = aux (pos + 1) (arity - 1);
        }
  in
  aux 0

let type_entry_to_vernac ~rev_namespace conflicts features t =
  let transparent = is_enabled features TransparentTypes in

  let type_name =
    Conflict.get_coq_type rev_namespace conflicts ~ty:t.type_name
  in

  match (t.type_value, t.type_model, transparent) with
  | Variant l, None, true ->
      Inductive
        [
          {
            inductive_name = type_name;
            inductive_type_args = t.type_params;
            inductive_constructors =
              List.map
                (variant_entry_to_constructor ~rev_namespace conflicts
                   t.type_name)
                l;
            inductive_type = TMono (ind_type t.type_arity);
          };
        ]
  | Entry.Record r, None, true ->
      Record
        {
          record_name = type_name;
          record_type_args = t.type_params;
          record_fields =
            List.map
              (field_entry_to_field ~rev_namespace t.type_name conflicts)
              r;
        }
  | Alias mono, None, true ->
      Definition
        {
          def_name = type_name;
          def_typeclass_args = [];
          def_prototype =
            {
              prototype_type_args = t.type_params;
              prototype_args = [];
              prototype_ret_type = of_mono_type_repr [] (ind_type t.type_arity);
            };
          def_body = (fun fmt _ -> pp_mono_type_repr fmt mono);
        }
  | _, Some m, _ ->
      Definition
        {
          def_name = type_name;
          def_typeclass_args = [];
          def_prototype =
            {
              prototype_type_args = [];
              prototype_args = [];
              prototype_ret_type =
                of_mono_type_repr t.type_params (ind_type t.type_arity);
            };
          def_body = (fun fmt _ -> pp_print_string fmt m);
        }
  | _ ->
      Axiom
        {
          axiom_typeclass_args = [];
          axiom_name = type_name;
          axiom_type = of_mono_type_repr t.type_params (ind_type t.type_arity);
        }

let io_primitives_vernac ~rev_namespace conflicts m =
  let io_axiom_name name =
    Conflict.get_coq_helper rev_namespace conflicts name Name.io_helper
  in

  let to_axiom prim =
    let axiom_type =
      type_lift "IO" (may_raise_t prim.prim_may_raise prim.prim_type)
    in
    let axiom_name = io_axiom_name prim.prim_name in
    Axiom { axiom_typeclass_args = []; axiom_name; axiom_type }
  in

  let to_extract_constant prim =
    let axiom_name = io_axiom_name prim.prim_name in
    let ocaml_name =
      Conflict.get_ocaml_value rev_namespace conflicts ~value:prim.prim_name
    in
    let proto = type_repr_to_prototype_repr prim.prim_type in
    let pargs = proto_vars proto in
    let cargs = call_vars proto in
    let pp_call = pp_fun_call (qualified_name m ocaml_name) cargs in
    let body =
      asprintf "(%a)"
        (if prim.prim_may_raise then pp_try_with pp_call else pp_call)
        ()
    in

    ExtractConstant
      {
        constant_qualid = axiom_name;
        constant_type_vars = [];
        constant_target =
          asprintf "@[<h>(fun%a k__ -> k__ %s)@]"
            (pp_list
               ~pp_prefix:(fun fmt _ -> pp_print_string fmt " ")
               ~pp_sep:pp_print_space pp_print_string)
            pargs body;
      }
  in

  let instance_vernac =
    let to_member prim =
      let proto = type_repr_to_prototype_repr prim.prim_type in
      let axiom_name = io_axiom_name prim.prim_name in
      let member_body = instance_member_body proto axiom_name in
      let member =
        Conflict.get_coq_value rev_namespace conflicts ~value:prim.prim_name
      in
      (member, member_body)
    in
    let instance_name =
      Conflict.get_coq_helper rev_namespace conflicts m.mod_name
        Name.io_instance
    in
    Instance
      {
        instance_name;
        instance_typeclass_args = [];
        instance_type =
          TMono
            (TParam
               ( CName (sprintf "Monad%s" m.mod_name),
                 [ TParam (CName "IO", []) ] ));
        instance_members = List.map to_member m.mod_primitives;
      }
  in

  Lazylist.push_list
    [
      Subsection "[IO] Instance";
      compacted_block_of_list @@ List.map to_axiom m.mod_primitives;
      compacted_block_of_list @@ List.map to_extract_constant m.mod_primitives;
      instance_vernac;
    ]

let interface_constructor_name rev_namespace conflicts name =
  Conflict.get_coq_helper rev_namespace conflicts ~owner:(List.hd rev_namespace)
    name Name.interface_cstr

let interface_vernac ~rev_namespace conflicts mod_name interface_name class_name
    instance_name all_prims vernacs =
  let interface_name =
    Conflict.get_coq_helper rev_namespace conflicts ~owner:mod_name mod_name
      interface_name
  in

  let hof (i, p) = Repr.higher_order_monadic (CPlaceholder i) p.prim_type in

  let hoprims, prims = List.partition hof all_prims in

  let prim_to_constructor (i, prim) =
    let name =
      interface_constructor_name rev_namespace conflicts prim.prim_name
    in
    let prim_type =
      Repr.fill_placeholder i
        (of_coq_name interface_name)
        (monadic_may_raise_t prim.prim_may_raise prim.prim_type)
    in
    {
      constructor_name = name;
      constructor_prototype =
        {
          prototype_type_args = [];
          prototype_args = [];
          prototype_ret_type = prim_type;
        };
    }
  in

  let prim_to_inj_helper (i, prim) =
    let owner = prim.prim_name in
    let def_name =
      Conflict.get_coq_helper rev_namespace conflicts owner Name.inject_helper
    in
    let cstr =
      interface_constructor_name rev_namespace conflicts prim.prim_name
    in
    let inj_type = monadic_may_raise_t prim.prim_may_raise prim.prim_type in
    let proto =
      Repr.type_repr_to_prototype_repr (Repr.fill_placeholder i "m" inj_type)
    in
    Definition
      {
        def_name;
        def_typeclass_args =
          [ asprintf "Inject %a m" pp_coq_name interface_name ];
        def_prototype = proto;
        def_body =
          (fun fmt _ ->
            fprintf fmt "inject (%a %a)" pp_coq_name cstr
              (pp_print_list ~pp_sep:pp_print_space pp_print_string)
              (proto_vars proto));
      }
  in

  let hof_prim_to_inj (i, prim) =
    let owner = prim.prim_name in
    let axiom_name =
      Conflict.get_coq_helper rev_namespace conflicts owner Name.inject_helper
    in
    let inj_type =
      Repr.fill_placeholder i "m"
      @@ monadic_may_raise_t prim.prim_may_raise prim.prim_type
    in
    Axiom
      {
        axiom_name;
        axiom_typeclass_args =
          [ asprintf "Inject %a m" pp_coq_name interface_name ];
        axiom_type = inj_type;
      }
  in

  let inj_instance =
    let monad_name =
      of_coq_name
        (Conflict.get_coq_helper rev_namespace conflicts mod_name class_name)
    in
    let to_member (_, prim) =
      let member =
        Conflict.get_coq_value rev_namespace conflicts ~value:prim.prim_name
      in
      let proto = type_repr_to_prototype_repr prim.prim_type in
      let helper =
        Conflict.get_coq_helper rev_namespace conflicts prim.prim_name
          Name.inject_helper
      in
      let member_body = instance_member_body proto helper in
      (member, member_body)
    in

    let instance_name =
      Conflict.get_coq_helper rev_namespace conflicts mod_name instance_name
    in

    Instance
      {
        instance_name;
        instance_typeclass_args =
          [ asprintf "Inject %a m" pp_coq_name interface_name ];
        instance_type =
          TMono (TParam (CName monad_name, [ TParam (CName "m", []) ]));
        instance_members = List.map to_member all_prims;
      }
  in

  let mod_inductive =
    Inductive
      [
        {
          inductive_name = interface_name;
          inductive_type_args = [];
          inductive_constructors = List.map prim_to_constructor prims;
          inductive_type = TMono (tlambda [ type_sort_mono ] type_sort_mono);
        };
      ]
  in

  vernacs
  |++ [ Subsection "Interface datatype"; mod_inductive ]
  |++ List.map prim_to_inj_helper prims
  |++ List.map hof_prim_to_inj hoprims
  |+ inj_instance

let lwt_primitives_vernac ~rev_namespace conflicts lwt_module m vernacs =
  let to_lwt t = TParam (CName "Lwt.t", [ t ]) in

  let axiom_name prim =
    let owner = prim.prim_name in
    Conflict.get_coq_helper rev_namespace conflicts owner Name.lwt_sync_helper
  in

  let to_axiom prim =
    Axiom
      {
        axiom_typeclass_args = [];
        axiom_name = axiom_name prim;
        axiom_type = map_codomain to_lwt prim.prim_type;
      }
  in

  let axioms = List.map to_axiom m.mod_primitives in

  let to_extract_target prim =
    let proto = type_repr_to_prototype_repr prim.prim_type in
    let pvars = proto_vars proto in
    let cvars = call_vars proto in
    let target =
      Conflict.get_ocaml_value rev_namespace conflicts ~value:prim.prim_name
    in
    asprintf "(fun %a => %a %a)"
      (pp_list ~pp_sep:(fun fmt _ -> pp_print_string fmt " ") pp_print_string)
      pvars pp_lwt_return lwt_module (pp_fun_call target cvars) ()
  in

  let to_extract prim =
    ExtractConstant
      {
        constant_qualid = axiom_name prim;
        constant_type_vars = [];
        constant_target = to_extract_target prim;
      }
  in

  let extracts = List.map to_extract m.mod_primitives in

  let to_member prim =
    let proto = type_repr_to_prototype_repr prim.prim_type in
    let member_body = instance_member_body proto (axiom_name prim) in
    let member =
      Conflict.get_coq_value rev_namespace conflicts ~value:prim.prim_name
    in
    (member, member_body)
  in

  let instance_name =
    let owner = m.mod_name in
    Conflict.get_coq_helper rev_namespace conflicts owner Name.lwt_sync_instance
  in

  let monad_name =
    let owner = m.mod_name in
    Conflict.get_coq_helper rev_namespace conflicts owner Name.prim_monad
  in

  let instance =
    Instance
      {
        instance_name;
        instance_typeclass_args = [];
        instance_type =
          TMono
            (TParam
               (CName (of_coq_name monad_name), [ TParam (CName "Lwt.t", []) ]));
        instance_members = List.map to_member m.mod_primitives;
      }
  in

  vernacs
  |++ [
        Subsection "[Lwt.t] Instance";
        compacted_block_of_list axioms;
        compacted_block_of_list extracts;
        instance;
      ]

let semantics_vernac ~rev_namespace conflicts m vernacs =
  let axiom_name owner =
    Conflict.get_coq_helper rev_namespace conflicts owner Name.semantics_helper
  in
  let target_name value =
    Conflict.get_ocaml_value rev_namespace conflicts ~value
  in
  let mod_name = String.uppercase_ascii m.mod_name in
  let mod_type = TParam (CName mod_name, []) in

  let prim_target prim =
    let target_name = qualified_name m (target_name prim.prim_name) in
    if prim.prim_may_raise then
      let proto = type_repr_to_prototype_repr prim.prim_type in
      let args = call_vars proto in
      asprintf "(fun %a -> %a)"
        (pp_list ~pp_sep:(fun fmt _ -> pp_print_string fmt " ") pp_print_string)
        args
        (pp_try_with (pp_fun_call target_name args))
        ()
    else of_ocaml_name target_name
  in

  let semantics_name =
    Conflict.get_coq_helper rev_namespace conflicts m.mod_name Name.semantics
  in

  vernacs
  |++ [
        Subsection "FreeSpec Semantics";
        compacted_block_of_list
        @@ List.map
             (fun prim ->
               Axiom
                 {
                   axiom_typeclass_args = [];
                   axiom_name = axiom_name prim.prim_name;
                   axiom_type = may_raise_t prim.prim_may_raise prim.prim_type;
                 })
             m.mod_primitives;
        compacted_block_of_list
        @@ List.map
             (fun prim ->
               ExtractConstant
                 {
                   constant_qualid = axiom_name prim.prim_name;
                   constant_type_vars = [];
                   constant_target = prim_target prim;
                 })
             m.mod_primitives;
        Definition
          {
            def_name = semantics_name;
            def_typeclass_args = [];
            def_prototype =
              {
                prototype_type_args = [];
                prototype_args = [];
                prototype_ret_type =
                  TMono (TParam (CName "semantics", [ mod_type ]));
              };
            def_body =
              (fun fmt _ ->
                fprintf fmt
                  "@[<v 2>bootstrap (fun a e =>@ local @[<v>match e in %s a \
                   return a with@ %a@ end@])@]"
                  mod_name
                  (pp_print_list ~pp_sep:pp_print_space (fun fmt prim ->
                       let proto = type_repr_to_prototype_repr prim.prim_type in
                       let args = proto_vars proto in
                       fprintf fmt "@[<hov 2>| @[<h>%a %a@]@ => @[<h>%a %a@]@]"
                         pp_coq_name
                         (interface_constructor_name rev_namespace conflicts
                            prim.prim_name)
                         (pp_print_list ~pp_sep:pp_print_space pp_print_string)
                         args pp_coq_name
                         (axiom_name prim.prim_name)
                         (pp_print_list ~pp_sep:pp_print_space pp_print_string)
                         args))
                  m.mod_primitives);
          };
      ]

let exceptions_vernac ~rev_namespace conflicts m vernacs =
  let exception_vernac v e =
    let exn_name = e.exception_name in
    let exn_type = TParam (CName "exn", []) in

    let proxy_name =
      Conflict.get_coq_helper rev_namespace conflicts exn_name
        Name.exn_proxy_type
    in
    let proxy_constructor_name =
      Conflict.get_coq_helper rev_namespace conflicts exn_name
        Name.exn_proxy_cstr
    in
    let proxy_type = TParam (CName (of_coq_name proxy_name), []) in

    let proxy_proto =
      { e.exception_prototype with prototype_ret_type = TMono proxy_type }
    in

    let proxy_inductive =
      Inductive
        [
          {
            inductive_name = proxy_name;
            inductive_type_args = [];
            inductive_type = type_sort;
            inductive_constructors =
              [
                {
                  constructor_name = proxy_constructor_name;
                  constructor_prototype = proxy_proto;
                };
              ];
          };
        ]
    in

    let to_exn =
      Conflict.get_coq_helper rev_namespace conflicts exn_name Name.to_exn
    in
    let of_exn =
      Conflict.get_coq_helper rev_namespace conflicts exn_name Name.of_exn
    in

    let to_exn_axiom =
      Axiom
        {
          axiom_typeclass_args = [];
          axiom_name = to_exn;
          axiom_type =
            TMono
              (TLambda
                 {
                   argtype = { position = 0; kind = PositionedArg };
                   domain = proxy_type;
                   codomain = exn_type;
                 });
        }
    in
    let of_exn_axiom =
      Axiom
        {
          axiom_typeclass_args = [];
          axiom_name = of_exn;
          axiom_type =
            TMono
              (TLambda
                 {
                   argtype = { position = 0; kind = PositionedArg };
                   domain = exn_type;
                   codomain = TParam (CName "option", [ proxy_type ]);
                 });
        }
    in

    let vars = proto_vars proxy_proto in

    let enclose = function _ :: _ :: _ -> true | _ -> false in

    let to_exn_extract =
      ExtractConstant
        {
          constant_qualid = to_exn;
          constant_type_vars = [];
          constant_target =
            asprintf "@[<h>(function | %a%a => %s%a)@]" pp_coq_name
              proxy_constructor_name
              (pp_list ~pp_prefix:pp_print_space ~pp_sep:pp_print_space
                 pp_print_string)
              vars exn_name
              (pp_list ~enclose
                 ~pp_prefix:(fun fmt _ -> pp_print_string fmt " (")
                 ~pp_sep:(fun fmt _ -> pp_print_string fmt ", ")
                 ~pp_suffix:(fun fmt _ -> pp_print_string fmt ")")
                 pp_print_string)
              vars;
        }
    in

    let of_exn_extract =
      ExtractConstant
        {
          constant_qualid = of_exn;
          constant_type_vars = [];
          constant_target =
            asprintf "@[<h>(function | %s%a => Some (%a%a) | _ => None)@]"
              exn_name
              (pp_list ~enclose
                 ~pp_prefix:(fun fmt _ -> pp_print_string fmt " (")
                 ~pp_sep:(fun fmt _ -> pp_print_string fmt ", ")
                 ~pp_suffix:(fun fmt _ -> pp_print_string fmt ")")
                 pp_print_string)
              vars pp_coq_name proxy_constructor_name
              (pp_list ~pp_prefix:pp_print_space ~pp_sep:pp_print_space
                 pp_print_string)
              vars;
        }
    in

    let instance_name =
      Conflict.get_coq_helper rev_namespace conflicts exn_name Name.exn_instance
    in

    let exn_instance =
      Instance
        {
          instance_name;
          instance_typeclass_args = [];
          instance_type =
            TMono
              (TParam
                 (CName "Exn", [ TParam (CName (of_coq_name proxy_name), []) ]));
          instance_members =
            [
              (unsafe_coq_name "to_exn", of_coq_name to_exn);
              (unsafe_coq_name "of_exn", of_coq_name of_exn);
            ];
        }
    in

    v
    |++ [
          Subsection (sprintf "[%s]" exn_name);
          proxy_inductive;
          compacted_block_of_list [ to_exn_axiom; of_exn_axiom ];
          compacted_block_of_list [ to_exn_extract; of_exn_extract ];
          exn_instance;
        ]
  in

  vernacs |+ Section "OCaml Exceptions"
  |+ Block (List.fold_left exception_vernac (of_list []) m.mod_exceptions)

let monad_vernac ~rev_namespace conflicts mod_name prims =
  let prim_to_members prim =
    let prim_type = may_raise_t prim.prim_may_raise prim.prim_type in
    let prim_name =
      Conflict.get_coq_value rev_namespace conflicts ~value:prim.prim_name
    in
    (prim_name, type_lift "m" prim_type)
  in

  let class_name =
    Conflict.get_coq_helper rev_namespace conflicts mod_name Name.prim_monad
  in

  Typeclass
    {
      class_name;
      class_typeclass_args = [];
      class_args = [ ("m", TMono (tlambda [ type_sort_mono ] type_sort_mono)) ];
      class_type = type_sort;
      class_members = List.map prim_to_members prims;
    }

let primitives_vernac ~rev_namespace conflicts lwt_module features m vernacs =
  let prims =
    List.map
      (fun p ->
        let t = p.prim_type in
        let i = Repr.fresh_placeholder t in
        let p =
          {
            p with
            prim_type =
              Repr.map_codomain (fun x -> TParam (CPlaceholder i, [ x ])) t;
          }
        in
        (i, p))
      m.mod_primitives
  in

  vernacs
  |++ [
        Section "Impure Primitives";
        Subsection "Monad Definition";
        monad_vernac ~rev_namespace conflicts m.mod_name m.mod_primitives;
      ]
  |> is_enabled features SimpleIO
     @? io_primitives_vernac ~rev_namespace conflicts m
  |> is_enabled features Interface
     @? interface_vernac ~rev_namespace conflicts m.mod_name Name.interface_type
          Name.prim_monad Name.inject_instance prims
  |> is_enabled features FreeSpec @? semantics_vernac ~rev_namespace conflicts m
  |> is_enabled features Lwt
     @? lwt_primitives_vernac ~rev_namespace conflicts lwt_module m

let lwt_vernac ~rev_namespace conflicts features m vernacs =
  let lwt_t = "Lwt.t" in

  let to_prim lwt =
    ( lwt.lwt_placeholder,
      {
        prim_name = lwt.lwt_name;
        prim_type = lwt.lwt_type;
        prim_may_raise = false;
        (* [Lwt.t] encapsulates exceptions *)
        prim_loc = lwt.lwt_loc;
      } )
  in

  let prims = List.map to_prim m.mod_lwt in

  let axiom_name lwt =
    let owner = lwt.lwt_name in
    Conflict.get_coq_helper rev_namespace conflicts owner Name.lwt_async_helper
  in

  let lwt_mono_t arg = TParam (CName lwt_t, arg) in

  let to_axiom lwt =
    Axiom
      {
        axiom_typeclass_args = [];
        axiom_name = axiom_name lwt;
        axiom_type =
          Repr.fill_placeholder lwt.lwt_placeholder lwt_t lwt.lwt_type;
      }
  in

  let axioms = List.map to_axiom m.mod_lwt in

  let member lwt =
    Conflict.get_coq_value rev_namespace conflicts ~value:lwt.lwt_name
  in

  let to_member lwt =
    let member_body =
      instance_member_body
        (type_repr_to_prototype_repr lwt.lwt_type)
        (axiom_name lwt)
    in
    (member lwt, member_body)
  in

  let lwt_to_members lwt =
    (member lwt, Repr.fill_placeholder lwt.lwt_placeholder "m" lwt.lwt_type)
  in

  let class_name =
    Conflict.get_coq_helper rev_namespace conflicts m.mod_name Name.async_monad
  in

  let tclass =
    Typeclass
      {
        class_name;
        class_typeclass_args = [];
        class_args =
          [ ("m", TMono (tlambda [ type_sort_mono ] type_sort_mono)) ];
        class_type = type_sort;
        class_members = List.map lwt_to_members m.mod_lwt;
      }
  in

  let instance_name =
    Conflict.get_coq_helper rev_namespace conflicts m.mod_name
      Name.lwt_async_instance
  in

  let instance =
    Instance
      {
        instance_name;
        instance_typeclass_args = [];
        instance_type =
          TMono (TParam (CName (of_coq_name class_name), [ lwt_mono_t [] ]));
        instance_members = List.map to_member m.mod_lwt;
      }
  in

  let target_name lwt =
    qualified_name m
      (Conflict.get_ocaml_value rev_namespace conflicts ~value:lwt.lwt_name)
  in

  let to_extract lwt =
    ExtractConstant
      {
        constant_qualid = axiom_name lwt;
        constant_type_vars = [];
        constant_target = of_ocaml_name @@ target_name lwt;
      }
  in

  let extracts = List.map to_extract m.mod_lwt in

  vernacs
  |++ [
        Section "Asynchronous Primitives";
        Subsection "Monad Definition";
        tclass;
        Subsection "[Lwt.t] Instance";
        compacted_block_of_list axioms;
        compacted_block_of_list extracts;
        instance;
      ]
  |> is_enabled features Interface
     @? interface_vernac ~rev_namespace conflicts m.mod_name
          Name.async_interface_type Name.async_monad Name.async_inject_instance
          prims

let rec module_vernac ~rev_namespace conflicts lwt_module features models m
    vernac =
  let rev_namespace = m.mod_name :: rev_namespace in

  vernac
  |> (not (empty m.mod_intro))
     @? intros_vernac ~rev_namespace conflicts lwt_module features models m
  |> (not (empty m.mod_exceptions))
     @? exceptions_vernac ~rev_namespace conflicts m
  |> (not (empty m.mod_functions))
     @? functions_vernac ~rev_namespace conflicts m
  |> (not (empty m.mod_primitives))
     @? primitives_vernac ~rev_namespace conflicts lwt_module features m
  |> (not (empty m.mod_lwt)) @? lwt_vernac ~rev_namespace conflicts features m

and intros_vernac ~rev_namespace conflicts lwt_module features models m vernacs
    =
  let transparent = is_enabled features TransparentTypes in

  let type_entry_to_inductive t =
    let type_name =
      Conflict.get_coq_type rev_namespace conflicts ~ty:t.type_name
    in
    match (t.type_value, t.type_model, transparent) with
    | Variant l, None, true ->
        {
          inductive_name = type_name;
          inductive_type_args = t.type_params;
          inductive_constructors =
            List.map
              (variant_entry_to_constructor ~rev_namespace conflicts t.type_name)
              l;
          inductive_type = TMono (ind_type t.type_arity);
        }
    | _ -> assert false
  in

  let mut_type_entries_to_ind t =
    [ Inductive (List.map type_entry_to_inductive t) ]
  in

  let type_entries_to_vernac = function
    | t :: _ as mut_types -> (
        match (t.type_value, t.type_model, transparent) with
        | Variant _, None, true -> mut_type_entries_to_ind mut_types
        | _ ->
            List.map
              (type_entry_to_vernac ~rev_namespace conflicts features)
              mut_types)
    | [] -> []
  in

  let module_to_vernac ~rev_namespace (m : Mod.t) =
    let coqmod_name = Conflict.get_coq_module conflicts ~m:m.mod_name in

    [
      CoqModule
        {
          coqmod_name;
          coqmod_content =
            Block
              (module_vernac ~rev_namespace conflicts lwt_module features models
                 m Lazylist.empty);
        };
    ]
  in

  let intro_list_to_vernac (l : intro list) =
    List.flatten
      (Mod.map_intro_list type_entries_to_vernac
         (module_to_vernac ~rev_namespace)
         l)
  in

  let to_extract = function
    | Right t ->
        Compat.concat_map
          (fun t ->
            let inductive_qualid =
              Conflict.get_coq_type rev_namespace conflicts ~ty:t.type_name
            in
            let inductive_target =
              of_ocaml_name
                (qualified_name m
                   (Conflict.get_ocaml_type rev_namespace conflicts
                      ~ty:t.type_name))
            in
            match (t.type_value, t.type_model, transparent) with
            | Variant l, None, true ->
                [
                  (let variant_target v =
                     of_ocaml_name
                       (qualified_name m
                          (Conflict.get_ocaml_constructor rev_namespace
                             conflicts ~owner:t.type_name ~cstr:v.variant_name))
                   in
                   ExtractInductive
                     {
                       inductive_qualid;
                       inductive_target;
                       inductive_variants_target = List.map variant_target l;
                     });
                ]
            | Record r, None, true ->
                let ns = String.concat "." m.mod_namespace in
                ExtractInductive
                  {
                    inductive_qualid;
                    inductive_target;
                    inductive_variants_target =
                      (let tuple =
                         asprintf "%a"
                           (pp_list
                              ~pp_sep:(fun fmt _ -> fprintf fmt ", ")
                              (fun fmt (f : field_entry) ->
                                pp_print_text fmt f.field_name))
                           r
                       in
                       let fields =
                         asprintf "%a"
                           (pp_list
                              ~pp_sep:(fun fmt _ -> fprintf fmt "; ")
                              (fun fmt (f : field_entry) ->
                                pp_print_text fmt f.field_name))
                           r
                       in
                       [ sprintf "%s.(fun (%s) -> { %s })" ns tuple fields ]);
                  }
                ::
                List.map
                  (fun (f : field_entry) ->
                    let constant_qualid =
                      Conflict.get_coq_field rev_namespace conflicts
                        ~owner:t.type_name ~field:f.field_name
                    in
                    ExtractConstant
                      {
                        constant_qualid;
                        constant_type_vars = [];
                        constant_target =
                          asprintf "%s.(fun x -> x.%s)" ns f.field_name;
                      })
                  r
            | _ ->
                let type_params =
                  fst
                    (pick_params t.type_arity (make_params_pool t.type_params))
                  @ t.type_params
                in
                [
                  ExtractConstant
                    {
                      constant_qualid = inductive_qualid;
                      constant_type_vars = type_params;
                      constant_target = inductive_target;
                    };
                ])
          t
    | _ -> []
  in

  vernacs
  |++ [
        block_of_list @@ intro_list_to_vernac m.mod_intro;
        compacted_block_of_list @@ Compat.concat_map to_extract m.mod_intro;
      ]

let of_mod lwt_module features models conflicts m =
  Block
    (of_list
       [ Comment "This file has been generated by coqffi."; ConfigPrologue ]
    |> requires_vernac features models
    |> module_vernac ~rev_namespace:[] conflicts lwt_module features models m
    |+ Comment "The generated file ends here.")
