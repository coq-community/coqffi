open Entry
open Format
open Feature
open Mod
open Repr
open Lazylist
open Pp

let pp_lwt_t fmt = function
  | Some lwt_module -> fprintf fmt "%s.t" lwt_module
  | _ -> assert false

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
  constructor_name : string;
  constructor_prototype : Repr.prototype_repr;
}

let pp_constructor fmt c =
  fprintf fmt "| @[<hov 2>@[<hov 2>%s%a%a%a%a@]@ : %a@]" c.constructor_name
    (pp_if_not_empty pp_print_space)
    c.constructor_prototype.prototype_type_args pp_type_args_list
    c.constructor_prototype.prototype_type_args
    (pp_if_not_empty pp_print_space)
    c.constructor_prototype.prototype_args pp_args_list
    c.constructor_prototype.prototype_args pp_type_repr
    c.constructor_prototype.prototype_ret_type

type inductive = {
  inductive_name : string;
  inductive_type_args : string list;
  inductive_type : Repr.type_repr;
  inductive_constructors : constructor list;
}

let pp_inductive fmt = function
  | [] -> ()
  | lind ->
      let pp_inductive_aux fmt ind =
        fprintf fmt "%s%a%a@ : %a :=@]@ %a" ind.inductive_name
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

type definition = {
  def_name : string;
  def_typeclass_args : string list;
  def_prototype : Repr.prototype_repr;
  def_body : Format.formatter -> unit -> unit;
}

let pp_definition fmt def =
  fprintf fmt "@[<hov 2>@[<hov 2>Definition %s%a%a%a%a%a@] :@ %a :=@ %a.@]"
    def.def_name
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
  class_name : string;
  class_typeclass_args : string list;
  class_args : (string * Repr.type_repr) list;
  class_type : Repr.type_repr;
  class_members : (string * Repr.type_repr) list;
}

let pp_typeclass fmt cls =
  fprintf fmt "@[<hov 2>@[<hov 2>Class %s%a%a%a%a@] :@ %a :=@ @[<v>{ %a@ @]}.@]"
    cls.class_name
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
       (fun fmt (m, t) -> fprintf fmt "%s : %a" m pp_type_repr t))
    cls.class_members

type instance = {
  instance_name : string;
  instance_typeclass_args : string list;
  instance_type : Repr.type_repr;
  instance_members : (string * string) list;
}

let pp_instance fmt inst =
  fprintf fmt "@[<hov 2>Instance %s%a@ : %a :=@ @[<v>{ %a@ }.@]@]"
    inst.instance_name
    (pp_list
       ~pp_prefix:(fun fmt _ -> pp_print_string fmt "`{")
       ~pp_suffix:(fun fmt _ -> pp_print_string fmt "} ")
       ~pp_sep:(fun fmt _ -> pp_print_string fmt ", ")
       pp_print_string)
    inst.instance_typeclass_args pp_type_repr inst.instance_type
    (pp_print_list
       ~pp_sep:(fun fmt _ -> fprintf fmt "@ ; ")
       (fun fmt (m, v) -> fprintf fmt "%s := %s" m v))
    inst.instance_members

type axiom = {
  axiom_name : string;
  axiom_typeclass_args : string list;
  axiom_type : Repr.type_repr;
}

let pp_axiom fmt ax =
  fprintf fmt "@[<hov 2>Axiom %s@ : %a%a.@]" ax.axiom_name
    (pp_list
       ~pp_prefix:(fun fmt _ -> pp_print_text fmt "forall `{")
       ~pp_suffix:(fun fmt _ -> pp_print_text fmt "}, ")
       ~pp_sep:(fun fmt _ -> pp_print_text fmt ",@ ")
       pp_print_text)
    ax.axiom_typeclass_args pp_type_repr ax.axiom_type

type extract_constant = {
  constant_qualid : string;
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

  fprintf fmt "@[<hov 2>Extract Constant %s%a@ => \"%a%s\".@]"
    extr.constant_qualid
    (pp_list
       ~pp_prefix:(fun fmt _ -> pp_print_char fmt ' ')
       ~pp_sep:(fun fmt _ -> pp_print_string fmt " ")
       (fun fmt -> fprintf fmt "\"'%s\""))
    extr.constant_type_vars print_args_prod extr.constant_type_vars
    extr.constant_target

type extract_inductive = {
  inductive_qualid : string;
  inductive_target : string;
  inductive_variants_target : string list;
}

let pp_extract_inductive fmt ind =
  fprintf fmt "@[<hov 2>Extract Inductive %s =>@ \"%s\"@ [@[<hov 2>%a@]].@]"
    ind.inductive_qualid ind.inductive_target
    (pp_list ~pp_sep:pp_print_space ~pp_prefix:pp_print_space
       ~pp_suffix:pp_print_space (fun fmt t -> fprintf fmt "\"%s\"" t))
    ind.inductive_variants_target

type coq_module = { coqmod_name : string; coqmod_content : t }

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
  | Typeclass cls -> pp_typeclass fmt cls
  | Instance inst -> pp_instance fmt inst
  | Axiom ax -> pp_axiom fmt ax
  | ExtractConstant extr -> pp_extract_constant fmt extr
  | ExtractInductive ind -> pp_extract_inductive fmt ind

and pp_module fmt m =
  fprintf fmt "@[<v>@[<v 2>Module %s.@ %a@]@ End %s.@]" m.coqmod_name pp_vernac
    m.coqmod_content m.coqmod_name

let block_of_list l = Block (of_list l)

let compacted_block_of_list l = CompactedBlock (of_list l)

let empty = function [] -> true | _ -> false

let ( @? ) cond f = if cond then f else fun x -> x

let call_vars proto =
  List.mapi (fun i _ -> sprintf "x%d" i) proto.prototype_args

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

let functions_vernac aliases m =
  let to_def f =
    let func_type = may_raise_t f.func_may_raise f.func_type in
    let func_name = Alias.coq_name aliases f.func_name in

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
    let target = qualified_name m (Alias.ocaml_name aliases f.func_name) in
    let func_name = Alias.coq_name aliases f.func_name in

    let func_extract f =
      if f.func_may_raise then
        let proto = type_repr_to_prototype_repr f.func_type in
        let args = call_vars proto in
        asprintf "(fun %a -> %a)"
          (pp_print_list
             ~pp_sep:(fun fmt _ -> pp_print_string fmt " ")
             pp_print_string)
          args
          (pp_try_with (pp_fun_call target args))
          ()
      else target
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

let variant_entry_to_constructor (v : variant_entry) : constructor =
  {
    constructor_name = v.variant_name;
    constructor_prototype = v.variant_prototype;
  }

let rec ind_type arity =
  if arity == 0 then type_sort_mono
  else TLambda (type_sort_mono, ind_type (arity - 1))

let type_entry_to_vernac features (t : type_entry) : t =
  let transparent = is_enabled features TransparentTypes in

  match (t.type_value, t.type_model, transparent) with
  | Variant l, None, true ->
      Inductive
        [
          {
            inductive_name = t.type_name;
            inductive_type_args = t.type_params;
            inductive_constructors = List.map variant_entry_to_constructor l;
            inductive_type = TMono (ind_type t.type_arity);
          };
        ]
  | Alias mono, None, true ->
      Definition
        {
          def_name = t.type_name;
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
          def_name = t.type_name;
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
          axiom_name = t.type_name;
          axiom_type = of_mono_type_repr t.type_params (ind_type t.type_arity);
        }

let io_primitives_vernac aliases m =
  let io_axiom_name name = sprintf "ffi_io_%s" name in

  let to_axiom prim =
    let axiom_type =
      type_lift "IO" (may_raise_t prim.prim_may_raise prim.prim_type)
    in
    let axiom_name = Alias.coq_name aliases prim.prim_name in
    Axiom
      {
        axiom_typeclass_args = [];
        axiom_name = io_axiom_name axiom_name;
        axiom_type;
      }
  in

  let to_extract_constant prim =
    let axiom_name = Alias.coq_name aliases prim.prim_name in
    let ocaml_name = Alias.ocaml_name aliases prim.prim_name in
    let proto = type_repr_to_prototype_repr prim.prim_type in
    let args = call_vars proto in
    let pp_call = pp_fun_call ~paren:false (qualified_name m ocaml_name) args in
    let body =
      asprintf "(%a)"
        (if prim.prim_may_raise then pp_try_with pp_call else pp_call)
        ()
    in

    ExtractConstant
      {
        constant_qualid = io_axiom_name axiom_name;
        constant_type_vars = [];
        constant_target =
          asprintf "@[<h>(fun%a k__ -> k__ %s)@]"
            (pp_list
               ~pp_prefix:(fun fmt _ -> pp_print_string fmt " ")
               ~pp_sep:pp_print_space pp_print_string)
            args body;
      }
  in

  let instance_vernac =
    Instance
      {
        instance_name = sprintf "IO_Monad%s" m.mod_name;
        instance_typeclass_args = [];
        instance_type =
          TMono
            (TParam
               ( CName (sprintf "Monad%s" m.mod_name),
                 [ TParam (CName "IO", []) ] ));
        instance_members =
          List.map
            (fun prim ->
              let axiom_name = Alias.coq_name aliases prim.prim_name in
              (axiom_name, io_axiom_name axiom_name))
            m.mod_primitives;
      }
  in

  Lazylist.push_list
    [
      Subsection "[IO] Instance";
      compacted_block_of_list @@ List.map to_axiom m.mod_primitives;
      compacted_block_of_list @@ List.map to_extract_constant m.mod_primitives;
      instance_vernac;
    ]

let constructor_name aliases name =
  Alias.coq_name aliases @@ String.capitalize_ascii name

let interface_vernac aliases mod_name all_prims vernacs =
  let hof (i, p) = Repr.higher_order_monadic (CPlaceholder i) p.prim_type in

  let hoprims, prims = List.partition hof all_prims in

  let prim_to_constructor (i, prim) =
    let prim_name = Alias.coq_name aliases prim.prim_name in
    let prim_type =
      Repr.fill_placeholder i
        (String.uppercase_ascii mod_name)
        (monadic_may_raise_t prim.prim_may_raise prim.prim_type)
    in
    {
      constructor_name = constructor_name aliases prim_name;
      constructor_prototype =
        {
          prototype_type_args = [];
          prototype_args = [];
          prototype_ret_type = prim_type;
        };
    }
  in

  let prim_to_inj_helper (i, prim) =
    let prim_name = Alias.coq_name aliases prim.prim_name in
    let inj_type = monadic_may_raise_t prim.prim_may_raise prim.prim_type in
    let proto =
      Repr.type_repr_to_prototype_repr (Repr.fill_placeholder i "m" inj_type)
    in
    Definition
      {
        def_name = sprintf "inj_%s" prim_name;
        def_typeclass_args =
          [ sprintf "Inject %s m" (String.uppercase_ascii mod_name) ];
        def_prototype = proto;
        def_body =
          (fun fmt _ ->
            fprintf fmt "inject (%s %a)"
              (constructor_name aliases prim_name)
              (pp_print_list ~pp_sep:pp_print_space pp_print_string)
              (call_vars proto));
      }
  in

  let hof_prim_to_inj (i, prim) =
    let prim_name = Alias.coq_name aliases prim.prim_name in
    let inj_type =
      Repr.fill_placeholder i "m"
      @@ monadic_may_raise_t prim.prim_may_raise prim.prim_type
    in
    Axiom
      {
        axiom_name = sprintf "inj_%s" prim_name;
        axiom_typeclass_args =
          [ sprintf "Inject %s m" (String.uppercase_ascii mod_name) ];
        axiom_type = inj_type;
      }
  in

  let inj_instance =
    let monad_name = sprintf "Monad%s" @@ String.capitalize_ascii mod_name in
    let mod_name = String.uppercase_ascii mod_name in
    Instance
      {
        instance_name = sprintf "Inject_%s" monad_name;
        instance_typeclass_args = [ sprintf "Inject %s m" mod_name ];
        instance_type =
          TMono (TParam (CName monad_name, [ TParam (CName "m", []) ]));
        instance_members =
          List.map
            (fun (_, x) ->
              let n = Alias.coq_name aliases x.prim_name in
              (n, sprintf "inj_%s" n))
            all_prims;
      }
  in

  let mod_inductive =
    Inductive
      [
        {
          inductive_name = String.uppercase_ascii mod_name;
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

let lwt_primitives_vernac lwt_module aliases m vernacs =
  let lwt_t = asprintf "%a" pp_lwt_t lwt_module in
  let to_lwt t = TParam (CName lwt_t, [ t ]) in

  let axiom_name prim =
    sprintf "ffi_lwt_%s" (Alias.coq_name aliases prim.prim_name)
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
    let vars = call_vars proto in
    asprintf "(fun %a => %a %a)"
      (pp_list ~pp_sep:(fun fmt _ -> pp_print_string fmt " ") pp_print_string)
      vars pp_lwt_return lwt_module
      (pp_fun_call (Alias.ocaml_name aliases prim.prim_name) vars)
      ()
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
    (Alias.coq_name aliases prim.prim_name, axiom_name prim)
  in

  let instance =
    Instance
      {
        instance_name = sprintf "Monad%s_Lwt" m.mod_name;
        instance_typeclass_args = [];
        instance_type =
          TMono
            (TParam
               ( CName (sprintf "Monad%s" m.mod_name),
                 [ TParam (CName lwt_t, []) ] ));
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

let semantics_vernac aliases m vernacs =
  let axiom_name name = sprintf "ffi_unsafe_%s" name in
  let mod_name = String.uppercase_ascii m.mod_name in
  let mod_type = TParam (CName mod_name, []) in

  let prim_target prim =
    let target_name =
      qualified_name m (Alias.ocaml_name aliases prim.prim_name)
    in
    if prim.prim_may_raise then
      let proto = type_repr_to_prototype_repr prim.prim_type in
      let args = call_vars proto in
      asprintf "(fun %a -> %a)"
        (pp_list ~pp_sep:(fun fmt _ -> pp_print_string fmt " ") pp_print_string)
        args
        (pp_try_with (pp_fun_call target_name args))
        ()
    else target_name
  in

  vernacs
  |++ [
        Subsection "FreeSpec Semantics";
        compacted_block_of_list
        @@ List.map
             (fun prim ->
               let prim_name = Alias.coq_name aliases prim.prim_name in
               Axiom
                 {
                   axiom_typeclass_args = [];
                   axiom_name = axiom_name prim_name;
                   axiom_type = may_raise_t prim.prim_may_raise prim.prim_type;
                 })
             m.mod_primitives;
        compacted_block_of_list
        @@ List.map
             (fun prim ->
               let prim_name = Alias.coq_name aliases prim.prim_name in
               ExtractConstant
                 {
                   constant_qualid = axiom_name prim_name;
                   constant_type_vars = [];
                   constant_target = prim_target prim;
                 })
             m.mod_primitives;
        Definition
          {
            def_name =
              sprintf "%s_unsafe_semantics" (String.lowercase_ascii m.mod_name);
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
                       let prim_name = Alias.coq_name aliases prim.prim_name in
                       let proto = type_repr_to_prototype_repr prim.prim_type in
                       let args = call_vars proto in
                       fprintf fmt "@[<hov 2>| @[<h>%s %a@]@ => @[<h>%s %a@]@]"
                         (constructor_name aliases prim_name)
                         (pp_print_list ~pp_sep:pp_print_space pp_print_string)
                         args (axiom_name prim_name)
                         (pp_print_list ~pp_sep:pp_print_space pp_print_string)
                         args))
                  m.mod_primitives);
          };
      ]

let exceptions_vernac _features m vernacs =
  let exception_vernac v e =
    let exn_name = e.exception_name in
    let exn_type = TParam (CName "exn", []) in
    let proxy_name = sprintf "%sExn" e.exception_name in
    let proxy_constructor_name = sprintf "Make%sExn" e.exception_name in
    let proxy_type = TParam (CName proxy_name, []) in

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

    let to_exn = sprintf "exn_of_%s" (String.lowercase_ascii exn_name) in
    let of_exn = sprintf "%s_of_exn" (String.lowercase_ascii exn_name) in

    let to_exn_axiom =
      Axiom
        {
          axiom_typeclass_args = [];
          axiom_name = to_exn;
          axiom_type = TMono (TLambda (proxy_type, exn_type));
        }
    in
    let of_exn_axiom =
      Axiom
        {
          axiom_typeclass_args = [];
          axiom_name = of_exn;
          axiom_type =
            TMono (TLambda (exn_type, TParam (CName "option", [ proxy_type ])));
        }
    in

    let vars = call_vars proxy_proto in

    let enclose = function _ :: _ :: _ -> true | _ -> false in

    let to_exn_extract =
      ExtractConstant
        {
          constant_qualid = to_exn;
          constant_type_vars = [];
          constant_target =
            asprintf "@[<h>(function | %s%a => %s%a)@]" proxy_constructor_name
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
            asprintf "@[<h>(function | %s%a => Some (%s%a) | _ => None)@]"
              exn_name
              (pp_list ~enclose
                 ~pp_prefix:(fun fmt _ -> pp_print_string fmt " (")
                 ~pp_sep:(fun fmt _ -> pp_print_string fmt ", ")
                 ~pp_suffix:(fun fmt _ -> pp_print_string fmt ")")
                 pp_print_string)
              vars proxy_constructor_name
              (pp_list ~pp_prefix:pp_print_space ~pp_sep:pp_print_space
                 pp_print_string)
              vars;
        }
    in

    let exn_instance =
      Instance
        {
          instance_name = sprintf "%s_Exn" proxy_name;
          instance_typeclass_args = [];
          instance_type =
            TMono (TParam (CName "Exn", [ TParam (CName proxy_name, []) ]));
          instance_members = [ ("to_exn", to_exn); ("of_exn", of_exn) ];
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

let monad_vernac aliases mod_name prims =
  let prim_to_members prim =
    let prim_type = may_raise_t prim.prim_may_raise prim.prim_type in
    let prim_name = Alias.coq_name aliases prim.prim_name in
    (prim_name, type_lift "m" prim_type)
  in

  Typeclass
    {
      class_name = sprintf "Monad%s" mod_name;
      class_typeclass_args = [];
      class_args = [ ("m", TMono (tlambda [ type_sort_mono ] type_sort_mono)) ];
      class_type = type_sort;
      class_members = List.map prim_to_members prims;
    }

let primitives_vernac lwt_module aliases features m vernacs =
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
        monad_vernac aliases m.mod_name m.mod_primitives;
      ]
  |> is_enabled features SimpleIO @? io_primitives_vernac aliases m
  |> is_enabled features Interface @? interface_vernac aliases m.mod_name prims
  |> is_enabled features FreeSpec @? semantics_vernac aliases m
  |> is_enabled features Lwt @? lwt_primitives_vernac lwt_module aliases m

let lwt_vernac lwt_module aliases features m vernacs =
  let lwt_t = asprintf "%a" pp_lwt_t lwt_module in
  let mod_name = sprintf "%s_Async" m.mod_name in

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

  let to_axiom_name lwt =
    sprintf "ffi_lwt_%s" (Alias.coq_name aliases lwt.lwt_name)
  in

  let lwt_mono_t arg = TParam (CName lwt_t, arg) in

  let to_axiom lwt =
    Axiom
      {
        axiom_typeclass_args = [];
        axiom_name = to_axiom_name lwt;
        axiom_type =
          Repr.fill_placeholder lwt.lwt_placeholder lwt_t lwt.lwt_type;
      }
  in

  let axioms = List.map to_axiom m.mod_lwt in

  let to_member lwt =
    (Alias.coq_name aliases lwt.lwt_name, to_axiom_name lwt)
  in

  let monad_name = sprintf "Monad%s" mod_name in

  let lwt_to_members lwt =
    let mem_name = Alias.coq_name aliases lwt.lwt_name in
    (mem_name, Repr.fill_placeholder lwt.lwt_placeholder "m" lwt.lwt_type)
  in

  let tclass =
    Typeclass
      {
        class_name = monad_name;
        class_typeclass_args = [];
        class_args =
          [ ("m", TMono (tlambda [ type_sort_mono ] type_sort_mono)) ];
        class_type = type_sort;
        class_members = List.map lwt_to_members m.mod_lwt;
      }
  in

  let instance =
    Instance
      {
        instance_name = sprintf "%s_Lwt" monad_name;
        instance_typeclass_args = [];
        instance_type = TMono (TParam (CName monad_name, [ lwt_mono_t [] ]));
        instance_members = List.map to_member m.mod_lwt;
      }
  in

  let to_extract lwt =
    ExtractConstant
      {
        constant_qualid = to_axiom_name lwt;
        constant_type_vars = [];
        constant_target = qualified_name m lwt.lwt_name;
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
  |> is_enabled features Interface @? interface_vernac aliases mod_name prims

let rec module_vernac lwt_module aliases features models m vernac =
  vernac
  |> (not (empty m.mod_intro))
     @? intros_vernac lwt_module aliases features models m
  |> (not (empty m.mod_exceptions)) @? exceptions_vernac features m
  |> (not (empty m.mod_functions)) @? functions_vernac aliases m
  |> (not (empty m.mod_primitives))
     @? primitives_vernac lwt_module aliases features m
  |> (not (empty m.mod_lwt)) @? lwt_vernac lwt_module aliases features m

and intros_vernac lwt_module aliases features models m vernacs =
  let transparent = is_enabled features TransparentTypes in

  let type_entry_to_inductive t =
    match (t.type_value, t.type_model, transparent) with
    | Variant l, None, true ->
        {
          inductive_name = t.type_name;
          inductive_type_args = t.type_params;
          inductive_constructors = List.map variant_entry_to_constructor l;
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
        | _ -> List.map (type_entry_to_vernac features) mut_types)
    | [] -> []
  in

  let module_to_vernac (m : Mod.t) =
    let coqmod_name = Alias.coq_name aliases m.mod_name in
    [
      CoqModule
        {
          coqmod_name;
          coqmod_content =
            Block
              (module_vernac lwt_module aliases features models m Lazylist.empty);
        };
    ]
  in

  let intro_list_to_vernac (l : intro list) =
    List.flatten (Mod.map_intro_list type_entries_to_vernac module_to_vernac l)
  in

  let to_extract = function
    | Right t ->
        List.map
          (fun t ->
            match (t.type_value, t.type_model, transparent) with
            | Variant l, None, true ->
                ExtractInductive
                  {
                    inductive_qualid = t.type_name;
                    inductive_target = qualified_name m t.type_name;
                    inductive_variants_target =
                      List.map (fun x -> qualified_name m x.variant_name) l;
                  }
            | _ ->
                let type_params =
                  fst
                    (pick_params t.type_arity (make_params_pool t.type_params))
                  @ t.type_params
                in
                ExtractConstant
                  {
                    constant_qualid = t.type_name;
                    constant_type_vars = type_params;
                    constant_target = qualified_name m t.type_name;
                  })
          t
    | _ -> []
  in

  vernacs
  |++ [
        block_of_list @@ intro_list_to_vernac m.mod_intro;
        compacted_block_of_list @@ Compat.concat_map to_extract m.mod_intro;
      ]

let rec update_aliases_from_intro aliases = function
  | Right mt ->
      List.fold_left
        (fun aliases t -> Alias.add_keyword aliases ~coq:t.type_name)
        aliases mt
  | Left m -> update_aliases aliases m.mod_intro

and update_aliases aliases = List.fold_left update_aliases_from_intro aliases

let of_mod lwt_module aliases features models m =
  let aliases = update_aliases aliases m.mod_intro in

  Block
    (of_list
       [ Comment "This file has been generated by coqffi."; ConfigPrologue ]
    |> requires_vernac features models
    |> module_vernac lwt_module aliases features models m
    |+ Comment "The generated file ends here.")
