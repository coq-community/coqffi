open Entry
open Format
open Interface
open Config
open Repr
open Lazylist
open Pp

type from_require_import = {
  import_from : string;
  import_module : string;
}

let pp_from_require_import fmt fri =
  fprintf fmt "From %s Require Import %s." fri.import_from fri.import_module

type from_require_export = {
  export_from : string;
  export_module : string;
}

let pp_from_require_export fmt fre =
  fprintf fmt "From %s Require Export %s." fre.export_from fre.export_module

type require =  {
  require_module : string;
}

let pp_require fmt req = fprintf fmt "Require %s." req.require_module

type constructor = {
  constructor_name : string;
  constructor_prototype : Repr.prototype_repr
}

let pp_constructor fmt c =
  fprintf fmt "| @[<hov 2>@[<hov 2>%s%a%a@]@ : %a@]"
    c.constructor_name
    (pp_if_not_empty pp_print_space) c.constructor_prototype.prototype_args
    pp_args_list c.constructor_prototype.prototype_args
    pp_type_repr c.constructor_prototype.prototype_ret_type

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
      fprintf fmt "%s%a%a@ : %a :=@]@ %a"
        ind.inductive_name
        (pp_if_not_empty pp_print_space) ind.inductive_type_args
        pp_type_args_list ind.inductive_type_args
        pp_type_repr ind.inductive_type
        (pp_print_list ~pp_sep:pp_print_space
           pp_constructor) ind.inductive_constructors
    in
    fprintf fmt "@[<v>@[<hov 2>Inductive %a.@]"
      (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ @[<hov 2>with ")
         pp_inductive_aux) lind

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
       ~pp_sep:pp_print_space pp_print_string) def.def_typeclass_args
    (pp_if_not_empty pp_print_space) def.def_prototype.prototype_type_args
    pp_type_args_list def.def_prototype.prototype_type_args
    (pp_if_not_empty pp_print_space) def.def_prototype.prototype_args
    pp_args_list def.def_prototype.prototype_args
    pp_type_repr def.def_prototype.prototype_ret_type
    def.def_body ()

type typeclass = {
  class_name : string;
  class_typeclass_args : string list;
  class_args : (string * Repr.type_repr) list;
  class_type : Repr.type_repr;
  class_members : (string * Repr.type_repr) list
}

let pp_typeclass fmt cls =
  fprintf fmt "@[<hov 2>@[<hov 2>Class %s%a%a%a%a@] :@ %a :=@ @[<v>{ %a@ @]}.@]"
    cls.class_name
    (pp_if_not_empty pp_print_space) cls.class_typeclass_args
    (pp_print_list ~pp_sep:pp_print_space pp_print_string) cls.class_typeclass_args
    (pp_if_not_empty pp_print_space) cls.class_args
    (pp_print_list ~pp_sep:pp_print_space
       (fun fmt (n, t) -> fprintf fmt "(%s : %a)"
           n
           pp_type_repr t)) cls.class_args
    pp_type_repr cls.class_type
    (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ ; ")
       (fun fmt (m, t) -> fprintf fmt "%s : %a"
           m
           pp_type_repr t)) cls.class_members

type instance = {
  instance_name : string;
  instance_typeclass_args : string list;
  instance_type : Repr.type_repr;
  instance_members : (string * string) list;
}

let pp_instance fmt inst =
  fprintf fmt "@[<hov 2>Instance %s%a@ : %a :=@ @[<v>{ %a@ }.@]@]"
    inst.instance_name
    (pp_list ~pp_prefix:(fun fmt _ -> pp_print_string fmt "`{")
       ~pp_suffix:(fun fmt _ -> pp_print_string fmt "} ")
       ~pp_sep:(fun fmt _ -> pp_print_string fmt ", ")
       pp_print_string) inst.instance_typeclass_args
    pp_type_repr inst.instance_type
    (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ ; ")
       (fun fmt (m,v) -> fprintf fmt "%s := %s" m v)) inst.instance_members

type axiom = {
  axiom_name : string;
  axiom_type : Repr.type_repr;
}

let pp_axiom fmt ax =
  fprintf fmt "@[<hov 2>Axiom %s@ : %a.@]"
    ax.axiom_name
    pp_type_repr ax.axiom_type

type extract_constant = {
  constant_qualid : string;
  constant_type_vars : string list;
  constant_target : string;
}

let pp_extract_constant fmt extr =
  let print_args_prod fmt = function
    | [] -> ()
    | [x] -> fprintf fmt "'%s " x
    | args -> fprintf fmt "(%a) "
                (pp_print_list ~pp_sep:(fun fmt _ -> pp_print_text fmt ", ")
                   (fun fmt -> fprintf fmt "'%s")) args in

  fprintf fmt "@[<hov 2>Extract Constant %s%a@ => \"%a%s\".@]"
    extr.constant_qualid
    (pp_list
       ~pp_prefix:(fun fmt _ -> pp_print_char fmt '"')
       ~pp_suffix:(fun fmt _ -> pp_print_char fmt '"')
       ~pp_sep:(fun fmt _ -> pp_print_string fmt " ")
       (fun fmt -> fprintf fmt "'%s")) extr.constant_type_vars
    print_args_prod extr.constant_type_vars
    extr.constant_target

type extract_inductive = {
  inductive_qualid : string;
  inductive_target : string;
  inductive_variants_target : string list;
}

let pp_extract_inductive fmt ind =
  fprintf fmt "@[<hov 2>Extract Inductive %s =>@ \"%s\"@ [@[<hov 2>%a@]].@]"
    ind.inductive_qualid
    ind.inductive_target
    (pp_list ~pp_sep:pp_print_space
       ~pp_prefix:pp_print_space
       ~pp_suffix:pp_print_space
       (fun fmt t -> fprintf fmt "\"%s\"" t)) ind.inductive_variants_target

type t =
  | Section of string
  | Subsection of string
  | Comment of string
  | Block of t Lazylist.t
  | CompactedBlock of t Lazylist.t
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

let rec pp_vernac fmt = function
  | Block l ->
    fprintf fmt "@[<v>%a@]"
      (pp_print_lazylist ~pp_sep:(fun fmt _ -> fprintf fmt "@ @ ") pp_vernac) l

  | CompactedBlock l ->
    fprintf fmt "@[<v>%a@]"
      (pp_print_lazylist ~pp_sep:pp_print_space pp_vernac) l

  | Comment str -> fprintf fmt "(* %s *)" str

  | Section str -> fprintf fmt "(** * %s *)" str

  | Subsection str -> fprintf fmt "(** ** %s *)" str

  | ConfigPrologue ->
      pp_print_list ~pp_sep:pp_print_space pp_print_string fmt [
        "Set Implicit Arguments.";
        "Unset Strict Implicit.";
        "Set Contextual Implicit.";
        "Generalizable All Variables."
      ]

  | FromRequireImport fri -> pp_from_require_import fmt fri
  | FromRequireExport fre -> pp_from_require_export fmt fre
  | Require req -> pp_require fmt req
  | Definition def -> pp_definition fmt def
  | Inductive ind -> pp_inductive fmt ind
  | Typeclass cls -> pp_typeclass fmt cls
  | Instance inst -> pp_instance fmt inst
  | Axiom ax -> pp_axiom fmt ax
  | ExtractConstant extr -> pp_extract_constant  fmt extr
  | ExtractInductive ind -> pp_extract_inductive fmt ind

let block_of_list l = Block (of_list l)
let compacted_block_of_list l = CompactedBlock (of_list l)

let empty = function
  | [] -> true
  | _ -> false

let (@?) cond f = if cond then f else (fun x -> x)

let requires_vernac features models =
  let requires_freespec = Lazylist.push (FromRequireImport {
      import_from = "FreeSpec.Core";
      import_module = "Core"
    }) in

  let requires_io = Lazylist.push (FromRequireImport {
      import_from = "SimpleIO";
      import_module = "IO_Monad"
    }) in

  let requires_interface = Lazylist.push (FromRequireImport {
      import_from = "CoqFFI";
      import_module = "Interface"
    }) in

  Lazylist.push @@ CompactedBlock
    (singleton (FromRequireExport {
         export_from = "CoqFFI";
         export_module = "Extraction"
       })
     |> is_enabled features SimpleIO @? requires_io
     |> is_enabled features Interface @? requires_interface
     |> is_enabled features FreeSpec @? requires_freespec
     |++ List.map (fun x -> Require { require_module = x }) models)

let functions_vernac m =
  let to_def f = match f.func_model with
    | Some model -> Definition {
        def_name = f.func_name;
        def_typeclass_args = [];
        def_prototype = {
          prototype_type_args = [];
          prototype_args = [];
          prototype_ret_type = f.func_type;
        };
        def_body = (fun fmt _ -> pp_print_string fmt model)
      }
    | _ -> Axiom { axiom_name = f.func_name; axiom_type = f.func_type }
  in

  let to_extr f = ExtractConstant {
      constant_qualid = f.func_name;
      constant_type_vars = [];
      constant_target = Interface.qualified_name m f.func_name
    } in

  Lazylist.push_list [
    Section "Pure functions";
    compacted_block_of_list @@ List.map to_def m.interface_functions;
    compacted_block_of_list @@ List.map to_extr m.interface_functions;
  ]

let types_vernac features m vernacs =
  let mut_types = find_mutually_recursive_types m.interface_types in
  let transparent = is_enabled features TransparentTypes in
  let to_type args mono = match args with
    | [] -> TMono mono
    | params -> TPoly (params, mono) in

  let to_constructor t v = {
    constructor_name = v.variant_name;
    constructor_prototype = {
      prototype_type_args = [];
      prototype_args = List.map (fun x -> TMono x) v.variant_args;
      prototype_ret_type = to_type
          []
          (TParam
             (t.type_name,
              List.map (fun x -> TParam (x, [])) t.type_params));
    }
  } in

  let type_entry_to_vernac t =
    match (t.type_value, t.type_model, transparent) with
    | (Variant l, None, true) -> Inductive [{
        inductive_name = t.type_name;
        inductive_type_args = t.type_params;
        inductive_constructors = List.map (to_constructor t) l;
        inductive_type = to_type [] type_sort_mono;
      }]
    | (_, Some m, _) -> Definition {
        def_name = t.type_name;
        def_typeclass_args = [];
        def_prototype = {
          prototype_type_args = [];
          prototype_args = [];
          prototype_ret_type = to_type t.type_params type_sort_mono;
        };
        def_body = fun fmt _ -> pp_print_string fmt m;
      }
    | _ -> Axiom {
        axiom_name = t.type_name;
        axiom_type = to_type t.type_params type_sort_mono
      } in

  let type_entry_to_inductive t =
    match (t.type_value, t.type_model, transparent) with
    | (Variant l, None, true) -> {
        inductive_name = t.type_name;
        inductive_type_args = t.type_params;
        inductive_constructors = List.map (to_constructor t) l;
        inductive_type = to_type [] type_sort_mono;
      }
    | _ -> assert false in

  let mut_type_entries_to_ind t = [
    Inductive (List.map type_entry_to_inductive t)
  ] in

  let type_entries_to_vernac = function
    | (t :: _) as mut_types ->
      (match (t.type_value, t.type_model, transparent) with
       | (Variant _, None, true) -> mut_type_entries_to_ind mut_types
       | _ -> List.map type_entry_to_vernac mut_types)
    | [] -> [] in

  let to_extract t = match (t.type_value, t.type_model, transparent) with
    | (Variant l, None, true) ->
      ExtractInductive {
        inductive_qualid = t.type_name;
        inductive_target = Interface.qualified_name m t.type_name;
        inductive_variants_target =
          List.map (fun x -> Interface.qualified_name m x.variant_name) l
      }
    | _ ->
      ExtractConstant {
        constant_qualid = t.type_name;
        constant_type_vars = t.type_params;
        constant_target = Interface.qualified_name m t.type_name
      }
  in

  vernacs
  |+ Section "Types"
  |++ Compat.concat_map type_entries_to_vernac mut_types
  |+ block_of_list @@ List.map to_extract m.interface_types

let call_vars proto =
  List.mapi (fun i _ -> sprintf "x%d" i) proto.prototype_args

let io_primitives_vernac m =
  (* TODO: tailrec? *)
  let to_axiom prim = Axiom {
      axiom_name = sprintf "io_%s" prim.prim_name;
      axiom_type = type_lift "IO" prim.prim_type;
    } in

  let to_extract_constant prim =
    let proto = type_repr_to_prototype_repr prim.prim_type in
    let args = call_vars proto in
    ExtractConstant {
      constant_qualid = sprintf "io_%s" prim.prim_name;
      constant_type_vars = [];
      constant_target =
        asprintf "@[<h>(fun%a%a%ak__ -> k__ %a%s%a%a%a)@]"
          (pp_if_not_empty pp_print_space) args
          (pp_print_list ~pp_sep:pp_print_space
             pp_print_string) args
          (pp_if_not_empty pp_print_space) args
          (pp_if_not_empty (fun fmt _ -> pp_print_string fmt "(")) args
          (Interface.qualified_name m prim.prim_name)
          (pp_if_not_empty pp_print_space) args
          (pp_print_list ~pp_sep:pp_print_space
             pp_print_string) args
          (pp_if_not_empty (fun fmt _ -> pp_print_string fmt ")")) args
    } in

  let instance_vernac = Instance {
      instance_name = sprintf "IO_Monad%s" m.interface_name;
      instance_typeclass_args = [];
      instance_type =
        TMono (TParam (sprintf "Monad%s" m.interface_name, [TParam ("IO", [])]));
      instance_members =
        List.map (fun prim -> (prim.prim_name, sprintf "io_%s" prim.prim_name))
          m.interface_primitives
    } in

  Lazylist.push_list [
    Subsection "[IO] Instance";
    compacted_block_of_list @@ List.map to_axiom m.interface_primitives;
    compacted_block_of_list @@ List.map to_extract_constant m.interface_primitives;
    instance_vernac;
  ]

let interface_vernac m vernacs =
  let prim_to_constructor prim = {
    constructor_name =
      String.capitalize_ascii prim.prim_name;
    constructor_prototype = {
      prototype_type_args = [];
      prototype_args = [];
      prototype_ret_type =
        type_lift
          (String.uppercase_ascii m.interface_name)
          prim.prim_type
    }
  } in

  let prim_to_inj_helper prim =
    let proto =
      Repr.type_repr_to_prototype_repr (type_lift "m" prim.prim_type) in
    Definition {
      def_name = sprintf "inj_%s" prim.prim_name;
      def_typeclass_args = [
        sprintf
          "Inject %s m"
          (String.uppercase_ascii m.interface_name)
      ];
      def_prototype = proto;
      def_body = fun fmt _ ->
        fprintf fmt "inject (%s %a)"
          (String.capitalize_ascii prim.prim_name)
          (pp_print_list ~pp_sep:pp_print_space pp_print_string) (call_vars proto)
    }
  in

  let inj_instance =
    let monad_name =
      sprintf "Monad%s" @@ String.capitalize_ascii m.interface_name in
    let interface_name = String.uppercase_ascii m.interface_name in
    Instance {
      instance_name = sprintf "Inject_%s" monad_name;
      instance_typeclass_args = [
        sprintf "Inject %s m" interface_name
      ];
      instance_type = TMono (TParam (monad_name, [TParam ("m", [])]));
      instance_members =
        List.map (fun x ->
            let n = x.prim_name in
            (n, sprintf "inj_%s" n))
          m.interface_primitives
    } in

  let interface_inductive = Inductive [{
      inductive_name = String.uppercase_ascii m.interface_name;
      inductive_type_args = [];
      inductive_constructors =
        List.map prim_to_constructor m.interface_primitives;
      inductive_type = TMono (tlambda [type_sort_mono] type_sort_mono)
    }]
  in

  vernacs
  |++ [
    Subsection "Interface datatype";
    interface_inductive
  ]
  |++ List.map prim_to_inj_helper m.interface_primitives
  |+ inj_instance

let semantics_vernac m vernacs =
  let interface_name = String.uppercase_ascii m.interface_name in
  let interface_type =
    TParam (interface_name, []) in

  vernacs
  |++ [
    Subsection "FreeSpec Semantics";
    compacted_block_of_list @@ List.map (fun prim -> Axiom {
      axiom_name = sprintf "unsafe_%s" prim.prim_name;
      axiom_type = prim.prim_type;
    }) m.interface_primitives;
    compacted_block_of_list @@ List.map (fun prim -> ExtractConstant {
      constant_qualid = sprintf "unsafe_%s" prim.prim_name;
      constant_type_vars = [];
      constant_target = qualified_name m prim.prim_name;
    }) m.interface_primitives;
    Definition {
      def_name = sprintf "%s_unsafe_semantics"
          (String.lowercase_ascii m.interface_name);
      def_typeclass_args = [];
      def_prototype = {
        prototype_type_args = [];
        prototype_args = [];
        prototype_ret_type =
          TMono (TParam ("semantics", [interface_type]));
      };
      def_body = fun fmt _ ->
        fprintf fmt
          "@[<v 2>bootstrap (fun a e =>@ local @[<v>match e in %s a return a with@ %a@ end@])@]"
          interface_name
          (pp_print_list ~pp_sep:pp_print_space
             (fun fmt prim ->
                let proto = type_repr_to_prototype_repr prim.prim_type in
                let args = call_vars proto in
                fprintf fmt "@[<hov 2>| @[<h>%s %a@]@ => @[<h>unsafe_%s %a@]@]"
                  (String.capitalize_ascii prim.prim_name)
                  (pp_print_list ~pp_sep:pp_print_space
                     pp_print_string) args
                  prim.prim_name
                  (pp_print_list ~pp_sep:pp_print_space
                     pp_print_string) args)) m.interface_primitives
    }
  ]

let primitives_vernac features m vernacs =
  let prim_to_members prim =
    (prim.prim_name, type_lift "m" prim.prim_type) in

  let monad_vernac = Typeclass {
     class_name = sprintf "Monad%s" m.interface_name;
     class_typeclass_args = [];
     class_args = ["m", TMono (tlambda [type_sort_mono] type_sort_mono)];
     class_type = type_sort;
     class_members = List.map prim_to_members m.interface_primitives;
   } in

  vernacs
  |++ [
    Section "Impure Primitives";
    Subsection "Monad Definition";
    monad_vernac;
  ]
  |> is_enabled features SimpleIO @? io_primitives_vernac m
  |> is_enabled features Interface @? interface_vernac m
  |> is_enabled features FreeSpec @? semantics_vernac m

let of_interface features models m =
  Block
    (of_list [
        Comment "This file has been generated by coqffi.";
        ConfigPrologue;
      ]
     |> requires_vernac features models
     |> not (empty m.interface_types) @? types_vernac features m
     |> not (empty m.interface_functions) @? functions_vernac m
     |> not (empty m.interface_primitives) @? primitives_vernac features m
     |+ Comment "The generated file ends here.")