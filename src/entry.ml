open Repr
open Parsetree
open Types
open Feature
open Error

type primitive_entry = {
  prim_name : string;
  prim_type : type_repr;
  prim_may_raise : bool;
  prim_loc : Location.t;
}

type lwt_entry = {
  lwt_name : string;
  lwt_type : type_repr;
  lwt_loc : Location.t;
}

type function_entry = {
  func_name : string;
  func_type : type_repr;
  func_model : string option;
  func_may_raise : bool;
  func_loc : Location.t;
}

type variant_entry = {
  variant_name : string;
  variant_prototype : prototype_repr;
}

type type_value =
  | Variant of variant_entry list
  | Opaque

type type_entry = {
  type_name : string;
  type_params : string list;
  type_arity : int;
  type_model : string option;
  type_value : type_value;
  type_loc : Location.t;
}

type mutually_recursive_types_entry = type_entry list

type exception_entry = {
  exception_name : string;
  exception_prototype : prototype_repr;
  exception_loc : Location.t;
}

type module_entry = {
  module_namespace : string list;
  module_name : string;
  module_intro : intro_entry list;
  module_functions : function_entry list;
  module_primitives : primitive_entry list;
  module_lwt : lwt_entry list;
  module_exceptions : exception_entry list;
  module_loc : Location.t;
}

and intro_entry =
  | IntroType of type_entry
  | IntroMod of module_entry

type entry =
  | EPrim of primitive_entry
  | ELwt of lwt_entry
  | EFunc of function_entry
  | EType of type_entry
  | EExn of exception_entry
  | EMod of module_entry

let polymorphic_params (decl : type_declaration) : string list =
  let minimize = List.sort_uniq String.compare in

  let existing_params params =
    let polymorphic_param (t : type_expr) : string option =
      match t.desc with
      | Tvar (Some "_") | Tvar None -> None
      | Tvar (Some x) -> Some x
      | _ -> failwith "Type parameters should be made with [Tvar] and nothing else" in

    minimize (List.filter_map polymorphic_param params) in

  Compat.fold_left_map
       (fun params t ->
         match t.desc with
         | Tvar (Some "_") | Tvar None ->
           let (x, params) = pick_param params in
           (params, x)
         | Tvar (Some x) -> (params, x)
         | _ -> failwith "Type parameters should be made with [Tvar] and nothing else")
       (make_params_pool @@ existing_params decl.type_params)
       decl.type_params
       |> snd

let has_attr name : attributes -> bool =
  List.exists (fun attr -> attr.attr_name.txt = name)

let get_attr_string name : attributes -> string option =
  let expr_to_string = function
    | Pexp_constant (Pconst_string (str, _, _)) -> Some str
    | _ -> None in

  let struct_to_string = function
    | Pstr_eval (expr, _) -> expr_to_string expr.pexp_desc
    | _ -> None in

  let get_attr attr =
    if attr.attr_name.txt = name
    then match attr.attr_payload with
         | PStr [ model ] -> struct_to_string model.pstr_desc
         | _ -> None
    else None in

  Compat.find_map get_attr

let has_coq_model : attributes -> string option =
  get_attr_string "coq_model"

let signature_ident = function
  | Sig_value (ident, _, _)
    | Sig_type (ident, _, _, _)
    | Sig_typext (ident, _, _, _)
    | Sig_module (ident, _, _, _, _)
    | Sig_modtype (ident, _, _)
    | Sig_class (ident, _, _, _)
    | Sig_class_type (ident, _, _, _)
    -> ident

let signature_loc = function
  | Sig_value (_, v, _) -> v.val_loc
  | Sig_type (_, ty, _, _) -> ty.type_loc
  | Sig_typext (_, ext, _, _) -> ext.ext_loc
  | Sig_module (_, _, m, _, _) -> m.md_loc
  | Sig_modtype (_, m, _) -> m.mtd_loc
  | Sig_class (_, c, _, _) -> c.cty_loc
  | Sig_class_type (_, c, _, _) -> c.clty_loc

let rec exclude cmp l1 l2 =
  match (l1, l2) with
  | (x1 :: rst1, x2 :: rst2) ->
    (match cmp x1 x2 with
     | x when x == 0 -> exclude cmp rst1 rst2
     | x when x < 0 -> x1 :: exclude cmp rst1 l2
     | _ -> x1 :: exclude cmp rst1 rst2)
  | _, [] -> l1
  | [], _ -> []

let prototype_of_constructor params_type args ret =
  let typify acc t =
    match type_repr_of_type_expr t with
    | TPoly (p, t) -> (p @ acc, TMono t)
    | t -> (acc, t) in

  let monoify = function
    | TPoly (p, t) -> (p, TMono t)
    | t -> ([], t) in

  match args with
  | Cstr_tuple args ->
    let (params_ret, ret) = monoify ret in
    let (params_constr, typs) = Compat.fold_left_map typify [] args in
    let params_constr = List.sort_uniq String.compare (params_ret @ params_constr) in
    let params_type = List.sort_uniq String.compare params_type in

    {
      prototype_type_args = exclude String.compare params_constr params_type;
      prototype_args = typs;
      prototype_ret_type = ret;
    }
  | _ -> assert false

let entry_of_value lf lwt_alias ident desc loc =
  let is_pure attrs model t =
    is_enabled lf PureModule
    || Repr.supposedly_pure t
    || Option.is_some model
    || has_attr "pure" attrs
  in

  let name = Ident.name ident in
  let repr = type_repr_of_type_expr desc.val_type in
  let model = has_coq_model desc.val_attributes in
  let may_raise = has_attr "may_raise" desc.val_attributes in

  let unwrap_lwt = function
    | TParam (type_name, [param]) when lwt_alias = Some type_name -> param
    | t -> t in

  if is_pure desc.val_attributes model repr
  then EFunc {
         func_name = name;
         func_type = repr;
         func_model = model;
         func_may_raise = may_raise;
         func_loc = loc;
       }
  else if asynchronous ~lwt_alias repr
  then ELwt {
         lwt_name = name;
         lwt_type = map_codomain unwrap_lwt repr;
         lwt_loc = loc;
       }
  else EPrim {
          prim_name = name;
          prim_type = repr;
          prim_may_raise = may_raise;
          prim_loc = loc;
        }

let entry_of_type lf ident decl loc =
  let type_name = Ident.name ident in

  (* Our goal is to compute a [type_entry] value for a given type
     declaration. The main difficulty of this approach is that OCaml
     is _way_ more permissive than Coq wrt. how a type in defined.

     For a parameterized inductive types, e,g,.

        Inductive foo (a : Type) := ...

     Coq enforces that the type of each constructor of [foo] is [foo
     a]. If we want two constructors to be of different types, we will
     have to do something like

         Inductive foo : Type -> Type := ...

     OCaml does not have any restriction of the sort, so one can have

         type 'a t =
           | Foo : int t
           | Bar : bool t

     For such a type, coqffi needs to generate [Inductive t : Type ->
     Type]. Similarly, coqffi needs to deal with unamed parameters,
     identified with [_]. Another way to define [t] is

         type _ t =
           | Foo : int t
           | Bar : bool t

     This means [coqffi] needs to do quite a bit of processing.

     First, we collect the type parameters. The [polymorphic_params]
     will give a name to each unamed parameters, using a [params_pool]
     from the [Repr] module. *)

  let params = polymorphic_params decl in

  (* Then, we compute two versions of the default type of the
     constructors. A potentially polymorphic one, and a monomorphic
     one.

     The polymorphic version is used to compute the
     [prototype_repr] value associated to each constructor. *)

  let poly_default_type_ret =
    of_mono_type_repr params (TParam (type_name, List.map (fun x -> TParam (x, [])) params)) in

  let to_variant_entry params v =
    let ret = Option.value ~default:poly_default_type_ret
              (Option.map type_repr_of_type_expr v.cd_res) in {
      variant_name = Ident.name v.cd_id;
      variant_prototype = prototype_of_constructor params v.cd_args ret;
    } in

  (* NB: Constructors of the form [Bar of int] do not have any type
     information, compared to [Foo : int -> t]. And of course, the two
     forms can be mixed together within the same type definition.

     This value is “monoified” once the prototype is constructed,
     since the [prototype_repr] record as a field for type arguments.
     As a consequence, when we try to verify (where we test if all
     constructors use the “correct” set of parameters introduced by
     the type declaration), we use the monomorphic version. *)

  let mono_default_type_ret =
    TMono (TParam (type_name, List.map (fun x -> TParam (x, [])) params)) in

  let unify v = v.variant_prototype.prototype_ret_type = mono_default_type_ret in

  (* We can now define two functions to contruct [type_value]
     terms. The first one expects the constructors to use the correct
     set of parameters. It call [to_variant_entry] with [params],
     which means the type parameters in [param] will not appear as
     type parameters specific to a given constructor. Prior to
     returning the result it has computed, this functions check it is
     consistent. It will return [None] if not. *)

  let type_value_with_unification decl =
    if is_enabled lf TransparentTypes
    then try
        match decl.type_kind with
        | Type_variant v -> let l = List.map (to_variant_entry params) v in
                            if List.for_all unify l
                            then Some (Variant l)
                            else None
        | _ -> Some Opaque
      with
        _ -> Some Opaque
    else Some Opaque in

  (* The other function will call [to_variant_entry] with an empty set
     of type parameters, because we will only call them when we will
     define inductive types without named type parameters (of the form
     [Inductive t : Type -> Type ... := ...]). *)

  let type_value_without_unification decl =
    if is_enabled lf TransparentTypes
    then try
        match decl.type_kind with
        | Type_variant v -> Variant (List.map (to_variant_entry []) v)
        | _ -> Opaque
      with _ -> Opaque
    else Opaque in

  (* Et voila. First, we try with [type_value_with_unification], and
     if it fails, [type_value_without_unification]. *)

  let (type_params, type_arity, type_value) =
    match type_value_with_unification decl with
    | Some type_value -> (params, 0, type_value)
    | None -> ([], List.length params, type_value_without_unification decl)
  in

  EType {
    type_name;
    type_params;
    type_model = has_coq_model decl.type_attributes;
    type_value;
    type_loc = loc;
    type_arity;
  }

let entry_of_exn ident cst loc =
  let name = Ident.name ident in
  EExn {
    exception_name = name;
    exception_prototype = prototype_of_constructor [] cst.ext_args (TMono (TParam ("exn", [])));
    exception_loc = loc;
  }

let add_primitive_entry (m : module_entry) (pr : primitive_entry) : module_entry = {
  m with
  module_primitives = m.module_primitives @ [pr]
}

let add_function_entry (m : module_entry) (f : function_entry) : module_entry = {
  m with
  module_functions = m.module_functions @ [f]
}

let add_type_entry (m : module_entry) (t : type_entry) : module_entry = {
  m with
  module_intro = m.module_intro @ [IntroType t]
}

let add_module_entry (m :module_entry) (e :module_entry) : module_entry = {
  m with
  module_intro = m.module_intro @ [IntroMod e]
}

let add_exception_entry (m :module_entry) (e : exception_entry) :module_entry = {
  m with
  module_exceptions = m.module_exceptions @ [e]
}

let add_lwt_entry (m : module_entry) (l : lwt_entry) : module_entry = {
  m with
  module_lwt = m.module_lwt @ [l]
}

let error_of_signature s exn : error = {
    error_loc = signature_loc s;
    error_entry = Ident.name (signature_ident s);
    error_exn = error_kind_of_exn exn;
  }

let add_entry (m :module_entry) = function
  | EPrim pr -> add_primitive_entry m pr
  | ELwt l -> add_lwt_entry m l
  | EFunc fn -> add_function_entry m fn
  | EType t -> add_type_entry m t
  | EExn e -> add_exception_entry m e
  | EMod m' -> add_module_entry m m'

let empty_module loc namespace name = {
    module_namespace = namespace;
    module_name = name;
    module_intro = [];
    module_functions = [];
    module_primitives = [];
    module_lwt = [];
    module_exceptions = [];
    module_loc = loc;
  }

let rec entry_of_signature namespace lf lwt_alias (s : Types.signature_item) : entry =
  let loc = signature_loc s in
  match s with
  | Sig_value (ident, desc, Exported) ->
    entry_of_value lf lwt_alias ident desc loc
  | Sig_type (ident, decl, _, Exported) ->
    entry_of_type lf ident decl loc
  | Sig_typext (ident, cst, Text_exception, Exported) ->
    entry_of_exn ident cst loc
  | Sig_module (name, _, decl, _, Exported) ->
    (match entry_of_module lf lwt_alias namespace name decl with
     | Some x -> x
     | _ -> raise_error (UnsupportedOCamlSignature s))
  | _ -> (* FIXME: this looks like it is a bit too strong *)
    raise_error (UnsupportedOCamlSignature s)

and entry_of_module lf lwt_alias namespace name decl =
  match decl.md_type with
  | Mty_signature sigs ->
    Some (EMod (module_of_signatures ~loc:(Some decl.md_loc) lf lwt_alias namespace (Ident.name name) sigs))
  | _ -> None

and module_of_signatures ?(loc=None) lf lwt_alias namespace name sigs =
  let foldf m s =
    try entry_of_signature namespace lf lwt_alias s |> add_entry m
    with e ->
      pp_error Format.err_formatter (error_of_signature s e);
      m
  in

  let loc = Option.value loc ~default:(Location.in_file (name ^ ".cmi")) in
  List.fold_left foldf (empty_module loc namespace name) sigs

type state = Unvisited | OnStack | Visited

type node = {
  node_state : state ref;
  node_type : type_entry;
  node_deps : string list;
  node_id : int ref;
  node_low : int ref;
}

let new_node t deps = {
  node_state = ref Unvisited;
  node_type = t;
  node_deps = deps;
  node_id = ref 0;
  node_low = ref 0;
}

let unvisited = function
  | Unvisited -> true
  | _ -> false

let on_stack = function
  | OnStack -> true
  | _ -> false

let dependencies t =
  match t.type_value with
  | Variant l ->
    List.sort_uniq String.compare
      (Compat.concat_map
         (fun e ->
           let typs =
             e.variant_prototype.prototype_ret_type :: e.variant_prototype.prototype_args in
           Compat.concat_map dependencies typs) l)
  | Opaque -> []

let find_mutually_recursive_types tl =
  let input_length = List.length tl in
  let id : int ref = ref 0 in

  let stack : (node list) ref = ref [] in

  let res : ((type_entry list) list) ref = ref [] in

  let nodes = List.map (fun t -> t.type_name) tl in

  let matrix : (string, node) Hashtbl.t = begin
    let tbl = Hashtbl.create input_length in
    List.iter
      (fun t ->
         let deps = dependencies t in
         let min_deps = List.filter
             (fun x -> List.exists (String.equal x) deps)
             nodes in
         Hashtbl.add tbl t.type_name (new_node t min_deps))
      tl;
    tbl
  end in

  let rec empty_stack at =
    match !stack with
    | node :: rst ->
      stack := rst;
      node.node_state := Visited;
      node.node_low := !(at.node_id);
      if !(node.node_id) == !(at.node_id) then [node.node_type]
      else node.node_type :: empty_stack at
    | _ -> assert false in

  let rec dfs at =
    stack := at :: !stack;
    at.node_state := OnStack;
    at.node_id := !id;
    at.node_low := !id;
    id := !id + 1;

    List.iter
      (fun t ->
         let dep = Hashtbl.find matrix t in
         if unvisited !(dep.node_state) then dfs dep;
         if on_stack !(dep.node_state)
         then at.node_low := min !(dep.node_low) !(at.node_low))
      at.node_deps;

    if !(at.node_id) = !(at.node_low) then
      res := List.rev (empty_stack at) :: !res in

  Hashtbl.iter (fun _ v ->
      if unvisited !(v.node_state) then dfs v) matrix;

  List.rev !res

let translate_function ~rev_namespace tbl f = {
    f with
    func_type = translate_type_repr ~rev_namespace tbl f.func_type
  }

let translate_primitive ~rev_namespace tbl prim = {
    prim with
    prim_type = translate_type_repr ~rev_namespace tbl prim.prim_type
  }

let translate_lwt ~rev_namespace tbl lwt = {
    lwt with
    lwt_type = translate_type_repr ~rev_namespace tbl lwt.lwt_type
  }

let translate_prototype ~rev_namespace tbl p =
  let tbl =
    List.fold_left (fun tbl x -> Translation.preserve x tbl) tbl
      p.prototype_type_args in
  {
    p with
    prototype_args = List.map (translate_type_repr ~rev_namespace tbl) p.prototype_args;
    prototype_ret_type = translate_type_repr ~rev_namespace tbl p.prototype_ret_type;
  }

let translate_exception ~rev_namespace tbl e = {
    e with
    exception_prototype = translate_prototype ~rev_namespace tbl e.exception_prototype
  }

let translate_variant ~rev_namespace tbl v = {
    v with
    variant_prototype = translate_prototype ~rev_namespace  tbl v.variant_prototype
  }

let translate_type_value ~rev_namespace tbl = function
  | Variant l -> Variant (List.map (translate_variant ~rev_namespace tbl) l)
  | Opaque -> Opaque

let translate_type ~rev_namespace tbl typ =
  let tbl' = List.fold_left (fun tbl t -> Translation.preserve ~rev_namespace t tbl)
             tbl
             typ.type_params in {
    typ with
    type_value = translate_type_value ~rev_namespace tbl' typ.type_value
  }
