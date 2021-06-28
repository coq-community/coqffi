(** The [Repr] module provides an intermediate representation for the subset of
    OCaml types supported by [coqffi]. This subset is chosen to be easily
    convertible into Coq types. *)

(** {1 Encoding Types} *)

(** [coqffi]’s representation of types are based on two types: [mono_type_repr]
    and [type_repr]. *)

type constant_repr = CPlaceholder of int | CName of string

type argument_kind =
  | PositionedArg
  | LabeledArg of string
  | OptionalArg of string

type argument = { position : int; kind : argument_kind }

type mono_type_repr =
  | TLambda of {
      argtype : argument;
      domain : mono_type_repr;
      codomain : mono_type_repr;
    }
      (** [TLambda { None, u, v }] ≡ [u -> v],
          [TLambda { Some "x", u, v }] ≡ [x:u -> v] *)
  | TProd of mono_type_repr list  (** [TProd [t0; ..; tn]] ≡ [(t0, .., tn)] *)
  | TParam of (constant_repr * mono_type_repr list)
      (** [TParam ("t", [t0; ..; tn])] ≡ [(t0, .., tn) t] *)

type type_repr =
  | TMono of mono_type_repr  (** A monomorphic type *)
  | TPoly of (string list * mono_type_repr)  (** A polymorphic type *)

(** Thus, [TPoly (["a"], TLambda (TParam ("a", []), TParam ("a", [])))] encodes
    the type of polymorphic functions ['a -> 'a], or [forall (a : Type), a -> a]
    in Coq.

    Note that [type_repr] and [mono_type_repr] can encode types that exists in
    Coq but not in OCaml. For instance, [TPoly (["f"]; TParam "f" [TParam
    ("int", [])])] encodes the type [forall (f : Type -> Type), f int] which is
    valid in Coq. However, there is no straightforward way to model higher
    kinded types in OCaml.

    This is not an issue, because [type_repr] values are expected to be computed
    from well-typed OCaml interface files. That being said, it is important to
    keep that in mind if we want to generate OCaml programs from manually
    defined [type_repr] values. *)

val to_mono_type_repr : type_repr -> mono_type_repr

val of_mono_type_repr : string list -> mono_type_repr -> type_repr

val fresh_placeholder : type_repr -> int

val place_placeholder : string -> type_repr -> int * type_repr

val fill_placeholder : int -> string -> type_repr -> type_repr

val higher_order_monadic : constant_repr -> type_repr -> bool

val supposedly_pure : type_repr -> bool

val asynchronous : lwt_module:string option -> type_repr -> bool

(** {1 Converting [Cmi_format]'s [type_expr] to [coqffi]’s [type_repr]} *)

val type_repr_of_type_expr : Types.type_expr -> type_repr

(** {1 Manipulating [type_repr]} *)

(** {2 Heplers Functions} *)

val mono_type_repr_of_type_expr : Types.type_expr -> mono_type_repr

val mono_has_labelled_arg : mono_type_repr -> bool

val has_labelled_arg : type_repr -> bool

val tlambda : mono_type_repr list -> mono_type_repr -> mono_type_repr
(** Create a function type, {e i.e.}, [tlambda [a; b; c] d ≡ a -> b ->
    c -> d] *)

val map_codomain : (mono_type_repr -> mono_type_repr) -> type_repr -> type_repr
(** Apply a function to the codomain of a function,, {e i.e.},
    [map_codomain f (a -> .. -> r) ≡ a -> .. -> f r] *)

val type_lift : string -> ?args:mono_type_repr list -> type_repr -> type_repr
(** Project the codomain of a function into a parameterized type, {e
    i.e.}, [type_lift T ~args:[x r] (a -> .. -> r) ≡ a -> .. -> T x y r] *)

val mono_dependencies : mono_type_repr -> string list
(** [mono_dependencies t] is the list of types which appear in [t] definition.
    For instance, [mono_dependencies (int * bool) ≡ ["int"; "bool"]] *)

val dependencies : type_repr -> string list
(** [dependencies t] is the list of types which appear in [t] definition. If [t]
    is a polymorphich type, then the type variables do not appear in the
    list. For instance, [dependencies ('a -> bool) ≡ ["bool"]] *)

(** {2 Types’ Translation} *)

(** One significant feature of Coq’s extraction mechanism is substituting a
    subset of Coq constants into OCaml counterparts. For instance, it is common
    to substitute the [string] Coq inductive type (a specialized list of [ascii]
    terms, themselves made of 8 booleans) to use OCaml native [string]
    instead.

    Since [coqffi] generates Coq definitions which are expected to later be
    extracted, it needs to perform the opposite task. That is, if an OCaml
    function takes an argument of type [string], then the generated Coq
    definition has to use the [string] type defined by Coq. In this case, both
    types share the exact same name, but this is not necessarily the case. This
    is why [coqffi] relies on a so-called translation phase.

    See the {!module:Translation} module for more information on how to get a
    translation table. *)

val translate_mono_type_repr :
  rev_namespace:string list -> Translation.t -> mono_type_repr -> mono_type_repr

val translate_type_repr :
  rev_namespace:string list -> Translation.t -> type_repr -> type_repr

val type_sort_mono : mono_type_repr

val type_sort : type_repr

type prototype_repr = {
  prototype_type_args : string list;
  prototype_args : (argument * type_repr) list;
  prototype_ret_type : type_repr;
}

val type_repr_to_prototype_repr : type_repr -> prototype_repr

type params_pool

val make_params_pool : string list -> params_pool

val pick_param : params_pool -> string * params_pool

val pick_params : int -> params_pool -> string list * params_pool

(** {2 Pretty-printing Coq Terms} *)

val pp_mono_type_repr : Format.formatter -> mono_type_repr -> unit
(** Output [TLambda] values as [t0 -> .. -> tn] *)

val pp_type_repr : Format.formatter -> type_repr -> unit
(** Output [TLambda] values as [forall (a1 : Type) .. (an : Type), t0 -> .. ->
    tn] *)
