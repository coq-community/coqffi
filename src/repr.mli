(** The [Repr] module provides an intermediate representation for the subset of
    OCaml types supported by [coqffi]. This subset is chosen to be easily
    convertible into Coq types. *)

(** {1 Encoding Types} *)

(** [coqffi]’s representation of types are based on two types: [mono_type_repr]
    and [type_repr]. *)

type mono_type_repr =
  | TLambda of (mono_type_repr * mono_type_repr) (** [TLambda (u, v)] ≡ [u ->
                                                     v] *)
  | TProd of (mono_type_repr list) (** [TProd [t0; ..; tn]] ≡ [(t0, ..,
                                       tn)] *)
  | TParam of (string * mono_type_repr list) (** [TParam ("t", [t0; ..; tn])] ≡
                                                 [(t0, .., tn) t] *)

type type_repr =
  | TMono of mono_type_repr (** A monomorphic type *)
  | TPoly of (string list * mono_type_repr) (** A polymorphic type *)

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

(** {1 Converting [Cmi_format]'s [type_expr] to [coqffi]’s [type_repr]} *)

exception UnsupportedOCamlType of Types.type_expr

val mono_type_repr_of_type_expr : Types.type_expr -> mono_type_repr

val type_repr_of_type_expr : Types.type_expr -> type_repr

(** {1 Manipulating [type_repr]} *)

(** {2 Heplers Functions} *)

(** Project the codomain of a function into the [impure] monad provided by
    FreeSpec, {e i.e.}, [impure_proj ix (a -> .. -> r) ≡ a -> .. -> impure ix
    r] *)
val impure_proj : string -> type_repr -> type_repr

(** Project the codomain of a function into a given interface, {e i.e.},
    [interface_proj I (a -> .. -> r) ≡ a -> .. -> I r] *)
val interface_proj : string -> type_repr -> type_repr

(** [mono_dependencies t] is the list of types which appear in [t] definition.
    For instance, [mono_dependencies (int * bool) ≡ ["int"; "bool"]] *)
val mono_dependencies : mono_type_repr -> string list

(** [dependencies t] is the list of types which appear in [t] definition. If [t]
    is a polymorphich type, then the type variables do not appear in the
    list. For instance, [dependencies ('a -> bool) ≡ ["bool"]] *)
val dependencies : type_repr -> string list

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

exception UnknownOCamlType of string

val translate_mono_type_repr : Translation.t -> mono_type_repr -> mono_type_repr

val translate_type_repr : Translation.t -> type_repr -> type_repr

(** {2 Pretty-printing Coq Terms} *)

(** Output [TLambda] values as [t0 -> .. -> tn] *)
val pp_mono_type_repr_arrows : Format.formatter -> mono_type_repr -> unit

(** Output [TLambda] values as [forall (a1 : Type) .. (an : Type), t0 -> .. ->
    tn] *)
val pp_type_repr_arrows : Format.formatter -> type_repr -> unit

(** Output [TLambda] values as [(a0 : Type) .. (an : Type) (x0 : t0) .. (xm :
    tm) : tn]). The first argument of [pp_type_repr_prototype] is a prefix to
    append before this prototype ({e e.g.}, "Definition name" ) *)
val pp_type_repr_prototype : string -> Format.formatter -> type_repr -> unit

(** For a function which takes [n + 1] arguments, output [x0 .. xn]. *)
val pp_type_repr_arg_list : Format.formatter -> type_repr -> unit
