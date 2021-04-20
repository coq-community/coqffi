open Repr
open Feature
open Types

type primitive_entry = {
  prim_name : string;
  prim_type : type_repr;
  prim_may_raise : bool;
  prim_loc : Location.t;
}

type lwt_entry = {
  lwt_placeholder : int;
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

type field_entry = { field_name : string; field_type : mono_type_repr }

type record_entry = field_entry list

type variant_entry = {
  variant_name : string;
  variant_prototype : prototype_repr;
}

type type_value =
  | Variant of variant_entry list
  | Record of record_entry
  | Alias of mono_type_repr
  | Opaque

type type_entry = {
  type_name : string;
  type_params : string list;
  type_arity : int;
  type_model : string option;
  type_value : type_value;
  type_loc : Location.t;
  type_rec : rec_status;
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

and intro_entry = IntroType of type_entry | IntroMod of module_entry

type entry =
  | EPrim of primitive_entry
  | ELwt of lwt_entry
  | EFunc of function_entry
  | EType of type_entry
  | EExn of exception_entry
  | EMod of module_entry

val module_of_signatures :
  ?loc:Location.t option ->
  features ->
  string option ->
  string list ->
  string ->
  Types.signature ->
  module_entry

val dependencies : type_entry -> string list

val find_mutually_recursive_types :
  type_entry list -> mutually_recursive_types_entry list
(** For [find_mutually_recursive_types l] to produce a correct output wrt. to
    type entries ordering, [l] should be correct wrt. the same criteria. *)

val translate_function :
  rev_namespace:string list -> Translation.t -> function_entry -> function_entry

val translate_primitive :
  rev_namespace:string list ->
  Translation.t ->
  primitive_entry ->
  primitive_entry

val translate_lwt :
  rev_namespace:string list -> Translation.t -> lwt_entry -> lwt_entry

val translate_exception :
  rev_namespace:string list ->
  Translation.t ->
  exception_entry ->
  exception_entry

val translate_type :
  rev_namespace:string list -> Translation.t -> type_entry -> type_entry
