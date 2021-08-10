open Entry
open Feature

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

val of_cmi_infos :
  translations:Translation.t ->
  features:features ->
  lwt_module:string option ->
  tezos_types:string list ->
  Cmi_format.cmi_infos ->
  t

val compute_conflicts : Conflict.t -> t -> Conflict.t

val map_intro_list :
  (mutually_recursive_types_entry -> 'a) -> (t -> 'a) -> intro list -> 'a list

val fold_intro_list :
  ('a -> mutually_recursive_types_entry -> 'a) ->
  ('a -> t -> 'a) ->
  'a ->
  intro list ->
  'a
