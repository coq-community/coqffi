open Entry
open Feature

type t = module_entry

type intro_list

val of_cmi_infos : features:features -> Cmi_format.cmi_infos -> t

val translate : Translation.t -> t -> t

val qualified_name : t -> string -> string

val compute_intro_list : t -> intro_list

val map_intro_list
    : (mutually_recursive_types_entry -> 'a)
      -> (t -> 'a)
      -> intro_list
      -> 'a list

val fold_intro_list
    : ('a -> mutually_recursive_types_entry -> 'a)
      -> ('a -> t -> 'a)
      -> 'a
      -> intro_list
      -> 'a
