type t =
  | TransparentTypes
  | PureModule
  | Interface
  | SimpleIO
  | FreeSpec

type feature = t

exception FreeSpecRequiresInterface

val feature_name : feature -> string

type features = (feature * bool) list

val find_duplicates : features -> feature list

val is_enabled : features -> feature -> bool
val is_disabled : features -> feature -> bool

val support_impure_values : features -> bool

val check_features_consistency : features -> unit
(** Throw an exception if an inconsistency is found. *)
