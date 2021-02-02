type t =
  | TransparentTypes
  | PureModule
  | Interface
  | SimpleIO
  | FreeSpec
  | Lwt

type feature = t

exception FreeSpecRequiresInterface
exception LwtExplicitelyDisableButLwtAliasSet

val name : feature -> string

type features = (feature * bool) list

val find_duplicates : features -> feature list

val is_enabled : features -> feature -> bool
val is_disabled : features -> feature -> bool

val support_impure_values : features -> bool

val check_features_consistency : string option -> features -> (string option * features)
(** Throw an exception if an inconsistency is found. *)
