type feature =
  | TransparentTypes
  | Interface
  | SimpleIO

val feature_name : feature -> string
val default : feature -> bool

type features = (feature * bool) list

val find_duplicates : features -> feature list

val is_enabled : features -> feature -> bool

val support_impure_values : features -> bool
