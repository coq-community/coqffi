type feature =
  | TransparentTypes
  | Interface

val feature_name : feature -> string
val default : feature -> bool

type features = (feature * bool) list

val find_duplicates : features -> feature list

val is_enabled : features -> feature -> bool
