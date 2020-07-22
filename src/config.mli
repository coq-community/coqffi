type extraction_profile =
  | Stdlib
  | Coqbase

type feature =
  | TransparentTypes
  | Interface

val feature_name : feature -> string
val default : feature -> bool

type features = (feature * bool) list

val find_duplicates : features -> feature list

val is_enabled : features -> feature -> bool

type generation_config = {
  gen_profile : extraction_profile;
  gen_features : features;
}
