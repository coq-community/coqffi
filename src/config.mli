type extraction_profile =
  | Stdlib
  | Coqbase

type impure_mode =
  | FreeSpec

type generation_config = {
  gen_profile : extraction_profile;
  gen_impure_mode : impure_mode option;
  gen_with_type_value : bool;
}

exception FreeSpecInvalidExtractionProfile

val validate : generation_config -> unit
