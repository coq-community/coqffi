type extraction_profile =
  | Stdlib
  | Coqbase

type impure_mode =
  | FreeSpec

type generation_config = {
  gen_profile : extraction_profile;
  gen_impure_mode : impure_mode option;
  gen_transparent_types : bool;
}

exception FreeSpecInvalidExtractionProfile

val validate : generation_config -> unit
