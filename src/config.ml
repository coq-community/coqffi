type impure_mode =
  | FreeSpec

type extraction_profile =
  | Stdlib
  | Coqbase

type generation_config = {
  gen_profile : extraction_profile;
  gen_impure_mode : impure_mode option;
  gen_transparent_types : bool;
}

exception FreeSpecInvalidExtractionProfile

let validate conf =
  match (conf.gen_impure_mode, conf.gen_profile) with
  | (Some FreeSpec, Stdlib) -> raise FreeSpecInvalidExtractionProfile
  | _ -> ()
