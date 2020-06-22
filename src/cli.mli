type extraction_profile =
  | Stdlib
  | Coqbase

type impure_mode =
  | FreeSpec

exception FreeSpecExtractionProfile
exception TooManyArguments
exception MissingInputArgument

val get_impure_mode : unit -> impure_mode option
val get_extraction_profile : unit -> extraction_profile

val get_input_path : unit -> string
val get_output_formatter : unit -> Format.formatter

val parse : unit -> unit

val usage : string
