type error_kind =
  | UnsupportedOCamlSignature of Types.signature_item
  | UnsupportedGADT
  | UnsupportedOCamlType of Types.type_expr
  | UnknownOCamlType of string
  | Anomaly of exn

exception CoqffiExn of error_kind

val raise_error : error_kind -> 'a

val error_kind_of_exn : exn -> error_kind

val pp_error_kind : Format.formatter -> error_kind -> unit

type t = {
  error_loc : Location.t;
  error_entry : string;
  error_exn : error_kind;
}

type error = t

val pp_error : Format.formatter -> t -> unit
