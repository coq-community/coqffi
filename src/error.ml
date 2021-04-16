type error_kind =
  | UnsupportedOCamlSignature of Types.signature_item
  | UnsupportedGADT
  | UnsupportedOCamlType of Types.type_expr
  | UnknownOCamlType of string
  | Anomaly of exn

exception CoqffiExn of error_kind

let raise_error e = raise (CoqffiExn e)

let error_kind_of_exn = function CoqffiExn e -> e | e -> Anomaly e

let pp_error_kind fmt =
  let open Format in
  function
  | UnsupportedOCamlSignature s -> (
      try
        fprintf fmt "Use of unsupported OCaml construction: %a"
          Printtyp.signature [ s ]
      with _ -> pp_print_string fmt "<unrepresentable construction>")
  | UnsupportedGADT -> fprintf fmt "GADT are currently not supported by coqffi"
  | UnsupportedOCamlType t ->
      fprintf fmt "Unsupported OCaml type construction %a" Printtyp.type_expr t
  | UnknownOCamlType t -> fprintf fmt "Type `%s' is not supported by coqffi" t
  | Anomaly _ ->
      fprintf fmt "Something went wrong, and you probably have found a bug"

type t = {
  error_loc : Location.t;
  error_entry : string;
  error_exn : error_kind;
}

type error = t

let pp_error fmt err =
  Format.fprintf fmt "%a: Giving up on entry `%s'\n%a\n" Location.print_loc
    err.error_loc err.error_entry pp_error_kind err.error_exn
