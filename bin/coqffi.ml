open Cmi_format
open Coqffi
open Coqffi.Interface

let coqbase_types_table =
  Translation.empty
  |> Translation.add "list" "list"
  |> Translation.add "bool" "bool"
  |> Translation.add "option" "option"
  |> Translation.add "unit" "unit"
  |> Translation.add "int" "i63"
  |> Translation.add "Coqbase.Bytestring.t" "bytestring"

let stdlib_types_table =
  Translation.empty
  |> Translation.add "list" "list"
  |> Translation.add "bool" "bool"
  |> Translation.add "option" "option"
  |> Translation.add "unit" "unit"

let types_table profile =
  let open Cli in
  match profile with
  | Coqbase -> coqbase_types_table
  | Stdlib -> stdlib_types_table

let process with_type_value profile mode input ochannel =
  read_cmi input
  |> interface_of_cmi_infos ~with_type_value
  |> translate (types_table profile)
  |> pp_interface profile mode ochannel

let _ =
  try begin
    Cli.parse ();
    let input = Cli.get_input_path () in
    let output = Cli.get_output_formatter () in
    let profile = Cli.get_extraction_profile () in
    let mode = Cli.get_impure_mode () in
    let with_type_value = Cli.with_type_value () in
    process with_type_value profile mode input output
  end
  with
  | Cli.TooManyArguments ->
    Format.printf "Too many arguments.\n%s\n" Cli.usage
  | Cli.MissingInputArgument ->
    Format.printf "Too many arguments.\n%s\n" Cli.usage
  | Entry.UnsupportedOCamlSignature s ->
    Format.printf "Use of unsupported OCaml construction: %a"
      Printtyp.signature [s]
  | Repr.UnsupportedOCamlType t ->
    Format.printf "Unsupported OCaml type construction %a"
      Printtyp.type_expr t
  | Repr.UnknownOCamlType t ->
    Format.printf "Type %s is not supported by the selected profile"
      t
