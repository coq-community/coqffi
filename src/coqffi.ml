open Cmi_format
open Entry

let types_table =
  Translation.empty
  |> Translation.add "list" "list"
  |> Translation.add "int" "i63"
  |> Translation.add "bool" "bool"
  |> Translation.add "option" "option"
  |> Translation.add "unit" "unit"
  |> Translation.add "Coqbase.Bytestring.t" "Base.Data.Bytestring.bytestring"

let process input ochannel =
  let _ = read_cmi input
  |> input_module_of_cmi_infos
  |> translate types_table
  |> pp_input_module ochannel in
  Format.pp_print_newline ochannel ()


let _ =
  let input = Sys.argv.(1) in
  let output = Sys.argv.(2) in
  let ochannel = if output = "-"
    then Format.std_formatter
    else open_out output |> Format.formatter_of_out_channel in
  process input ochannel
