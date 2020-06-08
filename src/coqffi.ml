open Cmi_format
open Interface

let types_table =
  let map = Hashtbl.create 10 in
  Hashtbl.add map "list" "list";
  Hashtbl.add map "int" "i63";
  Hashtbl.add map "bool" "bool";
  Hashtbl.add map "option" "option";
  Hashtbl.add map "unit" "unit";
  Hashtbl.add map "Coqbase.Bytestring.t" "Base.Data.Bytestring.bytestring";
  map

let _ =
  let input = Sys.argv.(1) in
  let output = Sys.argv.(2) in
  let ochannel = if output = "-"
    then Format.std_formatter
    else open_out output |> Format.formatter_of_out_channel in
  read_cmi input
  |> input_of_cmi_infos Format.err_formatter
  |> coq_of_ocaml_types types_table
  |> Binding.print_coq_interface ochannel
