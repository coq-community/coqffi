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
  read_cmi "./_build/default/demo/.demo.objs/byte/demo__Console.cmti"
  |> input_of_cmi_infos Format.err_formatter
  |> coq_of_ocaml_types types_table
  |> Binding.print_coq_interface Format.std_formatter
