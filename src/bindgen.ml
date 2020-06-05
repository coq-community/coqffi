open Cmi_format
open Interface

let _ =
  read_cmi "./_build/default/demo/.demo.objs/byte/demo__Console.cmti"
  |> input_of_cmi_infos Format.err_formatter
  |> Binding.print_coq_interface Format.std_formatter
