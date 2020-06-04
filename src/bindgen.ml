open Cmi_format
open Interface

let _ =
  read_cmi "./_build/default/demo/.demo.objs/byte/demo__Console.cmti"
  |> input_of_cmi_infos
  |> Binding.print_coq_interface
