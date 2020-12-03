open Format
open Repr

let pp_list ?(pp_prefix=fun _ _ -> ()) ?(pp_suffix=fun _ _ -> ())
    ~pp_sep pp fmt =
  function
  | _ :: _ as l -> begin
      pp_prefix fmt ();
      pp_print_list ~pp_sep pp fmt l;
      pp_suffix fmt ();
    end
  | _ -> ()

let pp_if_not_empty pp fmt = function
  | _ :: _ -> pp fmt ()
  | _ -> ()

let pp_args_list fmt args_list =
  let idx = ref 0 in
  pp_print_list ~pp_sep:pp_print_space
    (fun fmt arg -> begin
         let x = !idx in
         fprintf fmt "(x%d : %a)" x pp_type_repr arg;
         idx := x + 1
       end) fmt args_list

let pp_type_args_list fmt type_args_list =
  pp_print_list ~pp_sep:pp_print_space
    (fun fmt arg -> fprintf fmt "(%s : Type)" arg)
    fmt
    type_args_list
