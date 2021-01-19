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

let not_empty = function
  | _ :: _ -> true
  | _ -> false

let pp_if_not_empty pp fmt l = if not_empty l then pp fmt ()

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

let pp_try_with pp fmt _ =
  fprintf fmt "try Stdlib.Result.ok %a with e -> Stdlib.Result.error e"
    pp ()

let pp_fun_call ?(paren=true) fn_name args fmt _ =
  fprintf fmt "%a%s%a%a"
    (fun fmt l -> if not_empty l && paren then pp_print_string fmt "(") args
    fn_name
    (pp_list
       ~pp_prefix:(fun fmt _ -> pp_print_string fmt " ")
       ~pp_sep:(fun fmt _ -> pp_print_string fmt " ")
       pp_print_string) args
    (fun fmt l -> if not_empty l && paren then pp_print_string fmt ")") args
