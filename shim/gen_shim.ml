open Format
open Coqffi.Translation

let rec ( -- ) x y = if x < y then x :: (x + 1 -- y) else [ y ]

let pp_tuple_def fmt n =
  let pp_poly_type fmt n = fprintf fmt "'a%d" n in
  fprintf fmt "@[<v 2>type (%a) tupl%n =@ %a@]"
    (pp_print_list ~pp_sep:(fun fmt _ -> pp_print_string fmt ", ") pp_poly_type)
    (1 -- n) n
    (pp_print_list
       ~pp_sep:(fun fmt _ -> pp_print_string fmt " * ")
       pp_poly_type)
    (1 -- n)

let _ =
  printf "@[<v>%a@]"
    (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "@ @ ") pp_tuple_def)
    (3 -- max_tuple_size)
