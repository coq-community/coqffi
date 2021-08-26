open Format
open Coqffi.Translation

let rec ( -- ) x y = if x < y then x :: (x + 1 -- y) else [ y ]

let prelude =
  {|Class _OfProd a b :=
  { of_prod : a -> b
  ; to_prod : b -> a
  }.

Instance _OfProd_refl a : _OfProd a a :=
  { of_prod := fun x => x
  ; to_prod := fun x => x
  }.

Declare Scope tup_scope.

Notation "( x , y , .. , z )" :=
  (of_prod (pair .. (pair x y) .. z)) (only parsing)
  : tup_scope.|}

let rec pp_repeat pp fmt n =
  if 0 < n then (
    pp fmt ();
    pp_repeat pp fmt (n - 1))

let pp_poly_arg fmt n = fprintf fmt "a%d" n

let pp_poly_args fmt l =
  pp_print_list ~pp_sep:(fun fmt _ -> pp_print_string fmt " ") pp_poly_arg fmt l

let pp_term_arg fmt n = fprintf fmt "x%d" n

let pp_term_args fmt l =
  pp_print_list ~pp_sep:(fun fmt _ -> pp_print_string fmt " ") pp_term_arg fmt l

let pp_inductive fmt n =
  fprintf fmt
    "@[<v>Inductive tup%d (%a : Type) :=@ @[<v 2>| mktup%d %a@ : tup%d %a.@]@]"
    n pp_poly_args (1 -- n) n
    (pp_print_list
       ~pp_sep:(fun fmt _ -> pp_print_string fmt " ")
       (fun fmt n -> fprintf fmt "(%a : %a)" pp_term_arg n pp_poly_arg n))
    (1 -- n) n pp_poly_args (1 -- n)

let pp_arguments fmt n =
  fprintf fmt "@[<v>Arguments mktup%d [%a] (%a).@]" n pp_poly_args (1 -- n)
    pp_term_args (1 -- n)

let pp_prod_arg fmt n =
  fprintf fmt "%a%a, %a)"
    (pp_repeat (fun fmt _ -> pp_print_char fmt '('))
    (n - 1) pp_term_arg 1
    (pp_print_list ~pp_sep:(fun fmt _ -> pp_print_string fmt "), ") pp_term_arg)
    (2 -- n)

let pp_instance fmt n =
  fprintf fmt
    "@[<v 2>Instance tup%d_of_prod {%a}@ : _OfProd (%a) (tup%d %a) := @ %a.@]" n
    pp_poly_args (1 -- n)
    (pp_print_list ~pp_sep:(fun fmt _ -> pp_print_string fmt " * ") pp_poly_arg)
    (1 -- n) n pp_poly_args (1 -- n)
    (fun fmt _ ->
      fprintf fmt
        "@[<v>{ of_prod := fun '%a => mktup%d %a@ ; to_prod := fun '(mktup%d \
         %a) => %a }@]"
        pp_prod_arg n n pp_term_args (1 -- n) n pp_term_args (1 -- n)
        pp_prod_arg n)
    ()

let pp_extraction fmt n =
  fprintf fmt
    "@[<v>From Coq Require Extraction.@ @ @[<v 2>Module TupleExtraction.@ \
     %a@]@ End TupleExtraction.@]"
    (pp_print_list
       ~pp_sep:(fun fmt _ -> fprintf fmt "@ @ ")
       (fun fmt n ->
         fprintf fmt "Extract Inductive tup%d => \"Shim.tup%d\" [ \"\" ]." n n))
    (3 -- n)

let _ =
  printf "@[<v>%s@ @ %a@ @ %a@]" prelude
    (pp_print_list
       ~pp_sep:(fun fmt _ -> fprintf fmt "@ @ ")
       (fun fmt n ->
         fprintf fmt "%a@ @ %a@ @ %a" pp_inductive n pp_arguments n pp_instance
           n))
    (3 -- max_tuple_size) pp_extraction max_tuple_size
