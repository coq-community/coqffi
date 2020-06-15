open Format

type mono_type_repr =
  | TLambda of (mono_type_repr * mono_type_repr)
  | TProd of (mono_type_repr list)
  | TParam of (string * mono_type_repr list)

type type_repr =
  | TMono of mono_type_repr
  | TPoly of (string list * mono_type_repr)

exception UnsupportedOCamlType of Types.type_expr

let named_param_mono_type (name : string) (param : mono_type_repr list)
  : mono_type_repr =
  TParam (name, param)

let named_mono_type (name : string) : mono_type_repr =
  named_param_mono_type name []

let rec mono_type_repr_of_type_expr (t : Types.type_expr) : mono_type_repr =
  match t.desc with
  | Tvar (Some x) -> TParam (x, [])
  | Tarrow (_ , t1, t2, _) ->
    let i1 = mono_type_repr_of_type_expr t1 in
    let i2 = mono_type_repr_of_type_expr t2 in
    TLambda (i1, i2)
  | Ttuple l ->
    TProd (List.map mono_type_repr_of_type_expr l)
  | Tconstr (name, types, _) ->
    TParam (Path.name name, (List.map (fun x -> mono_type_repr_of_type_expr x) types))
  | _ ->
    raise (UnsupportedOCamlType t)

let type_repr_of_type_expr (t : Types.type_expr) : type_repr =
  let rec poly_vars (t : Types.type_expr) : string list =
    let minimize = List.sort_uniq String.compare in
    minimize
      (match t.desc with
       | Tvar (Some x) -> [x]
       | Tarrow (_, t1, t2, _) ->
         List.merge String.compare (poly_vars t1) (poly_vars t2)
       | Tconstr (_, types, _) ->
         List.concat_map (fun x -> poly_vars x) types
       | Ttuple(l) ->
         List.concat_map poly_vars l
       | _ ->
         raise (UnsupportedOCamlType t)) in

  let mono = mono_type_repr_of_type_expr t in

  match poly_vars t with
  | [] -> TMono mono
  | l -> TPoly (l, mono)

let map_codomain (f : mono_type_repr -> mono_type_repr) (t : type_repr) : type_repr =
  let rec aux = function
    | TLambda (t1, t2) -> TLambda (t1, aux t2)
    | t -> f t in

  match t with
  | TPoly (polys, mt) -> TPoly (polys, aux mt)
  | TMono mt -> TMono (aux mt)

let impure_proj (interface_name : string) : type_repr -> type_repr =
  map_codomain
    (fun t -> named_param_mono_type "impure" [named_mono_type interface_name; t])

let interface_proj (interface_name : string) (t : type_repr) : type_repr =
  map_codomain (fun t -> named_param_mono_type interface_name [t]) t

exception UnknownOCamlType of string

let rec translate_mono_type_repr (tbl : Translation.t) = function
  | TLambda (t1, t2) ->
    TLambda (translate_mono_type_repr tbl t1, translate_mono_type_repr tbl t2)
  | TProd typ_list ->
    TProd (List.map (translate_mono_type_repr tbl) typ_list)
  | TParam (name, typ_list) ->
    (match Translation.find tbl name with
     | Some name' -> TParam (name', List.map (translate_mono_type_repr tbl) typ_list)
     | None -> raise (UnknownOCamlType name))

let translate_type_repr (tbl : Translation.t) = function
  | TMono mono -> TMono (translate_mono_type_repr tbl mono)
  | TPoly (polys, mono) ->
    let tbl' = List.fold_left (fun tbl t -> Translation.add t t tbl) tbl polys in
    TPoly (polys, translate_mono_type_repr tbl' mono)

type type_pos =
  | PTop
  | PArrowLeft
  | PArrowRight
  | PProd
  | PParam

type pos_constr =
  | CArrow
  | CProd
  | CParam

let pp_mono_type_repr_arrows (fmt : formatter) mono =

  let paren pos constr =
    match (pos, constr) with
    | (PArrowLeft, CArrow) | (PProd, CArrow) | (PParam, _) -> true
    | (_, _) -> false in

  let open_paren pos constr =
    if paren pos constr then "(" else "" in

  let close_paren pos constr =
    if paren pos constr then ")" else "" in

  let rec pp_mono_type_repr_arrows_aux ~(pos : type_pos) (fmt : formatter) = function
    | TLambda (t1, t2) ->
      fprintf fmt "%s@[<hov>%a@ -> %a@]%s"
        (open_paren pos CArrow)
        (pp_mono_type_repr_arrows_aux ~pos:PArrowLeft) t1
        (pp_mono_type_repr_arrows_aux ~pos:PArrowRight) t2
        (close_paren pos CArrow)
    | TProd typ_list ->
      fprintf fmt "%s@[%a@]%s"
        (open_paren pos CProd)
        (pp_print_list
           ~pp_sep:(fun fmt _ -> pp_print_text fmt " * ")
           (pp_mono_type_repr_arrows_aux ~pos:PProd)) typ_list
        (close_paren pos CProd)
    | TParam (name, []) ->
      pp_print_text fmt name
    | TParam (name, args) ->
      fprintf fmt "%s@[<hv 2>%s@ %a@]%s"
        (open_paren pos CParam)
        name
        (pp_print_list ~pp_sep:pp_print_space
           (pp_mono_type_repr_arrows_aux ~pos:PParam)) args
        (close_paren pos CParam) in

  pp_mono_type_repr_arrows_aux ~pos:PTop fmt mono

let pp_type_repr_arrows (fmt : formatter) = function
  | TMono typ -> pp_mono_type_repr_arrows fmt typ
  | TPoly (polys, typ) ->
    fprintf fmt "@[<hov 2>@[<hov 2>forall %a@],@ %a@]"
      (pp_print_list ~pp_sep:pp_print_space
         (fun fmt -> fprintf fmt "(%s : Type)"))
      polys
      pp_mono_type_repr_arrows typ

let pp_type_repr_prototype prefix (fmt : formatter) (t : type_repr) =
  let type_repr  = function
    | TPoly (_, t) -> t
    | TMono t -> t in

  let polys = function
    | TPoly (p, _) -> p
    | TMono _ -> [] in

  let split =
    let rec aux acc = function
      | TLambda (t1, t2) ->
        aux (acc @ [t1]) t2
      | t -> (acc, t) in
    aux [] in

  let pp_args =
    let n = ref 0 in
    pp_print_list ~pp_sep:pp_print_space
      (fun fmt t -> begin
           fprintf fmt "(x%d : %a)"
             !n pp_mono_type_repr_arrows t;
           n := !n + 1
         end) in

  match (polys t, split (type_repr t)) with
  | ([], ([], ret_type)) ->
    fprintf fmt "@[<hov 2>%s@ : %a@]"
      prefix
      pp_mono_type_repr_arrows ret_type
  | ([], (args, ret_type)) ->
    fprintf fmt "@[<hov 2>@[%s@ %a@]@ : %a@]"
      prefix
      pp_args args
      pp_mono_type_repr_arrows ret_type
  | (polys, (args, ret_type)) ->
    fprintf fmt "@[<hv 2>@[<hov 4>%s@ %a@ %a@]@ : %a@]"
      prefix
      (pp_print_list ~pp_sep:pp_print_space
         (fun fmt typ -> fprintf fmt "(%s : Type)" typ)) polys
      pp_args args
      pp_mono_type_repr_arrows ret_type

let pp_mono_type_repr_arg_list fmt typ_list =
  let rec to_list = function
    | TLambda (t1, t2) -> t1 :: to_list t2
    | _ -> [] in

  let n = ref 0 in

  pp_open_hbox fmt ();
  pp_print_list ~pp_sep:(fun fmt _ -> pp_print_text fmt " ") (fun fmt _ ->
      fprintf fmt "x%d" !n;
      n := !n + 1) fmt (to_list typ_list);
  pp_close_box fmt ()

let pp_type_repr_arg_list fmt = function
  | TPoly (_, mono) -> pp_mono_type_repr_arg_list fmt mono
  | TMono mono -> pp_mono_type_repr_arg_list fmt mono
