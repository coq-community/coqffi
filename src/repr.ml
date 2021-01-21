open Format
open Error

type mono_type_repr =
  | TLambda of (mono_type_repr * mono_type_repr)
  | TProd of (mono_type_repr list)
  | TParam of (string * mono_type_repr list)

type type_repr =
  | TMono of mono_type_repr
  | TPoly of (string list * mono_type_repr)

let to_mono_type_repr = function
  | TMono mono -> mono
  | TPoly (_, mono) -> mono

let of_mono_type_repr params mono =
  match params with
  | [] -> TMono mono
  | params -> TPoly (params, mono)

let supposedly_pure t =
  match to_mono_type_repr t with
  | TLambda (_, _) -> false
  | _ -> true

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
    raise_error (UnsupportedOCamlType t)

let type_repr_of_type_expr (t : Types.type_expr) : type_repr =
  let rec poly_vars (t : Types.type_expr) : string list =
    let minimize = List.sort_uniq String.compare in
    minimize
      (match t.desc with
       | Tvar (Some x) -> [x]
       | Tarrow (_, t1, t2, _) ->
         List.merge String.compare (poly_vars t1) (poly_vars t2)
       | Tconstr (_, types, _) ->
         Compat.concat_map (fun x -> poly_vars x) types
       | Ttuple(l) ->
         Compat.concat_map poly_vars l
       | _ ->
         raise_error (UnsupportedOCamlType t)) in

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

let type_lift t_name ?(args=[]) : type_repr -> type_repr =
  map_codomain (fun t -> TParam (t_name, args @ [t]))

let rec tlambda lx r =
  match lx with
  | x :: rst -> TLambda (x, tlambda rst r)
  | [] -> r

let rec translate_mono_type_repr (tbl : Translation.t) = function
  | TLambda (t1, t2) ->
    TLambda (translate_mono_type_repr tbl t1, translate_mono_type_repr tbl t2)
  | TProd typ_list ->
    TProd (List.map (translate_mono_type_repr tbl) typ_list)
  | TParam (name, typ_list) ->
    (match Translation.find tbl ~ocaml:name with
     | Some name' -> TParam (name', List.map (translate_mono_type_repr tbl) typ_list)
     | None -> raise_error (UnknownOCamlType name))

let translate_type_repr (tbl : Translation.t) = function
  | TMono mono -> TMono (translate_mono_type_repr tbl mono)
  | TPoly (polys, mono) ->
    let tbl' = List.fold_left (fun tbl t -> Translation.preserve t tbl) tbl polys in
    TPoly (polys, translate_mono_type_repr tbl' mono)

let rec mono_dependencies (t : mono_type_repr) : string list =
  let merge : string list -> string list -> string list =
    List.merge String.compare in
  let fold_mono_list : mono_type_repr list -> string list =
    Compat.concat_map (fun t -> mono_dependencies t) in
  match t with
  | TLambda (t1, t2) ->
    merge (mono_dependencies t1) (mono_dependencies t2)
  | TProd tl -> fold_mono_list tl
  | TParam (t, params) -> merge [t] (fold_mono_list params)

let dependencies : type_repr -> string list = function
  | TMono t -> mono_dependencies t
  | TPoly (params, t) ->
    List.filter
      (fun t -> not (List.mem t params))
      (mono_dependencies t)

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

let pp_mono_type_repr (fmt : formatter) mono =

  let paren pos constr =
    match (pos, constr) with
    | (PArrowLeft, CArrow) | (PProd, CArrow) | (PParam, _) -> true
    | (_, _) -> false in

  let open_paren pos constr =
    if paren pos constr then "(" else "" in

  let close_paren pos constr =
    if paren pos constr then ")" else "" in

  let rec pp_mono_type_repr_aux ~(pos : type_pos) (fmt : formatter) = function
    | TLambda (t1, t2) ->
      fprintf fmt "%s@[<hov>%a@ -> %a@]%s"
        (open_paren pos CArrow)
        (pp_mono_type_repr_aux ~pos:PArrowLeft) t1
        (pp_mono_type_repr_aux ~pos:PArrowRight) t2
        (close_paren pos CArrow)
    | TProd typ_list ->
      fprintf fmt "%s@[%a@]%s"
        (open_paren pos CProd)
        (pp_print_list
           ~pp_sep:(fun fmt _ -> pp_print_text fmt " * ")
           (pp_mono_type_repr_aux ~pos:PProd)) typ_list
        (close_paren pos CProd)
    | TParam (name, []) ->
      pp_print_text fmt name
    | TParam (name, args) ->
      fprintf fmt "%s@[<hv 2>%s@ %a@]%s"
        (open_paren pos CParam)
        name
        (pp_print_list ~pp_sep:pp_print_space
           (pp_mono_type_repr_aux ~pos:PParam)) args
        (close_paren pos CParam) in

  pp_mono_type_repr_aux ~pos:PTop fmt mono

let pp_type_repr (fmt : formatter) = function
  | TMono typ -> pp_mono_type_repr fmt typ
  | TPoly (polys, typ) ->
    fprintf fmt "@[<hov 2>@[<hov 2>forall %a@],@ %a@]"
      (pp_print_list ~pp_sep:pp_print_space
         (fun fmt -> fprintf fmt "(%s : Type)"))
      polys
      pp_mono_type_repr typ

let type_sort_mono = TParam ("Type", [])

let type_sort = TMono type_sort_mono

type prototype_repr = {
  prototype_type_args : string list;
  prototype_args : type_repr list;
  prototype_ret_type : type_repr
}

let type_repr_to_prototype_repr =
  let rec split_mono_type args acc = function
    | TLambda (x, rst) -> split_mono_type args (TMono x :: acc) rst
    | t -> {
        prototype_type_args = args;
        prototype_args = List.rev acc;
        prototype_ret_type = TMono t
      } in
  function
  | TMono t -> split_mono_type [] [] t
  | TPoly (a, t) -> split_mono_type a [] t
