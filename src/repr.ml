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

let asynchronous ~lwt_alias typ =
  let rec mono_asynchronous = function
    | TLambda (_, r) -> mono_asynchronous r
    | TParam (type_name, [_]) when lwt_alias = Some type_name -> true
    | _ -> false in

  mono_asynchronous (to_mono_type_repr typ)

type params_pool = string Seq.t

let make_params_pool (existing_params : string list) : params_pool =
  let rec lazy_append (x : 'a Seq.t) (y : 'a Seq.t Lazy.t) : 'a Seq.t =
    match x () with
    | Nil -> Lazy.force y
    | Cons (x, rst) -> fun _ -> Cons (x, lazy_append rst y) in

  let type_params = List.to_seq
                      [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k";
                      "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v";
                      "w"; "x"; "y"; "z" ] in
  let rec aux prev =
    let next = Seq.flat_map (fun x -> Seq.map (fun t -> t ^ x) type_params) prev in
    lazy_append next (lazy (aux next)) in

  let rec remove potential_params existing_params _ =
    let open Seq in
    match (potential_params (), existing_params) with
    | (Cons (x, pot), y :: ext) ->
      if x = y
      then remove pot ext ()
      else Cons (x, fun _ -> remove pot ext ())
    | pot, [] -> pot
    | Nil, _ -> Nil in

  remove (lazy_append type_params (lazy (aux type_params))) existing_params

let pick_param params =
  let open Seq in
  match params () with
  | Cons (x, rst) -> (x, rst)
  | Nil -> failwith "This should not happen since [type_params] is a stream"

let pick_params =
  let rec aux acc n params =
    if 0 < n
    then let (p, params) = pick_param params in
         aux (p :: acc) (n-1) params
    else (List.rev acc, params)
  in aux []

let named_poly_vars (t : Types.type_expr) : string list =
  let minimize = List.sort_uniq String.compare in
  let rec named_poly_vars (t : Types.type_expr) =
    match t.desc with
    | Tvar (Some "_") | Tvar None -> []
    | Tvar (Some x) -> [x]
    | Tarrow (_, t1, t2, _) ->
      List.merge String.compare (named_poly_vars t1) (named_poly_vars t2)
    | Tconstr (_, types, _) ->
      Compat.concat_map (fun x -> named_poly_vars x) types
    | Ttuple(l) ->
      Compat.concat_map named_poly_vars l
    | _ ->
      raise_error (UnsupportedOCamlType t) in

  minimize (named_poly_vars t)

let rec mono_type_repr_of_type_expr_with_params params (t : Types.type_expr)
        : params_pool * mono_type_repr =
  match t.desc with
  | Tvar (Some "_") | Tvar None ->
    let (p, params) = pick_param params in
    (params, TParam (p, []))
  | Tvar (Some x) ->
    (params, TParam (x, []))
    (* FIXME: Support labeled arguments *)
  | Tarrow (Nolabel , t1, t2, _) ->
    let (params, i1) = mono_type_repr_of_type_expr_with_params params t1 in
    let (params, i2) = mono_type_repr_of_type_expr_with_params params t2 in
    (params, TLambda (i1, i2))
  | Ttuple l ->
    let (params, l) = Compat.fold_left_map mono_type_repr_of_type_expr_with_params params l in
    (params, TProd l)
  | Tconstr (name, types, _) ->
    let (params, t) = Compat.fold_left_map mono_type_repr_of_type_expr_with_params params types in
    (params, TParam (Path.name name, t))
  | _ ->
    raise_error (UnsupportedOCamlType t)

let mono_type_repr_of_type_expr (t : Types.type_expr) : mono_type_repr =
  let ext = named_poly_vars t in
  snd (mono_type_repr_of_type_expr_with_params (make_params_pool ext) t)

let all_poly_vars params t : string list =
  let minimize = List.sort_uniq String.compare in

  let rec poly_vars params (t : Types.type_expr) : params_pool * string list =
    match t.desc with
    | Tvar (Some "_") | Tvar None ->
      let (x, params) = pick_param params in
      (params, [x])
    | Tvar (Some x) -> (params, [x])
    | Tarrow (_, t1, t2, _) ->
      let (params, l1) = poly_vars params t1 in
      let (params, l2) = poly_vars params t2 in
      (params, List.merge String.compare l1 l2)
    | Tconstr (_, types, _) ->
      let (params, ll) = Compat.fold_left_map poly_vars params types in
      (params, List.flatten ll)
    | Ttuple l ->
      let (params, ll) = Compat.fold_left_map poly_vars params l in
      (params, List.flatten ll)
    | _ ->
      raise_error (UnsupportedOCamlType t) in

  minimize @@ snd (poly_vars params t)

let type_repr_of_type_expr (t : Types.type_expr) : type_repr =
  let ext = named_poly_vars t in

  let (_, mono) = mono_type_repr_of_type_expr_with_params (make_params_pool ext) t in

  match all_poly_vars (make_params_pool ext) t with
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

let rec translate_mono_type_repr ~rev_namespace (tbl : Translation.t) = function
  | TLambda (t1, t2) ->
    let t1' = translate_mono_type_repr ~rev_namespace tbl t1 in
    let t2' = translate_mono_type_repr ~rev_namespace tbl t2 in
    TLambda (t1', t2')
  | TProd typ_list ->
    TProd (List.map (translate_mono_type_repr ~rev_namespace tbl) typ_list)
  | TParam (ocaml, typ_list) ->
    (match Translation.find ~rev_namespace tbl ~ocaml with
     | Some name' ->
       let typ_list' = List.map (translate_mono_type_repr ~rev_namespace tbl) typ_list in
       TParam (name', typ_list')
     | None -> raise_error (UnknownOCamlType ocaml))

let translate_type_repr ~rev_namespace (tbl : Translation.t) = function
  | TMono mono -> TMono (translate_mono_type_repr ~rev_namespace tbl mono)
  | TPoly (polys, mono) ->
    let tbl' =
      List.fold_left
        (fun tbl t -> Translation.preserve ~rev_namespace t tbl)
        tbl polys in
    TPoly (polys, translate_mono_type_repr ~rev_namespace tbl' mono)

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
