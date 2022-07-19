open Format
open Error

type constant_repr = CPlaceholder of int | CName of string

type argument_kind =
  | PositionedArg
  | LabeledArg of string
  | OptionalArg of string

type argument = { position : int; kind : argument_kind }

type mono_type_repr =
  | TLambda of {
      argtype : argument;
      domain : mono_type_repr;
      codomain : mono_type_repr;
    }
  | TProd of mono_type_repr * mono_type_repr
  | TParam of (constant_repr * mono_type_repr list)

type type_repr =
  | TMono of mono_type_repr
  | TPoly of (string list * mono_type_repr)

let to_mono_type_repr = function TMono mono -> mono | TPoly (_, mono) -> mono

let of_mono_type_repr params mono =
  match params with [] -> TMono mono | params -> TPoly (params, mono)

let is_function t =
  match to_mono_type_repr t with TLambda _ -> true | _ -> false

let depends_on_types t names =
  let rec depends_on_type_mono m =
    match m with
    | TLambda { domain; codomain; _ } ->
        depends_on_type_mono domain || depends_on_type_mono codomain
    | TParam (CName const, _) when List.exists (String.equal const) names ->
        true
    | TProd (m1, m2) -> depends_on_type_mono m1 || depends_on_type_mono m2
    | TParam (_, ms) -> List.exists depends_on_type_mono ms
  in
  depends_on_type_mono (to_mono_type_repr t)

let asynchronous ~lwt_module typ =
  let lwt = Option.map (fun x -> x ^ ".t") lwt_module in

  let rec mono_asynchronous = function
    | TLambda { codomain; _ } -> mono_asynchronous codomain
    | TParam (CName type_name, [ _ ]) -> lwt = Some type_name
    | _ -> false
  in

  mono_asynchronous (to_mono_type_repr typ)

type params_pool = string Seq.t

let make_params_pool (existing_params : string list) : params_pool =
  let rec lazy_append (x : 'a Seq.t) (y : 'a Seq.t Lazy.t) : 'a Seq.t =
    match x () with
    | Nil -> Lazy.force y
    | Cons (x, rst) -> fun _ -> Cons (x, lazy_append rst y)
  in

  let type_params =
    List.to_seq
      [
        "a";
        "b";
        "c";
        "d";
        "e";
        "f";
        "g";
        "h";
        "i";
        "j";
        "k";
        "l";
        "m";
        "n";
        "o";
        "p";
        "q";
        "r";
        "s";
        "t";
        "u";
        "v";
        "w";
        "x";
        "y";
        "z";
      ]
  in
  let rec aux prev =
    let next =
      Seq.flat_map (fun x -> Seq.map (fun t -> t ^ x) type_params) prev
    in
    lazy_append next (lazy (aux next))
  in

  let rec remove potential_params existing_params _ =
    let open Seq in
    match (potential_params (), existing_params) with
    | Cons (x, pot), y :: ext ->
        if x = y then remove pot ext () else Cons (x, fun _ -> remove pot ext ())
    | pot, [] -> pot
    | Nil, _ -> Nil
  in

  remove (lazy_append type_params (lazy (aux type_params))) existing_params

let pick_param params =
  let open Seq in
  match params () with
  | Cons (x, rst) -> (x, rst)
  | Nil -> failwith "This should not happen since [type_params] is a stream"

let pick_params =
  let rec aux acc n params =
    if 0 < n then
      let p, params = pick_param params in
      aux (p :: acc) (n - 1) params
    else (List.rev acc, params)
  in
  aux []

let named_poly_vars (t : Types.type_expr) : string list =
  let minimize = List.sort_uniq String.compare in
  let rec named_poly_vars (t : Types.type_expr) =
    match Types.get_desc t with
    | Tvar (Some "_") | Tvar None -> []
    | Tvar (Some x) -> [ x ]
    | Tarrow (_, t1, t2, _) ->
        List.merge String.compare (named_poly_vars t1) (named_poly_vars t2)
    | Tconstr (_, types, _) ->
        Compat.concat_map (fun x -> named_poly_vars x) types
    | Ttuple l -> Compat.concat_map named_poly_vars l
    | _ -> raise_error (UnsupportedOCamlType t)
  in

  minimize (named_poly_vars t)

let mono_type_repr_of_type_expr_with_params params t :
    params_pool * mono_type_repr =
  let initial_pos = 1 in
  let rec aux pos params (t : Types.type_expr) =
    match Types.get_desc t with
    | Tvar (Some "_") | Tvar None ->
        let p, params = pick_param params in
        (params, TParam (CName p, []))
    | Tvar (Some x) ->
        (params, TParam (CName x, [])) (* FIXME: Support labeled arguments *)
    | Tarrow (label, t1, t2, _) ->
        let argtype =
          {
            position = pos;
            kind =
              (match label with
              | Optional opt -> OptionalArg opt
              | Labelled opt -> LabeledArg opt
              | _ -> PositionedArg);
          }
        in

        let params, domain = aux initial_pos params t1 in
        let params, codomain = aux (pos + 1) params t2 in
        (params, TLambda { argtype; domain; codomain })
    | Ttuple [ l1; l2 ] ->
        let params, t1 = aux initial_pos params l1 in
        let params, t2 = aux initial_pos params l2 in
        (params, TProd (t1, t2))
    | Ttuple l ->
        let len = List.length l in
        if len < 6 then
          let params, t = Compat.fold_left_map (aux initial_pos) params l in
          (params, TParam (CName (sprintf "Coq_coqffi.Shim.tuple%d" len), t))
        else assert false
    | Tconstr (name, types, _) ->
        let params, t = Compat.fold_left_map (aux initial_pos) params types in
        (params, TParam (CName (Path.name name), t))
    | _ -> raise_error (UnsupportedOCamlType t)
  in
  aux initial_pos params t

let rec fill_placeholder_mono i name = function
  | TParam (CPlaceholder i', params) when i = i' ->
      TParam (CName name, List.map (fill_placeholder_mono i name) params)
  | TParam (constant, params) ->
      TParam (constant, List.map (fill_placeholder_mono i name) params)
  | TProd (t1, t2) ->
      TProd (fill_placeholder_mono i name t1, fill_placeholder_mono i name t2)
  | TLambda lambda ->
      TLambda
        {
          lambda with
          domain = fill_placeholder_mono i name lambda.domain;
          codomain = fill_placeholder_mono i name lambda.codomain;
        }

let rec mono_has_labelled_arg = function
  | TLambda { argtype = { kind = OptionalArg _; _ }; _ }
  | TLambda { argtype = { kind = LabeledArg _; _ }; _ } ->
      true
  | TLambda { codomain; _ } -> mono_has_labelled_arg codomain
  | _ -> false

let has_labelled_arg t = to_mono_type_repr t |> mono_has_labelled_arg

(* [monadic {m} t] returns [true] when [t] features a subpart of the
   form {m _}. For instance, [monadic {m} {m bool}] and [monadic {m}
   {list (m bool)}] are true, while [monadic {m} {a}] is false. *)
let rec monadic m = function
  | TParam (m', _) when m = m' -> true
  | TParam (_, typs) -> List.exists (monadic m) typs
  | TProd (t1, t2) -> List.exists (monadic m) [ t1; t2 ]
  | TLambda { domain; codomain; _ } -> monadic m domain || monadic m codomain

(* [higher_order_monadic {m} t] returns [true] when [t] features a
   subpart of the form {m _}, more precisely

     - at the right side of a lambda, for instance [{m t -> a}] is true
     - in a tuple, for instance [{m t * a}] is true
     - as a parameter of a type, for instance [{m a}] is false, but
       [{list (m a)}] is true *)
let rec higher_order_monadic_mono m = function
  | TParam (_, typs) -> List.exists (monadic m) typs
  | TLambda { domain; codomain; _ } ->
      monadic m domain || higher_order_monadic_mono m codomain
  | t -> monadic m t

let higher_order_monadic m = function
  | TMono t | TPoly (_, t) -> higher_order_monadic_mono m t

let fill_placeholder i name = function
  | TMono mono -> TMono (fill_placeholder_mono i name mono)
  | TPoly (params, t) -> TPoly (params, fill_placeholder_mono i name t)

let next_placeholder = function CPlaceholder i -> i + 1 | CName _ -> 0

let rec fresh_placeholder_mono = function
  | TParam (constant, params) ->
      List.fold_left
        (fun i x -> max i (fresh_placeholder_mono x))
        (next_placeholder constant)
        params
  | TLambda { domain; codomain; _ } ->
      max (fresh_placeholder_mono domain) (fresh_placeholder_mono codomain)
  | TProd (t1, t2) ->
      List.fold_left (fun i x -> max i (fresh_placeholder_mono x)) 0 [ t1; t2 ]

let fresh_placeholder = function
  | TMono t | TPoly (_, t) -> fresh_placeholder_mono t

let place_placeholder_constant i name = function
  | CName name' when name = name' -> CPlaceholder i
  | constant -> constant

let rec place_placeholder_mono i name = function
  | TParam (constant, params) ->
      TParam
        ( place_placeholder_constant i name constant,
          List.map (place_placeholder_mono i name) params )
  | TProd (t1, t2) ->
      TProd (place_placeholder_mono i name t1, place_placeholder_mono i name t2)
  | TLambda lambda ->
      TLambda
        {
          lambda with
          domain = place_placeholder_mono i name lambda.domain;
          codomain = place_placeholder_mono i name lambda.codomain;
        }

let place_placeholder name = function
  | TMono t ->
      let i = fresh_placeholder_mono t in
      (i, TMono (place_placeholder_mono i name t))
  | TPoly (params, t) ->
      let i = fresh_placeholder_mono t in
      (i, TPoly (params, place_placeholder_mono i name t))

let mono_type_repr_of_type_expr (t : Types.type_expr) : mono_type_repr =
  let ext = named_poly_vars t in
  snd (mono_type_repr_of_type_expr_with_params (make_params_pool ext) t)

let all_poly_vars params t : string list =
  let minimize = List.sort_uniq String.compare in

  let rec poly_vars params (t : Types.type_expr) : params_pool * string list =
    match Types.get_desc t with
    | Tvar (Some "_") | Tvar None ->
        let x, params = pick_param params in
        (params, [ x ])
    | Tvar (Some x) -> (params, [ x ])
    | Tarrow (_, t1, t2, _) ->
        let params, l1 = poly_vars params t1 in
        let params, l2 = poly_vars params t2 in
        (params, List.merge String.compare l1 l2)
    | Tconstr (_, types, _) ->
        let params, ll = Compat.fold_left_map poly_vars params types in
        (params, List.flatten ll)
    | Ttuple l ->
        let params, ll = Compat.fold_left_map poly_vars params l in
        (params, List.flatten ll)
    | _ -> raise_error (UnsupportedOCamlType t)
  in

  minimize @@ snd (poly_vars params t)

let type_repr_of_type_expr (t : Types.type_expr) : type_repr =
  let ext = named_poly_vars t in

  let _, mono =
    mono_type_repr_of_type_expr_with_params (make_params_pool ext) t
  in

  match all_poly_vars (make_params_pool ext) t with
  | [] -> TMono mono
  | l -> TPoly (l, mono)

let map_codomain (f : mono_type_repr -> mono_type_repr) (t : type_repr) :
    type_repr =
  let rec aux = function
    | TLambda lambda -> TLambda { lambda with codomain = aux lambda.codomain }
    | t -> f t
  in

  match t with
  | TPoly (polys, mt) -> TPoly (polys, aux mt)
  | TMono mt -> TMono (aux mt)

let type_lift t_name ?(args = []) : type_repr -> type_repr =
  map_codomain (fun t -> TParam (CName t_name, args @ [ t ]))

let tlambda =
  let rec aux pos lx r =
    match lx with
    | domain :: rst ->
        TLambda
          {
            argtype = { position = pos; kind = PositionedArg };
            domain;
            codomain = aux (pos + 1) rst r;
          }
    | [] -> r
  in
  aux 0

let translate_constant_repr ~rev_namespace tbl = function
  | CName ocaml -> (
      match Translation.find ~rev_namespace ~ocaml tbl with
      | Some x -> CName x
      | _ -> raise_error (UnknownOCamlType ocaml))
  | x -> x

let rec translate_mono_type_repr ~rev_namespace (tbl : Translation.t) = function
  | TLambda lambda ->
      let domain = translate_mono_type_repr ~rev_namespace tbl lambda.domain in
      let codomain =
        translate_mono_type_repr ~rev_namespace tbl lambda.codomain
      in
      TLambda { lambda with domain; codomain }
  | TProd (t1, t2) ->
      TProd
        ( translate_mono_type_repr ~rev_namespace tbl t1,
          translate_mono_type_repr ~rev_namespace tbl t2 )
  | TParam (ocaml, typ_list) ->
      let name' = translate_constant_repr ~rev_namespace tbl ocaml in
      let typ_list' =
        List.map (translate_mono_type_repr ~rev_namespace tbl) typ_list
      in
      TParam (name', typ_list')

let translate_type_repr ~rev_namespace (tbl : Translation.t) = function
  | TMono mono -> TMono (translate_mono_type_repr ~rev_namespace tbl mono)
  | TPoly (polys, mono) ->
      let tbl' =
        List.fold_left
          (fun tbl t -> Translation.preserve ~rev_namespace t tbl)
          tbl polys
      in
      TPoly (polys, translate_mono_type_repr ~rev_namespace tbl' mono)

let rec mono_dependencies (t : mono_type_repr) : string list =
  let merge : string list -> string list -> string list =
    List.merge String.compare
  in
  let fold_mono_list : mono_type_repr list -> string list =
    Compat.concat_map (fun t -> mono_dependencies t)
  in
  match t with
  | TLambda { domain; codomain; _ } ->
      merge (mono_dependencies domain) (mono_dependencies codomain)
  | TProd (t1, t2) -> fold_mono_list [ t1; t2 ]
  | TParam (CName t, params) -> merge [ t ] (fold_mono_list params)
  | TParam (_, params) -> fold_mono_list params

let dependencies : type_repr -> string list = function
  | TMono t -> mono_dependencies t
  | TPoly (params, t) ->
      List.filter (fun t -> not (List.mem t params)) (mono_dependencies t)

type type_pos =
  | PTop
  | PArrowLeft
  | PArrowRight
  | PProdLeft
  | PProdRight
  | PParam

type pos_constr = CArrow | CProd | CParam

let pp_constant_repr fmt = function
  | CName x -> pp_print_text fmt x
  | CPlaceholder i -> fprintf fmt "?%d" i

let pp_mono_type_repr (fmt : formatter) mono =
  let paren pos constr =
    match (pos, constr) with
    | PArrowLeft, CArrow
    | PProdRight, CArrow
    | PProdLeft, CArrow
    | PProdRight, CProd
    | PParam, _ ->
        true
    | _, _ -> false
  in

  let open_paren pos constr = if paren pos constr then "(" else "" in

  let close_paren pos constr = if paren pos constr then ")" else "" in

  let rec pp_mono_type_repr_aux ~(pos : type_pos) (fmt : formatter) = function
    | TLambda { domain; codomain; _ } ->
        fprintf fmt "%s@[<hov>%a@ -> %a@]%s" (open_paren pos CArrow)
          (pp_mono_type_repr_aux ~pos:PArrowLeft)
          domain
          (pp_mono_type_repr_aux ~pos:PArrowRight)
          codomain (close_paren pos CArrow)
    | TProd (t1, t2) ->
        fprintf fmt "%s@[%a * %a@]%s" (open_paren pos CProd)
          (pp_mono_type_repr_aux ~pos:PProdLeft)
          t1
          (pp_mono_type_repr_aux ~pos:PProdRight)
          t2 (close_paren pos CProd)
    | TParam (name, []) -> pp_constant_repr fmt name
    | TParam (name, args) ->
        fprintf fmt "%s@[<hv 2>%a@ %a@]%s" (open_paren pos CParam)
          pp_constant_repr name
          (pp_print_list ~pp_sep:pp_print_space
             (pp_mono_type_repr_aux ~pos:PParam))
          args (close_paren pos CParam)
  in

  pp_mono_type_repr_aux ~pos:PTop fmt mono

let pp_type_repr (fmt : formatter) = function
  | TMono typ -> pp_mono_type_repr fmt typ
  | TPoly (polys, typ) ->
      fprintf fmt "@[<hov 2>@[<hov 2>forall %a@],@ %a@]"
        (pp_print_list ~pp_sep:pp_print_space (fun fmt ->
             fprintf fmt "(%s : Type)"))
        polys pp_mono_type_repr typ

let type_sort_mono = TParam (CName "Type", [])

let type_sort = TMono type_sort_mono

type prototype_repr = {
  prototype_type_args : string list;
  prototype_args : (argument * type_repr) list;
  prototype_ret_type : type_repr;
}

let type_repr_to_prototype_repr =
  let rec split_mono_type args acc = function
    | TLambda { domain; codomain; argtype } ->
        split_mono_type args ((argtype, TMono domain) :: acc) codomain
    | t ->
        {
          prototype_type_args = args;
          prototype_args = List.rev acc;
          prototype_ret_type = TMono t;
        }
  in
  function
  | TMono t -> split_mono_type [] [] t | TPoly (a, t) -> split_mono_type a [] t
