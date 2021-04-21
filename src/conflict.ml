module Table = Map.Make (String)

let of_namespace = String.concat "."

type kind =
  | Keyword
  | Value
  | Type
  | Field of string
  | Constructor of string
  | Helper of string

let is_helper = function Helper _ -> true | _ -> false

type local = (kind * (string * string)) list Table.t

let local_is_keyword candidate t =
  match Table.find_opt candidate t with
  | Some conflicts -> List.mem_assoc Keyword conflicts
  | _ -> false

type global = local Table.t

type t = { conflict_table : global; conflict_default : local }

let is_keyword candidate t = local_is_keyword candidate t.conflict_default

let add_conflict kind word ~ocaml ~coq t =
  Table.update word
    (function
      | Some conflicts ->
          if not (List.mem_assoc kind conflicts) then
            Some ((kind, (ocaml, coq)) :: conflicts)
          else if kind = Value || kind = Keyword then
            (* A conflict was already registered for the kind [Value]
               —which means [word] is a known OCaml operator— or
               [Keyword]. We don’t need to update the conflict
               table. *)
            Some conflicts
          else
            (* A conflict was already registered for [kind], which
               should not happen. This is a bug. *)
            assert false
      | None -> Some [ (kind, (ocaml, coq)) ])
    t

let add_keyword_conflict kw = add_conflict Keyword kw ~ocaml:kw ~coq:""

let add_operator_conflict ~op ~coq =
  add_conflict Value op ~ocaml:("( " ^ op ^ " )") ~coq

let default_local =
  Table.empty
  |> add_operator_conflict ~op:"+" ~coq:"add"
  |> add_operator_conflict ~op:"-" ~coq:"sub"
  |> add_operator_conflict ~op:"*" ~coq:"mul"
  |> add_operator_conflict ~op:"/" ~coq:"div"
  |> add_operator_conflict ~op:"=" ~coq:"eqb"
  |> add_operator_conflict ~op:"<>" ~coq:"neqb"
  |> add_operator_conflict ~op:"<" ~coq:"ltb"
  |> add_operator_conflict ~op:"<=" ~coq:"leb"
  |> add_operator_conflict ~op:">" ~coq:"gtb"
  |> add_operator_conflict ~op:">=" ~coq:"geb"
  |> add_operator_conflict ~op:"&&" ~coq:"andb"
  |> add_operator_conflict ~op:"||" ~coq:"orb"
  |> add_operator_conflict ~op:":=" ~coq:"assign_ref"
  |> add_operator_conflict ~op:"!" ~coq:"deref"
  |> add_operator_conflict ~op:"@" ~coq:"append"
  |> add_operator_conflict ~op:"^" ~coq:"concat"
  |> add_keyword_conflict "as" |> add_keyword_conflict "at"
  |> add_keyword_conflict "cofix"
  |> add_keyword_conflict "else"
  |> add_keyword_conflict "end"
  |> add_keyword_conflict "exists"
  |> add_keyword_conflict "exists2"
  |> add_keyword_conflict "fix" |> add_keyword_conflict "for"
  |> add_keyword_conflict "forall"
  |> add_keyword_conflict "fun" |> add_keyword_conflict "if"
  |> add_keyword_conflict "IF" |> add_keyword_conflict "in"
  |> add_keyword_conflict "let"
  |> add_keyword_conflict "match"
  |> add_keyword_conflict "mod"
  |> add_keyword_conflict "Prop"
  |> add_keyword_conflict "return"
  |> add_keyword_conflict "Set"
  |> add_keyword_conflict "then"
  |> add_keyword_conflict "Type"
  |> add_keyword_conflict "using"
  |> add_keyword_conflict "where"
  |> add_keyword_conflict "with"
  |> add_keyword_conflict "Definition"
  |> add_keyword_conflict "Definitions"
  |> add_keyword_conflict "Variable"
  |> add_keyword_conflict "Variables"
  |> add_keyword_conflict "Parameter"
  |> add_keyword_conflict "Parameters"
  |> add_keyword_conflict "Fixpoint"
  |> add_keyword_conflict "CoFixpoint"
  |> add_keyword_conflict "Inductive"
  |> add_keyword_conflict "CoInductive"
  |> add_keyword_conflict "Lemma"
  |> add_keyword_conflict "Theorem"

let add_keyword kw t =
  let conflict_default = add_keyword_conflict kw t.conflict_default in
  { t with conflict_default }

let add_operator op ~coq t =
  let conflict_default = add_operator_conflict ~op ~coq t.conflict_default in
  { t with conflict_default }

let add kind rev_namespace word ~ocaml ~coq t =
  let key = of_namespace rev_namespace in
  let conflict_table =
    Table.update key
      (function
        | Some c -> Some (add_conflict kind word ~ocaml ~coq c)
        | None -> Some (add_conflict kind word ~ocaml ~coq t.conflict_default))
      t.conflict_table
  in
  { t with conflict_table }

let add_type rev_namespace ty = add Type rev_namespace ty ~ocaml:ty ~coq:ty

let add_value rev_namespace value =
  add Value rev_namespace value ~ocaml:value ~coq:value

let add_field rev_namespace ~owner field =
  add (Field owner) rev_namespace field ~ocaml:field ~coq:field

let add_constructor rev_namespace ~owner cstr =
  add (Constructor owner) rev_namespace cstr ~ocaml:cstr ~coq:cstr

let lookup_operator orig t =
  match Table.find_opt orig t.conflict_default with
  | Some conflicts -> (
      match List.assoc_opt Value conflicts with
      | Some (_, coq) -> coq
      | None -> orig)
  | _ -> orig

let add_helper rev_namespace ?owner orig to_helper t =
  let coq = lookup_operator orig t in
  let owner = Option.value owner ~default:orig in

  add (Helper owner) rev_namespace (to_helper coq) ~ocaml:""
    ~coq:(to_helper coq) t

let default = { conflict_table = Table.empty; conflict_default = default_local }

type ocaml_name = string

type coq_name = string

let of_ocaml_name x = x

let pp_ocaml_name = Format.pp_print_text

let pp_coq_name = Format.pp_print_text

let of_coq_name x = x

let local_find_fresh_coq_name coq conflicts_table =
  let rec select_candidate cds =
    let c = Stream.next cds in
    if Table.mem c conflicts_table then select_candidate cds else c
  in

  select_candidate
    (Stream.from (fun i ->
         if 0 < i then Some (Format.sprintf "%s%d" coq i) else Some coq))

let resolve_coq_conflict kind conflicts conflicts_table =
  let coq = List.assoc kind conflicts |> snd in

  let entries = List.partition (fun (k, _) -> is_helper k) conflicts |> snd in

  match entries with
  | [ (kind', (_, coq)) ] when kind' = kind ->
      (* the conflict is introduced by helpers generated by
           coqffi, we keep the original name *)
      coq
  | _ ->
      (* the conflict is introduced by the OCaml module, we need
           to find a fresh name *)
      let base =
        match kind with
        | Value -> coq ^ "_trm"
        | Field owner | Constructor owner | Helper owner ->
            Format.sprintf "%s_%s" coq owner
        | _ -> assert false
      in

      local_find_fresh_coq_name base conflicts_table

let candidate kind conflicts =
  match conflicts with
  | [ (kind', (_, coq)) ] when kind = kind' -> Some coq
  | _ -> None

(** [resolve_type_conflict rev_namespace t kind coq] returns a
    [coq_name] for [coq] which does not conflict with a type introduce
    in the module identified by [rev_namespace] or any of its parents.

    Preconditions:
     - [kind <> Type] This is enforced because [get_coq], which is the sole
     caller of this function, never modifies the name of a type *)
let rec resolve_coq_type_conflict rev_namespace t kind coq =
  assert (kind <> Type);
  match rev_namespace with
  | [] ->
      (* We have finished to look for potential conflicts and by
         construction [coq] is a fresh [coq_names] *)
      coq
  | _ :: rst ->
      (* [rev_namespace] is not empty, which means we are still inside
         a module hierarchy. We need to (1) check whether [coq] is a
         suitable candidate for this level and find a better suiter if
         needed, and (2) ensure the selected candidate does not
         conflict with the rest of the hierarchy. *)
      let conflicts_table =
        Table.find (of_namespace rev_namespace) t.conflict_table
      in

      let coq =
        match Table.find_opt coq conflicts_table with
        | Some conflicts ->
            (* We only care for types , so we check if there is a
               conflict with our candidate. *)
            if List.mem_assoc Type conflicts then
              (* There is one, we need to find a new candidate. We need
                 to tweak the [conflict] listing, because
                 [resolve_coq_conflict] will not verify if [kind] is
                 present in the list of conflicts, it assumes it is. But
                 in our case, the actual entry related to [coq] is
                 introduced in a module higher in the hierarchy. *)
              resolve_coq_conflict kind
                ((kind, ("", coq)) :: conflicts)
                conflicts_table
            else
              (* No type named [coq] has been declared, so we know
                 [coq] is a suitable candidate for the current level. *)
              coq
        | None -> coq
      in

      (* now [coq] is a suitable name for the current level, we
         can recursively check in the rest of the hierarchy. *)
      resolve_coq_type_conflict rst t kind coq

let get_coq kind rev_namespace t word =
  match kind with
  | Type -> word
  | Keyword -> assert false
  | _ ->
      let conflicts_table =
        Table.find (of_namespace rev_namespace) t.conflict_table
      in

      let conflicts = Table.find word conflicts_table in

      let coq =
        match candidate kind conflicts with
        | Some coq -> coq
        | None -> resolve_coq_conflict kind conflicts conflicts_table
      in

      (* [coq] is a suitable candidate for the current module, we need
         to check if the candidate could conflict with a type (at this
         point, we know [kind <> Type])*)
      resolve_coq_type_conflict (List.tl rev_namespace) t kind coq

let get_ocaml kind rev_namespace t word =
  Table.find (of_namespace rev_namespace) t.conflict_table
  |> Table.find word |> List.assoc kind |> fst

let get_ocaml_type rev_namespace t ~ty = get_ocaml Type rev_namespace t ty

let get_coq_type rev_namespace t ~ty = get_coq Type rev_namespace t ty

let get_ocaml_value rev_namespace t ~value =
  get_ocaml Value rev_namespace t value

let get_coq_value rev_namespace t ~value = get_coq Value rev_namespace t value

let get_ocaml_field rev_namespace t ~owner ~field =
  get_ocaml (Field owner) rev_namespace t field

let get_coq_field rev_namespace t ~owner ~field =
  get_coq (Field owner) rev_namespace t field

let get_ocaml_constructor rev_namespace t ~owner ~cstr =
  get_ocaml (Constructor owner) rev_namespace t cstr

let get_coq_constructor rev_namespace t ~owner ~cstr =
  get_coq (Constructor owner) rev_namespace t cstr

let get_coq_helper rev_namespace t ?owner orig to_helper =
  let coq = lookup_operator orig t in
  let owner = Option.value owner ~default:orig in

  get_coq (Helper owner) rev_namespace t (to_helper coq)

let get_coq_module t ~m = if is_keyword m t then Format.sprintf "%sM" m else m

let qualified_name ns name = String.concat "." (ns @ [ name ])

let unsafe_coq_name x = x
