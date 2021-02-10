type t =
  | TransparentTypes
  | PureModule
  | Interface
  | SimpleIO
  | FreeSpec
  | Lwt
  | Witness

type feature = t

exception FreeSpecRequiresInterface

type features = (feature * bool) list

let name = function
  | TransparentTypes -> "transparent-types"
  | PureModule -> "pure-module"
  | Interface -> "interface"
  | SimpleIO -> "simple-io"
  | FreeSpec -> "freespec"
  | Lwt -> "lwt"
  | Witness -> "witness"

let find_duplicates : features -> feature list =
  let rec find_dup dups = function
    | (f, _) :: rst ->
      find_dup
        (if List.mem_assoc f rst then (f :: dups) else dups)
        rst
    | [] -> List.sort_uniq compare dups in
  find_dup []

let is_set lf f = List.assoc_opt f lf

let rec is_enabled lf f =
  Option.value ~default:(default lf f) @@ List.assoc_opt f lf
and default lf = function
  | SimpleIO -> true
  | Interface -> is_enabled lf FreeSpec
  | _ -> false

let is_disabled lf f = not (is_enabled lf f)

let support_impure_values lf = is_enabled lf SimpleIO || is_enabled lf FreeSpec

exception LwtExplicitelyDisableButLwtAliasSet

let check_features_consistency lwt_alias lf ~wduplicate =
  let open Format in

  let warning fmt feature =
    fprintf fmt
      "Warning: Feature `%s' has been selected several times.@ "
      (name feature) in

  if wduplicate
  then fprintf err_formatter "%a@?" (pp_print_list warning) (find_duplicates lf);

  if is_enabled lf FreeSpec && is_disabled lf Interface
  then raise FreeSpecRequiresInterface;

  match lwt_alias, is_set lf Lwt with
  | Some lwt_alias, Some true -> (Some lwt_alias, lf)
  | Some _, Some false -> raise LwtExplicitelyDisableButLwtAliasSet
  | Some lwt_alias, None -> (Some lwt_alias, (Lwt, true) :: lf)
  | None, Some true -> (Some "Lwt.t", lf)
  | _, _ -> (None, lf)
