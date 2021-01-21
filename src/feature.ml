type t =
  | TransparentTypes
  | PureModule
  | Interface
  | SimpleIO
  | FreeSpec

type feature = t

exception FreeSpecRequiresInterface

type features = (feature * bool) list

let name = function
  | TransparentTypes -> "transparent-types"
  | PureModule -> "pure-module"
  | Interface -> "interface"
  | SimpleIO -> "simple-io"
  | FreeSpec -> "freespec"

let find_duplicates : features -> feature list =
  let rec find_dup dups = function
    | (f, _) :: rst ->
      find_dup
        (if List.mem_assoc f rst then (f :: dups) else dups)
        rst
    | [] -> List.sort_uniq compare dups in
  find_dup []

let rec is_enabled lf f =
  Option.value ~default:(default lf f) @@ List.assoc_opt f lf
and default lf = function
  | SimpleIO -> true
  | Interface -> is_enabled lf FreeSpec
  | _ -> false

let is_disabled lf f = not (is_enabled lf f)

let support_impure_values lf = is_enabled lf SimpleIO || is_enabled lf FreeSpec

let check_features_consistency lf =
  if is_enabled lf FreeSpec && is_disabled lf Interface
  then raise FreeSpecRequiresInterface
