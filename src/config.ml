type feature =
  | TransparentTypes
  | Interface
  | SimpleIO

let default = function
  | SimpleIO -> true
  | _ -> false

type features = (feature * bool) list

let feature_name = function
  | TransparentTypes -> "transparent-types"
  | Interface -> "interface"
  | SimpleIO -> "simple-io"

let find_duplicates : features -> feature list =
  let rec find_dup dups = function
    | (f, _) :: rst ->
      find_dup
        (if List.mem_assoc f rst then (f :: dups) else dups)
        rst
    | [] -> List.sort_uniq compare dups in
  find_dup []

let is_enabled lf f = Option.value ~default:(default f) @@ List.assoc_opt f lf

let support_impure_values lf =
  is_enabled lf SimpleIO
