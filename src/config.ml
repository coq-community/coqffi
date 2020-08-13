type feature =
  | TransparentTypes
  | Interface

let default _ = false

type features = (feature * bool) list

let feature_name = function
  | TransparentTypes -> "transparent-types"
  | Interface -> "interface"

let find_duplicates : features -> feature list =
  let rec find_dup dups = function
    | (f, _) :: rst ->
      find_dup
        (if List.mem_assoc f rst then (f :: dups) else dups)
        rst
    | [] -> List.sort_uniq compare dups in
  find_dup []

let is_enabled lf f = Option.value ~default:(default f) @@ List.assoc_opt f lf
