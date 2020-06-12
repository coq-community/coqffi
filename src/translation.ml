module Table = Map.Make(String)

type t = string Table.t

let empty = Table.empty

let add = Table.add

let find tbl from = Table.find_opt from tbl
