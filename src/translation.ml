open Config

module Table = Map.Make(String)

type t = string Table.t

let empty = Table.empty

let add = Table.add

let find tbl from = Table.find_opt from tbl

let coqbase_types_table =
  empty
  |> add "list" "list"
  |> add "bool" "bool"
  |> add "option" "option"
  |> add "unit" "unit"
  |> add "int" "i63"
  |> add "Coqbase.Bytestring.t" "bytestring"
  |> add "Coqbase.Sum.t" "sum"

let stdlib_types_table =
  empty
  |> add "list" "list"
  |> add "bool" "bool"
  |> add "option" "option"
  |> add "unit" "unit"

let types_table = function
  | Coqbase -> coqbase_types_table
  | Stdlib -> stdlib_types_table
