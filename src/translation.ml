open Config

module Table = Map.Make(String)

type t = string Table.t

let empty = Table.empty

let translate ~ocaml:tml ~coq:tcoq t = Table.add tml tcoq t

let preserve typ t = translate t ~ocaml:typ ~coq:typ

let find tbl ~ocaml:from = Table.find_opt from tbl

let coqbase_types_table =
  empty
  |> preserve "list"
  |> preserve "bool"
  |> preserve "option"
  |> preserve "unit"
  |> translate ~ocaml:"int" ~coq:"i63"
  |> translate ~ocaml:"Coqbase.Bytestring.t" ~coq:"bytestring"
  |> translate ~ocaml:"char" ~coq:"byte"
  |> translate ~ocaml:"Coqbase.Sum.t" ~coq:"sum"

let stdlib_types_table =
  empty
  |> preserve "list"
  |> preserve "bool"
  |> preserve "option"
  |> preserve "unit"
  |> translate ~ocaml:"string" ~coq:"string"
  |> translate ~ocaml:"char" ~coq:"ascii"

let types_table = function
  | Coqbase -> coqbase_types_table
  | Stdlib -> stdlib_types_table
