module Table = Map.Make(String)

type t = string Table.t

let empty = Table.empty

let translate ~ocaml:tml ~coq:tcoq t = Table.add tml tcoq t

let preserve typ t = translate t ~ocaml:typ ~coq:typ

let find tbl ~ocaml:from = Table.find_opt from tbl

let types_table =
  empty
  |> preserve "bool"
  |> translate ~ocaml:"char" ~coq:"ascii"
  |> translate ~ocaml:"int" ~coq:"i63"
  |> translate ~ocaml:"Stdlib.Seq.t" ~coq:"Seq.t"
  |> preserve "list"
  |> preserve "option"
  |> preserve "string"
  |> preserve "unit"
