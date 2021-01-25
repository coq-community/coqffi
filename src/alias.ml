type t = {
    alias_coq : string;
    alias_operator : bool;
  }

module Table = Map.Make(String)

type table = t Table.t

let add_operator ~ocaml ~coq tbl =
  Table.add ocaml {
      alias_coq = coq ;
      alias_operator = true;
    } tbl

let add_keyword ~ocaml ~coq tbl =
  Table.add ocaml {
      alias_coq = coq ;
      alias_operator = false;
    } tbl

let default =
  Table.empty
  |> add_operator ~ocaml:"+" ~coq:"add"
  |> add_operator ~ocaml:"-" ~coq:"sub"
  |> add_operator ~ocaml:"*" ~coq:"mul"
  |> add_operator ~ocaml:"/" ~coq:"div"
  |> add_operator ~ocaml:"=" ~coq:"eqb"
  |> add_operator ~ocaml:"<>" ~coq:"neqb"
  |> add_operator ~ocaml:"<" ~coq:"ltb"
  |> add_operator ~ocaml:"<=" ~coq:"leb"
  |> add_operator ~ocaml:">" ~coq:"gtb"
  |> add_operator ~ocaml:">=" ~coq:"geb"
  |> add_keyword ~ocaml:"return" ~coq:"ret"
  |> add_keyword ~ocaml:"exists" ~coq:"exist"

let ocaml_name t orig =
  match Table.find_opt orig t with
  | Some { alias_coq = _; alias_operator = true } -> Format.sprintf "(%s)" orig
  | _ -> orig

let coq_name t orig =
  match Table.find_opt orig t with
  | Some alias -> alias.alias_coq
  | _ -> orig
