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

let add_alias ~ocaml ~coq tbl =
  Table.add ocaml {
      alias_coq = coq;
      alias_operator = false;
    } tbl

let add_keyword ~coq tbl =
  add_alias ~ocaml:coq ~coq:(coq ^ "_") tbl

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
  |> add_operator ~ocaml:"&&" ~coq:"andb"
  |> add_operator ~ocaml:"||" ~coq:"orb"
  |> add_operator ~ocaml:":=" ~coq:"assign_ref"
  |> add_operator ~ocaml:"!" ~coq:"deref"
  |> add_operator ~ocaml:"@" ~coq:"append"
  |> add_operator ~ocaml:"^" ~coq:"concat"
  |> add_alias ~ocaml:"_" ~coq:"___"
  |> add_keyword ~coq:"as"
  |> add_keyword ~coq:"at"
  |> add_keyword ~coq:"cofix"
  |> add_keyword ~coq:"else"
  |> add_keyword ~coq:"end"
  |> add_keyword ~coq:"exists"
  |> add_keyword ~coq:"exists2"
  |> add_keyword ~coq:"fix"
  |> add_keyword ~coq:"for"
  |> add_keyword ~coq:"forall"
  |> add_keyword ~coq:"fun"
  |> add_keyword ~coq:"if"
  |> add_keyword ~coq:"IF"
  |> add_keyword ~coq:"in"
  |> add_keyword ~coq:"let"
  |> add_keyword ~coq:"match"
  |> add_keyword ~coq:"mod"
  |> add_keyword ~coq:"Prop"
  |> add_keyword ~coq:"return"
  |> add_keyword ~coq:"Set"
  |> add_keyword ~coq:"then"
  |> add_keyword ~coq:"Type"
  |> add_keyword ~coq:"using"
  |> add_keyword ~coq:"where"
  |> add_keyword ~coq:"with"
  |> add_keyword ~coq:"Definition"
  |> add_keyword ~coq:"Definitions"
  |> add_keyword ~coq:"Variable"
  |> add_keyword ~coq:"Variables"
  |> add_keyword ~coq:"Parameter"
  |> add_keyword ~coq:"Parameters"
  |> add_keyword ~coq:"Fixpoint"
  |> add_keyword ~coq:"CoFixpoint"
  |> add_keyword ~coq:"Inductive"
  |> add_keyword ~coq:"CoInductive"
  |> add_keyword ~coq:"Lemma"
  |> add_keyword ~coq:"Theorem"

let ocaml_name t orig =
  match Table.find_opt orig t with
  | Some { alias_coq = _; alias_operator = true } -> Format.sprintf "( %s )" orig
  | _ -> orig

let coq_name t orig =
  match Table.find_opt orig t with
  | Some alias -> alias.alias_coq
  | _ -> orig
