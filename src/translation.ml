let max_tuple_size = 20

module Table = Map.Make (String)

module Namespace = struct
  type t = string Table.t

  let empty : t = Table.empty

  let translate ~ocaml ~coq = Table.add ocaml coq

  let preserve ocaml = translate ~ocaml ~coq:ocaml

  let find ~ocaml:from = Table.find_opt from
end

type t = Namespace.t Table.t

let empty = Table.empty

let qualname prefix suffix = prefix ^ "." ^ suffix

let rec translate ?(rev_namespace = []) ~ocaml ~coq t =
  let prefix = String.concat "." rev_namespace in
  let t =
    Table.update prefix
      (fun ns ->
        Option.value ~default:Namespace.empty ns
        |> Namespace.translate ~ocaml ~coq
        |> Option.some)
      t
  in
  match rev_namespace with
  | [] -> t
  | x :: rev_namespace ->
      translate ~rev_namespace ~ocaml:(qualname x ocaml) ~coq:(qualname x coq) t

let preserve ?(rev_namespace = []) ocaml =
  translate ~rev_namespace ~ocaml ~coq:ocaml

let rec find ?(rev_namespace = []) ~ocaml t =
  let prefix = String.concat "." rev_namespace in
  match Option.bind (Table.find_opt prefix t) (Namespace.find ~ocaml) with
  | Some coq -> Some coq
  | None -> (
      match rev_namespace with
      | _ :: rev_namespace -> find ~rev_namespace ~ocaml t
      | _ -> None)

let types_table =
  let rec ( -- ) x y = if x < y then x :: (x + 1 -- y) else [ y ] in
  let translate_tuples l ns =
    List.fold_left
      (fun ns x ->
        Namespace.translate
          ~ocaml:(Format.sprintf "Coq_coqffi.Shim.tupl%d" x)
          ~coq:(Format.sprintf "tupl%d" x)
          ns)
      ns l
  in
  let ns =
    Namespace.empty |> Namespace.preserve "bool"
    |> Namespace.translate ~ocaml:"char" ~coq:"ascii"
    |> Namespace.translate ~ocaml:"int" ~coq:"i63"
    |> Namespace.translate ~ocaml:"Stdlib.Seq.t" ~coq:"Seq.t"
    |> Namespace.translate ~ocaml:"Stdlib.result" ~coq:"sum"
    |> Namespace.translate ~ocaml:"int32" ~coq:"i32"
    |> translate_tuples (3 -- max_tuple_size)
    |> Namespace.preserve "float" |> Namespace.preserve "list"
    |> Namespace.preserve "option"
    |> Namespace.preserve "string"
    |> Namespace.preserve "unit" |> Namespace.preserve "exn"
  in
  Table.add "" ns empty
