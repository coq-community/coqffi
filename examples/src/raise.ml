let unwrap = function
  | Some x -> x
  | None -> raise (Invalid_argument "unwrap: Expected [Some], found [None]")

let print = print_string
