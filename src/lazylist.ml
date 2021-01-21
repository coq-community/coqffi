open Format

type 'a t =
  | LSeg of 'a * 'a list * 'a t Lazy.t
  | LNil

let empty = LNil

let push_list = function
  | [] -> fun l -> l
  | x :: r ->
    let rec push_segment = function
      | LSeg (y, r', lr) -> LSeg (y, r', lazy (push_segment @@ Lazy.force lr))
      | _ -> LSeg (x, r, lazy LNil)
    in push_segment

let push x = push_list [x]

let of_list = function
  | [] -> LNil
  | x :: rst -> LSeg (x, rst, lazy LNil)

let singleton x = LSeg (x, [], lazy LNil)

let (|+) ll v = push v ll
let (|++) ll lv = push_list lv ll

let unpack = function
  | LNil -> None
  | LSeg (x, [], lr) -> Some ((x, Lazy.force lr))
  | LSeg (x, y :: rst, lr) -> Some ((x, LSeg (y, rst, lr)))

let pp_print_lazylist ~pp_sep f fmt ll =
  let prod_iter f g = fun (x, y) -> f x; g y in
  let rec aux = function
    | LNil -> ()
    | LSeg (x, l, rst) -> begin
        pp_sep fmt ();
        pp_print_list ~pp_sep f fmt (x :: l);
        aux (Lazy.force rst)
      end in
  Option.iter (prod_iter (f fmt) aux) @@ unpack ll
