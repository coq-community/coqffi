exception LeftSideFailed of exn

exception RightSideFailed of exn

exception BothSideFailed of exn * exn

let ( || ) f g x =
  let try_or f res x =
    try res := Some (Ok (f x)) with e -> res := Some (Error e)
  in

  let res_f = ref None in
  let res_g = ref None in

  let pid_f = Thread.create (try_or f res_f) x in
  let pid_g = Thread.create (try_or g res_g) x in

  Thread.join pid_f;
  Thread.join pid_g;

  match (!res_f, !res_g) with
  | Some (Ok x), Some (Ok y) -> (x, y)
  | Some (Error err), Some (Error err') -> raise (BothSideFailed (err, err'))
  | Some (Error err), _ -> raise (LeftSideFailed err)
  | _, Some (Error err) -> raise (RightSideFailed err)
  | _, _ -> assert false

let qed _ = ()

let ( @? ) x f = match x with Some x -> fun y -> qed (f x y) | _ -> qed

let ( @> ) f g x = g (f x)
