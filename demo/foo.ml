type ('k, 'v) t = unit

type 'a maybe = Just of 'a | Nothing

type err =
  | ErrVar1 of int maybe
  | ErrVar2
  | ErrVar3 of (int * bool) * bool
  | ErrVar4 of int * (bool * bool)

let add _ _ _ = assert false
let singleton _ _ = assert false

let swap f x y = f y x
