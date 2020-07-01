type ('key, 'value) t

type 'a maybe = Just of 'a | Nothing

type err =
  | ErrVar1 of int maybe
  | ErrVar2
  | ErrVar3 of (int * bool) * bool
  | ErrVar4 of int * (bool * bool)

val add : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t [@@impure]
val singleton : 'k -> 'v -> ('k, 'v) t [@@impure]

val swap : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  [@@coq_model "fun _ _ _ f x y => f y x"]
