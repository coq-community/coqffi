open Coqbase

type fd

val openfile : Bytestring.t -> fd [@@impure]
val read_all : fd -> Bytestring.t [@@impure]
val write : fd -> Bytestring.t -> unit [@@impure]
val closefile : fd -> unit [@@impure]

val fd_equal : fd -> fd -> bool

val swap : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  [@@coq_model "fun _ _ _ f x y => f y x"]
