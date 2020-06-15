open Coqbase

type fd

val openfile : Bytestring.t -> fd
val read_all : fd -> Bytestring.t
val write : fd -> Bytestring.t -> unit
val closefile : fd -> unit

val fd_equal : fd -> fd -> bool
  [@@ffi_pure]

val swap : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  [@@ffi_pure] [@@coq_model "fun _ _ _ f x y => f y x"]
