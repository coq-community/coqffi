open Coqbase

type fd

val fd_equal : fd -> fd -> bool

val openfile : Bytestring.t -> fd [@@impure]
val read_all : fd -> Bytestring.t [@@impure]
val write : fd -> Bytestring.t -> unit [@@impure]
val closefile : fd -> unit [@@impure]
