open Coqbase

type fd

type file_flags =
  | O_RDONLY
  | O_WRONLY
  | O_RDWR

val openfile : Bytestring.t -> file_flags list -> fd [@@impure]
val read_all : fd -> Bytestring.t [@@impure]
val write : fd -> Bytestring.t -> unit [@@impure]
val closefile : fd -> unit [@@impure]

val fd_equal : fd -> fd -> bool
