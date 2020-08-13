type fd

val std_out : fd

val openfile : string -> fd [@@impure]
val closefile : fd -> unit [@@impure]
val read_all : fd -> string [@@impure]
val write : fd -> string -> unit [@@impure]
