type fd

val std_out : fd
val fd_equal : fd -> fd -> bool [@@pure]

val openfile : string -> fd
val closefile : fd -> unit
val read_all : fd -> string
val write : fd -> string -> unit
