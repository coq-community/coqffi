open Coqbase

type fd = Unix.file_descr

type file_flags =
  | O_RDONLY
  | O_WRONLY
  | O_RDWR

let openfile path _ = Unix.openfile (Bytestring.to_string path) [Unix.O_RDONLY] 0
let read_all _ = Bytestring.of_string "to be implemented"
let write _ _ = ()
let closefile = Unix.close

let fd_equal _ _ = false
