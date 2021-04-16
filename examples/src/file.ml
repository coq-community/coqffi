type fd = Unix.file_descr

let std_out = Unix.stdout

let fd_equal = ( = )

let openfile path = Unix.openfile path [ Unix.O_RDONLY ] 0

let read_all fd =
  let stats = Unix.fstat fd in
  let buff = Bytes.create stats.st_size in
  let _ = Unix.read fd buff 0 stats.st_size in
  Bytes.to_string buff

let write fd str =
  let _ = Unix.write fd (Bytes.of_string str) 0 (String.length str) in
  ()

let closefile = Unix.close
