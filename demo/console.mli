open Coqbase

val echo : Bytestring.t -> unit

val scan : unit -> Bytestring.t

val check : ('a -> 'b) -> int
[@@ffi_pure] [@@coq_model "Data.Test.check"]

val uncheck : 'a -> int
[@@ffi_pure]
