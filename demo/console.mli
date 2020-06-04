open Coqbase

val echo : Bytestring.t -> unit

val scan : unit -> Bytestring.t

val check : unit -> int
[@@ffi_pure]
[@@coq_model "Data.Test.check"]

val uncheck : unit -> int
[@@ffi_pure]
