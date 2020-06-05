open Coqbase

val echo : Bytestring.t -> unit

val scan : unit -> Bytestring.t

val head : 'a list -> 'a option
[@@ffi_pure] [@@coq_model "Coq.Lists.List.hd_error"]

val tail : 'a list -> 'a list
[@@ffi_pure]
