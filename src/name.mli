(** {1 Synchronous primitives} *)

val prim_monad : string -> string
(** [prim_monad modname] *)

(** {2 [IO] instance} *)

val io_helper : string -> string
(** [io_helper primname] *)

val io_instance : string -> string
(** [io_instance modname] *)

(** {2 [Lwt.t] instance} *)

val lwt_sync_helper : string -> string
(** [lwt_sync_helper modname] *)

val lwt_sync_instance : string -> string
(** [lwt_sync_instance primname] *)

(** {2 [Inject] instance} *)

val interface_type : string -> string

val interface_cstr : string -> string
(** [interface_cstr primname] *)

val inject_helper : string -> string
(** [inject_helper primname] returns the name of the [Inject]-based
    helper for the [primname] primitive. *)

val inject_instance : string -> string
(** [inject_instance modname] returns the name of [Inject] instance
    for primitives of module [modname]. *)

(** {3 [semantics] } *)

val semantics_helper : string -> string
(** [semantics_helper primname] *)

val semantics : string -> string
(** [semantics modname] *)

(** {1 Asynchronous primitives} *)

val async_monad : string -> string
(** [async_monad modname] *)

(** {2 [Lwt.t] instance} *)

val lwt_async_helper : string -> string
(** [lwt_async_helper modname] *)

val lwt_async_instance : string -> string
(** [lwt_async_instance primname] *)

(** {2 [Inject] instance} *)

val async_interface_type : string -> string

val async_inject_instance : string -> string
(** [inject_instance modname] returns the name of [Inject] instance
    for primitives of module [modname]. *)

(** {1 [exn]} *)

val to_exn : string -> string

val of_exn : string -> string

val exn_proxy_type : string -> string

val exn_proxy_cstr : string -> string

val exn_instance : string -> string
