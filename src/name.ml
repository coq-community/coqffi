open Format

let ( @> ) f g x = g (f x)

let prim_monad = sprintf "Monad%s"

let io_helper = sprintf "io_%s"

let io_instance = prim_monad @> sprintf "IO_%s"

let lwt_sync_helper = sprintf "lwt_%s"

let lwt_sync_instance = prim_monad @> sprintf "Lwt_%s"

let interface_type = String.uppercase_ascii

let interface_cstr = String.capitalize_ascii

let inject_helper = sprintf "inj_%s"

let inject_instance = prim_monad @> sprintf "Inject_%s"

let semantics_helper = sprintf "sem_%s"

let semantics = String.uncapitalize_ascii @> sprintf "unsafe_%s_semantics"

let async_monad = sprintf "AsyncMonad%s"

let lwt_async_helper = lwt_sync_helper

let lwt_async_instance = async_monad @> sprintf "Lwt_%s"

let async_interface_type = interface_type @> sprintf "ASYNC_%s"

let async_inject_instance = async_monad @> sprintf "Inject_%s"

let to_exn = String.lowercase_ascii @> sprintf "exn_of_%s"

let of_exn = String.lowercase_ascii @> sprintf "%s_of_exn"

let exn_proxy_type = sprintf "%s_exn"

let exn_proxy_cstr = Fun.id

let exn_instance = sprintf "%s_Exn"
