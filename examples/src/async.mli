val sync_computation : 'a -> 'a

val async_computation : 'a -> 'a Lwt.t

val bind : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

val join : int Lwt.t Lwt.t
