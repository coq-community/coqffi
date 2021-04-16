let sync_computation x = x

let async_computation x = Lwt.return x

let bind = Lwt.bind

let join = Lwt.return (Lwt.return 2)
