type (_, _) result = Ok | Error

val cannot_infer : (unit, 'trace) result Lwt.t
