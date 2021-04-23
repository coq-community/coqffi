let test ?(fallback = "test") _ = fallback

let value x ~default = match x with Some x -> x | _ -> default

let valueexn x ~default = match x with Some x -> x | _ -> default
