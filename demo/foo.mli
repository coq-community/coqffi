type ('key, 'value) t

val add : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t
val singleton : 'k -> 'v -> ('k, 'v) t

val swap : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  [@@coq_model "fun _ _ _ f x y => f y x"] [@@ffi_pure]
