type t = { foo : bool; bar : int }

type u = U of t

type 'a v = { foobar : 'a; moo : t }

type w = W of { noo : int }

module M : sig
  type t = { f1 : int; f2 : int }
end
