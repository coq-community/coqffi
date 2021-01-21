module T : sig
  type 'a t

  val x : int t
end

type t' = bool T.t
