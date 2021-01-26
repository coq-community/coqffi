module T : sig
  type 'a t

  val x : int t
end

type t' = bool T.t

module T' : sig
  val y : int T.t
end
