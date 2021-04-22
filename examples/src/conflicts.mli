module Cstr : sig
  type t = A of int

  type u = A
end

module Fields : sig
  type t = { f : int }

  type u = { f : int }
end

module Type : sig
  type t

  val t : t
end

module Parent : sig
  type t

  module Child : sig
    val t : t
  end
end

val t : Parent.t

module Kw : sig
  val set : int -> unit

  type t = Set
end
