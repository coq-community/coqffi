module Cstr = struct
  type t = A of int

  type u = A
end

module Fields = struct
  type t = { f : int }

  type u = { f : int }
end

module Type = struct
  type t = int

  let t = 1
end

module Parent = struct
  type t = int

  module Child = struct
    let t = 1
  end
end

let t : Parent.t = 1
