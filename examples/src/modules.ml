module T = struct
  type 'a t = 'a option

  let x : int t = Some 2
end

type t' = bool T.t

module T' = struct
  let y : int T.t = T.x
end
