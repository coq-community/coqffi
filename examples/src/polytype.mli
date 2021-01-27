type 'a llist =
  | LCons of 'a * (unit -> 'a llist)
  | LNil

type box = Box : 'a -> box
