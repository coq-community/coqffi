type 'a llist =
  | LCons of 'a * (unit -> 'a llist)
  | LNil

type box = Box : 'a -> box

type (_, 'a) gadt = Gadt : int * 'a -> (int, 'a) gadt

type _ m =
  | M1 : box m
  | M2 : (int, box llist) gadt m
