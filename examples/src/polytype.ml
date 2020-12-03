type 'a llist =
  | LCons of 'a * (unit -> 'a llist)
  | LNil
