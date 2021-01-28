let concat_map = List.concat_map
let find_map = List.find_map
let fold_left_map = List.fold_left_map
let rec sorted_intersect cmp l1 l2 =
  match (l1, l2) with
  | (x1 :: rst1, x2 :: rst2) ->
     (match cmp x1 x2 with
      | n when n = 0 -> true
      | n when n < 0 -> sorted_intersect cmp rst1 (x2 :: rst2)
      | _ -> sorted_intersect cmp (x1 :: rst1) rst2)
  | _ -> false
