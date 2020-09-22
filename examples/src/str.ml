let get s idx =
  try
    Some (Stdlib.String.get s idx)
  with _ ->
    None
