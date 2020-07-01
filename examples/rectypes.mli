type even =
  | Zero
  | ESucc of odd
and odd = OSucc of even
