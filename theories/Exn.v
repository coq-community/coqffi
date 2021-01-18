From Coq Require Extraction.

Axiom exn : Type.

Class Exn (e : Type) := { to_exn : e -> exn
                        ; of_exn : exn -> option e
                        }.

Module ExnExtraction.
  Extract Inlined Constant exn => "exn".
End ExnExtraction.
