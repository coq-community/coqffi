Class _OfProd a b :=
  { of_prod : a -> b
  ; to_prod : b -> a
  }.

Instance _OfProd_refl a : _OfProd a a :=
  { of_prod := fun x => x
  ; to_prod := fun x => x
  }.

Declare Scope tup_scope.

Notation "( x , y , .. , z )" := (of_prod (pair .. (pair x y) .. z)) (only parsing) : tup_scope.

Inductive tup3 (a1 a2 a3 : Type) :=
| mktup3 (x1 : a1) (x2 : a2) (x3 : a3) : tup3 a1 a2 a3.

Arguments mktup3 [a1 a2 a3] (x1 x2 x3).

Instance tuple3_of_prod {a1 a2 a3} : _OfProd (a1 * a2 * a3) (tup3 a1 a2 a3) :=
  { of_prod := fun '((x, y), z) => mktup3 x y z
  ; to_prod := fun '(mktup3 x y z) => ((x, y), z)
  }.

Inductive tup4 (a1 a2 a3 a4 : Type) :=
| mktup4 (x1 : a1) (x2 : a2) (x3 : a3) (x4 : a4) : tup4 a1 a2 a3 a4.

Arguments mktup4 [a1 a2 a3 a4] (x1 x2 x3 x4).

Instance tuple4_of_prod {a1 a2 a3 a4} : _OfProd (a1 * a2 * a3 * a4) (tup4 a1 a2 a3 a4) :=
  { of_prod := fun '(((x1, x2), x3), x4) => mktup4 x1 x2 x3 x4
  ; to_prod := fun '(mktup4 x1 x2 x3 x4) => (((x1, x2), x3), x4)
  }.

Inductive tup5 (a1 a2 a3 a4 a5 : Type) :=
| mktup5 (x1 : a1) (x2 : a2) (x3 : a3) (x4 : a4) (x5 : a5) : tup5 a1 a2 a3 a4 a5.

Arguments mktup5 [a1 a2 a3 a4 a5] (x1 x2 x3 x4 x5).

Instance tuple5_of_prod {a1 a2 a3 a4 a5} : _OfProd (a1 * a2 * a3 * a4 * a5) (tup5 a1 a2 a3 a4 a5) :=
  { of_prod := fun '((((x1, x2), x3), x4), x5) => mktup5 x1 x2 x3 x4 x5
  ; to_prod := fun '(mktup5 x1 x2 x3 x4 x5) => ((((x1, x2), x3), x4), x5)
  }.

From Coq Require Extraction.

Module TupleExtraction.
  Extract Inductive tup3 => "Coq_coqffi.Shim.tup3" [ "" ].

  Extract Inductive tup4 => "Coq_coqffi.Shim.tup4" [ "" ].

  Extract Inductive tup5 => "Coq_coqffi.Shim.tup5" [ "" ].
End TupleExtraction.
