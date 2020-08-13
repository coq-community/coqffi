From Coq Require Import NArith ZArith Int63 Program.Wf.
From ExtLib Require Import RelDec.

#[local] Open Scope Z_scope.
#[local] Open Scope bool_scope.

(** * Bounds *)

Definition max_uint (p : positive) : Z := 2 ^ (Z.pos p).
Definition min_int (p : positive) : Z := - 2 ^ (Z.pos p - 1).
Definition max_int (p : positive) : Z := 2 ^ (Z.pos p - 1).

(** * Functions *)

Record i63 := mk_i63 { un_i63 : int}.

Register i63 as base.data.i63.type.
Register mk_i63 as base.data.i63.mk_i63.

Declare Scope i63_scope.
Delimit Scope i63_scope with i63.
Bind Scope i63_scope with i63.

Definition irepr (x : Z) : option i63 :=
  if (min_int 63 <=? x)
  then if (x <? 0)
       then Some (mk_i63 (of_Z (x + max_uint 63)))
       else if (x <? max_int 63)
            then Some (mk_i63 (of_Z x))
            else None
  else None.

Definition iproj (x : i63) : Z :=
  let x := to_Z (un_i63 x) in
  if x <? max_int 63
  then x
  else x - max_uint 63.

Numeral Notation i63 irepr iproj : i63_scope.

Definition ibinop (op : int -> int -> int) (x y : i63) : i63 :=
  mk_i63 (op (un_i63 x) (un_i63 y)).

Definition i63add : i63 -> i63 -> i63 := ibinop Int63.add.
Definition i63mul : i63 -> i63 -> i63 := ibinop Int63.mul.
Definition i63sub : i63 -> i63 -> i63 := ibinop Int63.sub.
Definition i63div : i63 -> i63 -> i63 := ibinop Int63.div.
Definition i63lxor : i63 -> i63 -> i63 := ibinop Int63.lxor.

Infix "+" := i63add : i63_scope.
Infix "-" := i63sub : i63_scope.
Infix "*" := i63mul : i63_scope.
Infix "/" := i63div : i63_scope.

Definition i63eqb (x y : i63) : bool :=
  eqb (un_i63 x) (un_i63 y).

Instance i63_RelDec : @RelDec i63 eq :=
  { rel_dec := i63eqb }.

#[refine]
Instance i63_RelDec_Correct : RelDec_Correct i63_RelDec := {}.

Proof.
  intros [x] [y].
  cbv.
  split; intros equ.
  + apply eqb_correct in equ.
    now rewrite equ.
  + inversion equ; subst.
    apply eqb_refl.
Qed.

Definition i63le (x y : i63) : Prop := iproj x <= iproj y.
Definition i63lt (x y : i63) : Prop := iproj x < iproj y.

Infix "<" := i63lt : i63_scope.
Infix "<=" := i63le : i63_scope.

Definition max_i63 : i63 := mk_i63 (of_Z (max_int 63 - 1)).
Definition min_i63 : i63 := mk_i63 (of_Z (min_int 63)).

Definition i63ltb (x y : i63) : bool :=
  let max_int := un_i63 max_i63 in
  let x := un_i63 x in
  let y := un_i63 y in
  if ((x < max_int)%int63 && (y < max_int)%int63)
     || ((max_int < x)%int63 && (max_int < y)%int63)
  then (x < y)%int63
  else if (max_int < x)%int63
  then true
  else false.

Definition i63leb (x y : i63) : bool :=
  (i63eqb x y) || i63ltb x y.

Infix "<?" := i63ltb : i63_scope.
Infix "<=?" := i63leb : i63_scope.

Definition i63_mod (x y : i63) : i63 :=
  mk_i63 ((un_i63 x) \% (un_i63 y))%int63.

Infix "mod" := i63_mod : i63_scope.

#[local] Close Scope Z_scope.
#[local] Open Scope i63_scope.

Definition abs (x : i63) : i63 :=
  if x <? 0
  then -1 * x
  else x.

Definition abs_nat (x : i63) : nat :=
  Z.to_nat (Int63.to_Z (un_i63 (abs x))).

Axioms
  (le_max_int : forall (x : i63), x <= max_i63)
  (le_min_int : forall (x : i63), min_i63 <= x)

  (le_trans : forall (x y z : i63), x <= y -> y <= z -> x <= z)
  (le_leb_equiv : forall (x y : i63), x <=? y = true <-> x <= y)
  (lt_ltb_equiv : forall (x y : i63), x <? y = true <-> x < y)

  (neg_lt_le : forall (x y : i63), x <? y = false <-> y <=? x = true)

  (le_i63sub_pos : forall (x y : i63),
      0 < x -> i63add min_i63 x <= y -> i63sub y x < y)

  (abs_nat_lt_inj : forall (x : i63),
      0 < x -> (0 < abs_nat x)%nat)
  (abs_nat_div_inj : forall (x y : i63),
      abs_nat (x / y) = Nat.div (abs_nat x) (abs_nat y)).

Axiom i63_well_founded_lt : well_founded i63lt.

(** * Extraction *)

Module IntExtraction.
  Extract Inlined Constant int => int.
  Extract Inductive i63 => int [ "" ].

  Extract Inlined Constant abs => "abs".
  Extract Inlined Constant i63add => "(+)".
  Extract Inlined Constant i63sub => "(-)".
  Extract Inlined Constant i63mul => "(*)".
  Extract Inlined Constant i63div => "(/)".
  Extract Inlined Constant i63eqb => "(=)".
  Extract Inlined Constant i63ltb => "(<)".
  Extract Inlined Constant i63leb => "(<=)".
End IntExtraction.
