From Coq Require Import RelationClasses ZArith Int63 Lia.
From CoqFFI Require Export Int.

Create HintDb i63.
Open Scope i63_scope.

(** * Comparisons *)

Axioms
  (* FIXME: https://github.com/coq/coq/pull/13262 *)
  (int_max_int : forall (x : int), (x <= Int63.max_int = true)%int63)
  (i63le_max_int : forall (x : i63), x <= max_i63)
  (i63le_min_int : forall (x : i63), min_i63 <= x).

Hint Resolve i63le_max_int : i63.
Hint Resolve i63le_min_int : i63.

#[program]
Instance i63_le_Transitive : Transitive i63le.

Next Obligation.
  unfold i63le.
  transitivity (to_Z y).
  + exact H.
  + exact H0.
Qed.

#[program]
Instance i63_lt_Transitive : Transitive i63lt.

Next Obligation.
  unfold i63lt.
  transitivity (to_Z y).
  + exact H.
  + exact H0.
Qed.

#[program]
Instance i63_le_Reflexive : Reflexive i63le.

Next Obligation.
  unfold i63le.
  reflexivity.
Qed.

Axioms
  (i63le_leb_true_equiv : forall (x y : i63), x <=? y = true <-> x <= y)
  (i63lt_ltb_true_equiv : forall (x y : i63), x <? y = true <-> x < y)
  (i63le_leb_false_equiv : forall (x y : i63), x <=? y = false <-> ~ x <= y)
  (i63lt_ltb_false_equiv : forall (x y : i63), x <? y = false <-> ~ x < y)
  (i63eq_eqb_true_equiv : forall (x y : i63), i63eqb x y = true <-> x = y)
  (i63eq_eqb_false_equiv : forall (x y : i63), i63eqb x y = false <-> ~ x = y).

Lemma un_i63_bound (x : i63) : (to_Z x < max_int 63)%Z.

Proof.
  unfold to_Z.
  case_eq (φ (un_i63 x)%int63 <? max_int 63)%Z; intros equ; try rewrite equ in *.
  + now apply Zlt_is_lt_bool.
  + apply Z.ltb_ge in equ.
    assert (max_int 63 < max_uint 63)%Z by now intros.
    assert (0 < max_int 63)%Z by now intros.
    assert ((φ (un_i63 x))%int63 <= (φ Int63.max_int)%int63)%Z. {
      assert (((φ (un_i63 x))%int63 <= (φ (Int63.max_int))%int63)%Z
              <-> Int63.leb (un_i63 x) (Int63.max_int) = true).
      apply Bool.reflect_iff.
      apply lebP.
      apply H1.
      apply int_max_int.
    }
    assert ((φ Int63.max_int)%int63 < max_uint 63)%Z by now intros.
    lia.
Qed.

Lemma i63neg_le_lt (x y : i63) : ~ (x <= y) <-> y < x.

Proof.
  unfold i63lt, i63le.
  lia.
Qed.

Lemma i63leb_false_ltb (x y : i63) : x <=? y = false <-> y <? x = true.

Proof.
  rewrite i63le_leb_false_equiv.
  rewrite i63lt_ltb_true_equiv.
  apply i63neg_le_lt.
Qed.

Lemma i63neg_lt_le (x y : i63) : ~ (x < y) <-> y <= x.

Proof.
  unfold i63lt, i63le.
  lia.
Qed.

Lemma i63ltb_false_leb (x y : i63) : x <? y = false <-> y <=? x = true.

Proof.
  rewrite i63lt_ltb_false_equiv.
  rewrite i63le_leb_true_equiv.
  apply i63neg_lt_le.
Qed.

Lemma i63lt_le_trans (x y z : i63) : x < y -> y <= z -> x < z.

Proof.
  unfold i63le, i63lt in *.
  lia.
Qed.

Lemma i63le_lt_trans (x y z : i63) : x <= y -> y < z -> x < z.

Proof.
  unfold i63le, i63lt in *.
  lia.
Qed.

Lemma i63lt_le (x y : i63) : x < y -> x <= y.

Proof.
  unfold i63le, i63lt.
  lia.
Qed.

Axiom i63_well_founded_lt : well_founded i63lt.

(** * Arithmetic *)

Lemma i63add_neutral (x : i63): 0 + x = x.

Proof.
  apply i63eq_eqb_true_equiv.
  unfold i63eqb.
  cbn.
  assert (((φ (0 + un_i63 x))%int63 = (φ (un_i63 x))%int63)%Z
          <-> Int63.eqb (Int63.add 0 (un_i63 x)) (un_i63 x) = true).
  apply Bool.reflect_iff.
  apply eqbP.
  apply H.
  rewrite add_spec.
  change ((φ 0)%int63 + (φ (un_i63 x))%int63)%Z with (φ (un_i63 x))%int63.
  apply Zmod_small.
  apply to_Z_bounded.
Qed.

Lemma i63add_assoc (x y z : i63) : x + (y + z) = (x + y) + z.

Proof.
  unfold i63add, ibinop.
  apply i63eq_eqb_true_equiv.
  cbn.
  rewrite add_assoc.
  apply eqb_refl.
Qed.

Lemma i63add_comm (x y : i63) : x + y = y + x.

Proof.
  unfold i63add, ibinop.
  apply i63eq_eqb_true_equiv.
  cbn.
  rewrite add_comm.
  apply eqb_refl.
Qed.

Lemma i63mul_neutral (x : i63) : 1 * x = x.

Proof.
  apply i63eq_eqb_true_equiv.
  unfold i63eqb.
  cbn.
  assert (((φ (1 * un_i63 x))%int63 = (φ (un_i63 x))%int63)%Z
          <-> Int63.eqb (Int63.mul 1 (un_i63 x)) (un_i63 x) = true).
  apply Bool.reflect_iff.
  apply eqbP.
  apply H.
  rewrite mul_spec.
  change (φ (1)%int63) with 1%Z.
  rewrite Z.mul_1_l.
  apply Zmod_small.
  apply to_Z_bounded.
Qed.

Lemma i63_mul_assoc (x y z : i63) : x * (y * z) = (x * y) * z.

Proof.
  apply i63eq_eqb_true_equiv.
  unfold i63eqb.
  cbn.
  generalize (un_i63 x) (un_i63 y) (un_i63 z).
  clear x y z.
  intros a b c.
  assert (((φ (a * (b * c)))%int63 = (φ (a * b * c))%int63)%Z
          <-> Int63.eqb (a * (b * c))%int63 (a * b * c)%int63 = true).
  apply Bool.reflect_iff.
  apply eqbP.
  apply H.
  repeat rewrite mul_spec.
  rewrite (Z.mul_comm (((φ a)%int63 * (φ b)%int63) mod wB) _).
  repeat rewrite Zmult_mod_idemp_r.
  rewrite (Z.mul_comm (φ c)%int63 _).
  now rewrite Z.mul_assoc.
Qed.

Lemma i63mul_comm (x y : i63) : x * y = y * x.

Proof.
  apply i63eq_eqb_true_equiv.
  unfold i63eqb.
  cbn.
  generalize (un_i63 x) (un_i63 y).
  clear x y; intros x y.
  assert ((φ (x * y)%int63 = (φ (y * x))%int63)%Z
          <-> Int63.eqb (Int63.mul x y) (Int63.mul y x) = true).
  apply Bool.reflect_iff.
  apply eqbP.
  apply H.
  repeat rewrite mul_spec.
  now rewrite Z.mul_comm.
Qed.

Definition i63add_safe (x y : i63) : Prop :=
  to_Z (x + y) = (to_Z x + to_Z y)%Z.

Definition i63sub_safe (x y : i63) : Prop :=
  to_Z (x - y) = (to_Z x - to_Z y)%Z.

Lemma i63add_safe_le (x y : i63) (safe : i63add_safe x y) (ypos : 0 <= y)
  : x <= x + y.

Proof.
  unfold i63lt, i63le in *.
  rewrite safe.
  change (to_Z 0) with 0%Z in ypos.
  lia.
Qed.

Lemma i63add_safe_lt (x y : i63) (safe : i63add_safe x y) (ypos : 0 < y)
  : x < x + y.

Proof.
  unfold i63lt, i63le in *.
  rewrite safe.
  change (to_Z 0) with 0%Z in ypos.
  lia.
Qed.

Axioms
  (i63sub_max_safe : forall (x : i63) (xpos : 0 <= x),
      i63sub_safe max_i63 x)
  (i63add_min_safe : forall (x : i63) (xpos : 0 <= x),
      i63add_safe min_i63 x)
  (i63sub_unwrap : forall (x y : i63) (ypos : 0 <= y),
      min_i63 + y <= x <-> i63sub_safe x y)
  (i63add_unwrap : forall (x y : i63) (ypos : 0 <= y),
      x <= max_i63 - y <-> i63add_safe x y)
  (i63sub_add_cancel : forall (x y : i63), x - y + y = x)
  (i63add_sub_cancel : forall (x y : i63), x + y - y = x).

Lemma i63add_unwrap_neg (x y : i63) (xneg : x <= 0) (ypos : 0 <= y)
  : i63add_safe x y.

Proof.
  apply i63add_unwrap; auto.
  transitivity 0; auto.
  unfold i63le in *.
  rewrite i63sub_max_safe; auto.
  assert (to_Z y <= to_Z max_i63)%Z by apply i63le_max_int.
  change (to_Z 0) with 0%Z in *.
  lia.
Qed.

Lemma i63add_safe_pos (x y z : i63) (safe : i63add_safe x z)
    (ymin : 0 <= y) (ymax : y <= z)
  : i63add_safe x y.

Proof.
  assert (zpos : 0 <= z) by now transitivity y.
  apply i63add_unwrap; auto.
  apply i63add_unwrap in safe; auto.
  transitivity (max_i63 - z); auto.
  unfold i63le in *.
  repeat rewrite i63sub_max_safe; unfold i63le; auto.
  lia.
Qed.

Lemma i63sub_safe_pos (x y : i63) (ymin : 0 <= y) (ymax : y <= x)
  : i63sub_safe x y.

Proof.
  unfold i63sub_safe.
  apply i63sub_unwrap.
  + exact ymin.
  + assert (min_i63 + max_i63 <= 0) by (intro H; discriminate).
    assert (x <= max_i63) by auto with i63.
    unfold i63le in *.
    rewrite i63add_min_safe; auto.
    change (to_Z 0) with 0%Z in *.
    rewrite i63add_min_safe in *; auto with i63.
    lia.
Qed.

Lemma i63add_safe_lt_add (x y z : i63) (safe1 : i63add_safe x y)
    (safe2 : i63add_safe x z) (lt : y < z)
  : x + y < x + z.

Proof.
  unfold i63lt in *.
  rewrite safe1, safe2.
  lia.
Qed.

Lemma i63lt_le_pred (x y : i63) : x < y -> x <= y - 1.

Proof.
  intros equ.
  assert (i63sub_safe y 1). {
    apply i63sub_unwrap.
    + intros F; discriminate.
    + unfold i63le, i63lt in *.
      rewrite i63add_min_safe.
      ++ assert (to_Z min_i63 <= to_Z x)%Z by apply i63le_min_int.
         change (to_Z 1) with 1%Z.
         lia.
      ++ intro F; discriminate.
  }
  unfold i63le, i63lt in *.
  rewrite H.
  change (to_Z 1) with 1%Z.
  lia.
Qed.

Lemma i63lt_le_succ (x y : i63) : x < y -> x + 1 <= y.

Proof.
  intros equ.
  assert (i63add_safe x 1). {
    apply i63add_unwrap.
    + intros F; discriminate.
    + unfold i63le, i63lt in *.
      rewrite i63sub_max_safe.
      ++ change (to_Z 1) with 1%Z.
         assert (to_Z y <= to_Z max_i63)%Z by apply i63le_max_int.
         lia.
      ++ intro F; discriminate.
  }
  unfold i63le, i63lt in *.
  rewrite H.
  change (to_Z 1) with 1%Z.
  lia.
Qed.
