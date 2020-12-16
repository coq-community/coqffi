Set Implicit Arguments.
Unset Strict Implicit.
Set Contextual Implicit.
Generalizable All Variables.

From Coq Require Import Extraction.


(** * Types *)

Inductive t (a : Type) : Type :=
| Seq : (unit -> node a) -> t a
with node (a : Type) : Type :=
| Nil : node a
| Cons (x0 : a) (x1 : t a) : node a.

(** * Pure functions *)

Definition unseq `(s : t a) : node a :=
  match s with
  | Seq f => f tt
  end.

Definition unpack `(s : t a) : option (a * t a) :=
  match unseq s with
  | Cons x rst => Some (x, rst)
  | _ => None
  end.

Definition empty {a} : t a := Seq (fun _ => Nil).

Definition ret `(x : a) : t a := Seq (fun _ => Cons x empty).

Definition cons `(x : a) (rst : t a) := Seq (fun _ => Cons x rst).

Fixpoint append `(x : t a) (y : t a) : t a :=
  Seq (fun _ => match unseq x with
                | Cons x rst => Cons x (append rst y)
                | Nil => unseq y
                end).

Fixpoint map `(f : a -> b) (x : t a) : t b :=
  Seq (fun _ => match unseq x with
                | Cons x rst => Cons (f x) (map f rst)
                | Nil => Nil
                end).

Fixpoint filter `(f : a -> bool) (x : t a) : t a :=
  Seq (fun _ => match unseq x with
                | Cons x rst => if f x
                                then Cons x (filter f rst)
                                else unseq (filter f rst)
                | Nil => Nil
                end).

Fixpoint filter_map `(f : a -> option b) (x : t a) : t b :=
  Seq (fun _ => match unseq x with
                | Cons x rst => match f x with
                                | Some y => Cons y (filter_map f rst)
                                | _ => unseq (filter_map f rst)
                                end
                | Nil => Nil
                end).

Fixpoint flat_map `(f : a -> t b) (x : t a) : t b :=
  Seq (fun _ => match unseq x with
                | Cons x rst => unseq (append (f x) (flat_map f rst))
                | Nil => Nil
                end).

Fixpoint fold_left `(f : a -> b -> a) (acc : a) (x : t b) : a :=
  match unseq x with
  | Cons x rst => fold_left f (f acc x) rst
  | Nil => acc
  end.

Module SeqExtraction.
  Extract Inductive t => "Stdlib.Seq.t" [""].
  Extract Inductive node => "Stdlib.Seq.node"
    [ "Stdlib.Seq.Nil" "Stdlib.Seq.Cons" ].
  Extract Constant empty => "Stdlib.Seq.empty".
  Extract Constant ret => "Stdlib.Seq.return".
  (* since OCaml 4.11 *)
  Extract Constant cons => "Stdlib.Seq.cons".
  (* since OCaml 4.11 *)
  Extract Constant append => "Stdlib.Seq.append".
  Extract Constant map => "Stdlib.Seq.map".
  Extract Constant filter => "Stdlib.Seq.filter".
  Extract Constant filter_map => "Stdlib.Seq.filter_map".
  Extract Constant flat_map => "Stdlib.Seq.flat_map".
  Extract Constant fold_left => "Stdlib.Seq.fold_left".
End SeqExtraction.
