(** ** Primitive Signed Integers *)

From CoqFFI Require Export Int.
Import IntExtraction.


(** * Float *)

From Coq Require Export PrimFloat.
(** ** Strings *)

From Coq Require Export Ascii String.
From Coq Require Import ExtrOcamlNativeString.

(** ** Booleans *)

Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inlined Constant orb => "(||)".
Extract Inlined Constant andb => "(&&)".

(** ** Options *)

Extract Inductive option => "option" [ "Some" "None" ].

(** ** Unit *)

Extract Inductive unit => unit [ "()" ].

(** ** Products *)

Extract Inductive prod => "( * )" [ "" ].

(** ** Lists *)

Extract Inductive list => "list" [ "[]" "( :: )" ].

(** ** Seq *)

From CoqFFI Require Seq.
Import Seq.SeqExtraction.

(** ** Result *)

From CoqFFI Require Result.
Import Result.ResultExtraction.

(** Exceptions *)

From CoqFFI Require Export Exn.
Import ExnExtraction.
