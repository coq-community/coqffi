From CoqFFI Require Import Int.
From Coq Require Import String Ascii.

Open Scope i63_scope.

Fixpoint get (s : string) (idx : i63) : option ascii :=
  match s with
  | String c rst => if idx =? 0
                    then Some c
                    else if 0 <? idx
                         then get rst (idx - 1)
                         else None
  | _ => None
  end.
