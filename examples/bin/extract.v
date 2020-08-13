From Coq Require Import Extraction.
From SimpleIO Require Import SimpleIO.
From ExtLib Require Import Monad.
Import MonadLetNotation.
Open Scope monad_scope.

From Examples Require Import File.

Generalizable All Variables.

Definition read100 `{Monad m, MonadFile m} : m unit :=
  let* fd := openfile "README.md" in
  let* content := read_all fd in
  write std_out content;;
  closefile fd.

Definition main : io_unit := IO.unsafe_run read100.

Extraction "read100.ml" main.
