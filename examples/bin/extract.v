From Coq Require Import Extraction.
From SimpleIO Require Import SimpleIO.
From ExtLib Require Import Monad.
Import MonadLetNotation.
Open Scope monad_scope.
From CoqFFI Require Import Int Seq.
Open Scope i63_scope.

From Examples Require Import File Sleep.

Generalizable All Variables.

Definition cat `{Monad m, MonadFile m} : m unit :=
  let* fd := openfile "README.md" in
  let* content := read_all fd in
  write std_out content;;
  closefile fd.

Definition cat_main : io_unit := IO.unsafe_run cat.

Extraction "cat.ml" cat_main.

Definition sleep_plenty `{Monad m, MonadFile m, MonadSleep m}
  : m unit :=
  write std_out "Hello...";;
  let x := (5 * 1 + 1 - 3) / 3 in
  sleep x;;
  write std_out " sleepy world!\n".

Definition sleep_plenty_main : io_unit := IO.unsafe_run sleep_plenty.

Extraction "sleep_plenty.ml" sleep_plenty_main.
