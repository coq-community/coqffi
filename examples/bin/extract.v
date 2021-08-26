From Coq Require Import Extraction.
From SimpleIO Require Import SimpleIO.
From ExtLib Require Import Monad.
Import MonadLetNotation.
Open Scope monad_scope.
From CoqFFI Require Import Extraction.
Open Scope i63_scope.

From Examples Require Import File Sleep Records.
Import M.
From CoqFFI Require Import String.

Generalizable All Variables.

Definition cat `{Monad m, MonadFile m} : m unit :=
  let* fd := openfile "README.md" in
  let* content := read_all fd in
  write std_out content;;
  closefile fd.

Definition cat_main : io_unit := IO.unsafe_run cat.

Extraction "cat.ml" cat_main.

Definition sleep_plenty `{Monad m, MonadFile m, MonadSleep m} (d : i63)
  : m unit :=
  write std_out "Hello...";;
  let y : tup4 i63 i63 i63 i63 := mktup4 1 2 3 4 in
  match y with
  | mktup4 a b c d =>
    let x := (a * b + c - d) / d in
    sleep x;;
    write std_out " sleepy world!\n"
  end.

Definition sleep_plenty_main : io_unit :=
  let x := {| f1 := 1; f2 := 2 |} in
  IO.unsafe_run (sleep_plenty (x.(f1) + x.(f2))).

Extraction "sleep_plenty.ml" sleep_plenty_main.
