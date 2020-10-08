From Coq Require Export List.
From ExtLib Require Export Monad.
From CoqFFI Require Export Int String.

#[global] Close Scope nat_scope.
#[global] Open Scope i63_scope.
#[global] Open Scope bool_scope.
#[global] Open Scope string_scope.

Export ListNotations.
Export MonadLetNotation.

#[global] Generalizable All Variables.
