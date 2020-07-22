Generalizable All Variables.
Set Implicit Arguments.

Class Inject (i m : Type -> Type) :=
  inject : forall {a : Type}, i a -> m a.
