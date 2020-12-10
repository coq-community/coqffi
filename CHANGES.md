# `coqffi.1.0.0`

This will be the initial release of `coqffi`.

## Unreleased

- **Fix:** Extraction of inductive constructors
  (`-ftransparent-types`)
- **Fix:** Generate the missing instance for the interface inductive
  type (`-finterface`)

## `coqffi.1.0.0~beta1`

- **Feature:** Generation of the boilerplate to use `coq-simple-io`
  (`-fsimple-io`)
- **Feature:** Generation of an inductive type which describes the
  sets of impure primitives of the OCaml module interface
  (`-finterface`)
- **Feature:** Generation of a FreeSpec semantics (`-ffreespec`)
- **Experimental Feature:** Generation of inductive types when
  possible (`-ftransparent-types`)
