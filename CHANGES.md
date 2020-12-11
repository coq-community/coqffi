# `coqffi.1.0.0`

This will be the initial release of `coqffi`.

## Unreleased

- **Breaking Change:** OCaml functions are now assumed impure and can
  be explicitly marked as pure with the `pure` attribute, while the
  rest of OCaml values are always considered pure (OCaml values
  were previously considered pure by default, and could be marked
  using the `impure` attribute).
- **Breaking Change:** The `coq_model` attribute implies the marked
  value is pure (it was previously ignored when used in conjuction
  with the `impure` attribute).

## `coqffi.1.0.0~beta2`

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
