# `coqffi.1.0.0`

This will be the initial release of `coqffi`.

## Unreleased

- **Feature:** Support OCaml GADT (`-ftransparent-types`)
- **Feature:** Users can defined their own aliases using a
  configuration file (`-c CONFIG`)
- **Feature:** Add a new feature to support asynchronous functions
  (powered by the `Lwt` framework) (`-flwt`)
- **Fix:** Support polymorphic constructor such as `type box = Box :
  'a -> box`
- **Fix:** Complete the list of reserved Gallina keywords using [the
  Coq reference manual][coq-refman]
- **Fix:** `coqffi` will not fail when confronted to an OCaml type of
  the form `type foo = Foo { bar : bool; foobar : int }`
- **Fix:** Close the `nat_scope` scope in generated module to avoid
  confusion between `prod` and the `nat` multiplication
- **Fix:** `coqffi` now translates correctly the types used outside of
  the module which introduces them
- **Fix:** `coqffi` now generates axioms for types it cannot translate
  (`-ftransparent-types`)

  [coq-refman]: https://coq.github.io/doc/v8.9/refman/language/gallina-specification-language.html

## `coqffi.1.0.0~beta3`

- **Breaking Change:** OCaml functions are now assumed impure and can
  be explicitly marked as pure with the `pure` attribute, while the
  rest of OCaml values are always considered pure (OCaml values
  were previously considered pure by default, and could be marked
  using the `impure` attribute)
- **Breaking Change:** The `coq_model` attribute implies the marked
  value is pure (it was previously ignored when used in conjuction
  with the `impure` attribute)
- **Feature:** Add a new feature to assume all OCaml values of
  the input OCaml module interface are pure (`-fpure-module`)
- **Feature:** Add `Seq.t` and `result` to the list of the primitive
  types supported by `coqffi`
- **Feature**: In presence of a constuction of the form `exception Foo
  of bar`, coqffi now generates a “proxy” type `FooExn`, along with
  conversion functions from and to `exn`, and an instance for the
  `Exn` typeclass provided by the `CoqFFI` theory
- **Feature:** Support the `may_raise` attribute to mark pure function
  and impure primivites susceptible to raise exceptions
- **Feature:** `coqffi` will no longer abort when confronting an input
  it cannot handle, but will rather print a warning explaining why
  a given entry will not be part of the generated Coq module
- **Feature:** `coqffi` now supports signature modules
- **Feature:** `coqffi` now uses aliases for some OCaml operators,
  and Gallina reserved keywords
- **Feature:** `coqffi` now supports unamed OCaml polymorphic types
- **Fix:** Make sure module names are always capitalized

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
