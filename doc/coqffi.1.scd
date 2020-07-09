coqffi(1)

# NAME

coqffi - Coq/OCAML FFI made easy (using extraction)

# SYNOPSIS

*coqffi* _input_ [ _options_ ] [ _features_ ] *-o* _output_

# DESCRIPTION

*coqffi* generates binding to easily use OCaml modules in a Coq development.

_input_
	The compiled interface (*.cmi*) of the OCaml module to be used in Coq

*-o* _output_
	The name of the Coq file to generate

# OPTIONS

*-e* _profile_
	The so-called extraction profile determined the set of base
	types which can be used by the OCaml module, in addition to
	the types defined by this module. _profile_ can be one of the
	following:

	- *stdlib*
	- *coq-base*

	See the *EXTRACTION PROFILES* section for more details.

*-m* _impure mode_
	The Coq framework to use in order to model impure functions.

	By default, *coqffi* assumes OCaml functions are pure, but
	they can be marked with the _@@impure_ attribute. Since
	Gallina is a purely functional programming language, a
	framework has to be sude to model them. Currently, the following
	_impure mode_ are supported:

	- *FreeSpec*

# FEATURES

## Experimental

*-ftransparent-types*++
*-fno-transparent-types*
	Enable or disable the support of OCaml transparent types.

	By default, *coqffi* considers any types introduced by an
	OCaml module as opaque. If *-ftransparent-types* is used, then
	*coqffi* will try to translate some OCaml type definition into
	a compatible Coq counterpart.

	*Warning:* This feature is experimental, and may lead to the
	generation of invalid Coq type. More precisely, it does not
	enforce the “strict-positive occurence” constraints of Coq
	constructors.

# EXTRACTION PROFILES

*Note*: OCaml tuples are supported by all extraction profiles.

## stdlib

The list of OCaml base types supported by the *stdlib* profile is:

- _'a_ *list*
- _'a_ *option*
- *bool*
- *unit*
- *char*
- *string*

## coq-base

The list of OCaml base types supported by the *coq-base* profile is:

- _'a_ *list*
- _'a_ *option*
- *bool*
- *unit*
- *int*
- *char*
- *Coqbase.Bytestring.t* (from the *coq-base* package)
- *Coqbase.Sum.t* (from the *coq-base* package)
