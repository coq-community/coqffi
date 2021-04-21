open Conflict

type from_require_import = { import_from : string; import_module : string }

type from_require_export = { export_from : string; export_module : string }

type require = { require_module : string }

type constructor = {
  constructor_name : coq_name;
  constructor_prototype : Repr.prototype_repr;
}

type inductive = {
  inductive_name : coq_name;
  inductive_type_args : string list;
  inductive_type : Repr.type_repr;
  inductive_constructors : constructor list;
}

type field = { field_name : coq_name; field_type : Repr.mono_type_repr }

type record = {
  record_name : coq_name;
  record_type_args : string list;
  record_fields : field list;
}

type definition = {
  def_name : coq_name;
  def_typeclass_args : string list;
  def_prototype : Repr.prototype_repr;
  def_body : Format.formatter -> unit -> unit;
}

type typeclass = {
  class_name : coq_name;
  class_typeclass_args : string list;
  class_args : (string * Repr.type_repr) list;
  class_type : Repr.type_repr;
  class_members : (coq_name * Repr.type_repr) list;
}

type instance = {
  instance_name : coq_name;
  instance_typeclass_args : string list;
  instance_type : Repr.type_repr;
  instance_members : (coq_name * string) list;
}

type axiom = {
  axiom_name : coq_name;
  axiom_typeclass_args : string list;
  axiom_type : Repr.type_repr;
}

type extract_constant = {
  constant_qualid : coq_name;
  constant_type_vars : string list;
  constant_target : string;
}

type extract_inductive = {
  inductive_qualid : coq_name;
  inductive_target : string;
  inductive_variants_target : string list;
}

type coq_module = { coqmod_name : coq_name; coqmod_content : t }

and t =
  | Section of string
  | Subsection of string
  | Comment of string
  | Block of t Lazylist.t
  | CompactedBlock of t Lazylist.t
  | CoqModule of coq_module
  | ConfigPrologue
  | FromRequireImport of from_require_import
  | FromRequireExport of from_require_export
  | Require of require
  | Definition of definition
  | Inductive of inductive list
  | Typeclass of typeclass
  | Instance of instance
  | Record of record
  | Axiom of axiom
  | ExtractConstant of extract_constant
  | ExtractInductive of extract_inductive

val of_mod :
  string option -> Feature.features -> string list -> Conflict.t -> Mod.t -> t

val pp_vernac : Format.formatter -> t -> unit
