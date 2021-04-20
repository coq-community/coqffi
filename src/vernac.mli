type from_require_import = { import_from : string; import_module : string }

type from_require_export = { export_from : string; export_module : string }

type require = { require_module : string }

type constructor = {
  constructor_name : string;
  constructor_prototype : Repr.prototype_repr;
}

type inductive = {
  inductive_name : string;
  inductive_type_args : string list;
  inductive_type : Repr.type_repr;
  inductive_constructors : constructor list;
}

type record = {
  record_name : string;
  record_type_args : string list;
  record_fields : Entry.field_entry list;
}

type definition = {
  def_name : string;
  def_typeclass_args : string list;
  def_prototype : Repr.prototype_repr;
  def_body : Format.formatter -> unit -> unit;
}

type typeclass = {
  class_name : string;
  class_typeclass_args : string list;
  class_args : (string * Repr.type_repr) list;
  class_type : Repr.type_repr;
  class_members : (string * Repr.type_repr) list;
}

type instance = {
  instance_name : string;
  instance_typeclass_args : string list;
  instance_type : Repr.type_repr;
  instance_members : (string * string) list;
}

type axiom = {
  axiom_name : string;
  axiom_typeclass_args : string list;
  axiom_type : Repr.type_repr;
}

type extract_constant = {
  constant_qualid : string;
  constant_type_vars : string list;
  constant_target : string;
}

type extract_inductive = {
  inductive_qualid : string;
  inductive_target : string;
  inductive_variants_target : string list;
}

type coq_module = { coqmod_name : string; coqmod_content : t }

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
  string option -> Alias.table -> Feature.features -> string list -> Mod.t -> t

val pp_vernac : Format.formatter -> t -> unit
