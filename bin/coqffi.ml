open Cmi_format
open Coqffi
open Coqffi.Config
open Coqffi.Interface
open Cmdliner

let process conf input ochannel =
  read_cmi input
  |> interface_of_cmi_infos ~transparent_types:conf.gen_transparent_types
  |> translate (Translation.types_table conf.gen_profile)
  |> pp_interface conf ochannel

exception TooManyArguments
exception InconsistentFlags of string
exception MissingInputArgument

let input_cmi_arg =
  let doc =
    "The compiled interface ($(b,.cmi)) of the OCaml module to be used in Coq" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INPUT" ~doc)

let output_arg =
  let doc = "The name of the Coq file to generate" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let immpure_mode_arg =
  let doc =
    "The Coq framework to use in order to model impure functions.
     By default, $(b,coqffi) assumes OCaml functions are pure, but
     they can be marked with the $(i,@@impure) attribute. Since
     Gallina is a purely functional programming language, a
     framework has to be used to model them." in

  let mode_enum = Arg.enum ["FreeSpec", FreeSpec] in

  Arg.(value & opt (some mode_enum) None
       & info ["m"; "impure-mode"] ~docv:"MODE" ~doc)

let profile_arg =
  let doc =
    "The so-called extraction profile determined the set of base
     types which can be used by the OCaml module, in addition to
     the types defined by this module. See $(b,EXTRACTION PROFILES)
     to get more details" in

  let profile_enum = Arg.enum ["stdlib", Stdlib; "coq-base", Coqbase] in

  Arg.(value & opt profile_enum Stdlib
       & info ["p"; "extraction-profile"] ~docv:"PROFILE" ~doc)

let transparent_types_opt =
  let doc =
    "Enable the support of OCaml transparent types.  By default, $(b,coqffi)
     considers any types introduced by an OCaml module as opaque. If
     $(b,-ftransparent-types) is used, then $(b,coqffi) will try to translate
     some OCaml type definition into a compatible Coq counterpart.
     $(b,Warning:) This feature is experimental, and may lead to the generation
     of invalid Coq types. Typically, it does not enforce the “strict-positive
     occurence” constraints of Coq constructors." in

  Arg.(value & flag & info ["ftransparent-types"] ~doc)

let coqffi_info =
  let doc = "Coq/OCAML FFI made easy" in
  let man = [
    `S "EXTRACTION PROFILES";

    `P "$(b,Note:) OCaml tuples are supported by all extraction profiles.";

    `P "The default extraction profile is $(b,stdlib).";

    `P "The list of OCaml base types supported by the $(b,stdlib) profile is:";
    `Noblank;
    `Pre "  - $(b,bool)"; `Noblank;
    `Pre "  - $(b,char)"; `Noblank;
    `Pre "  - $(b,string)"; `Noblank;
    `Pre "  - $(b,unit)"; `Noblank;
    `Pre "  - $(i,'a) $(b,list)"; `Noblank;
    `Pre "  - $(i,'a) $(b,option)"; `Noblank;
    `P "This extraction profile does not have any OCaml dependency.";

    `P "The list of OCaml base types supported by the $(b,coq-base) profile is:";
    `Noblank;
    `Pre "  - $(b,bool)"; `Noblank;
    `Pre "  - $(b,char)"; `Noblank;
    `Pre "  - $(b,int)"; `Noblank;
    `Pre "  - $(b,unit)"; `Noblank;
    `Pre "  - $(b,Coqbase.Bytestring.t)"; `Noblank;
    `Pre "  - ($(i,'a), $(i,'b)) $(b,Coqbase.Sum.t)"; `Noblank;
    `Pre "  - $(i,'a) $(b,list)"; `Noblank;
    `Pre "  - $(i,'a) $(b,option)"; `Noblank;
    `P "This extraction profile depends on the $(b,coqbase.lib) library from the
        $(b,coqbase) Opam package.";
    `S Manpage.s_bugs;
    `P "Email bug reports to <thomas.letan at ssi.gouv.fr>.";
  ] in
  Term.(info "coqffi" ~exits:default_exits ~doc ~man ~version:"coqffi.1.0.0+dev")

let run_coqffi (input : string) (output : string option)
    (impure_mode : impure_mode option) (profile : extraction_profile)
    (transparent_types : bool) =

  let parse _ =
    let ochannel = match output with
      | Some path -> open_out path |> Format.formatter_of_out_channel
      | _ -> Format.std_formatter in

    let conf = {
      gen_profile = profile;
      gen_impure_mode = impure_mode;
      gen_transparent_types = transparent_types;
    } in

    (input, ochannel, conf) in

  try begin
    let (input, output, conf) = parse () in
    validate conf;
    process conf input output
  end
  with
  | InconsistentFlags opt ->
    Format.printf "-f%s and -fno-%s cannot be used together.\n"
      opt opt
  | Entry.UnsupportedOCamlSignature s ->
    Format.printf "Use of unsupported OCaml construction: %a"
      Printtyp.signature [s]
  | Repr.UnsupportedOCamlType t ->
    Format.printf "Unsupported OCaml type construction %a"
      Printtyp.type_expr t
  | Repr.UnknownOCamlType t ->
    Format.printf "Type %s is not supported by the selected profile"
      t

let coqffi_t =
  Term.(const run_coqffi
        $ input_cmi_arg
        $ output_arg
        $ immpure_mode_arg
        $ profile_arg
        $ transparent_types_opt)

let _ =
  Term.(exit @@ eval (coqffi_t, coqffi_info))
