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

type feature =
  TransparentTypes

let feature_name = function
  | TransparentTypes -> "transparent-types"

let check_features fs =
  let rec find_dup dups : ('a * 'b) list -> 'a list = function
    | (f, _) :: rst ->
      find_dup
        (if List.mem_assoc f rst then (f :: dups) else dups)
        rst
    | [] -> List.sort_uniq compare dups in

  Format.(
    fprintf err_formatter "%a@?"
      (pp_print_list ~pp_sep:pp_print_newline
         (fun fmt f ->
            fprintf fmt
              "Warning: Feature `%s' has been selected several times.\n\
              The first occurence will be used."
              (feature_name f)))
      (find_dup [] fs))

let transparent_feature_opt fs =
  Option.value ~default:false @@ List.assoc_opt TransparentTypes fs

let features_opt =
  let doc =
    "Enable (using $(b,-f)$(i,feature-name)) or disable (using
     $(b,-fno-)$(i,feature-name)) the feature called $(i,feature-name). This
     option can be used several times (to enable or disable several
     features). If it is used several times for the same feature, then a warning
     is emitted, and the first occurence is used. See $(b,FEATURES) for a
     comprehensive list of the features available." in

  let feature_enum f =
    let name = feature_name f in [
      name, (f, true);
      "no-" ^ name, (f, false)
    ] in

  let features_enum =
    Arg.enum (List.concat [
        feature_enum  TransparentTypes
      ]) in

  Arg.(value & opt_all features_enum [] & info ["f"] ~doc ~docv:"FEATURE")

let coqffi_info =
  let doc = "Coq/OCAML FFI made easy" in
  let man = [
    `S Manpage.s_description;

    `P "$(b,coqffi) automatically generates FFI bindings to OCaml libraries.
        More precisely, $(b,coqffi) generates the necessary boilerplate for a
        Coq development to use the functions and types described in an OCaml
        module interface ($(b,.mli)).";

    `S Manpage.s_arguments;

    `S Manpage.s_options;

    `S "FEATURES";

    `P "$(b,transparent-types)"; `Noblank;
    `I (
      "$(b,no-transparent-types)",
      "By default, $(b,coqffi) considers any types introduced by an OCaml module
       as opaque. If $(b,-ftransparent-types) is used, then $(b,coqffi) will try
       to translate some OCaml type definition into a compatible Coq
       counterpart.  $(b,Warning:) This feature is experimental, and may lead to
       the generation of invalid Coq types. Typically, it does not enforce the
       “strict-positive occurence” constraints of Coq constructors."
    );

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
    (features : (feature * bool) list) =

  let parse _ =
    let ochannel = match output with
      | Some path -> open_out path |> Format.formatter_of_out_channel
      | _ -> Format.std_formatter in

    check_features features;

    let conf = {
      gen_profile = profile;
      gen_impure_mode = impure_mode;
      gen_transparent_types = transparent_feature_opt features;
    } in

    (input, ochannel, conf) in

  try begin
    let (input, output, conf) = parse () in
    validate conf;
    process conf input output
  end
  with
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
        $ features_opt)

let _ =
  Term.(exit @@ eval (coqffi_t, coqffi_info))
