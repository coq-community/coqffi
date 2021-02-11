open Cmi_format
open Coqffi
open Cmdliner
open Config
open Format

let process coqns models lwt_alias config features input ochannel (wchannel : formatter option) =
  let open Flow in
  let flush fmt pp = fprintf fmt "%a@?" pp in

  read_cmi input
  |> Mod.of_cmi_infos ~features ~translations:config.config_translations ~lwt_alias
  |> (Vernac.of_mod config.config_aliases features models @> flush ochannel Vernac.pp_vernac 
      || wchannel @? fun wc -> Witness.from_mod ~coqns config.config_aliases @> flush wc Witness.pp)
  |> qed

exception TooManyArguments
exception MissingInputArgument
exception WitnessMissingOutputArgument

let input_cmi_arg =
  let doc =
    "The compiled interface ($(b,.cmi)) of the OCaml module to be used in Coq" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INPUT" ~doc)

let aliases_arg =
  let doc = "A configuration file containing aliases" in
  Arg.(value & opt (some string) None & info ["a"; "aliases"] ~docv:"ALIASES" ~doc)

let include_arg =
  let doc = "A witness file which contains a list of translation types" in
  Arg.(value & opt_all string [] & info ["I"; "include-types"] ~docv:"WITNESS" ~doc)

let lwt_alias_arg =
  let doc = "The alias to Lwt.t used in the OCaml module" in
  Arg.(value & opt (some string) None & info ["lwt-alias"] ~docv:"LWT ALIAS" ~doc)

let output_arg =
  let doc = "The name of the Coq module to generate" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let directory_arg =
  let doc = "The directory wherein to store the generated Coq module" in
  Arg.(value & opt (some string) None & info ["d"; "directory"] ~docv:"DIRECTORY" ~doc)

let models_opt =
  let doc =
    "Coq fully qualified modules to be required prior to defining the
    bindings. This can be used in conjunction with the $(i,coq_model) attribute,
    where a binding is not introduced by an axiom, but rather as an alias for an
    already existing Coq term." in

  Arg.(value & opt_all string [] & info ["r"; "require"] ~doc ~docv:"MODULE")

let features_opt =
  let doc =
    "Enable (using $(b,-f)$(i,feature-name)) or disable (using
     $(b,-fno-)$(i,feature-name)) the feature called $(i,feature-name). This
     option can be used several times (to enable or disable several
     features). If it is used several times for the same feature, then a warning
     is emitted, and the first occurence is used. See $(b,FEATURES) for a
     comprehensive list of the features available." in

  let feature_enum tname =
    let name = Feature.name tname in [
      name, (tname, true);
      "no-" ^ name, (tname, false)
    ] in

  let features_enum =
    Arg.enum (List.concat [
        feature_enum TransparentTypes;
        feature_enum PureModule;
        feature_enum Interface;
        feature_enum SimpleIO;
        feature_enum FreeSpec;
        feature_enum Lwt;
        feature_enum Witness;
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

    `P "$(b,pure-module)"; `Noblank;
    `I (
      "$(b,no-pure-module)",
      "By default, $(b,coqffi) considers OCaml functions are impure,
       and let users marked “pure functions” with the $(i,pure)
       attribute. If $(b,-ftransparent-types) is used, then
       $(b,coqffi) will consider all OCaml values listed in the
       $(i,INPUT) module are pure."
    );

    `P "$(b,simple-io)"; `Noblank;
    `I (
      "$(b,no-simple-io)", "By default, $(b,coqffi) uses the $(i,IO)
      monad provided by the $(b,coq-simple-io) package to model impure
      computations. One can disable the generation of
      $(b,coq-simple-io)'s helpers with $(b,-fno-)$(i,simple-io)."
    );

    `P "$(b,interface)"; `Noblank;
    `I (
      "$(b,no-interface)",
      "When the $(b,interface) feature is enabled, $(b, coqffi) generates a
       parameterized inductive type which describes the set of impure
       primitives provided by the module. This type can be used with
       the monads of verification frameworks such as Interaction Tree
       or FreeSpec. It is disabled by default."
    );

    `P "$(b,freespec)"; `Noblank;
    `I (
      "$(b,no-freespec)", "When the $(b,freespec) feature is enabled,
      $(b,coqffi) generates a FreeSpec semantics for the interface
      generated by the $(b,interface) feature (which means said
      feature needs to be enabled). It is disable by default."
    );

    `P "$(b,lwt)"; `Noblank;
    `I (
      "$(b,no-lwt)", "When the $(b,lwt) feature is enabled,
      $(b,coqffi) reserved a special treatment to values of type
      $(i,Lwt.t). They are marked as “asynchronous primitives,” and
      are gathered in a dedicated typeclass. The name of the $(b,Lwt)
      type can be changed using the $(b,--lwt-alias) option, and using
      this option enables the $(b,lwt) feature. The $(b,-r) option is
      expected to be use to make a type $(i,Lwt.t) available to the
      generated Coq module. It is disabled by default."
    );

    `P "$(b,witness)"; `Noblank;
    `I (
      "$(b,no-lwt)", "When the $(b,witness) feature is enabled,
      $(b,coqffi) generates an auxiliary file in addition to the Coq
      module.  This file contains a summary of the type introduced by
      the generated module, and is a suitable input file for the
      $(b,-t) option. The name of the auxiliary file is derived from
      the $(b,directory) and $(b,output) options (the $(i,.coqffi)
      extension is used). It is disabled by default."
    );

    `S "SUPPORTED TYPES";

    `P "In addition to tuples and types introduced in the input module,
        $(b,coqffi) supports the following base types:";

    `Pre "  - $(b,bool)"; `Noblank;
    `Pre "  - $(b,char)"; `Noblank;
    `Pre "  - $(b,int)"; `Noblank;
    `Pre "  - $(i,'a) $(b,list)"; `Noblank;
    `Pre "  - $(i,'a) $(b,Stdlib.Seq.t)"; `Noblank;
    `Pre "  - $(i,'a) $(b,option)"; `Noblank;
    `Pre "  - ($(i,'a), $(i, 'e)) $(b,result)"; `Noblank;
    `Pre "  - $(b,string)"; `Noblank;
    `Pre "  - $(b,unit)"; `Noblank;
    `Pre "  - $(b,exn)";
    `P "Besides, $(b,coqffi) also supports extending the $(b,exn)
    type, using the $(i,exception )$(b,Foo)$(i, of ) $(b,bar)
    construction. In such a case, $(b,coqffi) will generate a “proxy”
    inductive type $(b,FooExn), along with conversion functions from
    and to $(b,exn).";
    `S Manpage.s_bugs;
    `P "Email bug reports to <thomas.letan at ssi.gouv.fr>.";
  ] in
  Term.(info "coqffi" ~exits:default_exits ~doc ~man ~version:"coqffi.dev")

let run_coqffi (input : string) (aliases : string option) (includes : string list)
      (lwt_alias : string option)
      (output : string option) (directory : string option) (features : Feature.features)
      (models : string list) =

  let path directory output ext = match directory, output with
    | Some directory, Some output -> Some (directory ^ "/" ^ output ^ "." ^ ext)
    | None, Some output -> Some (output ^ "." ^ ext)
    | _, _ -> None in

  let output_path = path directory output "v" in

  let witness_path = path directory output "coqffi" in

  let coqns = Option.value ~default:"" output in

  try begin
    let ochannel = match output_path with
      | Some path -> open_out path |> Format.formatter_of_out_channel
      | _ -> Format.std_formatter in

    let wchannel =
      if Feature.is_enabled features Witness
      then match witness_path with
           | Some path -> Some (open_out path |> Format.formatter_of_out_channel)
           | _ -> raise WitnessMissingOutputArgument
                  (* -fwitness is set, we need to be able to decide a path for the witness *)
      else None in

    let config = Config.config_from_path aliases includes in

    let (lwt_alias, features) =
      Feature.check_features_consistency lwt_alias features ~wduplicate:true in

    process coqns models lwt_alias config features input ochannel wchannel
  end
  with
  | Feature.FreeSpecRequiresInterface ->
    Format.fprintf Format.err_formatter
      "Error: The feature `freespec' requires the feature `interface' to be enabled"
  | Feature.LwtExplicitelyDisableButLwtAliasSet ->
    Format.fprintf Format.err_formatter
      "Error: The feature `lwt' was explicitely disabled, yet an alias for the `Lwt.t' has been specified"
  | Config.SectionShouldBeList (secname, sexp) ->
    Format.fprintf Format.err_formatter
      "Error: Error in the configuration file; `%s' should be a list, but is `%a'"
      secname
      Sexplib.Sexp.pp sexp
  | Config.FieldShouldBeString (fieldname, sexp) ->
    Format.fprintf Format.err_formatter
      "Error: Error in the configuration file; `%s' should be a string in `%a'"
      fieldname
      Sexplib.Sexp.pp sexp
  | Config.MissingField (fieldname, sexp) ->
    Format.fprintf Format.err_formatter
      "Error: Error in the configuration file; expecting field `%s' in `%a'"
      fieldname
      Sexplib.Sexp.pp sexp
  | Config.IllFormedAliasesEntry sexp ->
    Format.fprintf Format.err_formatter
      "Error: Error in the configuration file; `%a' in not a correct alias entry"
      Sexplib.Sexp.pp sexp
  | WitnessMissingOutputArgument ->
    Format.fprintf Format.err_formatter
      "Error: The `witness' feature is enabled, but no OUTPUT is given"

let coqffi_t =
  Term.(const run_coqffi
        $ input_cmi_arg
        $ aliases_arg
        $ include_arg
        $ lwt_alias_arg
        $ output_arg
        $ directory_arg
        $ features_opt
        $ models_opt)

let _ =
  Term.(exit @@ eval (coqffi_t, coqffi_info))
