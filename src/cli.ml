type impure_mode =
  | FreeSpec

type extraction_profile =
  | Stdlib
  | Coqbase

exception FreeSpecExtractionProfile
exception TooManyArguments
exception MissingInputArgument

let impure_mode_opt : impure_mode option ref =
  ref None

let extraction_opt : extraction_profile ref =
  ref Stdlib

let input_opt : string option ref =
  ref None

let output_opt : string option ref =
  ref None

let with_type_value_opt : bool ref =
  ref false

let with_type_value _ = !with_type_value_opt

let get_impure_mode _ = !impure_mode_opt
let get_extraction_profile _ = !extraction_opt

let get_input_path _ =
  match !input_opt with
  | Some path -> path
  | _ -> raise MissingInputArgument

let get_output_formatter _ =
  match !output_opt with
  | Some path -> open_out path |> Format.formatter_of_out_channel
  | _ -> Format.std_formatter

let specs = [
  ("-p",
   Arg.Symbol (["stdlib"; "coq-base"], fun profile ->
       match profile with
       | "stdlib" -> extraction_opt := Stdlib
       | "coq-base" -> extraction_opt := Coqbase
       | _ -> assert false),
   "  Select an extraction profile for base types");

  ("-m",
   Arg.Symbol (["FreeSpec"], fun mode ->
       match mode with
       | "FreeSpec" -> impure_mode_opt := Some FreeSpec
       | _ -> assert false),
   "  Select a framework to model impure computations");

  ("-o",
   Arg.String (fun path -> output_opt := Some path),
   " Select a framework to model impure computations");

  ("--with-type-value",
   Arg.Unit (fun _ -> with_type_value_opt := true),
   " Bind OCaml type values to Coq inductive types");
]

let parse _ =
  let validate _ =
    match !impure_mode_opt with
    | Some FreeSpec -> (match !extraction_opt with
        | Stdlib -> raise FreeSpecExtractionProfile
        | _ -> ())
    | _ -> () in

  let n = ref 0 in

  Arg.parse specs (fun arg ->
      if !n = 0
      then begin
        input_opt := Some arg;
        n := 1
      end
      else raise TooManyArguments)
    "coqffi";

  validate ()

let usage =
  {|coqffi INPUT [-p {stdlib|coq-base}] [-m {FreeSpec}] [-o OUTPUT]|}
