(** Main Module *)
open Batteries
open Koat2
open ProgramTypes
open RVGTypes

type main_params = {
  version: bool[@default false];
  (** Print the git version *)
} [@@deriving cmdliner]

let subcommand run params_cmdliner_term description command =
  Cmdliner.Term.(const run $ params_cmdliner_term (), info ~doc:description command)

let subcommands =
  [
    AnalyseCommand.(subcommand run params_cmdliner_term description command);
    PrfCommand.(subcommand run params_cmdliner_term description command);
    LocalSizeBoundCommand.(subcommand run params_cmdliner_term description command);
    SizeCommand.(subcommand run params_cmdliner_term description command);
    SMTCommand.(subcommand run params_cmdliner_term description command);
    NormalizeCommand.(subcommand run params_cmdliner_term description command);
  ]

let default_cmd params =
  if params.version then
    `Ok (Printf.printf "%s\n" VersionString.version)
  else `Help (`Pager, None)

let () =
  (* Read the arguments from the shell via an api and call run *)
  let main_command = Cmdliner.Term.(ret (const default_cmd $ main_params_cmdliner_term())),
  Cmdliner.Term.info Sys.argv.(0) in
  (* Logger.init ["lsb", Logger.DEBUG; "size", Logger.DEBUG; "prf", Logger.DEBUG] (Logger.make_dbg_formatter IO.stdout); *)
  Cmdliner.Term.(exit @@ eval_choice main_command subcommands)
