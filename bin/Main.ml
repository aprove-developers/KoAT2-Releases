open Base

(** Main Module *)

type main_params = { version : bool [@default false]  (** Print the git version *) } [@@deriving cmdliner]

let subcommand run params_cmdliner_term description command =
  Cmdliner.Term.(const run $ params_cmdliner_term (), info ~doc:description command)


let subcommands =
  [
    AriToKoatCommand.(subcommand run params_cmdliner_term description command);
    AnalyseCommand.(subcommand run params_cmdliner_term description command);
    MPRFCommand.(subcommand run params_cmdliner_term description command);
    LocalSizeBoundCommand.(subcommand run params_cmdliner_term description command);
    PlrfCommand.(subcommand run params_cmdliner_term description command);
    ProbabilisticAnalyseCommand.(subcommand run params_cmdliner_term description command);
    SizeCommand.(subcommand run params_cmdliner_term description command);
    SMTCommand.(subcommand run params_cmdliner_term description command);
    CfrCommand.(subcommand run params_cmdliner_term description command);
  ]


let default_cmd params =
  if params.version then
    `Ok
      (KoAT.print_logo ();
       Stdio.printf "%s\n" Koat2.VersionString.version)
  else
    `Help (`Pager, None)


let () =
  (* Read the arguments from the shell via an api and call run *)
  let main_command =
    (Cmdliner.Term.(ret (const default_cmd $ main_params_cmdliner_term ())), Cmdliner.Term.info Sys.argv.(0))
  in
  Cmdliner.Term.(exit @@ eval_choice main_command subcommands)
