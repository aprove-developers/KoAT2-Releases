open Base

(** Main Module *)

type main_params = { version : bool [@default false]  (** Print the git version *) } [@@deriving cmdliner]

let subcommand run params_cmdliner_term description command =
  let term = Cmdliner.Term.(const run $ params_cmdliner_term ()) in
  let info = Cmdliner.Cmd.info ~doc:description command in
  Cmdliner.Cmd.v info term


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
  let default_term = Cmdliner.Term.(ret (const default_cmd $ main_params_cmdliner_term ())) in
  let main_info = Cmdliner.Cmd.info Sys.argv.(0) in
  let main_group = Cmdliner.Cmd.group ~default:default_term main_info subcommands in

  exit (Cmdliner.Cmd.eval main_group)
