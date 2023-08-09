(* TODO: This code is extremely awful but I really don't know how to improve it *)
let git_version : string =
  [%meta
    let rec read_from_channel inp_chann =
      try
        let next_line = input_line inp_chann in
        next_line ^ "\n" ^ read_from_channel inp_chann
      with
      | End_of_file -> ""
    in
    let read_process cmd = read_from_channel (Unix.open_process_in cmd) in

    let env_var = Sys.getenv_opt "KOAT2_GIT_VERSION" in
    match env_var with
    | None ->
        let version = read_process "cd $DUNE_SOURCEROOT; git rev-parse --short HEAD"
        and date = read_process "cd $DUNE_SOURCEROOT; git log -1 --format=%cs" in
        let version_str = version ^ " from " ^ date in

        let b = Buffer.create 10 in
        String.iter
          (fun c ->
            if not (Char.equal c '\n') then
              Buffer.add_char b c)
          version_str;
        let str_without_newlines = Buffer.contents b in
        Ppxlib.Parse.expression (Lexing.from_string @@ "\"" ^ str_without_newlines ^ "\"")
    | Some x -> Ppxlib.Parse.expression (Lexing.from_string @@ "\"" ^ x ^ "\"")]


let version = Printf.sprintf "KoAT2 version %s (%s)" git_version Z3.Version.full_version
