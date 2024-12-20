let git_version : string =
  let version_str = "VERSION_STR" in
  if not (String.equal version_str "VERSION_STR") then
    version_str
  else
    match Build_info.V1.version () with
    | None -> "dev"
    | Some v -> Build_info.V1.Version.to_string v


let version = Printf.sprintf "KoAT2 version %s (%s)" git_version Z3.Version.full_version
