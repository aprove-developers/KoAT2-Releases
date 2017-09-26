open Batteries

module Make =
  struct
    module M = Map.Make(Var)
    type var = Var.t
    type t = var M.t

    let from entries =
      let addEntry entry = match entry with
        | (key, value) -> M.add key value in
      List.fold_left (fun map keyadder -> keyadder map) M.empty (List.map addEntry entries)

    let from_native entries =
      from (List.map (fun (var, value) -> (Var.of_string var, Var.of_string value)) entries)

    let id vars = from (List.map (fun var -> (var, var)) vars)

    let find var map ~default = if M.mem var map then M.find var map else default
  end
