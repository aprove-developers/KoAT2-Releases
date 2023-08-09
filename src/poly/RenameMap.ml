open OurBase
module M = MakeMapCreators1 (Var)

type var = Var.t
type t = var M.t

let from entries =
  let addEntry entry map =
    match entry with
    | key, data -> Map.add_exn map ~key ~data
  in
  List.fold_left ~f:(fun map keyadder -> keyadder map) ~init:M.empty (List.map ~f:addEntry entries)


let of_sequence = M.of_sequence_exn

let from_native entries =
  from (List.map ~f:(fun (var, value) -> (Var.of_string var, Var.of_string value)) entries)


let id vars = from (List.map ~f:(fun var -> (var, var)) vars)
let find var map ~default = Map.find_default map ~default var
