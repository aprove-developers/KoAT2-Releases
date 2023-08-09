open OurBase
include MakeSetCreators0 (Var)

let equal = Set.equal
let map_to_list f varset = Set.to_list varset |> List.map ~f
let map_to_array f varset = Set.to_array varset |> Array.map ~f

let to_string ?(pretty = false) varset =
  Set.to_sequence varset |> Util.sequence_to_string ~f:(Var.to_string ~pretty)


let of_string_list list = list |> List.map ~f:Var.of_string |> of_list
