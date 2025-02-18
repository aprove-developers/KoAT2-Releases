open! OurBase
include MakeSetCreators0 (VarRec)

let equal = Set.equal
let of_varset (vars : VarSet.t) = Set.map (module VarRec) vars ~f:VarRec.of_var

let to_string ?(pretty = false) varset =
  Set.to_sequence varset |> Util.sequence_to_string ~f:(VarRec.to_string ~pretty)


let of_string_list list = list |> List.map ~f:Var.of_string |> VarSet.of_list |> of_varset
