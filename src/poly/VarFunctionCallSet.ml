open! OurBase
include MakeSetCreators0 (VarFunctionCall)

let equal = Set.equal
let of_varset (vars : VarSet.t) = Set.map (module VarFunctionCall) vars ~f:VarFunctionCall.of_var

let to_string ?(pretty = false) varset =
  Set.to_sequence varset |> Util.sequence_to_string ~f:(VarFunctionCall.to_string ~pretty)


let of_string_list list = list |> List.map ~f:Var.of_string |> VarSet.of_list |> of_varset
