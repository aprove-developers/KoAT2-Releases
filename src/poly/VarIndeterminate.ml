open OurBase

(** Variables as indeterminates for polynomials, monomials, ... *)
include Var
let rename m v = RenameMap.find v m ~default:v
let vars = VarSet.singleton

let of_var = identity
