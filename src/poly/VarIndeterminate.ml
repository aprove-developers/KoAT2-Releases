open! OurBase

include Var
(** Variables as indeterminates for polynomials, monomials, ... *)

let rename m v = RenameMap.find v m ~default:v
let vars = VarSet.singleton
let of_var = identity
