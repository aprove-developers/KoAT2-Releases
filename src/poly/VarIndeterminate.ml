open Batteries

(** Variables as indeterminates for polynomials, monomials, ... *)
include Var
let rename m v = RenameMap.find v m v
let vars = VarSet.singleton

let of_var = identity
