open Batteries
open BoundsInst
open Polynomials
open ProgramModules
open ProgramTypes
module Automorphism =
  struct
  type t = {
    poly : RationalPolynomial.t VarMap.t;
    inv_poly :  RationalPolynomial.t VarMap.t;
  }

  let to_string t =
    "Automorphism:\n" ^ VarMap.fold (fun var poly str -> str ^ (Var.to_string var) ^ " -> " ^ (RationalPolynomial.to_string poly) ^ "\n") t.poly "" ^
    "Inverse: \n" ^     VarMap.fold (fun var poly str -> str ^ (Var.to_string var) ^ " -> " ^ (RationalPolynomial.to_string poly) ^ "\n") t.inv_poly ""

  let identity = {poly = VarMap.empty; inv_poly = VarMap.empty}

  let apply_to_bound t = List.fold_right (fun var bound ->
    let bound_of_inv_poly = Bound.of_poly @@ RationalPolynomial.overapprox (VarMap.find var t.inv_poly) in
    Bound.substitute var ~replacement:bound_of_inv_poly bound) (List.of_enum @@ VarMap.keys t.inv_poly)
end
