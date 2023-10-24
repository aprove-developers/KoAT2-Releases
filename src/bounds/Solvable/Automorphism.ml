open! OurBase
open Polynomials
open! ProgramModules
open ProgramTypes

type t = { poly : RationalPolynomial.t VarMap.t; inv_poly : RationalPolynomial.t VarMap.t }

let to_string t =
  "Automorphism:\n"
  ^ Map.fold
      ~f:(fun ~key ~data str -> str ^ Var.to_string key ^ " -> " ^ RationalPolynomial.to_string data ^ "\n")
      t.poly ~init:""
  ^ "Inverse: \n"
  ^ Map.fold
      ~f:(fun ~key ~data str -> str ^ Var.to_string key ^ " -> " ^ RationalPolynomial.to_string data ^ "\n")
      t.inv_poly ~init:""


let identity = { poly = Map.empty (module Var); inv_poly = Map.empty (module Var) }

let apply_to_bound bound = function
  | None -> bound
  | Some _ -> bound
