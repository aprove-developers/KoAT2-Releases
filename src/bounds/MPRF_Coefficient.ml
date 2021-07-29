open Batteries
open BoundsInst
open Polynomials
open Atoms
open Constraints
open ProgramTypes
open Formulas

let logger = Logging.(get Time)

let rec fac = function
  | 0 -> 1
  | 1 -> 1
  | i -> i * fac (i - 1)

let rec gamma = function
  | 1 -> 1.0
  | i -> 2.0 +. (gamma (i - 1)) /. ((float_of_int i) -. 1.0) +. 1.0 /. (float_of_int (fac (i - 1)))

(** Constructs the nested sum of bounds of all functions of the MPRF (over-approximates the maximum of the bounds)*)
let sumBound_of_list coeffs =
  List.fold_left Bound.add Bound.one coeffs

let coefficient (rank: MultiphaseRankingFunction.t) =
  let depth = MultiphaseRankingFunction.depth rank in
  (gamma depth) *. (float_of_int (fac depth))
  |> ceil
  |> int_of_float
  |> tap (fun coeff ->  Logger.log logger Logger.INFO (fun () -> "coefficient ", ["beta" , string_of_int coeff]))