open Batteries
open BoundsInst

let logger = Logging.(get Time)

let rec fac = function
  | 0 -> 1
  | 1 -> 1
  | i -> i * fac (i - 1)

let rec gamma = function
  | 1 -> 1.0
  | i -> 2.0 +. (gamma (i - 1)) /. (float_of_int (i - 1)) +. 1.0 /. (float_of_int (fac (i - 1)))

let coefficient depth =
  (gamma depth) *. (float_of_int (fac depth))
  |> ceil
  |> int_of_float
  |> tap (fun coeff ->  Logger.log logger Logger.INFO (fun () -> "coefficient ", ["beta" , string_of_int coeff]))
