open BoundsInst
open Formulas

module Solver = SMT.Z3Solver

let bounds b upper_b = 
  let bound_diff = RealBound.(upper_b - b) in
  try
    Solver.bound_lt_zero (RealFormula.mk_true) bound_diff
    |> not
  (* inf *)
  with Failure s -> 
    s = "inf not supported in SMT-Solving"
