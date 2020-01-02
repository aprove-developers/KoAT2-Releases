open BoundsInst
open Formulas
open Polynomials

module Solver = SMT.Z3Solver

let bounds_pos_vars b upper_b =
  let bound_diff = RealBound.(upper_b - b) in
  let vars = RealBound.vars bound_diff in
  let ensure_vars_positive =
    VarSet.fold (fun v -> RealFormula.mk_and (RealFormula.mk_gt (RealPolynomial.of_var v) RealPolynomial.zero)) vars RealFormula.mk_true
  in
  try
    Solver.bound_lt_zero ensure_vars_positive bound_diff
    |> not
  (* inf *)
  with Failure s ->
    s = "inf not supported in SMT-Solving"
