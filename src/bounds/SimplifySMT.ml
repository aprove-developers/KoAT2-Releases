open BoundsInst
open Batteries
open Formulas

let simplify_bound_with_smt logger invariants b =
  let cached_z3 invariants cmp b1 b2 =
    SMT.Z3Solver.cmp_bounds invariants cmp b1 b2
  in

  let helper cmp b1 b2 =
    try
      match cached_z3 invariants cmp b1 b2 with
      | true -> Some true
      | _    ->
        (* Check the other direction, this is not necessarily implied by the previous step. E.g. we have not 1<X and also not neg X>1 for no invariants*)
        let res = match cmp with
          | `GE -> cached_z3 invariants `GT b2 b1
          | `GT -> cached_z3 invariants `GE b2 b1
        in
        match res with
        | true -> Some false
        | _    -> None

    with _ -> None
  in
  let execute () =
    (if VarSet.disjoint (Formulas.RealFormula.vars invariants) (RealBound.vars b) then
      b
    else
      RealBound.simplify_opt_invariants helper b)
  in
  Logger.with_log logger Logger.DEBUG
    (fun () -> "simplify_bound_with_smt", ["invariants", Formulas.RealFormula.to_string invariants;"bound", RealBound.to_string b])
    ~result:(RealBound.to_string)
    execute

let simplify_bound_with_smt_all_positive logger b =
  let vars = RealBound.vars b in
  let invariants =
    VarSet.fold
      (fun v -> RealFormula.mk_and (RealFormula.mk_ge (Polynomials.RealPolynomial.of_var v) (Polynomials.RealPolynomial.zero)))
      vars
      RealFormula.mk_true
  in
  simplify_bound_with_smt logger invariants b