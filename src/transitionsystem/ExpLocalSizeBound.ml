open Batteries
open Formulas
open Polynomials
open ProgramTypes
open BoundsInst
open RVTransitions

module RV = RVGTypes.Make_RV (TransitionForExpectedSize)

module Valuation = Valuation.Make (OurFloat)

type kind = [`Upper | `Lower]

let default kind = 
  match kind with
  | `Upper -> RealBound.infinity
  | `Lower -> RealBound.minus_infinity

module Solver = SMT.IncrementalZ3Solver

(* a polynom f is concave (convexe) in a variable var iff for all a b it holds 
 * f'' (a) < 0 ( f'' (a) > 0) *)
let concave_convex_check comp_operator guard poly var = 
  let solver = Solver.create () in
  let poly = 
    RealPolynomial.derivative var poly
    |> RealPolynomial.derivative var
  in
  let formula = comp_operator poly (OurFloat.of_int 0 |> RealPolynomial.of_constant) in
  Solver.add_real solver guard;
  Solver.add_real solver (RealFormula.neg formula);
  Solver.unsatisfiable solver 

let poly_is_concave_in_var gt = 
  let guard = GeneralTransition.guard gt |> Constraints.RealConstraint.of_intconstraint |> RealFormula.mk in
  concave_convex_check RealFormula.mk_le guard

let poly_is_convex_in_var gt= 
  let guard = GeneralTransition.guard gt |> Constraints.RealConstraint.of_intconstraint |> RealFormula.mk in
  concave_convex_check RealFormula.mk_ge guard

let poly_is_convex_in_all_vars gt poly =
  let vars = RealPolynomial.vars poly in 
  vars
  |> VarSet.to_list
  |> List.map (poly_is_convex_in_var gt poly)
  |> List.for_all identity

let poly_is_concave_in_all_vars gt poly =
  let vars = RealPolynomial.vars poly in 
  vars
  |> VarSet.to_list
  |> List.map (poly_is_concave_in_var gt poly)
  |> List.for_all identity

let appr_substitution_is_valid kind gt poly =
  match kind with
  | `Upper -> poly_is_concave_in_all_vars gt poly
  | `Lower -> poly_is_convex_in_all_vars gt poly


let simplify_poly_with_guard guard (poly: RealPolynomial.t) = 
  let check_var var = 
    let var_var = Var.fresh_id Var.Int () in 
    let guard_formula = guard |> Formula.mk in
    let guard_formula_var_var = guard
                                |> Constraints.Constraint.map_polynomial 
                                     (Polynomial.substitute var ~replacement:(Polynomial.of_var var_var)) 
                                |> Formula.mk 
    in
    let solver = Solver.create () in
    let var1 = Var.fresh_id Var.Int () in
    let guard_impls_var1 = 
      Formula.implies (guard_formula)
        (Formula.mk_eq (Polynomial.of_var var) (Polynomial.of_var var1))
    in
    let var2 = Var.fresh_id Var.Int () in
    let guard_impls_var2 = 
      Formula.implies (guard_formula_var_var)
        (Formula.mk_eq (Polynomial.of_var var_var) (Polynomial.of_var var2))
    in
    let exists_two_different_impls = 
      let vars_uneq_form = Polynomial.(Formula.mk_uneq (of_var var1) (of_var var2)) in
      Solver.add solver (guard_impls_var1);
      Solver.push solver;
      Solver.add solver (guard_impls_var2);
      Solver.add solver (vars_uneq_form);
      Solver.satisfiable solver
    in
    if not exists_two_different_impls then
      begin
        Solver.pop solver;
        Solver.model_real solver
        |> Option.map (Valuation.eval var1)
      end
    else
      None

  in
  RealPolynomial.vars poly
  |> VarSet.to_list
  |> List.map (fun v -> (v, check_var v))
  |> List.filter (Option.is_some % Tuple2.second)
  |> List.map (fun (v,valuation) -> (v,Option.get valuation))
  |> Valuation.from
  |>  RealPolynomial.eval_partial poly
  |>  RealPolynomial.simplify

let exp_poly (((gt, l), var): RV.t): RealPolynomial.t =
  let transitions = 
    GeneralTransition.transitions gt
    |> TransitionSet.filter (Location.equal l % Transition.target)
  in
  let total_probability = 
    TransitionSet.fold (fun t -> OurFloat.(+) (Transition.label t |> TransitionLabel.probability) ) transitions OurFloat.zero
  in
  let transitions_with_prob = 
    TransitionSet.to_list transitions
    |> List.map (fun t -> (t, Transition.label t |> fun label -> OurFloat.(TransitionLabel.probability label / total_probability)))
  in

  let handle_update_element ue = 
    match ue with
    | TransitionLabel.UpdateElement.Poly p ->
        RealPolynomial.of_intpoly p
    | TransitionLabel.UpdateElement.Dist d ->
        ProbDistribution.expected_value d
  in

  let handle_transition trans = 
    Transition.label trans
    |> flip TransitionLabel.update var
    |? TransitionLabel.UpdateElement.Poly (Polynomial.of_var var)
    |> handle_update_element
  in
  transitions_with_prob
  |> List.map (fun (t,p) -> (p, handle_transition t))
  |> List.map (fun (p,up) -> RealPolynomial.(of_constant p * up))
  |> List.enum
  |> RealPolynomial.sum
  |> simplify_poly_with_guard (GeneralTransition.guard gt)

let det_update kind gt (transition,var): RealPolynomial.t option = 
  let handle_update_element ue = 
    match (ue,kind) with
    | (TransitionLabel.UpdateElement.Poly p,_) -> Some (RealPolynomial.of_intpoly p)
    | (TransitionLabel.UpdateElement.Dist d, `Upper) -> ProbDistribution.deterministic_upper_polynomial d |> Option.map RealPolynomial.of_intpoly 
    | (TransitionLabel.UpdateElement.Dist d, `Lower) -> ProbDistribution.deterministic_lower_polynomial d |> Option.map RealPolynomial.of_intpoly 
  in
  Transition.label transition
  |> flip TransitionLabel.update var
  |> flip Option.bind handle_update_element
  |> Option.map (simplify_poly_with_guard (GeneralTransition.guard gt))

let vars rv = 
  let var = RV.variable rv in 
  let gt = RV.transition rv |> RVTransitions.TransitionForExpectedSize.gt in
  let exp_vars = exp_poly rv |> RealPolynomial.vars in
  let update_vars kind = 
    GeneralTransition.transitions gt
    |> TransitionSet.to_list
    |> List.map (fun t -> det_update kind gt (t,var))
    |> List.fold_left
         (fun varset poly_opt ->
           VarSet.union (Option.map (RealPolynomial.vars % simplify_poly_with_guard (GeneralTransition.guard gt))
                          poly_opt
                         |? VarSet.empty) varset)
          VarSet.empty
  in
  VarSet.union (update_vars `Lower) (update_vars `Upper)
  |> VarSet.union (exp_vars)
