open Batteries
open Formulas
open Polynomials
open ProgramTypes
open BoundsInst
open RVTransitions

module RV = RVGTypes.Make_RV (TransitionForExpectedSize)

module NPRV = RVGTypes.Make_RV (Transition)

module IntValuation = Valuation.Make (OurInt)
module Valuation = Valuation.Make (OurFloat)

type hessian_check = Positivesemidefinite | Negativesemidefinite

type poly_matrix = RealPolynomial.t list list

let default = RealBound.infinity

module Solver = SMT.IncrementalZ3Solver
module SolverNonOpt = SMT.Z3Solver

let print_matrix =
  Util.enum_to_string (Util.enum_to_string RealPolynomial.to_string) % List.enum % List.map (List.enum)

(* Using the standard definition of convexity/concavity to handle bounds instead of polynomials *)
let concave_convex_check_v2_ (comp_operator: hessian_check) bound: bool =
  let module VarMap = Map.Make(Var) in
  let vars = RealBound.vars bound in
  (* vector a *)
  let a =
    VarMap.empty
    |> VarSet.fold (fun v -> VarMap.add v (Var.fresh_id Var.Real () |> RealPolynomial.of_var)) vars
  in
  let b =
    VarMap.empty
    |> VarSet.fold (fun v -> VarMap.add v (Var.fresh_id Var.Real () |> RealPolynomial.of_var)) vars
  in
  let lambda = Var.fresh_id Var.Real () |> RealPolynomial.of_var in
  let lefthand =
    RealBound.substitute_f
      (fun v ->
        let vara = VarMap.find v a in
        let varb = VarMap.find v b in
        RealPolynomial.( lambda * vara + (one-lambda) * varb) |> RealBound.of_poly)
      bound
  in
  let righthand =
    RealBound.(
      (lambda |> RealBound.of_poly) * (RealBound.substitute_f (RealBound.of_poly % flip VarMap.find a) bound) +
      (RealPolynomial.(one - lambda) |> RealBound.of_poly) * (RealBound.substitute_f (RealBound.of_poly % flip VarMap.find b) bound)
    )
  in
  let constr =
    RealFormula.Infix.(lambda <= RealPolynomial.one && lambda>=RealPolynomial.zero)
  in
  (* find contra *)
  let bound' = RealBound.(lefthand - righthand) in
  let res =
    try
      match comp_operator with
      | Positivesemidefinite ->
          SolverNonOpt.bound_gt_zero constr bound'
      | Negativesemidefinite ->
          SolverNonOpt.bound_lt_zero constr bound'
    with
      (* TODO *)
      Failure _ -> true
  in
  Bool.neg res

(* a multivariate polynome f is concave (convexe) iff
 * its hessian matrix is negative semi-definite (positive semi-definite) *)
let concave_convex_check (comp_operator: hessian_check) (poly: RealPolynomial.t) : bool =
  let varlist = RealPolynomial.vars poly |> VarSet.to_list in
  let hessian =
    varlist
    |> List.map (fun v1 -> varlist |> List.map (fun v2 -> (v1,v2) ))
    (* Computing derivatives *)
    |> List.map (List.map (fun (v1,v2) -> RealPolynomial.derivative poly v1 |> flip RealPolynomial.derivative v2) )
  in

  let vectx =
    List.init (List.length varlist) (fun _ -> Var.fresh_id Var.Real () |> RealPolynomial.of_var)
  in

  (* Due to symmetry we can multiplicate from the right side *)
  let term_left =
    List.map (RealPolynomial.sum % List.enum % List.map2 RealPolynomial.mul vectx) hessian
  in
  let term_complete =
    List.map2 RealPolynomial.mul term_left vectx
    |> List.enum |> RealPolynomial.sum
  in

  let solver = Solver.create () in
  let constr =
    match comp_operator with
    | Positivesemidefinite -> RealFormula.mk_ge term_complete (RealPolynomial.of_constant (OurFloat.zero))
    | Negativesemidefinite -> RealFormula.mk_le term_complete (RealPolynomial.of_constant (OurFloat.zero))
  in

  Solver.add_real solver (RealFormula.neg constr);
  Solver.unsatisfiable solver

let poly_is_concave =
  concave_convex_check Negativesemidefinite

let poly_is_convexe =
  concave_convex_check Positivesemidefinite

let concave_convex_check_v2 =
  Util.memoize ~extractor:identity (uncurry concave_convex_check_v2_)

let bound_is_concave =
  (curry concave_convex_check_v2) Negativesemidefinite

let bound_is_convexe =
  (curry concave_convex_check_v2) Positivesemidefinite

(* TODO avoid invocation of Z3 for linearity check  *)
let appr_substitution_is_valid bound = bound_is_concave bound && bound_is_convexe bound

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

let bound_nondet_var prog_vars inv var =
  let vars =
    Var.fresh_id_list Var.Real (VarSet.cardinal prog_vars)
  in
  let coeffs = vars |> List.map (RealPolynomial.of_var) in
  let constant_var = Var.fresh_id Var.Real () in
  let constant = constant_var |> RealPolynomial.of_var in
  let para_poly =
    RealParameterPolynomial.of_coeff_list coeffs (VarSet.to_list prog_vars)
    |> RealParameterPolynomial.(add (of_constant constant))
  in
  let nondet_var = RealParameterPolynomial.of_var var in
  let comp_operator kind =
    match kind with
    | `Upper -> Atoms.RealParameterAtom.mk_ge
    | `Lower -> Atoms.RealParameterAtom.mk_le
  in
  let kind_coeff = function
    | `Upper -> RealParameterPolynomial.of_constant (RealPolynomial.of_constant @@ OurNum.of_int 1)
    | `Lower -> RealParameterPolynomial.of_constant (RealPolynomial.of_constant @@ OurNum.of_int (-1))
  in
  let solver = Solver.create () in
  let formula kind =
    Constraints.RealParameterConstraint.farkas_transform inv ((comp_operator kind) RealParameterPolynomial.((kind_coeff kind) * para_poly) nondet_var)
    |> RealFormula.mk
  in
  Solver.add_real solver (formula `Upper);
  Solver.add_real solver (formula `Lower);
  Solver.minimize_absolute solver (constant_var :: vars);
  match Solver.model_real solver with
  | Some model ->
      RealParameterPolynomial.eval_coefficients (flip Valuation.eval model) para_poly
      |> RealBound.of_poly |> RealBound.abs
  | None -> default

let substitute_nondet_var prog_vars temp_vars inv b =
  VarSet.fold (fun v -> RealBound.appr_substitute_abs v @@ bound_nondet_var prog_vars inv v) temp_vars b

let elsb_ program (((gt, l), var): RV.t): RealBound.t =
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
  |> RealBound.of_poly
  |> substitute_nondet_var
      (Program.input_vars program)
      (VarSet.diff (Program.vars program) (Program.input_vars program))
      (GeneralTransition.invariants gt |> Constraints.RealConstraint.of_intconstraint)
  |> RealBound.set_linear_vars_to_probabilistic_and_rest_to_nonprobabilistic
  |> RealBound.abs


let exact_lsb_abs_ program ((t, var): NPRV.t): RealBound.t =
  let handle_update_element ue =
    match ue with
    | TransitionLabel.UpdateElement.Poly p ->
        RealPolynomial.of_intpoly p |> RealBound.of_poly
    | TransitionLabel.UpdateElement.Dist d ->
        RealBound.max
          (RealBound.abs (ProbDistribution.deterministic_upper_bound d |> RealBound.of_intbound))
          (RealBound.abs (ProbDistribution.deterministic_lower_bound d |> RealBound.of_intbound))
  in
  let handle_transition trans =
    Transition.label trans
    |> flip TransitionLabel.update var
    |? TransitionLabel.UpdateElement.Poly (Polynomial.of_var var)
    |> handle_update_element
  in

  t
  |> handle_transition
  |> substitute_nondet_var
      (Program.input_vars program)
      (VarSet.diff (Program.vars program) (Program.input_vars program))
      (TransitionLabel.guard (Transition.label t) |> Constraints.RealConstraint.of_intconstraint)
  |> RealBound.abs

let elsb_memo =
  Util.memoize
    ~extractor:(fun (program,((gt,l),var)) -> (GeneralTransition.id gt, Location.to_string l, Var.to_string var))
    (fun (program,rv) -> elsb_ program rv)

let elsb p rv = elsb_memo (p,rv)

let exact_lsb_abs =
  let exact_lsb_abs_memo =
    Util.memoize
      ~extractor:(fun (program,(t,var)) -> (Transition.id t, Var.to_string var))
      (fun (program,rv) -> exact_lsb_abs_ program rv)
  in
  curry exact_lsb_abs_memo

let vars program rv =
  elsb program rv |> RealBound.vars