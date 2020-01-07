open Batteries
open Formulas
open Polynomials
open ProgramTypes
open BoundsInst
open RVTransitions

module RV = RVGTypes.Make_RV (TransitionForExpectedSize)

module NPRV = RVGTypes.RVG.RV

module IntValuation = Valuation.Make (OurInt)
module Valuation = Valuation.Make (OurFloat)

let logger = Logging.(get ELSB)

type concave_convexe_op = Convexe | Concave

type poly_matrix = RealPolynomial.t list list

let default = RealBound.infinity

module Solver = SMT.IncrementalZ3Solver
module SolverNonOpt = SMT.Z3Solver

let print_matrix =
  Util.enum_to_string (Util.enum_to_string RealPolynomial.to_string) % List.enum % List.map (List.enum)


type concave_convexe_cache = (concave_convexe_op * BoundsInst.RealBound.t, bool) Hashtbl.t
type elsb_bound_cache = (int * string * string, BoundsInst.RealBound.t * BoundsInst.RealBound.t) Hashtbl.t
type elsb_cache = (concave_convexe_cache * elsb_bound_cache)

let new_cache: unit -> elsb_cache = fun () -> (Hashtbl.create 10, Hashtbl.create 10)
let get_concave_convexe (cache: elsb_cache) = Tuple2.first cache
let get_elsb_bound (cache: elsb_cache) = Tuple2.second cache

(** Using the standard definition of convexity/concavity to handle bounds instead of polynomials.
   Note that our bounds constist of non-concave terms like abs(X). However our bounds bound exactly theses terms.
   Therefore we substitute all terms like abs(X) by fresh variables v' and then check if the bound is concave
   w.r.t. these newly generated variables. Furthermore  the concavity/convexity needs only to be checked for a,b geq 0
   in f(lambda * a + (1-lambda) * b) op lambda * f(a) + (1-lambda) * f(b)
*)
let concave_convex_check_v2_ (comp_operator: concave_convexe_op) bound: bool =
  let module VarMap = Map.Make(Var) in
  let vars = RealBound.vars bound in
  (* vector a *)
  let a =
    VarSet.fold (fun v -> VarMap.add v (Var.fresh_id Var.Real () |> RealPolynomial.of_var))
      vars VarMap.empty
  in
  let b =
    VarSet.fold (fun v -> VarMap.add v (Var.fresh_id Var.Real () |> RealPolynomial.of_var))
      vars VarMap.empty
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
    (* add constraints that require a,b => 0*)
    |> VarMap.fold (fun _ poly -> RealFormula.mk_and (RealFormula.Infix.(poly >= RealPolynomial.zero))) a
    |> VarMap.fold (fun _ poly -> RealFormula.mk_and (RealFormula.Infix.(poly >= RealPolynomial.zero))) b
  in
  (* find contra *)
  let bound' = RealBound.(lefthand - righthand) in
  let res =
    try
      match comp_operator with
      | Convexe ->
          SolverNonOpt.bound_gt_zero constr bound'
      | Concave ->
          SolverNonOpt.bound_lt_zero constr bound'
    with
      Failure _ -> false
  in
  Bool.neg res

(* a multivariate polynome f is concave (convexe) iff
 * its hessian matrix is negative semi-definite (positive semi-definite) *)
let concave_convex_check (comp_operator: concave_convexe_op) (poly: RealPolynomial.t) : bool =
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
    | Convexe -> RealFormula.mk_ge term_complete (RealPolynomial.of_constant (OurFloat.zero))
    | Concave -> RealFormula.mk_le term_complete (RealPolynomial.of_constant (OurFloat.zero))
  in

  Solver.add_real solver (RealFormula.neg constr);
  Solver.unsatisfiable solver

let poly_is_concave =
  concave_convex_check Concave

let poly_is_convexe =
  concave_convex_check Convexe

let concave_convex_check_v2 cache =
  Util.memoize (get_concave_convexe cache) ~extractor:identity (uncurry concave_convex_check_v2_)

let bound_is_concave cache =
  (curry (concave_convex_check_v2 cache)) Concave

let bound_is_convexe cache =
  (curry (concave_convex_check_v2 cache)) Convexe

(* TODO avoid invocation of Z3 for linearity check  *)
let appr_substitution_is_valid cache bound = (bound_is_concave cache bound) && (bound_is_convexe cache bound)

let eliminate_var other_vars inv var =
  let new_vars =
    Var.fresh_id_list Var.Real (VarSet.cardinal other_vars)
  in
  let coeffs = new_vars |> List.map (RealPolynomial.of_var) in
  let constant_var = Var.fresh_id Var.Real () in
  let constant = constant_var |> RealPolynomial.of_var in
  let para_poly =
    RealParameterPolynomial.of_coeff_list coeffs (VarSet.to_list other_vars)
    |> RealParameterPolynomial.(add (of_constant constant))
  in
  let comp_operator = function
    | `Upper -> Atoms.RealParameterAtom.mk_ge
    | `Lower -> Atoms.RealParameterAtom.mk_le
  in
  let kind_coeff = function
    | `Upper -> RealParameterPolynomial.of_constant (RealPolynomial.of_constant @@ OurNum.of_int 1)
    | `Lower -> RealParameterPolynomial.of_constant (RealPolynomial.of_constant @@ OurNum.of_int (-1))
  in
  let var_poly = RealParameterPolynomial.of_var var in
  let solver = Solver.create () in
  let formula kind =
    Constraints.RealParameterConstraint.farkas_transform inv ((comp_operator kind) RealParameterPolynomial.((kind_coeff kind) * para_poly) var_poly)
    |> RealFormula.mk
  in
  Solver.add_real solver @@ formula `Upper;
  Solver.add_real solver @@ formula `Lower;
  Solver.minimize_absolute_with_weight solver ((constant_var,1) :: List.map (fun v -> v,2) new_vars);
  match Solver.model_real solver with
  | Some model ->
      RealParameterPolynomial.eval_coefficients (flip Valuation.eval model) para_poly
      |> fun p -> Some (RealBound.of_poly p)
  | None -> None

let elsb_ program (((gt, l), v): RV.t): RealBound.t * RealBound.t =
  let execute () =
    let transitions =
      GeneralTransition.transitions gt
      |> TransitionSet.filter (Location.equal l % Transition.target)
    in
    let transitions_with_prob =
      TransitionSet.to_list transitions
      |> List.map (fun t -> (t, Transition.label t |> fun label -> OurFloat.(TransitionLabel.probability label)))
    in
    let handle_update_element ue =
      match ue with
      | TransitionLabel.UpdateElement.Poly p ->
          (RealBound.of_poly RealPolynomial.(of_intpoly p - of_var v), RealBound.of_poly @@ RealPolynomial.of_intpoly p)
      | TransitionLabel.UpdateElement.Dist d ->
          (ProbDistribution.expected_value_abs d, RealBound.(of_var v + ProbDistribution.expected_value_abs d))
    in
    let handle_transition trans =
      Transition.label trans
      |> flip TransitionLabel.update v
      |? TransitionLabel.UpdateElement.Poly (Polynomial.of_var v)
      |> handle_update_element
    in
    let bound_nondet_vars =
      VarSet.fold
        (fun v b ->
          let upper_bound =
            Option.default RealBound.infinity
              (eliminate_var
                (Program.input_vars program)
                (Constraints.RealConstraint.of_intconstraint @@ GeneralTransition.guard gt) v)
          in
          RealBound.appr_substitution_abs_one v upper_bound b)
        (VarSet.diff (Program.vars program) (Program.input_vars program))
    in
    let simplify_with_guard b =
      VarSet.fold
        (fun v b' ->
          let other_vars = VarSet.remove v (RealBound.vars b') in
          let inv = Constraints.RealConstraint.of_intconstraint (GeneralTransition.guard gt) in
          let v_subst = Option.default (RealBound.of_var v) (eliminate_var other_vars inv v) in
          RealBound.appr_substitution_abs_one v v_subst b')
        (RealBound.vars b) b
    in
    transitions_with_prob
    |> List.enum
    |> Enum.map (fun (t,p) -> (p, handle_transition t))
    |> Enum.map (fun (p,(bound,bound_plus_var)) -> RealBound.(of_constant p * abs bound, of_constant p * abs bound_plus_var))
    |> Enum.map (fun (b,b_p_v) -> RealBound.simplify_vars_nonnegative b, RealBound.simplify_vars_nonnegative b_p_v)
    |> fun en -> (RealBound.sum % Enum.map Tuple2.first @@ Enum.clone en, RealBound.sum % Enum.map Tuple2.second @@ Enum.clone en)
    |> fun (b,b_v_p) -> (bound_nondet_vars b, bound_nondet_vars b_v_p)
    |> fun (b,b_v_p) -> (simplify_with_guard b, simplify_with_guard b_v_p)
    |> fun (b,b_v_p) -> (RealBound.simplify_vars_nonnegative b, RealBound.simplify_vars_nonnegative b_v_p)
  in
  Logger.with_log
    logger Logger.DEBUG
    (fun () -> "elsb",["gt", GeneralTransition.to_id_string gt; "l", Location.to_string l; "var", Var.to_string v])
    ~result:(fun (elsb, elsb_plus_var) -> "elsb: "^ RealBound.to_string elsb ^ " elsb_plus_var: "^ RealBound.to_string elsb_plus_var)
    execute

let elsb cache =
  curry @@ Util.memoize (get_elsb_bound cache)
    ~extractor:(fun (program,((gt,l),var)) -> (GeneralTransition.id gt, Location.to_string l, Var.to_string var))
    (fun (program,rv) -> elsb_ program rv)

let vars cache program rv =
  Tuple2.second RealBound.(elsb cache program rv) |> RealBound.vars
