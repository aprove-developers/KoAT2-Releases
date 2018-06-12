open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open Valuation

module SMTSolver = SMT.Z3Solver

module Valuation = Valuation.Make(OurInt)

module LexRSM1DMap = Hashtbl.Make(Location)

let logger = Logging.(get LexRSM) 

type constraint_type = [ `Non_Increasing | `Decreasing | `Bounded ] [@@deriving show, eq]

let as_realparapoly label var =
  match TransitionLabel.update label var with
  (** Correct? In the nondeterministic case we just make it deterministic? *)
  | None -> RealParameterPolynomial.of_var var
  | Some p -> p |> RealPolynomial.of_intpoly |> RealParameterPolynomial.of_polynomial

(** Farkas Lemma applied to a linear constraint and a cost function given as System Ax<= b, cx<=d. A,b,c,d are the inputs *)
let real_apply_farkas a_matrix b_right c_left d_right =
  let num_of_fresh = List.length b_right in
  let fresh_vars = Var.fresh_id_list Var.Real num_of_fresh in
  let dual_constr = RealConstraint.dualise fresh_vars a_matrix c_left in
  let cost_constr = RealPolynomial.of_coeff_list b_right fresh_vars in
  RealConstraint.Infix.(dual_constr && cost_constr <= d_right)
  
(** Invokes farkas quantifier elimination. Uses apply_farkas*)
let real_farkas_transform constr param_atom =
  let vars = VarSet.union (RealConstraint.vars constr) (RealParameterAtom.vars param_atom) in
  let costfunction = RealParameterConstraint.lift param_atom in
  let a_matrix = RealConstraint.get_matrix vars constr in
  let b_right = RealConstraint.get_constant_vector constr in
  let c_left = List.flatten (RealParameterConstraint.get_matrix vars costfunction) in
  (* TODO What, if the list is empty? *)
  let (d_right :: _) = RealParameterConstraint.get_constant_vector costfunction in
  real_apply_farkas a_matrix b_right c_left d_right

(** Given a list of variables an affine template-polynomial is generated*)            
let ranking_template (vars: VarSet.t): ParameterPolynomial.t * Var.t list =
  let vars = VarSet.elements vars in
  let num_vars = List.length vars in
  let fresh_vars = Var.fresh_id_list Var.Int num_vars in
  let fresh_coeffs = List.map Polynomial.of_var fresh_vars in
  let linear_poly = ParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Int () in
  let constant_poly = ParameterPolynomial.of_constant (Polynomial.of_var constant_var) in
  ParameterPolynomial.(linear_poly + constant_poly),
  List.append fresh_vars [constant_var]

module TemplateTable = Hashtbl.Make(Location)

let template_table: ParameterPolynomial.t TemplateTable.t = TemplateTable.create 10

let fresh_coeffs: Var.t list ref = ref []

let compute_ranking_templates (vars: VarSet.t) (locations: Location.t list): unit =
  let execute () =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars) = ranking_template vars in
      (location, parameter_poly, fresh_vars)
    in
    let templates = List.map ins_loc_prf locations in
    templates
    |> List.iter (fun (location,polynomial,_) -> TemplateTable.add template_table location polynomial);
    templates
    |> List.map (fun (_,_,fresh_vars)-> fresh_vars)
    |> List.flatten
    |> (fun fresh_vars -> fresh_coeffs := fresh_vars)
  in
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "compute_ranking_templates", [])
                  ~result:(fun () ->
                    template_table
                    |> TemplateTable.enum
                    |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ ParameterPolynomial.to_string polynomial)
                  )
                  execute

let prob_branch_poly (l,t,l') =
    let template = (fun key -> key |> TemplateTable.find template_table |> RealParameterPolynomial.of_int_parapoly) in
    let prob = (l,t,l') |> Transition.label |> TransitionLabel.probability |> OurFloat.of_float in
    RealParameterPolynomial.mul (prob |> RealPolynomial.of_constant |> RealParameterPolynomial.of_polynomial) (RealParameterPolynomial.substitute_f (as_realparapoly t) (template l'))

let expected_poly gtrans =
    TransitionSet.fold (fun trans poly -> RealParameterPolynomial.add (prob_branch_poly trans) poly) (gtrans |> GeneralTransition.transitions) RealParameterPolynomial.zero

let general_transition_constraint_ (constraint_type, gtrans): RealFormula.t =
  let template = (fun key -> key |> TemplateTable.find template_table |> RealParameterPolynomial.of_int_parapoly) in
  let atom =
    match constraint_type with
    | `Non_Increasing ->  RealParameterAtom.Infix.((gtrans |> GeneralTransition.start |> template) >= (expected_poly gtrans))
    | `Decreasing ->      RealParameterAtom.Infix.((gtrans |> GeneralTransition.start |> template) >= (RealParameterPolynomial.add (expected_poly gtrans) (RealParameterPolynomial.of_polynomial RealPolynomial.one)))
    | `Bounded ->         RealParameterAtom.Infix.((gtrans |> GeneralTransition.start |> template) >= RealParameterPolynomial.of_polynomial RealPolynomial.zero)    
  in
  real_farkas_transform (gtrans |> GeneralTransition.guard |> RealConstraint.of_intconstraint) atom
  |> RealFormula.mk

let general_transition_constraint = Util.memoize ~extractor:(Tuple2.map2 GeneralTransition.id) general_transition_constraint_
  
let general_transitions_constraint (constraint_type: constraint_type) (transitions : GeneralTransition.t list): RealFormula.t =
  transitions
  |> List.map (fun t -> general_transition_constraint (constraint_type, t))
  |> RealFormula.all
  
let non_increasing_constraint transition =
  general_transition_constraint (`Non_Increasing, transition)

let non_increasing_constraints transitions =
  general_transitions_constraint `Non_Increasing (GeneralTransitionSet.to_list transitions)
  
let bounded_constraint transition =
  general_transition_constraint (`Bounded, transition)

let decreasing_constraint transition =
  general_transition_constraint (`Decreasing, transition)

let make_LexRSM1DMap transitions valuation =
  let return_map = LexRSM1DMap.create 10 in
  ignore(
  transitions
  |> GeneralTransitionSet.to_list
  |> List.map GeneralTransition.start
  |> List.map (fun location -> 
      TemplateTable.find template_table location
      |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)
      |> LexRSM1DMap.add return_map location
  ));
  return_map


module Solver = SMT.IncrementalZ3Solver

let rec backtrack_1d = function
  | ([],n,ys,solver) -> (n,ys)
  | (x::xs,n,ys,solver) -> 
          Solver.push solver;
          let decr = decreasing_constraint x in
          Logger.(log logger INFO (fun () -> "add constraint", ["Constraint:",  RealFormula.to_string decr]));
          Solver.add_real solver decr;
          if Solver.satisfiable solver then (
            let (n1, ys1) = backtrack_1d (xs, n+1, (GeneralTransitionSet.add x ys), solver) in
            Solver.pop solver;
            let (n2, ys2) = backtrack_1d (xs, n, ys, solver) in
            if n1 >= n2 then
              (n1, ys1)
            else
              (n2, ys2)
          ) else (
            Solver.pop solver;
            backtrack_1d (xs, n, ys, solver)
          )

let find_1d_lexrsm transitions remaining_transitions =
  let solver = Solver.create () 
  and remaining_list = remaining_transitions |> GeneralTransitionSet.to_list
  and transition_list = transitions |> GeneralTransitionSet.to_list
  in
  (*Correct? Everything must be non increasing and bounded by 0?*)
  (*TODO: replace remaining with all for 0-bounded lexrsm map*)
  ignore(remaining_list |> List.map (fun gtrans -> 
                              Solver.add_real solver (non_increasing_constraint gtrans);
                              Solver.add_real solver (bounded_constraint gtrans)));
  if Solver.satisfiable solver then
    let (n, ranked) = backtrack_1d (remaining_list, 0, GeneralTransitionSet.empty, solver) in
    ranked |> GeneralTransitionSet.to_list |> List.map (fun gtrans -> Solver.add_real solver (decreasing_constraint gtrans)) |> ignore;
    Solver.minimize_absolute solver !fresh_coeffs; (* Check if minimization is forgotten. *)
    if n = 0 then 
      None
    else
      Solver.model solver
      |> Option.map (make_LexRSM1DMap remaining_transitions)
      |> Option.map (fun map -> (ranked, map))
  else
    None

let lexrsm1dmap_to_string map = 
  map
  |> LexRSM1DMap.to_list
  |> List.map (fun (key, value) -> String.concat ": " [key |> Location.to_string; value |> Polynomial.to_string])
  |> String.concat ", "

let lexrsmmap_to_string map =
  map
  |> List.map lexrsm1dmap_to_string
  |> String.concat "; "

let rec find_lexrsm transitions remaining =
  if GeneralTransitionSet.is_empty remaining then
    []
  else
    let optionmap = find_1d_lexrsm transitions remaining in
    if Option.is_none optionmap then
      []
    else
      let (ranked, map) = Option.get optionmap in
      Logger.(log logger INFO (fun () -> "add 1d map", [
                                               "ranked transitions:", GeneralTransitionSet.to_string ranked;
                                               "map", lexrsm1dmap_to_string map]));
      map :: find_lexrsm transitions (GeneralTransitionSet.diff remaining ranked)

let test program = 
    print_string("\n");
    print_string("Generalized Transitions:\n");
    print_string(program |> Program.generalized_transitions |> GeneralTransitionSet.to_string);
    print_string("\n");
    if TemplateTable.is_empty template_table then
      compute_ranking_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    let transitions = Program.generalized_transitions program in 
    find_lexrsm transitions transitions
    |> (function |[] -> print_string("no lexrsm map found")
                 |xs -> xs |> lexrsmmap_to_string |> print_string);
    print_string("\n");