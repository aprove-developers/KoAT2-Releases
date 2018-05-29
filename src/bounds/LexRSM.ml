open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes

let logger = Logging.(get PRF) 

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
let ranking_template (vars: VarSet.t): RealParameterPolynomial.t * Var.t list =
  let vars = VarSet.elements vars in
  let num_vars = List.length vars in
  let fresh_vars = Var.fresh_id_list Var.Int num_vars in
  let fresh_coeffs = List.map RealPolynomial.of_var fresh_vars in
  let linear_poly = RealParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Int () in
  let constant_poly = RealParameterPolynomial.of_constant (RealPolynomial.of_var constant_var) in
  RealParameterPolynomial.(linear_poly + constant_poly),
  List.append fresh_vars [constant_var]

module TemplateTable = Hashtbl.Make(Location)

let template_table: RealParameterPolynomial.t TemplateTable.t = TemplateTable.create 10

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
                    |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ RealParameterPolynomial.to_string polynomial)
                  )
                  execute

let prob_branch_poly (l,t,l') =
    let template = TemplateTable.find template_table in
    let prob = (l,t,l') |> Transition.label |> TransitionLabel.probability |> OurFloat.of_float in
    RealParameterPolynomial.mul (prob |> RealPolynomial.of_constant |> RealParameterPolynomial.of_polynomial) (RealParameterPolynomial.substitute_f (as_realparapoly t) (template l'))

let expected_poly gtrans =
    TransitionSet.fold (fun trans poly -> RealParameterPolynomial.add (prob_branch_poly trans) poly) (gtrans |> GeneralTransition.transitions) RealParameterPolynomial.zero

let general_transition_constraint_ (constraint_type, gtrans): RealFormula.t =
  let template = TemplateTable.find template_table in
  let atom =
    match constraint_type with
    | `Non_Increasing -> RealParameterAtom.Infix.((gtrans |> GeneralTransition.start |> template) >= (expected_poly gtrans))
    | `Decreasing -> RealParameterAtom.Infix.((gtrans |> GeneralTransition.start |> template) >= (RealParameterPolynomial.add (RealParameterPolynomial.of_polynomial RealPolynomial.one) (expected_poly gtrans)))
    | `Bounded -> RealParameterAtom.Infix.((gtrans |> GeneralTransition.start |> template) >= RealParameterPolynomial.of_polynomial (RealPolynomial.one))    
  in
  print_string("\n");
  atom |> RealParameterAtom.to_string |> print_string;
  print_string("\n");
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

let test program = 
    if TemplateTable.is_empty template_table then
      compute_ranking_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    program |> Program.generalized_transitions |> GeneralTransitionSet.to_list |> List.map decreasing_constraint |> List.map RealFormula.to_string |> String.concat "; " |> print_string;
    print_string("\n");
    print_string("Generalized Transitions:\n");
    print_string(program |> Program.generalized_transitions |> GeneralTransitionSet.to_string);
    print_string("\n")