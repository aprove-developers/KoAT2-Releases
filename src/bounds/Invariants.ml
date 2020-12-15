(** Constraints are based on https://ieeexplore.ieee.org/document/6679413 *)
open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes

(* let cache = Util.cache ~extractor:(Tuple3.map3 Transition.id) *)
        
let logger = Logging.(get Inv)  

type constraint_type = [ `Initiation | `Disability | `Consecution | `Non_Increasing | `Decreasing | `Bounded ] [@@deriving show, eq]

type measure = [ `Cost | `Time ] [@@deriving show, eq]

module Valuation = Valuation.Make(OurFloat)

(** Ranking templates location wise *)
module TemplateTable = Hashtbl.Make(Location)

let cache = Util.cache ~extractor:(Tuple3.map3 Transition.id)

(** Given a list of variables an affine template-polynomial is generated*)            
let invariant_template (vars: VarSet.t): RealParameterPolynomial.t * Var.t list =
  let vars = VarSet.elements vars in
  let num_vars = List.length vars in
  let fresh_vars = Var.fresh_id_list Var.Real num_vars in
  let fresh_coeffs = List.map RealPolynomial.of_var fresh_vars in
  let linear_poly = RealParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Real () in
  let constant_poly = RealParameterPolynomial.of_constant (RealPolynomial.of_var constant_var) in
  RealParameterPolynomial.(linear_poly + constant_poly),
  List.append fresh_vars [constant_var]

let fresh_coeffs: Var.t list ref = ref []

let template_table_inv: RealParameterPolynomial.t TemplateTable.t = TemplateTable.create 10
                                 
let compute_invariant_templates (vars: VarSet.t) (locations: Location.t list): unit =
  let execute () =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars) = invariant_template vars in
      (location, parameter_poly, fresh_vars)
    in
    let templates = List.map ins_loc_prf locations in
    templates
    |> List.iter (fun (location,polynomial,_) -> TemplateTable.add template_table_inv location polynomial);
    templates
    |> List.map (fun (_,_,fresh_vars)-> fresh_vars)
    |> List.flatten
    |> (fun fresh_vars -> fresh_coeffs := fresh_vars)
  in
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "compute_invariant_templates", [])
                  ~result:(fun () ->
                    template_table_inv
                    |> TemplateTable.enum
                    |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ RealParameterPolynomial.to_string polynomial)
                  )
                  execute

(** Invariants are stored location wise.  If no invariant is stored for some location, we return true. *)
let table_inv: RealConstraint.t TemplateTable.t = TemplateTable.create 10

(** An invariant template is a RealConstraint. *)
let get_inv location = 
  location
  |> TemplateTable.find_option table_inv
  |> Option.map identity
  |? RealConstraint.mk_true

let inv_from_valuation valuation location =
  location
  |> TemplateTable.find template_table_inv
  |> RealParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurFloat.zero)

(* The polynomial p is constructed and the constraint p <= 0 is stored. *)
let store_inv valuation (l,t,l') =
  inv_from_valuation valuation l
  |> RealConstraint.mk_ge RealPolynomial.zero
  |> RealConstraint.mk_and (get_inv l) 
  |> TemplateTable.add table_inv l 


let store_inv_set valuation transitions =
  transitions
  |> TransitionSet.iter (fun (l,t,l') ->
    inv_from_valuation valuation l
    |> RealConstraint.mk_ge RealPolynomial.zero
    |> TemplateTable.add table_inv l)

let as_parapoly label var =
  match TransitionLabel.update label var with
  (** Correct? In the nondeterministic case we just make it deterministic? *)
  | None -> RealParameterPolynomial.of_var var
  | Some p -> RealParameterPolynomial.of_intpoly p

let decreaser measure t =
  match measure with
  | `Cost -> TransitionLabel.cost t
  | `Time -> Polynomial.one

let transition_constraint_ (measure, constraint_type, (l,t,l')) template: RealFormula.t =
  let template_inv = TemplateTable.find template_table_inv in
  let atom =
    match constraint_type with
    | `Initiation -> RealParameterAtom.Infix.(RealParameterPolynomial.zero >= RealParameterPolynomial.substitute_f (as_parapoly t) (template_inv l'))
    | `Disability -> RealParameterAtom.Infix.(RealParameterPolynomial.zero >= RealParameterPolynomial.one)
    | `Consecution -> RealParameterAtom.Infix.(RealParameterPolynomial.zero >= RealParameterPolynomial.substitute_f (as_parapoly t) (template_inv l'))
    | `Non_Increasing -> RealParameterAtom.Infix.(template l >= RealParameterPolynomial.substitute_f (as_parapoly t) (template l'))
    | `Decreasing -> RealParameterAtom.Infix.(template l >= RealParameterPolynomial.(of_intpoly (decreaser measure t) + substitute_f (as_parapoly t) (template l')))
    | `Bounded -> RealParameterAtom.Infix.(template l >= RealParameterPolynomial.of_intpoly (decreaser measure t))  
  in
  let constr =
    match constraint_type with
    (** TODO build up a look up table for already calculated invariants and use them as initiation. *)
    | `Initiation -> RealParameterConstraint.(mk_and (of_realconstraint(get_inv l)) (of_intconstraint (TransitionLabel.guard t)))
    | _ -> RealParameterConstraint.(mk_and (mk_and 
                                      (of_realconstraint(get_inv l))
                                      (mk [RealParameterAtom.Infix.(template_inv l <= RealParameterPolynomial.zero)]))
                                      (of_intconstraint (TransitionLabel.guard t)))
  in
  RealParameterConstraint.farkas_transform constr atom
  |> RealFormula.mk
  |> tap (fun formula -> Logger.(log logger DEBUG (fun () -> "transition_constraint_", [
                                              "measure", show_measure measure;
                                              "constraint_type", show_constraint_type constraint_type;
                                              "transition", Transition.to_id_string (l,t,l');
                                              "constraint ", RealParameterConstraint.to_string constr;
                                              "atom", RealParameterAtom.to_string atom;
                                              "formula: ", RealFormula.to_string formula])))
       
let transition_constraint = cache#add transition_constraint_ 
  
let transitions_constraint measure (constraint_type: constraint_type) (transitions : Transition.t list) template: RealFormula.t =
  transitions
  |> List.map (fun t -> transition_constraint (measure, constraint_type, t) template)
  |> RealFormula.all
  
let non_increasing_constraint measure transition template =
  transition_constraint (measure, `Non_Increasing, transition) template

let non_increasing_constraints measure transitions template =
  transitions_constraint measure `Non_Increasing (TransitionSet.to_list transitions) 
  
let bounded_constraint measure transition template =
  transition_constraint (measure, `Bounded, transition) template

let decreasing_constraint measure transition template =
  transition_constraint (measure, `Decreasing, transition) template

let initiation_constraint measure transition template =
  transition_constraint (measure, `Initiation, transition) template

let disability_constraint measure transition template =
  transition_constraint (measure, `Disability, transition) template

let consecution_constraint measure transition template =
  transition_constraint (measure, `Consecution, transition) template

let consecution_constraints measure transitions template =
  transitions_constraint measure `Consecution (TransitionSet.to_list transitions)