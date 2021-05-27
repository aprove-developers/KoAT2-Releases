(** Constraints are based on https://ieeexplore.ieee.org/document/6679413 *)
open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes

let logger = Logging.(get Inv)

type constraint_type = [ `Initiation | `Disability | `Consecution | `Non_Increasing | `Decreasing ] [@@deriving show, eq]

type measure = [ `Cost | `Time ] [@@deriving show, eq]

module Valuation = Valuation.Make(OurFloat)

(** Ranking templates location wise *)
module TemplateTable = Hashtbl.Make(Location)

let constraint_cache = Util.cache ~extractor:(Tuple4.map4 Transition.id)

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
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute_invariant_templates", [])
                  ~result:(fun () ->
                    template_table_inv
                    |> TemplateTable.enum
                    |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ RealParameterPolynomial.to_string polynomial)
                  )
                  execute

let template_table_is_empty =
  TemplateTable.is_empty template_table_inv

let template_table_clear =
TemplateTable.clear template_table_inv

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
  |> tap (fun inv -> Logger.(log logger INFO (fun () -> "add_inv", ["trans:", Transition.to_id_string (l,t,l'); "inv: ", RealConstraint.to_string inv];)))
  |> TemplateTable.add table_inv l



let store_inv_set valuation transitions =
  transitions
  |> TransitionSet.iter (fun (l,t,l') ->
    inv_from_valuation valuation l
    |> RealConstraint.mk_ge RealPolynomial.zero
    |> RealConstraint.mk_and (get_inv l)
    |> tap (fun inv -> Logger.(log logger INFO (fun () -> "add_inv", ["trans:", Transition.to_id_string (l,t,l'); "inv: ", RealConstraint.to_string inv];)))
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

let log_constraints logger measure constraint_type t constr atom formula =
  Logger.(log logger DEBUG (fun () -> "transition_constraint_", [
                                              "measure", show_measure measure;
                                              "constraint_type", show_constraint_type constraint_type;
                                              "transition", Transition.to_id_string t;
                                              "constraint ", RealParameterConstraint.to_string constr;
                                              "atom", RealParameterAtom.to_string atom;
                                              "formula: ", RealFormula.to_string formula]))

(* Methods define properties of mrf *)
(* method for mrf and functions f_2 to f_d of depth i *)
let transition_constraint_i (template0, template1, measure, constraint_type, (l,t,l')): RealFormula.t =
  let template_inv = TemplateTable.find template_table_inv in
  let poly = RealParameterPolynomial.add (template0 l) (template1 l) in
  let atom =
    match constraint_type with
    | `Initiation -> RealParameterAtom.Infix.(RealParameterPolynomial.zero >= RealParameterPolynomial.substitute_f (as_parapoly t) (template_inv l'))
    | `Disability -> RealParameterAtom.Infix.(RealParameterPolynomial.zero >= RealParameterPolynomial.one)
    | `Consecution -> RealParameterAtom.Infix.(RealParameterPolynomial.zero >= RealParameterPolynomial.substitute_f (as_parapoly t) (template_inv l'))
    | `Non_Increasing -> RealParameterAtom.Infix.(poly >= RealParameterPolynomial.substitute_f (as_parapoly t) (template1 l'))
    | `Decreasing -> RealParameterAtom.Infix.(poly >= RealParameterPolynomial.(RealParameterPolynomial.of_intpoly (decreaser measure t) + substitute_f (as_parapoly t) (template1 l')))
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
  |> tap (fun formula -> log_constraints logger measure constraint_type (l,t,l') constr atom formula)

(* method for mrf and function f_1*)
let transition_constraint_1 (template1, measure, constraint_type, (l,t,l')): RealFormula.t =
  let template_inv = TemplateTable.find template_table_inv in
  let atom =
    match constraint_type with
      | `Initiation -> RealParameterAtom.Infix.(RealParameterPolynomial.zero >= RealParameterPolynomial.substitute_f (as_parapoly t) (template_inv l'))
      | `Disability -> RealParameterAtom.Infix.(RealParameterPolynomial.zero >= RealParameterPolynomial.one)
      | `Consecution -> RealParameterAtom.Infix.(RealParameterPolynomial.zero >= RealParameterPolynomial.substitute_f (as_parapoly t) (template_inv l'))
      | `Non_Increasing -> RealParameterAtom.Infix.(template1 l >= RealParameterPolynomial.substitute_f (as_parapoly t) (template1 l'))
      | `Decreasing -> RealParameterAtom.Infix.(template1 l >= RealParameterPolynomial.(RealParameterPolynomial.of_intpoly (decreaser measure t) + substitute_f (as_parapoly t) (template1 l')))
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
  |> tap (fun formula -> log_constraints logger measure constraint_type (l,t,l') constr atom formula)


(* method for mrf and function f_d*)
let transition_constraint_d bound (template1, measure, constraint_type, (l,t,l')): RealFormula.t =
  let template_inv = TemplateTable.find template_table_inv in
  let atom =
    match constraint_type with
    | `Decreasing  -> RealParameterAtom.Infix.((template1 l) >= bound)
    | _ -> RealParameterAtom.Infix.(RealParameterPolynomial.one >= RealParameterPolynomial.zero)
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
  |> tap (fun formula -> log_constraints logger measure constraint_type (l,t,l') constr atom formula)


(* use all three functions above combined*)
let transition_constraint_ (depth, measure, constraint_type, (l,t,l')) templates: RealFormula.t =
  let res = ref RealFormula.mk_true in
  for i = 1 to (depth - 1) do
    res := ((List.nth templates (i - 1)), (List.nth templates i), measure, constraint_type, (l,t,l'))
           |> transition_constraint_i
           |> RealFormula.mk_and !res
  done;
  res := ((List.nth templates 0), measure, constraint_type, (l,t,l'))
      |> transition_constraint_1
      |> RealFormula.mk_and !res;
  if depth > 1 then
    res := ((List.nth templates (depth - 1)), measure, constraint_type, (l,t,l'))
        |> transition_constraint_d RealParameterPolynomial.zero
        |> RealFormula.mk_and !res
  else
    res := ((List.nth templates (depth - 1)), measure, constraint_type, (l,t,l'))
        |> transition_constraint_d RealParameterPolynomial.one
        |> RealFormula.mk_and !res;
  !res

let transition_constraint = constraint_cache#add transition_constraint_

let transitions_constraint depth measure (constraint_type: constraint_type) (transitions : Transition.t list) templates: RealFormula.t =
  transitions
  |> List.map (fun t -> transition_constraint (depth, measure, constraint_type, t) templates)
  |> RealFormula.all

let non_increasing_constraint depth measure transition =
  transition_constraint (depth, measure, `Non_Increasing, transition)

let non_increasing_constraints depth measure transitions =
  transitions_constraint depth measure `Non_Increasing (TransitionSet.to_list transitions)

let decreasing_constraint depth measure transition =
  transition_constraint (depth, measure, `Decreasing, transition)

let initiation_constraint depth measure transition =
  transition_constraint (depth, measure, `Initiation, transition)

let disability_constraint depth measure transition =
  transition_constraint (depth, measure, `Disability, transition)

let consecution_constraint depth measure transition =
  transition_constraint (depth, measure, `Consecution, transition)

let consecution_constraints depth measure transitions =
  transitions_constraint depth measure `Consecution (TransitionSet.to_list transitions)

let clear_cache =
  constraint_cache#clear