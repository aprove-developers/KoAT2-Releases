open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open Program.Types
   
module SMTSolver = SMT.Z3Solver
module Valuation = Valuation.Make(OurInt)
  
type measure = [ `Cost | `Time ] [@@deriving show, eq]

type constraint_type = [ `Non_Increasing | `Decreasing | `Bounded ]
             
type t = {
    pol : Location.t -> Polynomial.t;
    decreasing : Transition.t;
    non_increasing : Transition.t list;
  }

let one = ParameterPolynomial.one
  
let logger = Logging.(get PRF)  

let rank f = f.pol
           
let decreasing f = f.decreasing
                          
let transitions f = f.non_increasing

let pol_to_string (locations: Location.t list) (content_to_string: 'a -> string) (pol: Location.t -> 'a) =
  locations
  |> List.enum
  |> Util.enum_to_string (fun l -> Location.to_string l ^ ": " ^ content_to_string (pol l))

let to_string {pol; decreasing; non_increasing} =
  let locations = non_increasing |> List.enum |> Program.locations |> List.of_enum |> List.unique ~eq:Location.equal in
  "{rank:" ^ pol_to_string locations Polynomial.to_string pol ^ ";decreasing:" ^ Transition.to_id_string decreasing ^ "}"

let as_parapoly label var =
  match TransitionLabel.update label var with
  (** Correct? In the nondeterministic case we just make it deterministic? *)
  | None -> ParameterPolynomial.of_var var
  | Some p -> ParameterPolynomial.of_polynomial p

(** Farkas Lemma applied to a linear constraint and a cost function given as System Ax<= b, cx<=d. A,b,c,d are the inputs *)
let apply_farkas a_matrix b_right c_left d_right =
  let num_of_fresh = List.length b_right in
  let fresh_vars = Var.fresh_id_list Var.Real num_of_fresh in
  let dual_constr = Constraint.dualise fresh_vars a_matrix c_left in
  let cost_constr = Polynomial.of_coeff_list b_right fresh_vars in
  Constraint.Infix.(dual_constr && cost_constr <= d_right)
  
(** Invokes farkas quantifier elimination. Uses apply_farkas*)
let farkas_transform constr param_atom =
  let vars = VarSet.union (Constraint.vars constr) (ParameterAtom.vars param_atom) in
  let costfunction = ParameterConstraint.lift param_atom in
  let a_matrix = Constraint.get_matrix vars constr in
  let b_right = Constraint.get_constant_vector constr in
  let c_left = List.flatten (ParameterConstraint.get_matrix vars costfunction) in
  (* TODO What, if the list is empty? *)
  let (d_right :: _) = ParameterConstraint.get_constant_vector costfunction in
  apply_farkas a_matrix b_right c_left d_right
  
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
  
let ranking_templates (vars: VarSet.t) (locations: Location.t list): (Location.t -> ParameterPolynomial.t) * Var.t list =
  let module TemplateTable = Hashtbl.Make(Location) in
  let ins_loc_prf location =
    (* Each location needs its own ranking template with different fresh variables *)
    let (parameter_poly, fresh_vars) = ranking_template vars in
    (location, parameter_poly, fresh_vars)
  in
  let enum_of_prf = List.map ins_loc_prf locations in
  let prf_table = TemplateTable.of_list (List.map (fun (a,b,c)-> (a,b)) enum_of_prf) in
  let varlist = List.flatten (List.map (fun (a,b,c)-> c) enum_of_prf) in
  (TemplateTable.find prf_table, varlist)
  |> tap (fun (template, _) -> Logger.log logger Logger.DEBUG (fun () -> "ranking_templates", ["result", pol_to_string locations ParameterPolynomial.to_string template]))
  
(** Internal memoization for local size bounds *)
module Ranking_Cache =
  Hashtbl.Make(
      struct
        type t = measure * TransitionSet.t * Transition.t
        let equal (m1,t1,t'1) (m2,t2,t'2) =
          equal_measure m1 m2
          && TransitionSet.equal t1 t2
          && Transition.same t'1 t'2
        let hash (m,t,t') =
          Hashtbl.hash (m,
                        List.map Transition.id (TransitionSet.to_list t),
                        Transition.id t')
      end
    )
   
let table =
  Ranking_Cache.create 10
  
let memoize f =  
  let g x = 
    match Ranking_Cache.find_option table x with
    | Some y -> y
    | None ->
       let y = f x in
       Ranking_Cache.add table x y;
       y
  in g
   
let find_with measure vars non_increasing_transitions decreasing_transition =
  let f (measure, non_increasing_transitions, decreasing_transition) =
    
    let decreaser t =
      match measure with
      | `Cost -> TransitionLabel.cost t
      | `Time -> Polynomial.one
    in

    let transition_constraint (constraint_type: constraint_type) (pol: Location.t -> ParameterPolynomial.t) ((l,t,l'): Transition.t): Constraint.t =
      let atom =
        match constraint_type with
        | `Non_Increasing -> ParameterAtom.Infix.(pol l >= ParameterPolynomial.substitute_f (as_parapoly t) (pol l'))
        | `Decreasing -> ParameterAtom.Infix.(pol l >= ParameterPolynomial.(of_polynomial (decreaser t) + substitute_f (as_parapoly t) (pol l')))
        | `Bounded -> ParameterAtom.Infix.(pol l >= ParameterPolynomial.of_polynomial (decreaser t))      
      in
      farkas_transform (TransitionLabel.guard t) atom
    in
    
    let transitions_constraint (constraint_type: constraint_type) (pol : Location.t -> ParameterPolynomial.t) (transitions : Transition.t list): Constraint.t =
      transitions
      |> List.map (transition_constraint constraint_type pol)
      |> Constraint.all
    in
    
    let locations =
      non_increasing_transitions
      |> TransitionSet.add decreasing_transition
      |> TransitionSet.enum
      |> Program.locations
      |> List.of_enum
      |> List.unique
    in

    let (template, fresh_coeffs) =
      ranking_templates vars locations
    in

    let non_increasing_constraint =
      transitions_constraint `Non_Increasing template (TransitionSet.to_list non_increasing_transitions)
    in
    
    let bounded =
      transition_constraint `Bounded template decreasing_transition
    in

    let decreasing =
      transition_constraint `Decreasing template decreasing_transition
    in

    let valuation =
      Constraint.Infix.(non_increasing_constraint && bounded && decreasing)
      |> Formula.mk
      |> SMTSolver.get_model ~coeffs_to_minimise:fresh_coeffs
    in

    if List.for_all (Valuation.is_defined valuation) fresh_coeffs then
      let rank location =
        location
        |> template 
        |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval var valuation)
      in
      Some {
          pol = rank;
          decreasing = decreasing_transition;
          non_increasing = (TransitionSet.to_list non_increasing_transitions);
        }
    else
      None

  in memoize f (measure, non_increasing_transitions, decreasing_transition)
    
let find measure vars (transitions: TransitionSet.t) (bounded_transitions: TransitionSet.t) =

  let execute () =
    transitions
    |> TransitionSet.powerset
    |> Util.find_map (fun increasing_transitions ->
           TransitionSet.diff transitions bounded_transitions
           |> TransitionSet.enum
           |> Util.find_map (fun decreasing_transition ->
                  find_with measure vars (TransitionSet.diff transitions increasing_transitions) decreasing_transition
                )
         ) 
  in

  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "find ranking function", ["measure", show_measure measure;
                                                       "transitions", TransitionSet.to_string transitions;
                                                       "bounded_transitions", TransitionSet.to_string bounded_transitions;
                                                       "vars", VarSet.to_string vars])
                  ~result:(Util.option_to_string to_string)
                  execute
