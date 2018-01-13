open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open Program.Types
   
module SMTSolver = SMT.Z3Solver
module Valuation = Valuation.Make(OurInt)
  
type measure = [ `Cost | `Time ] [@@deriving show]

type t = {
    pol : Location.t -> Polynomial.t;
    strictly_decreasing : Transition.t list;
    transitions : Transition.t list;
  }

let one = ParameterPolynomial.one
  
let logger = Logging.(get PRF)  

let rank f = f.pol
           
let strictly_decreasing f = f.strictly_decreasing
                          
let transitions f = f.transitions

let pol_to_string (locations: Location.t list) (content_to_string: 'a -> string) (pol: Location.t -> 'a) =
  locations |> List.map (fun l -> Location.to_string l ^ ": " ^ content_to_string (pol l)) |> String.concat ", "  

let to_string {pol; strictly_decreasing; transitions} =
  let locations = transitions |> List.enum |> Program.locations |> List.of_enum in
  "pol: [" ^ pol_to_string locations Polynomial.to_string pol ^ "] T'>: " ^ (List.map Transition.to_id_string strictly_decreasing |> String.concat ", ")

let find measure vars transitions appr =

  (** Farkas Lemma applied to a linear constraint and a cost function given as System Ax<= b, cx<=d. A,b,c,d are the inputs *)
  let apply_farkas a_matrix b_right c_left d_right =
    let num_of_fresh = List.length b_right in
    let fresh_vars = Var.fresh_id_list Var.Real num_of_fresh in
    let dual_constr = Constraint.dualise fresh_vars a_matrix c_left in
    let cost_constr = Polynomial.of_coeff_list b_right fresh_vars in
    Constraint.Infix.(dual_constr && cost_constr <= d_right)
  in
  
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
  in
  
  (** Given a list of variables an affine template-polynomial is generated*)            
  let ranking_template (vars: VarSet.t): ParameterPolynomial.t * Var.t list =
    let vars = VarSet.elements vars in
    let num_vars = List.length vars in
    let fresh_vars = Var.fresh_id_list Var.Int num_vars in
    let fresh_coeffs = List.map Polynomial.of_var fresh_vars in
    let linear_poly = ParameterPolynomial.of_coeff_list fresh_coeffs vars in
    let constant_var = Var.fresh_id Var.Int () in
    let constant_poly = ParameterPolynomial.of_constant (Polynomial.of_var constant_var) in
    (ParameterPolynomial.(linear_poly + constant_poly)),(List.append fresh_vars [constant_var])
  in
  
  let generate_ranking_template vars locations =
    let module PrfTable = Hashtbl.Make(Location) in
    let execute () =
      let ins_loc_prf location =
        (* Each location needs its own ranking template with different fresh variables *)
        let (parameter_poly, fresh_vars) = ranking_template vars in
        (location, parameter_poly, fresh_vars)
      in
      let enum_of_prf = List.map ins_loc_prf locations in
      let prf_table = PrfTable.of_list (List.map (fun (a,b,c)-> (a,b)) enum_of_prf) in
      let varlist = List.flatten (List.map (fun (a,b,c)-> c) enum_of_prf) in
      (PrfTable.find prf_table, varlist)
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "generated_ranking_template", [])
                       ~result:(fun (pol, _) -> pol_to_string locations ParameterPolynomial.to_string pol)
                       execute
  in
                                    
  let as_parapoly label var =
    match TransitionLabel.update label var with
    (** Correct? In the nondeterministic case we just make it deterministic? *)
    | None -> ParameterPolynomial.of_var var
    | Some p -> ParameterPolynomial.of_polynomial p
  in

  let decreaser t =
    match measure with
    | `Cost -> TransitionLabel.cost t
    | `Time -> Polynomial.one
  in

  (* If the cost of the transition is nonlinear, then we have to do it the old way as long as we do not infer nonlinear ranking functions *)
  let strictly_decreasing_constraint (pol : Location.t -> ParameterPolynomial.t) ((l, t, l'): Transition.t): Constraint.t =
    let guard = TransitionLabel.guard t in
    farkas_transform guard ParameterAtom.Infix.(pol l >= ParameterPolynomial.(of_polynomial (decreaser t) + substitute_f (as_parapoly t) (pol l')))
  in
    
  let bounded_constraint (pol : Location.t -> ParameterPolynomial.t) ((l, t, _) : Transition.t): Constraint.t =
    farkas_transform (TransitionLabel.guard t) ParameterAtom.Infix.(pol l >= ParameterPolynomial.of_polynomial (decreaser t))
  in
  
  (** Returns a non increasing constraint for a single transition. *)
  let non_increasing_constraint (pol : Location.t -> ParameterPolynomial.t) (trans : Transition.t): Constraint.t =
    let execute () =
      let (src, trans_label, target) = trans in
      let guard = TransitionLabel.guard trans_label in
      let updated_target = ParameterPolynomial.substitute_f (as_parapoly trans_label) (pol target) in
      let new_atom = ParameterAtom.Infix.(pol src >= updated_target) in
      farkas_transform guard new_atom
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "non increasing constraint", ["transition", Transition.to_id_string trans])
                       ~result:Constraint.to_string
                       execute
  in
  
  (** Returns a constraint such that all transitions must fulfil the non increasing constraint. *)
  let non_increasing_constraints (pol : Location.t -> ParameterPolynomial.t) (transitions : Transition.t list): Constraint.t =
    let execute () =
      transitions
      |> List.map (non_increasing_constraint pol)
      |> Constraint.all
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "determined non_incr constraints", [])
                       ~result:Constraint.to_string
                       execute
  in
     
  (** Returns, if we can add a strictly decreasing constraint to the given constraint such that it is still satisfiable. *)
  let addable_as_strictly_decreasing (pol : Location.t -> ParameterPolynomial.t) (constr: Constraint.t) (transition: Transition.t): bool =
    let execute () =
      let open Constraint.Infix in
      (constr && bounded_constraint pol transition && strictly_decreasing_constraint pol transition)
      |> Formula.mk
      |> SMTSolver.satisfiable
    in Logger.with_log logger Logger.DEBUG 
                       (fun () -> "addable as strictly decreasing", ["transition", Transition.to_id_string transition])
                       ~result:Bool.to_string
                       execute
  in
  
  (*Given a set of transitions the pair (constr,bound) is generated. Constr is the constraint for the ranking function and bounded consists of all strictly oriented transitions *)
  let add_strictly_decreasing (pol : Location.t -> ParameterPolynomial.t) (transitions: Transition.t list) (non_incr: Constraint.t): Transition.t list =
    let combine (constr,transitions) transition =
      let open Constraint.Infix in
      let add =
        (constr && bounded_constraint pol transition && strictly_decreasing_constraint pol transition, transition::transitions)
      in
      if addable_as_strictly_decreasing pol constr transition then
        add
      else
        (constr, transitions)
    in
    List.fold_left combine (non_incr,[]) transitions
    |> Tuple2.second
  in

  (** Tries to add a single transition from the given list as strictly decreasing transition to the given constraint.
      This should always lead to better or equal results than searching for sets of strictly decreasing transitions. *)
  let single_strictly_decreasing (pol : Location.t -> ParameterPolynomial.t) (transitions: Transition.t list) (non_incr: Constraint.t): Transition.t list =
    match transitions |> List.find_opt (fun trans -> addable_as_strictly_decreasing pol non_incr trans)  with
    | Some transition -> [transition]
    | None -> []
  in
  
  
  (** Checks if a transition is unbounded *)
  let unbounded appr transition =  
    Bound.is_infinity (Approximation.timebound appr transition)
  in
  
  let find_with (pol, fresh_coeffs) non_increasing_transitions strictly_decreasing_transitions =
    let non_increasing_constraint = non_increasing_constraints pol non_increasing_transitions in
    let strictly_decreasing_transitions = single_strictly_decreasing pol strictly_decreasing_transitions non_increasing_constraint in
    let get_bounded trans = bounded_constraint pol trans in
    let bounded = Constraint.all (List.map get_bounded strictly_decreasing_transitions) in (*Problem: How to generate the final constraints*)
    let get_strictly_decreasing trans = strictly_decreasing_constraint pol trans in
    let strictly_decreasing = Constraint.all (List.map get_strictly_decreasing strictly_decreasing_transitions) in
    let model = SMTSolver.get_model ~coeffs_to_minimise:fresh_coeffs (Formula.mk (Constraint.Infix.(non_increasing_constraint && bounded && strictly_decreasing))) in
    {
      pol = (fun loc -> Polynomial.eval_partial (ParameterPolynomial.flatten (pol loc)) model);
      strictly_decreasing = strictly_decreasing_transitions;
      transitions = non_increasing_transitions;
    }
  in
  
  let found rank =
    not (List.is_empty rank.strictly_decreasing)
  in

  let execute () =
    let (pol, fresh_coeffs) = generate_ranking_template vars (transitions |> List.enum |> Program.locations |> List.of_enum |> List.unique) in
    transitions
    |> Set.of_list
    |> Util.powerset
    |> Util.find_map (fun increasing_transitions ->
           transitions
           |> List.filter (unbounded appr)
           |> List.enum
           |> Util.find_map (fun strictly_decreasing_transition ->
                  find_with (pol, fresh_coeffs) (Set.to_list (Set.diff (Set.of_list transitions) increasing_transitions)) (List.singleton strictly_decreasing_transition)
                  |> Option.some
                  |> Option.filter found
                )
         ) 
  in

  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "find ranking function", ["transitions", String.concat ", " (List.map Transition.to_id_string transitions);
                                                       "vars", VarSet.to_string vars])
                  ~result:(Util.option_to_string to_string)
                  execute
   
let find_ measure program appr =
  let transitions =
    program
    |> Program.transitions
    |> TransitionSet.to_list
  in
  find measure (Program.vars program) transitions appr
