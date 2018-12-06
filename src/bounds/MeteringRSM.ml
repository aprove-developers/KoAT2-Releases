open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open Valuation

module SMTSolver = SMT.Z3Opt

module Valuation = Valuation.Make(OurInt)

module MeteringRSMMap = Hashtbl.Make(Location)

let logger = Logging.(get MeteringRSM)

type constraint_type = [ `Metering | `Bounded | `Cond_Diff_Bound | `Inactive] [@@deriving show, eq]

let as_realparapoly label var =
  match TransitionLabel.update label var with
  (** Correct? In the nondeterministic case we just make it deterministic? *)
  | None -> RealParameterPolynomial.of_var var
  | Some (TransitionLabel.UpdateElement.Poly p) -> p |> RealPolynomial.of_intpoly |> RealParameterPolynomial.of_polynomial
  (** TODO In the probabilistic case we make it nondeterminstic? *)
  | Some (TransitionLabel.UpdateElement.Dist d) -> RealParameterPolynomial.of_var var

(** Given a list of variables an affine template-polynomial is generated*)
let metering_template (vars: VarSet.t): ParameterPolynomial.t * Var.t list =
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

let compute_metering_templates (vars: VarSet.t) (locations: Location.t list): unit =
  let execute () =
    let ins_loc_prf location =
      (* Each location needs its own metering template with different fresh variables *)
      let (parameter_poly, fresh_vars) = metering_template vars in
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
                  (fun () -> "compute_metering_templates", [])
                  ~result:(fun () ->
                    template_table
                    |> TemplateTable.enum
                    |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ ParameterPolynomial.to_string polynomial)
                  )
                  execute

let prob_branch_poly ?(diff = RealParameterPolynomial.zero) (l,t,l') =
    let template = (fun key -> key |> TemplateTable.find template_table |> RealParameterPolynomial.of_int_parapoly) in
    let prob = t |> TransitionLabel.probability in
    RealParameterPolynomial.mul (prob |> RealPolynomial.of_constant |> RealParameterPolynomial.of_polynomial) (RealParameterPolynomial.( add (substitute_f (as_realparapoly t) (template l')) (neg diff)))

let expected_poly gtrans =
    TransitionSet.fold (fun trans poly -> RealParameterPolynomial.add (prob_branch_poly trans) poly) (gtrans |> GeneralTransition.transitions) RealParameterPolynomial.zero

let diff_poly start_template gtrans=
  let execute() =
    gtrans
    |> GeneralTransition.transitions
    |> TransitionSet.to_list
    |> List.map (prob_branch_poly ~diff:start_template)
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "diff_poly", [])
                    ~result:(fun polylist ->
                      polylist
                      |> List.map RealParameterPolynomial.to_string
                      |> String.concat ","
                    )
                    execute

let bound_absolute poly =
  let execute () =
    let fresh_var = Var.(fresh_id Int ()) in
    let fresh_const = fresh_var |> RealPolynomial.of_var |> RealParameterPolynomial.of_constant in
                    [RealParameterAtom.Infix.(poly <= fresh_const); RealParameterAtom.Infix.(poly >= RealParameterPolynomial.(neg (fresh_const))); RealParameterAtom.Infix.(fresh_const > RealParameterPolynomial.zero)]
  in
    Logger.with_log logger Logger.DEBUG
                  (fun () -> "bound_absolute", [])
                    ~result:(fun atomlist ->
                      atomlist
                      |> List.map RealParameterAtom.to_string
                      |> String.concat ","
                    )
                    execute

let general_transition_constraint_ (constraint_type, gtrans): RealFormula.t =
  let start_template = (gtrans |> GeneralTransition.start |> TemplateTable.find template_table |> RealParameterPolynomial.of_int_parapoly) in
  let int_guard = (gtrans |> GeneralTransition.guard |> RealConstraint.of_intconstraint) in
  match constraint_type with
  | `Inactive ->
    let atom = RealParameterAtom.Infix.(start_template  <= RealParameterPolynomial.zero) in
    let neg_guard = int_guard |> RealFormula.mk |> RealFormula.neg |> RealFormula.constraints in
    neg_guard
    |> List.map (fun constr -> RealParameterConstraint.farkas_transform constr atom)
    |> List.map RealFormula.mk
    |> List.fold_left RealFormula.mk_or RealFormula.mk_false
    (*|> RealFormula.mk*)

  | _ ->
  let atoms =
    match constraint_type with
    | `Metering ->  [RealParameterAtom.Infix.(start_template <= (RealParameterPolynomial.add (expected_poly gtrans) (RealParameterPolynomial.one)))]
    | `Bounded -> [RealParameterAtom.Infix.(start_template  >= RealParameterPolynomial.one)]
    | `Inactive ->  [RealParameterAtom.Infix.(start_template  <= RealParameterPolynomial.zero)]
    | `Cond_Diff_Bound ->
        gtrans
      |> diff_poly start_template
      |> List.map bound_absolute
      |> List.flatten
  in
  atoms
  |> List.map (RealParameterConstraint.farkas_transform int_guard)
  |> List.fold_left (RealConstraint.mk_and) (RealConstraint.mk_true)
  |> RealFormula.mk


let general_transition_constraint = Util.memoize ~extractor:(Tuple2.map2 GeneralTransition.id) general_transition_constraint_

let bounded_constraint transition =
  let execute () =
    general_transition_constraint (`Bounded, transition)
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> ("bounded_constraint of "^(GeneralTransition.to_string transition)), [])
                  ~result:RealFormula.to_string
                  execute

let metering_constraint transition =
  let execute () =
    general_transition_constraint (`Metering, transition)
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> ("metering_constraint of "^(GeneralTransition.to_string transition)), [])
                  ~result:RealFormula.to_string
                  execute

let inactive_constraint transition =
  let execute () =
    general_transition_constraint (`Inactive, transition)
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> ("inactive_constraint of "^(GeneralTransition.to_string transition)), [])
                  ~result:RealFormula.to_string
                  execute

let cond_diff_bounded_constraint transition =
  let execute () =
    general_transition_constraint (`Cond_Diff_Bound, transition)
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> ("cond_diff_bounded_constraint of "^(GeneralTransition.to_string transition)), [])
                  ~result:RealFormula.to_string
                  execute

let add_to_MeteringRSMMap map transitions valuation =
  transitions
  |> GeneralTransitionSet.start_locations
  |> LocationSet.to_list
  |> List.map (fun location ->
      TemplateTable.find template_table location
      |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)
      |> (fun poly ->
        let opt = MeteringRSMMap.find_option map location in
        if Option.is_some opt then
          poly
          |> MeteringRSMMap.replace map location
          |> ignore
        else
          poly
          |> MeteringRSMMap.add map location
          |> ignore
      )
    )
  |> ignore


module Solver = SMT.IncrementalZ3Solver

let find_metering_rsm_model transitions (*remaining_transitions*) =
  let solver = Solver.create ()
  and remaining_list = transitions |> GeneralTransitionSet.to_list
  in
  ignore(remaining_list |> List.map (fun gtrans ->
                              Solver.add_real solver (metering_constraint gtrans);
                              Solver.add_real solver (bounded_constraint gtrans);
                              Solver.add_real solver (inactive_constraint gtrans);
                              Solver.add_real solver (cond_diff_bounded_constraint gtrans)));
  if Solver.satisfiable solver then(
    (*Solver.maximize_vars solver !fresh_coeffs;*)
    Solver.model solver
  )
  else
    None

let find_metering_rsm map transitions =
  let model = find_metering_rsm_model transitions in
  if Option.is_none model then
    None
  else
    let valuation = Option.get model in
      add_to_MeteringRSMMap map transitions valuation;
      Some map


let metering_rsmmap_to_string map =
  map
  |> MeteringRSMMap.to_list
  |> List.map (fun (loc, poly) -> String.concat ": " [loc |> Location.to_string; poly |> (Polynomial.to_string)])
  |> String.concat "; "

let reset () =
  TemplateTable.clear template_table

let test program =
(*    print_string("Generalized Transitions:\n");
    print_string(program |> Program.generalized_transitions |> GeneralTransitionSet.to_string);
    print_string("\n");*)
    if TemplateTable.is_empty template_table then
      compute_metering_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    let transitions = Program.generalized_transitions program in
    let metering_map = MeteringRSMMap.create 10 in
    find_metering_rsm metering_map transitions
    |> fun option -> Option.map (fun metering_map -> (metering_rsmmap_to_string metering_map) ^ "\n") option |? "no metering function found\n" |> print_string

let compute_map gtrans =
  let transitions = gtrans |> GeneralTransitionSet.singleton
  and metering_map = MeteringRSMMap.create 10 in
  let execute () =
    find_metering_rsm metering_map transitions
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute_map", [])
                  ~result:(fun option -> Option.map (fun metering_map -> (metering_rsmmap_to_string metering_map) ) option |? "no metering function found")
                  execute


let compute_metering_function gtrans =
  let execute () =
    let metering_map = compute_map gtrans |> Option.get in
    MeteringRSMMap.find metering_map (GeneralTransition.start gtrans)
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute_metering_function", ["transition:", gtrans |> GeneralTransition.to_string;])
                  ~result:(Polynomial.to_string)
                  execute

let find program =
  let execute () =
    if TemplateTable.is_empty template_table then
      compute_metering_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    program
    |> Program.generalized_transitions
    |> GeneralTransitionSet.filter GeneralTransition.is_loop
    |> GeneralTransitionSet.elements
    |> List.map compute_metering_function
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "find_metering_function", ["transitions", program |> Program.generalized_transitions |> GeneralTransitionSet.filter GeneralTransition.is_loop |> GeneralTransitionSet.to_string;])
                  ~result: (fun polylist -> polylist |> List.map (Polynomial.to_string) |> String.concat ",")
                  execute
  |> ignore
