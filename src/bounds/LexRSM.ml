open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open Valuation

module SMTSolver = SMT.Z3Solver

module Valuation = Valuation.Make(OurInt)

module LexRSMMap = Hashtbl.Make(Location)

let logger = Logging.(get LexRSM) 

type constraint_type = [ `Non_Increasing | `Decreasing | `Bounded | `C_ranked ] [@@deriving show, eq]

let as_realparapoly label var =
  match TransitionLabel.update label var with
  (** Correct? In the nondeterministic case we just make it deterministic? *)
  | None -> RealParameterPolynomial.of_var var
  | Some p -> p |> RealPolynomial.of_intpoly |> RealParameterPolynomial.of_polynomial

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

let cbound_template: ParameterPolynomial.t Option.t ref = ref None

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

let compute_cbound_template (vars: VarSet.t): unit =
  let execute () =
    vars
    |> ranking_template
    |> fun (poly, vars) -> cbound_template := Option.some poly
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute_cbound_template", [])
                  ~result:(fun () -> !cbound_template |> Option.get |> ParameterPolynomial.to_string)
                  execute

let prob_branch_poly (l,t,l') =
    let template = (fun key -> key |> TemplateTable.find template_table |> RealParameterPolynomial.of_int_parapoly) in
    let prob = t |> TransitionLabel.probability |> OurFloat.of_float in
    RealParameterPolynomial.mul (prob |> RealPolynomial.of_constant |> RealParameterPolynomial.of_polynomial) (RealParameterPolynomial.substitute_f (as_realparapoly t) (template l'))

let expected_poly gtrans =
    TransitionSet.fold (fun trans poly -> RealParameterPolynomial.add (prob_branch_poly trans) poly) (gtrans |> GeneralTransition.transitions) RealParameterPolynomial.zero

let general_transition_constraint_ (constraint_type, gtrans): RealFormula.t =
  let template = (fun gtrans -> gtrans |> GeneralTransition.start |> TemplateTable.find template_table |> RealParameterPolynomial.of_int_parapoly) in
  let c_template = !cbound_template |> Option.get |> RealParameterPolynomial.of_int_parapoly in
  let atom =
    match constraint_type with
    | `Non_Increasing ->  RealParameterAtom.Infix.((template gtrans) >= (expected_poly gtrans))
    | `Decreasing ->      RealParameterAtom.Infix.((template gtrans) >= (RealParameterPolynomial.add (expected_poly gtrans) (RealParameterPolynomial.of_polynomial RealPolynomial.one)))
    | `Bounded ->         RealParameterAtom.Infix.((template gtrans) >= RealParameterPolynomial.of_polynomial RealPolynomial.zero)
    | `C_ranked ->        RealParameterAtom.Infix.((RealParameterPolynomial.add (template gtrans) c_template) >= (expected_poly gtrans))
  in
  RealParameterConstraint.farkas_transform (gtrans |> GeneralTransition.guard |> RealConstraint.of_intconstraint) atom
  |> RealFormula.mk

let general_transition_constraint = Util.memoize ~extractor:(Tuple2.map2 GeneralTransition.id) general_transition_constraint_
  
let non_increasing_constraint transition =
  general_transition_constraint (`Non_Increasing, transition)

let bounded_constraint transition =
  general_transition_constraint (`Bounded, transition)

let decreasing_constraint transition =
  general_transition_constraint (`Decreasing, transition)

let c_ranked_constraint transition = 
  general_transition_constraint (`C_ranked, transition)

let add_to_LexRSMMap map transitions valuation =
  transitions 
  |> GeneralTransitionSet.start_locations
  |> LocationSet.to_list
  |> List.map (fun location -> 
      TemplateTable.find template_table location
      |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)
      |> (fun poly -> 
        let opt_list = LexRSMMap.find_option map location in
        if Option.is_some (opt_list) then
          [poly]
          |> List.append (Option.get opt_list)
          |> LexRSMMap.replace map location
          |> ignore
        else 
          [poly]
          |> LexRSMMap.add map location
          |> ignore
      )
    )
  |> ignore

let evaluate_cbound valuation =
  !cbound_template
  |> Option.get
  |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)


module Solver = SMT.IncrementalZ3Solver

let add_c_ranked_constraints solver transitions = 
  let c_template = !cbound_template |> Option.get |> RealParameterPolynomial.of_int_parapoly
  in
  Solver.push solver;
  (*C has to be positive for all initial values*)
  RealParameterAtom.Infix.(c_template >= RealParameterPolynomial.of_polynomial RealPolynomial.zero)
  |> RealParameterConstraint.farkas_transform RealConstraint.mk_true 
  |> RealFormula.mk
  |> Solver.add_real solver; 
  transitions
  |> List.map (c_ranked_constraint)
  |> List.map (Solver.add_real solver)

let rec backtrack_1d = function
  | ([],n,ys,solver) -> (n,ys)
  | (x::xs,n,ys,solver) -> 
          Solver.push solver;
          let decr = decreasing_constraint x in
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

let find_1d_lexrsm transitions remaining_transitions cbound =
  let solver = Solver.create () 
  and remaining_list = remaining_transitions |> GeneralTransitionSet.to_list
  and ranked_list = GeneralTransitionSet.diff transitions remaining_transitions |> GeneralTransitionSet.to_list
  in
  (*Correct? Everything must be non increasing and bounded by 0?*)
  ignore(remaining_list |> List.map (fun gtrans -> 
                              Solver.add_real solver (non_increasing_constraint gtrans);
                              Solver.add_real solver (bounded_constraint gtrans)));
  (*add c bound constraints*)
  if cbound then
    add_c_ranked_constraints solver ranked_list
    |> ignore;
  if Solver.satisfiable solver then
    let (n, ranked) = backtrack_1d (remaining_list, 0, GeneralTransitionSet.empty, solver) in
    ranked |> GeneralTransitionSet.to_list |> List.map (fun gtrans -> Solver.add_real solver (decreasing_constraint gtrans)) |> ignore;
    Solver.minimize_absolute solver !fresh_coeffs; (* Check if minimization is forgotten. *)
    if n = 0 then 
      (None, GeneralTransitionSet.empty)
    else
      Solver.model solver
      |> fun eval -> (eval, ranked)
  else
    (None, GeneralTransitionSet.empty)

let lexrsmmap_to_string map =
  map
  |> LexRSMMap.to_list
  |> List.map (fun (loc, poly_list) -> String.concat ": " [loc |> Location.to_string; poly_list |> List.map (Polynomial.to_string) |> String.concat ", "])
  |> String.concat "; "

let cbound_to_string cbound =
  cbound
  |> List.map Polynomial.to_string
  |> String.concat ", "

let rec find_lexrsm map transitions remaining cbound =
  if GeneralTransitionSet.is_empty remaining then
    Some (map, [])
  else
    let (eval, ranked) = find_1d_lexrsm transitions remaining cbound in
    if Option.is_none eval then
      None
    else
      let eval = Option.get eval in
      let c_poly = evaluate_cbound eval in
      add_to_LexRSMMap map transitions eval;
      find_lexrsm map transitions (GeneralTransitionSet.diff remaining ranked) cbound
      |> (fun option -> 
          if Option.is_some option then
            option 
            |> Option.get 
            |> (fun (map, c_list) -> (map, c_poly :: c_list))
            |> Option.some
          else
            None
        )

let rec make_ranking_function start_poly c_bound exp =
  match (start_poly, c_bound) with
    | ([],[]) -> Polynomial.zero
    | (x::xs, y::ys) -> Polynomial.add (Polynomial.mul x (Polynomial.pow (Polynomial. add y Polynomial.one) exp)) (make_ranking_function xs ys (exp-1))

let reset () =
  cbound_template := None;
  TemplateTable.clear template_table

let test program cbounded = 
    print_string("\n");
    print_string("Generalized Transitions:\n");
    print_string(program |> Program.generalized_transitions |> GeneralTransitionSet.to_string);
    print_string("\n");
    if TemplateTable.is_empty template_table then
      compute_ranking_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    if Option.is_none !cbound_template then
      compute_cbound_template (Program.input_vars program);
    let transitions = Program.generalized_transitions program in 
    let lexmap = LexRSMMap.create 10 in
    find_lexrsm lexmap transitions transitions cbounded
    |> tap (fun option -> Option.map (fun (lexmap, cbound) -> (lexrsmmap_to_string lexmap) ^ "\n" ^ (cbound_to_string cbound) ^ "\n") option |? "no ranking function found\n" |> print_string)
    |> Option.map (fun (lexmap, cbound) -> 
          let start_poly = LexRSMMap.find lexmap (Program.start program) in
          make_ranking_function start_poly cbound (List.length start_poly - 1)
        )
    |> Option.map (Polynomial.to_string) |? "still no ranking function found\n"
    |> print_string

let compute_map program cbounded =
  let transitions = Program.generalized_transitions program 
  and lexmap = LexRSMMap.create 10 in
  let execute () =
    find_lexrsm lexmap transitions transitions cbounded
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "find_map", [])
                  ~result:(fun option -> Option.map (fun (lexmap, cbound) -> (lexrsmmap_to_string lexmap) ^ "; c: " ^ (cbound_to_string cbound)) option |? "no ranking function found")
                  execute

let compute_ranking_function program cbounded =
  let (lexmap, c_vector) = compute_map program cbounded |> Option.get in
  let start_poly = LexRSMMap.find lexmap (Program.start program) in
  make_ranking_function start_poly c_vector (List.length start_poly - 1)

let find program cbounded =
  let execute () =
    if TemplateTable.is_empty template_table then
      compute_ranking_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    if Option.is_none !cbound_template then
      compute_cbound_template (Program.input_vars program);
    compute_ranking_function program cbounded
  in
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "find_ranking_function", ["transitions", program |> Program.generalized_transitions |> GeneralTransitionSet.to_string;"cbounded", Bool.to_string cbounded])
                  ~result:(Polynomial.to_string)
                  execute 
  |> ignore
