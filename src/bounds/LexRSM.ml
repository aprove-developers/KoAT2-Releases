open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open Valuation

module SMTSolver = SMT.Z3Solver

module Valuation = Valuation.Make(OurFloat)

module LexRSMMap = Hashtbl.Make(Location)

module RankingTable = Hashtbl.Make(struct include GeneralTransition let equal = GeneralTransition.same 
                                                                    let hash = Hashtbl.hash % GeneralTransition.id end)

type t = {
    rank : Location.t -> RealPolynomial.t;
    decreasing : GeneralTransition.t;
    non_increasing : GeneralTransitionSet.t;
  }

let rank t = t.rank
let decreasing t = t.decreasing
let non_increasing t = t.non_increasing

let time_ranking_table: t RankingTable.t = RankingTable.create 10

let logger = Logging.(get LexRSM)

type constraint_type = [ `Non_Increasing | `Decreasing | `Bounded | `C_ranked ] [@@deriving show, eq]

let as_realparapoly label var =
  match TransitionLabel.update label var with
  (** Correct? In the nondeterministic case we just make it deterministic? *)
  | None -> RealParameterPolynomial.of_var var
  | Some (TransitionLabel.UpdateElement.Poly p) -> p |> RealPolynomial.of_intpoly |> RealParameterPolynomial.of_polynomial
  (** TODO Is this the real life? Is this just fantasy?*)
  | Some (TransitionLabel.UpdateElement.Dist d) -> ProbDistribution.expected_value d |> RealParameterPolynomial.of_polynomial

(** Given a list of variables an affine template-polynomial is generated*)
let ranking_template (vars: VarSet.t): RealParameterPolynomial.t * Var.t list =
  let vars = VarSet.elements vars in
  let num_vars = List.length vars in
  let fresh_vars = Var.fresh_id_list Var.Real num_vars in
  let fresh_coeffs = List.map RealPolynomial.of_var fresh_vars in
  let linear_poly = RealParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Int () in
  let constant_poly = RealParameterPolynomial.of_constant (RealPolynomial.of_var constant_var) in
  RealParameterPolynomial.(linear_poly + constant_poly),
  List.append fresh_vars [constant_var]

module TemplateTable = Hashtbl.Make(Location)

let template_table: RealParameterPolynomial.t TemplateTable.t = TemplateTable.create 10

let cbound_template: RealParameterPolynomial.t Option.t ref = ref None

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

let compute_cbound_template (vars: VarSet.t): unit =
  let execute () =
    vars
    |> ranking_template
    |> fun (poly, vars) -> cbound_template := Option.some poly
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute_cbound_template", [])
                  ~result:(fun () -> !cbound_template |> Option.get |> RealParameterPolynomial.to_string)
                  execute

let prob_branch_poly (l,t,l') =
    let template = (fun key -> key |> TemplateTable.find template_table) in
    let prob = t |> TransitionLabel.probability in
    RealParameterPolynomial.mul (prob |> RealPolynomial.of_constant |> RealParameterPolynomial.of_polynomial) (RealParameterPolynomial.substitute_f (as_realparapoly t) (template l'))

let expected_poly gtrans =
    TransitionSet.fold (fun trans poly -> RealParameterPolynomial.add (prob_branch_poly trans) poly) (gtrans |> GeneralTransition.transitions) RealParameterPolynomial.zero

let general_transition_constraint_ (constraint_type, gtrans): RealFormula.t =
  let template gtrans = gtrans |> GeneralTransition.start |> TemplateTable.find template_table in
  let atom =
    match constraint_type with
    | `Non_Increasing ->  RealParameterAtom.Infix.((template gtrans) >= (expected_poly gtrans))
    | `Decreasing ->      RealParameterAtom.Infix.((template gtrans) >= (RealParameterPolynomial.add (expected_poly gtrans) (RealParameterPolynomial.of_polynomial RealPolynomial.one)))
    | `Bounded ->         RealParameterAtom.Infix.((template gtrans) >= RealParameterPolynomial.of_polynomial RealPolynomial.one)
    | `C_ranked ->        let c_template = !cbound_template |> Option.get in RealParameterAtom.Infix.((RealParameterPolynomial.add (template gtrans) c_template) >= (expected_poly gtrans))
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
      |> RealParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurFloat.zero)
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
  |> RealParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurFloat.zero)



module Solver = SMT.IncrementalZ3Solver

let add_c_ranked_constraints solver transitions =
  let c_template = !cbound_template |> Option.get
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
  |> ignore

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

let rec backtrack_1d_non_increasing = function
  | ([],n,ys,solver) -> (n,ys)
  | (x::xs,n,ys,solver) ->
          Solver.push solver;
          Solver.add_real solver (non_increasing_constraint x);
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

let find_1d_lexrsm_non_increasing transitions decreasing =
  let solver = Solver.create ()
  in
  Solver.add_real solver (decreasing_constraint decreasing);
  Solver.add_real solver (bounded_constraint decreasing);
  if Solver.satisfiable solver then
    let (n, non_incr) = 
      backtrack_1d_non_increasing (GeneralTransitionSet.remove decreasing transitions |> GeneralTransitionSet.to_list, 
                                   0, GeneralTransitionSet.empty, solver)
    in
    non_incr |> GeneralTransitionSet.to_list |> List.map (Solver.add_real solver % non_increasing_constraint) |> ignore;
    Solver.minimize_absolute solver !fresh_coeffs; 
    Solver.model_real solver
    |> fun eval -> (eval, GeneralTransitionSet.add decreasing non_incr)
  else
    (None, GeneralTransitionSet.empty)

let find_1d_lexrsm transitions remaining_transitions cbounded =
  let solver = Solver.create ()
  and remaining_list = remaining_transitions |> GeneralTransitionSet.to_list
  and ranked_list = GeneralTransitionSet.diff transitions remaining_transitions |> GeneralTransitionSet.to_list
  in
  (*Correct? Everything must be non increasing and bounded by 0?*)
  ignore(remaining_list |> List.map (fun gtrans ->
                              Solver.add_real solver (non_increasing_constraint gtrans);
                              Solver.add_real solver (bounded_constraint gtrans)));
  (*add c bound constraints*)
  if cbounded then
    add_c_ranked_constraints solver ranked_list;
  if Solver.satisfiable solver then
    let (n, ranked) = backtrack_1d (remaining_list, 0, GeneralTransitionSet.empty, solver) in
    ranked |> GeneralTransitionSet.to_list |> List.map (fun gtrans -> Solver.add_real solver (decreasing_constraint gtrans)) |> ignore;
    Solver.minimize_absolute solver !fresh_coeffs; (* Check if minimization is forgotten. *)
    if n = 0 then
      (Solver.model_real solver, GeneralTransitionSet.empty)
    else
      Solver.model_real solver
      |> fun eval -> (eval, ranked)
  else
    (None, GeneralTransitionSet.empty)

let lexrsmmap_to_string map =
  map
  |> LexRSMMap.to_list
  |> List.map (fun (loc, poly_list) -> String.concat ": " [loc |> Location.to_string; poly_list |> List.map (RealPolynomial.to_string) |> String.concat ", "])
  |> String.concat "; "

let cbound_to_string cbound =
  cbound
  |> List.map RealPolynomial.to_string
  |> String.concat ", "

let rec find_lexrsm map transitions remaining cbounded =
  if GeneralTransitionSet.is_empty remaining then
    Some (map, [], GeneralTransitionSet.empty)
  else
    let (eval, ranked) = find_1d_lexrsm transitions remaining cbounded in
    if Option.is_none eval then
      None
    else
      if not (GeneralTransitionSet.is_empty ranked) then
        (Option.may (add_to_LexRSMMap map transitions) eval;
        find_lexrsm map transitions (GeneralTransitionSet.diff remaining ranked) cbounded
        |> Option.map (fun (map, c_list, non_increasing) ->
                                let new_c_list = if cbounded then
                                  ((Option.map (evaluate_cbound) eval |? RealPolynomial.zero) :: c_list)
                                  else []
                                in
                                (map, new_c_list, non_increasing)))
      else
        (* No further general transitions can be ranked here *)
        Some (map, [], remaining)

let rec make_ranking_function start_poly c_bound exp =
  match (start_poly, c_bound) with
    | ([],[]) -> RealPolynomial.zero
    | (x::xs, y::ys) -> RealPolynomial.add (RealPolynomial.mul x (RealPolynomial.pow (RealPolynomial. add y RealPolynomial.one) exp)) (make_ranking_function xs ys (exp-1))
    | _ -> failwith "impossible"

let reset () =
  cbound_template := None;
  TemplateTable.clear template_table

let compute_map program transitions cbounded =
  let lexmap = LexRSMMap.create 10 in
  let execute () =
    find_lexrsm lexmap transitions transitions cbounded
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "find_map", [])
                  ~result:(fun option -> Option.map (fun (lexmap, cbound, non_increasing) ->
                    let cbound_string = if cbound = [] then "" else "; c: " ^ (cbound_to_string cbound) in
                    let non_inc_string = if non_increasing = GeneralTransitionSet.empty then ""
                                         else "; non_increasing: " ^ (GeneralTransitionSet.to_string non_increasing)
                    in
                    (lexrsmmap_to_string lexmap) ^ cbound_string ^ non_inc_string) option |? "no lexRSMMap found")
                  execute

let init_computation vars locations transitions program cbounded =
  if TemplateTable.is_empty template_table then
    compute_ranking_templates vars locations;
  if cbounded then
    if Option.is_none !cbound_template then
      compute_cbound_template vars;
  compute_map program transitions cbounded

let compute_as_termination program =
  let execute () =
    init_computation
      (Program.input_vars program)
      (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list)
      (Program.generalized_transitions program) program false
    |> Option.is_some
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute_ast", [])
                  ~result:(fun bool -> if bool then "The program terminates almost surely." else "The program does not terminate almost surely.")
                  execute

let compute_expected_complexity program =
  let execute () =
    init_computation
      (Program.input_vars program)
      (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list)
      (Program.generalized_transitions program) program true
    |> Option.map (fun (lexmap, c_vector,_) ->
                      let start_poly = LexRSMMap.find lexmap (Program.start program) in
                      make_ranking_function start_poly c_vector (List.length start_poly -1))
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "find_ranking_function", ["transitions", program |> Program.generalized_transitions |> GeneralTransitionSet.to_string])
                  ~result:(fun option -> Option.map RealPolynomial.to_string option |? "no ranking function could be found")
                  execute

let pprf_to_string t = 
  "{decreasing: " ^ GeneralTransition.to_id_string t.decreasing ^
  " non_incr: " ^ GeneralTransitionSet.to_id_string t.non_increasing ^ 
  " rank: [" ^ (GeneralTransitionSet.start_locations t.non_increasing |> LocationSet.to_list
                |> List.map (fun loc -> Location.to_string loc ^ ": " ^ (t.rank loc |> RealPolynomial.to_string)) |> String.concat "; ")
  ^ "]}"

let ranking_table_to_string rtable = 
  RankingTable.to_list rtable
  |> List.map (pprf_to_string % Tuple2.second)
  |> String.concat "; "

let compute_ranking_table program =
  Program.sccs program
  |> Enum.map (GeneralTransitionSet.from_transitionset) 
  |> Enum.iter 
       (fun scc ->
          GeneralTransitionSet.iter
            (fun decr ->
              let (eval, non_incr) = find_1d_lexrsm_non_increasing scc decr in
              let rankfunc loc = 
                match eval with 
                  | None -> failwith ""
                  | (Some valuation) -> 
                      TemplateTable.find template_table loc
                      |> RealParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurFloat.zero)
              in
              let ranking = {
                              decreasing = decr;
                              non_increasing = non_incr;
                              rank = rankfunc;
                            }
              in
              RankingTable.add time_ranking_table decr ranking
            )
            scc
       );
  Logger.log logger Logger.DEBUG
                  (fun () -> "compute_ranking_table", ["ranking_table", ranking_table_to_string time_ranking_table])

let find program gt = 
  let vars = Program.input_vars program in
  let locations = (Program.graph program |> TransitionGraph.locations |> LocationSet.to_list) in
  if TemplateTable.is_empty template_table then
    compute_ranking_templates vars locations;
  if RankingTable.is_empty time_ranking_table then
    compute_ranking_table program;
 RankingTable.find_option time_ranking_table gt

let find_whole_prog program goal =
  if goal = "EXPECTEDCOMPLEXITY" then compute_expected_complexity program |> ignore
  else if goal = "ASTERMINATION" then compute_as_termination program |> ignore
    else raise (Failure ("Unexpected Goal"))
