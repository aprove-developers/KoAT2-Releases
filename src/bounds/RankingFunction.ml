open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open CFR 
   
module SMTSolver = SMT.IncrementalZ3SolverOld
module Valuation = Valuation.Make(OurInt)
                 
type measure = [ `Cost | `Time ] [@@deriving show, eq]

type constraint_type = [ `Non_Increasing | `Decreasing | `Bounded ] [@@deriving show, eq]
                     
type t = {
    rank : Location.t -> Polynomial.t;
    decreasing : Transition.t;
    non_increasing : TransitionSet.t;
  }

let cache = Util.cache ~extractor:(Tuple3.map3 Transition.id)

let one = ParameterPolynomial.one
        
let logger = Logging.(get PRF)  

let rank f = f.rank
           
let decreasing f = f.decreasing
                 
let non_increasing f = TransitionSet.to_list f.non_increasing

let rank_to_string (locations: Location.t list) (content_to_string: 'a -> string) (pol: Location.t -> 'a) =
  locations
  |> List.enum
  |> Util.enum_to_string (fun l -> Location.to_string l ^ ": " ^ content_to_string (pol l))

let only_rank_to_string {rank; decreasing; non_increasing} =
  let locations = non_increasing |> TransitionSet.enum |> Program.locations |> List.of_enum |> List.unique ~eq:Location.equal in
  rank_to_string locations Polynomial.to_string rank

let to_string {rank; decreasing; non_increasing} =
  "{rank:" ^ only_rank_to_string {rank; decreasing; non_increasing} ^ ";decreasing:" ^ Transition.to_id_string decreasing ^ "}"

let as_parapoly label var =
  match TransitionLabel.update label var with
  (** Correct? In the nondeterministic case we just make it deterministic? *)
  | None -> ParameterPolynomial.of_var var
  | Some p -> ParameterPolynomial.of_polynomial p
 
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

let decreaser measure t =
  match measure with
  | `Cost -> TransitionLabel.cost t
  | `Time -> Polynomial.one

let transition_constraint_ (measure, constraint_type, (l,t,l')): Formula.t =
  let template = TemplateTable.find template_table in
  let atom =
    match constraint_type with
    | `Non_Increasing -> ParameterAtom.Infix.(template l >= ParameterPolynomial.substitute_f (as_parapoly t) (template l'))
    | `Decreasing -> ParameterAtom.Infix.(template l >= ParameterPolynomial.(of_polynomial (decreaser measure t) + substitute_f (as_parapoly t) (template l')))
    | `Bounded -> ParameterAtom.Infix.(template l >= ParameterPolynomial.of_polynomial (decreaser measure t))  
  in
  ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
  |> Formula.mk  
       
let transition_constraint = cache#add transition_constraint_
  
let transitions_constraint measure (constraint_type: constraint_type) (transitions : Transition.t list): Formula.t =
  transitions
  |> List.map (fun t -> transition_constraint (measure, constraint_type, t))
  |> Formula.all
  
let non_increasing_constraint measure transition =
  transition_constraint (measure, `Non_Increasing, transition)

let non_increasing_constraints measure transitions =
  transitions_constraint measure `Non_Increasing (TransitionSet.to_list transitions)
  
let bounded_constraint measure transition =
  transition_constraint (measure, `Bounded, transition)

let decreasing_constraint measure transition =
  transition_constraint (measure, `Decreasing, transition)
  
let rank_from_valuation valuation location =
  location
  |> TemplateTable.find template_table
  |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)

let make decreasing_transition non_increasing_transitions valuation =
  {
    rank = rank_from_valuation valuation;
    decreasing = decreasing_transition;
    non_increasing = non_increasing_transitions;
  }

(* let find_with measure non_increasing_transitions decreasing_transition =
  Formula.Infix.(
    non_increasing_constraints measure non_increasing_transitions
    && bounded_constraint measure decreasing_transition
    && decreasing_constraint measure decreasing_transition)
  |> SMTSolver.get_model ~coeffs_to_minimise:!fresh_coeffs
  |> Option.map (make decreasing_transition non_increasing_transitions) *)
  
module RankingTable = Hashtbl.Make(struct include Transition let equal = Transition.same end)
                    
let time_ranking_table: t RankingTable.t = RankingTable.create 10

let cost_ranking_table: t RankingTable.t = RankingTable.create 10

let ranking_table = function
  | `Time -> time_ranking_table
  | `Cost -> cost_ranking_table

let try_decreasing (opt: SMTSolver.t) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: measure) applied_cfr =
  non_increasing
  |> Stack.enum
  |> Enum.filter (fun t -> not (RankingTable.mem (ranking_table measure) t))
  |> Enum.iter (fun decreasing ->
         let current_time = Unix.time() in
         Logger.(log logger DEBUG (fun () -> "try_decreasing", ["measure", show_measure measure;
                                                                "decreasing", Transition.to_id_string decreasing;
                                                                "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing)]));
         SMTSolver.push opt;
         SMTSolver.add opt (bounded_constraint measure decreasing);
         SMTSolver.add opt (decreasing_constraint measure decreasing);
         
         if SMTSolver.satisfiable opt then (   
           SMTSolver.minimize_absolute opt !fresh_coeffs; (* Check if minimization is forgotten. *)
           SMTSolver.model opt
           |> Option.map (make decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum))
           |> Option.may (fun ranking_function ->
                  to_be_found := !to_be_found - 1;
                  RankingTable.add (ranking_table measure) decreasing ranking_function;
                  Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                               "measure", show_measure measure;
                                               "decreasing", Transition.to_id_string decreasing;
                                               "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                               "rank", only_rank_to_string ranking_function]))
                )
         );
         SMTSolver.pop opt;
         CFR.delta_current_cfr := !CFR.delta_current_cfr +. (Unix.time() -. current_time);
         CFR.poll_timeout ~applied_cfr:applied_cfr
       );
  if !to_be_found <= 0 then
    raise Exit

           
let rec backtrack (steps_left: int) (index: int) (opt: SMTSolver.t) (scc: Transition.t array) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: measure) applied_cfr =
  if SMTSolver.satisfiable opt then (
    if steps_left == 0 then (
      try_decreasing opt non_increasing to_be_found measure applied_cfr
    ) else (
      for i=index to Array.length scc - 1 do
        let transition = Array.get scc i in
        SMTSolver.push opt;
        SMTSolver.add opt (non_increasing_constraint measure transition);
        Stack.push transition non_increasing;
        backtrack (steps_left - 1) (i + 1) opt scc non_increasing to_be_found measure applied_cfr; 
        ignore (Stack.pop non_increasing);
        SMTSolver.pop opt;
      done;
      try_decreasing opt non_increasing to_be_found measure applied_cfr
    )
  )

let compute_ measure applied_cfr program =
  program
  |> Program.sccs
  |> Enum.iter (fun scc ->
         try
           backtrack (TransitionSet.cardinal scc)
                     0
                     (SMTSolver.create ())
                     (Array.of_enum (TransitionSet.enum scc))
                     (Stack.create ())
                     (ref (TransitionSet.cardinal scc))
                     measure
                     applied_cfr;
           scc
           |> TransitionSet.iter (fun t ->
                  if not (RankingTable.mem (ranking_table measure) t) then
                    Logger.(log logger WARN (fun () -> "no_ranking_function", ["measure", show_measure measure; "transition", Transition.to_id_string t]))
                )
         with Exit -> ()
       )

let compute_scc measure applied_cfr program scc =
  try
    backtrack (TransitionSet.cardinal scc)
              0
              (SMTSolver.create ())
              (Array.of_enum (TransitionSet.enum scc))
              (Stack.create ())
              (ref (TransitionSet.cardinal scc))
              measure
              applied_cfr;
    scc
    |> TransitionSet.iter (fun t ->
          if not (RankingTable.mem (ranking_table measure) t) then
            Logger.(log logger WARN (fun () -> "no_ranking_function", ["measure", show_measure measure; "transition", Transition.to_id_string t]))
        )
  with Exit -> ()
       

  
let find measure applied_cfr program transition =
  let execute () =
    if TemplateTable.is_empty template_table then
      compute_ranking_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);      
    if RankingTable.is_empty (ranking_table measure) then
      compute_ measure applied_cfr program;
    (try
      RankingTable.find_all (ranking_table measure) transition
    with Not_found -> [])
    |> List.rev
  in
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "find_ranking_functions", ["measure", show_measure measure;
                                                        "transition", Transition.to_id_string transition])
                  ~result:(Util.enum_to_string to_string % List.enum)
                  execute

let find_scc measure applied_cfr program transition scc =
  let execute () =
    if TemplateTable.is_empty template_table then
      compute_ranking_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);      
    if RankingTable.is_empty (ranking_table measure) then
      compute_scc measure applied_cfr program scc;
    (try
      RankingTable.find_all (ranking_table measure) transition
    with Not_found -> [])
    |> List.rev
  in
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "find_ranking_functions_scc", ["measure", show_measure measure;
                                                        "transition", Transition.to_id_string transition])
                  ~result:(Util.enum_to_string to_string % List.enum)
                  execute


let reset () =
  cache#clear;
  RankingTable.clear time_ranking_table;
  RankingTable.clear cost_ranking_table;
  TemplateTable.clear template_table