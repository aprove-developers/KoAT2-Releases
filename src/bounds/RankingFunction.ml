open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes

module SMTSolver = SMT.Z3Opt
module Valuation = Valuation.Make(OurInt)


type constraint_type = [ `Non_Increasing | `Decreasing | `Bounded ] [@@deriving show, eq]

type t = {
    rank : Location.t -> Polynomial.t;
    decreasing : Transition.t;
    non_increasing : TransitionSet.t;
  }

module RankingTable = Hashtbl.Make(struct include Transition let equal = Transition.same end)
module TemplateTable = Hashtbl.Make(Location)

type trans_constraint_cache = ([`Cost | `Time] * [`Bounded | `Decreasing | `Non_Increasing] * int, Formulas.Formula.t) Hashtbl.t
type ranking_cache = (t RankingTable.t * t RankingTable.t * Polynomials.ParameterPolynomial.t TemplateTable.t * trans_constraint_cache)
let new_cache: unit -> ranking_cache =
  fun () -> (RankingTable.create 10, RankingTable.create 10, TemplateTable.create 10, Hashtbl.create 10)

let get_time_ranking_table     (cache: ranking_cache) = Tuple4.first cache
let get_cost_ranking_table     (cache: ranking_cache) = Tuple4.second cache
let get_template_table         (cache: ranking_cache) = Tuple4.third cache
let get_trans_constraint_cache (cache: ranking_cache) = Tuple4.fourth cache

type measure = [ `Cost | `Time ] [@@deriving show, eq]
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
  | Some (TransitionLabel.UpdateElement.Poly p) -> ParameterPolynomial.of_polynomial p
  (** TODO is there a better way in the probabilistic case ? *)
  | Some (TransitionLabel.UpdateElement.Dist _) -> ParameterPolynomial.of_var var

(** Given a list of variables an affine template-polynomial is generated*)
let ranking_template (vars: VarSet.t): ParameterPolynomial.t * Var.t list * Var.t =
  let vars = VarSet.elements vars in
  let num_vars = List.length vars in
  let fresh_vars = Var.fresh_id_list Var.Int num_vars in
  let fresh_coeffs = List.map Polynomial.of_var fresh_vars in
  let linear_poly = ParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Int () in
  let constant_poly = ParameterPolynomial.of_constant (Polynomial.of_var constant_var) in
  ParameterPolynomial.(linear_poly + constant_poly),
  List.append fresh_vars [constant_var],
  constant_var

let fresh_coeffs: Var.t list ref = ref []
let fresh_constants: Var.t list ref = ref []

let compute_ranking_templates cache (vars: VarSet.t) (locations: Location.t list): unit =
  let execute () =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars,fresh_constant) = ranking_template vars in
      (location, parameter_poly, fresh_vars, fresh_constant)
    in
    let templates = List.map ins_loc_prf locations in
    templates
    |> List.iter (fun (location,polynomial,_,_) -> TemplateTable.add (get_template_table cache) location polynomial);
    templates
    |> List.map (fun (_,_,fresh_vars,fresh_constant)-> fresh_vars, fresh_constant)
    |> List.fold_left (fun (f_vs,f_cs) (f_v,f_c) -> List.append f_v f_vs, f_c::f_cs) ([],[])
    |> (fun (fresh_vars,fresh_consts) -> fresh_coeffs := fresh_vars; fresh_constants := fresh_consts)
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute_ranking_templates", [])
                  ~result:(fun () ->
                    get_template_table cache
                    |> TemplateTable.enum
                    |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ ParameterPolynomial.to_string polynomial)
                  )
                  execute

let decreaser measure t =
  match measure with
  | `Cost -> TransitionLabel.cost t
  | `Time -> Polynomial.one

let transition_constraint_ cache (measure, constraint_type, (l,t,l')): Formula.t =
  let template = TemplateTable.find (get_template_table cache) in
  let atom =
    match constraint_type with
    | `Non_Increasing -> ParameterAtom.Infix.(template l >= ParameterPolynomial.substitute_f (as_parapoly t) (template l'))
    | `Decreasing -> ParameterAtom.Infix.(template l >= ParameterPolynomial.(of_polynomial (decreaser measure t) + substitute_f (as_parapoly t) (template l')))
    | `Bounded -> ParameterAtom.Infix.(template l >= ParameterPolynomial.of_polynomial (decreaser measure t))
  in
  ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
  |> Formula.mk

let transition_constraint cache =
  Util.memoize (get_trans_constraint_cache cache) ~extractor:(Tuple3.map3 Transition.id) (transition_constraint_ cache)

let transitions_constraint cache measure (constraint_type: constraint_type) (transitions : Transition.t list): Formula.t =
  transitions
  |> List.map (fun t -> transition_constraint cache (measure, constraint_type, t))
  |> Formula.all

let non_increasing_constraint cache measure transition =
  transition_constraint cache (measure, `Non_Increasing, transition)

let non_increasing_constraints cache measure transitions =
  transitions_constraint cache measure `Non_Increasing (TransitionSet.to_list transitions)

let bounded_constraint cache measure transition =
  transition_constraint cache (measure, `Bounded, transition)

let decreasing_constraint cache measure transition =
  transition_constraint cache (measure, `Decreasing, transition)

let rank_from_valuation cache valuation location =
  location
  |> TemplateTable.find (get_template_table cache)
  |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)

let make cache decreasing_transition non_increasing_transitions valuation =
  {
    rank = rank_from_valuation cache valuation;
    decreasing = decreasing_transition;
    non_increasing = non_increasing_transitions;
  }

(*let find_with measure non_increasing_transitions decreasing_transition =
  Formula.Infix.(
    non_increasing_constraints measure non_increasing_transitions
    && bounded_constraint measure decreasing_transition
    && decreasing_constraint measure decreasing_transition)
  |> SMTSolver.get_model ~coeffs_to_minimise:!fresh_coeffs
  |> Option.map (make decreasing_transition non_increasing_transitions)*)

let get_ranking_table = function
  | `Time -> get_time_ranking_table
  | `Cost -> get_cost_ranking_table

module Solver = SMT.IncrementalZ3Solver

let try_decreasing cache (opt: Solver.t) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: measure) =
  non_increasing
  |> Stack.enum
  |> Enum.filter (fun t -> not (RankingTable.mem (get_ranking_table measure cache) t))
  |> Enum.iter (fun decreasing ->
         Logger.(log logger DEBUG (fun () -> "try_decreasing", ["measure", show_measure measure;
                                                                "decreasing", Transition.to_id_string decreasing;
                                                                "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing)]));
         Solver.push opt;
         Solver.add opt (bounded_constraint cache measure decreasing);
         Solver.add opt (decreasing_constraint cache measure decreasing);
         if Solver.satisfiable opt then (
           let fresh_coeffs =
            !fresh_coeffs
            |> List.filter (fun c -> not @@ List.mem c !fresh_constants)
           in
           Solver.minimize_absolute_iteratively opt @@ List.append fresh_coeffs !fresh_constants;
           Solver.model opt
           |> Option.map (make cache decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum))
           |> Option.may (fun ranking_function ->
                  to_be_found := !to_be_found - 1;
                  RankingTable.add (get_ranking_table measure cache) decreasing ranking_function;
                  Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                               "measure", show_measure measure;
                                               "decreasing", Transition.to_id_string decreasing;
                                               "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                               "rank", only_rank_to_string ranking_function]))
                )
         );
         Solver.pop opt
       );
  if !to_be_found <= 0 then
    raise Exit


let rec backtrack cache (steps_left: int) (index: int) (opt: Solver.t) (scc: Transition.t array) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: measure) =
  if Solver.satisfiable opt then (
    if steps_left == 0 then (
      try_decreasing cache opt non_increasing to_be_found measure
    ) else (
      for i=index to Array.length scc - 1 do
        let transition = Array.get scc i in
        Solver.push opt;
        Solver.add opt (non_increasing_constraint cache measure transition);
        Stack.push transition non_increasing;
        backtrack cache (steps_left - 1) (i + 1) opt scc non_increasing to_be_found measure;
        ignore (Stack.pop non_increasing);
        Solver.pop opt;
      done;
      try_decreasing cache opt non_increasing to_be_found measure
    )
  )

let compute_ cache measure program =
  program
  |> Program.sccs
  |> Enum.iter (fun scc ->
         try
           backtrack cache
                     (TransitionSet.cardinal scc)
                     0
                     (Solver.create ())
                     (Array.of_enum (TransitionSet.enum scc))
                     (Stack.create ())
                     (ref (TransitionSet.cardinal scc))
                     measure;
           scc
           |> TransitionSet.iter (fun t ->
                  if not (RankingTable.mem (get_ranking_table measure cache) t) then
                    Logger.(log logger WARN (fun () -> "no_ranking_function", ["measure", show_measure measure; "transition", Transition.to_id_string t]))
                )
         with Exit -> ()
       )

let find cache measure program transition =
  let execute () =
    if TemplateTable.is_empty (get_template_table cache) then
      compute_ranking_templates cache (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    if RankingTable.is_empty (get_ranking_table measure cache) then
      compute_ cache measure program;
    (try
      RankingTable.find_all (get_ranking_table measure cache) transition
    with Not_found -> [])
    |> List.rev
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "find_ranking_functions", ["measure", show_measure measure;
                                                        "transition", Transition.to_id_string transition])
                  ~result:(Util.enum_to_string to_string % List.enum)
                  execute