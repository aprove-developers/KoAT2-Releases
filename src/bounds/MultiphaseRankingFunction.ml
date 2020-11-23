open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes
open CFR

(** Class is derived from RankingFunction.ml*)

module SMTSolver = SMT.IncrementalZ3Solver
module Valuation = Valuation.Make(OurInt)

type measure = [ `Cost | `Time ] [@@deriving show, eq]

let logger = Logging.(get PRF)

let decreaser measure t =
    match measure with
    | `Cost -> TransitionLabel.cost t
    | `Time -> Polynomial.one


(* method transforms polynome to parapolynom*)
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

let as_parapoly_real label var =
  match TransitionLabel.update label var with
  (** Correct? In the nondeterministic case we just make it deterministic? *)
  | None -> RealParameterPolynomial.of_var var
  | Some p -> RealParameterPolynomial.of_intpoly p
 
(** Given a list of variables an affine template-polynomial is generated*)            
let ranking_template_real (vars: VarSet.t): RealParameterPolynomial.t * Var.t list =
  let vars = VarSet.elements vars in
  let num_vars = List.length vars in
  let fresh_vars = Var.fresh_id_list Var.Real num_vars in
  let fresh_coeffs = List.map RealPolynomial.of_var fresh_vars in
  let linear_poly = RealParameterPolynomial.of_coeff_list fresh_coeffs vars in
  let constant_var = Var.fresh_id Var.Real () in
  let constant_poly = RealParameterPolynomial.of_constant (RealPolynomial.of_var constant_var) in
  RealParameterPolynomial.(linear_poly + constant_poly),
  List.append fresh_vars [constant_var]

module TemplateTable = Hashtbl.Make(Location)

type constraint_type = [`Non_Increasing | `Decreasing] [@@deriving show, eq]

let maxDepth = ref 5

type mprf = (Location.t -> Polynomial.t) list

type t = {
  rank : mprf;
  decreasing : Transition.t;
  non_increasing : TransitionSet.t;
  depth : int;
}

let cache = Util.cache ~extractor:(Tuple4.map4 Transition.id)

let rank f = f.rank

let decreasing f = f.decreasing

let non_increasing f = TransitionSet.to_list f.non_increasing

let depth f = f.depth

(* output methods *)
let rank_to_string (locations: Location.t list) (content_to_string: ((Location.t -> 'a) list) * Location.t -> string) (rank: (Location.t -> 'a) list) =
  locations
  |> List.enum
  |> Util.enum_to_string (fun l -> Location.to_string l ^ ": " ^ content_to_string (rank ,l))

let polyList_to_string ((rank: (Location.t -> 'a) list) , (l : Location.t)) =
  rank
  |> List.enum
  |> Util.enum_to_string (fun p -> Polynomial.to_string(p l) ^ " ")

let only_rank_to_string {rank; decreasing; non_increasing; depth} =
  let locations = non_increasing |> TransitionSet.enum |> Program.locations |> List.of_enum |> List.unique ~eq:Location.equal in
  rank_to_string locations polyList_to_string rank

let to_string {rank; decreasing; non_increasing; depth} =
  "{multirank:" ^ only_rank_to_string {rank; decreasing; non_increasing; depth} ^ ";decreasing:" ^ Transition.to_id_string decreasing ^ "}"

let template_tables: ((ParameterPolynomial.t TemplateTable.t) list) ref = ref []
let template_tables_real: ((RealParameterPolynomial.t TemplateTable.t) list) ref = ref []

let list_init depth = 
  template_tables := (List.init depth (fun i -> TemplateTable.create 10))

let list_init_real depth = 
  template_tables_real := (List.init depth (fun i -> TemplateTable.create 10))

let fresh_coeffs: Var.t list ref = ref []

let numberOfGeneratedTemplates = ref 0

let compute_ranking_templates_ (depth: int) (vars: VarSet.t) (locations: Location.t list) ranking_template_ template_table_ to_string: unit =
  let execute (i:int) =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars) = ranking_template_ vars in
      (location, parameter_poly, fresh_vars)
    in
    let templates = List.map ins_loc_prf locations in
    templates
    |> List.iter (fun (location,polynomial,_) -> TemplateTable.add (List.nth !template_table_ i) location polynomial);
    templates
    |> List.map (fun (_,_,fresh_vars)-> fresh_vars)
    |> List.flatten
    |> (fun fresh_vars -> fresh_coeffs := fresh_vars)
  in
  for i = !numberOfGeneratedTemplates to depth - 1 do
    Logger.with_log logger Logger.DEBUG
      (fun () -> "compute_mprf_templates_" ^ string_of_int i, [])
      ~result:(fun () ->
          (List.nth !template_table_ i)
          |> TemplateTable.enum
          |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ to_string polynomial)
        )
      (fun () -> execute i);
  done

let compute_ranking_templates (depth: int) (vars: VarSet.t) (locations: Location.t list) : unit =
  compute_ranking_templates_ depth vars locations ranking_template template_tables ParameterPolynomial.to_string

let compute_ranking_templates_real (depth: int) (vars: VarSet.t) (locations: Location.t list) : unit =
  compute_ranking_templates_ depth vars locations ranking_template_real template_tables_real RealParameterPolynomial.to_string

(* Methods define properties of mrf *)

(* method for mrf and functions f_2 to f_d of depth i *)
let transition_constraint_i (template_table0, template_table1, measure, constraint_type, (l,t,l')): Formula.t =
  let template0 = TemplateTable.find template_table0 in
  let template1 = TemplateTable.find template_table1 in
  let poly = ParameterPolynomial.add (template0 l) (template1 l) in
  let atom =
    match constraint_type with
    | `Non_Increasing -> ParameterAtom.Infix.(poly >= ParameterPolynomial.substitute_f (as_parapoly t) (template1 l'))
    | `Decreasing -> ParameterAtom.Infix.(poly >= ParameterPolynomial.(ParameterPolynomial.of_polynomial (decreaser measure t) + substitute_f (as_parapoly t) (template1 l')))
  in
  ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
  |> Formula.mk

(* method for mrf and function f_1*)
let transition_constraint_1 (template_table1, measure, constraint_type, (l,t,l')): Formula.t =
  let template1 = TemplateTable.find template_table1 in
  let atom =
    match constraint_type with
      | `Non_Increasing -> ParameterAtom.Infix.(template1 l >= ParameterPolynomial.substitute_f (as_parapoly t) (template1 l'))
      | `Decreasing -> ParameterAtom.Infix.(template1 l >= ParameterPolynomial.(ParameterPolynomial.of_polynomial (decreaser measure t) + substitute_f (as_parapoly t) (template1 l')))
  in
  ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
  |> Formula.mk

(* method for mrf and function f_d*)
let transition_constraint_d (template_table1, measure, constraint_type, (l,t,l')): Formula.t =
  let template1 = TemplateTable.find template_table1 in
    match constraint_type with
    | `Non_Increasing -> Formula.mk_true
    | `Decreasing  -> (
      let atom = ParameterAtom.Infix.((template1 l)  >= ParameterPolynomial.zero) in
        ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
        |> Formula.mk)

(* use all three functions above combined*)
let transition_constraint_ (depth, measure, constraint_type, (l,t,l')): Formula.t =
  let res = ref Formula.mk_true in
  for i = 1 to (depth - 1) do
    res := ((List.nth !template_tables (i - 1)), (List.nth !template_tables i), measure, constraint_type, (l,t,l'))
           |> transition_constraint_i
           |> Formula.mk_and !res
  done;
  res := ((List.nth !template_tables 0), measure, constraint_type, (l,t,l'))
         |> transition_constraint_1
         |> Formula.mk_and !res;

  res := ((List.nth !template_tables (depth - 1)), measure, constraint_type, (l,t,l'))
         |> transition_constraint_d
         |> Formula.mk_and !res;
  !res

let transition_constraint = cache#add  (transition_constraint_)

let transitions_constraint depth measure (constraint_type: constraint_type) (transitions : Transition.t list): Formula.t =
  transitions
  |> List.map (fun t -> transition_constraint (depth, measure, constraint_type, t))
  |> Formula.all


let non_increasing_constraint depth measure transition = 
  transition_constraint (depth, measure, `Non_Increasing, transition)

let non_increasing_constraints depth measure transitions =
  transitions_constraint depth measure `Non_Increasing (TransitionSet.to_list transitions)

let decreasing_constraint depth measure  transition =
  transition_constraint (depth, measure, `Decreasing, transition)

(** A valuation is a function which maps from a finite set of variables to values *)

let rank_from_valuation depth (i:int) valuation location =
  location
  |> TemplateTable.find (List.nth !template_tables i)
  |> ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)

let make depth decreasing_transition non_increasing_transitions valuation  =
{
  rank = List.init depth (fun i -> rank_from_valuation depth i valuation);
  decreasing = decreasing_transition;
  non_increasing = non_increasing_transitions;
  depth = depth;
}

let make_inv depth decreasing_transition non_increasing_transitions valuation =
  { 
    rank = List.init depth (fun i -> MPRF_Invariants.rank_from_valuation (TemplateTable.find (List.nth !template_tables_real i)) valuation);
    decreasing = decreasing_transition;
    non_increasing = non_increasing_transitions;
    depth = depth;
  }

module RankingTable = Hashtbl.Make(struct include Transition let equal = Transition.same end)

let time_ranking_table: t RankingTable.t = RankingTable.create 10

let cost_ranking_table: t RankingTable.t = RankingTable.create 10

let ranking_table = function
  | `Time -> time_ranking_table
  | `Cost -> cost_ranking_table

(** We are searching for a real model, hence we need to cast reals to integers. *)
let change_valuation (values: RealPolynomial.valuation) = 
  Valuation.from (List.map (fun x -> (x,  MPRF_Invariants.Valuation.eval x values |> OurFloat.upper_int)) (MPRF_Invariants.Valuation.vars values))

let try_decreasing ?(inv = false) depth (opt: SMTSolver.t) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: measure) applied_cfr =
  non_increasing
  |> Stack.enum
  |> Enum.filter (fun t -> not (RankingTable.mem (ranking_table measure) t))
  |> Enum.iter (fun decreasing ->
        if not (RankingTable.mem (ranking_table measure) decreasing) then (
        let current_time = Unix.gettimeofday() in
        Logger.(log logger DEBUG (fun () -> "try_decreasing", ["measure", show_measure measure;
                                                                "decreasing", Transition.to_id_string decreasing;
                                                                "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                                                "depth", string_of_int depth]));
        SMTSolver.push opt;
        if inv then (
          let templates_real = List.init depth (fun n -> TemplateTable.find (List.nth !template_tables_real n)) in 
          SMTSolver.add_real opt (MPRF_Invariants.decreasing_constraint depth measure decreasing templates_real);
          SMTSolver.add_real opt (MPRF_Invariants.consecution_constraint depth measure decreasing templates_real);
        ) else (
          SMTSolver.add opt (decreasing_constraint depth measure decreasing);
        );
        if SMTSolver.satisfiable opt then (
          (* SMTSolver.minimize_absolute opt !fresh_coeffs;  Check if minimization is forgotten. *)
          SMTSolver.model_real opt
          |> Option.map (
            if inv then (
              make_inv depth decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum)
              % tap (fun x -> MPRF_Invariants.store_inv x decreasing)
              % tap (fun x -> MPRF_Invariants.store_inv_set x (non_increasing |> Stack.enum |> TransitionSet.of_enum))
            ) else 
              make depth decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum) % change_valuation)
          |> Option.may (fun ranking_function ->
              to_be_found := !to_be_found - 1;
              RankingTable.add (ranking_table measure) decreasing ranking_function;
              Logger.(log logger INFO (fun () -> "add_mrf", [
                  "measure", show_measure measure;
                  "decreasing", Transition.to_id_string decreasing;
                  "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                  "rank", only_rank_to_string ranking_function];))
            )
        );
        SMTSolver.pop opt; 
        if applied_cfr then (
          CFR.delta_current_cfr := !CFR.delta_current_cfr +. (Unix.gettimeofday() -. current_time);
          CFR.poll_timeout ~applied_cfr:applied_cfr))
    );
  if !to_be_found <= 0 then
    raise Exit


let rec backtrack ?(inv = false) depth (steps_left: int) (index: int) (opt: SMTSolver.t) (scc: Transition.t array) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: measure) (program: Program.t) applied_cfr =
    if SMTSolver.satisfiable opt then (
      if steps_left == 0 then (
        try_decreasing ~inv:inv depth opt non_increasing to_be_found measure applied_cfr
      ) else (
        for i=index to Array.length scc - 1 do
          let transition = Array.get scc i in
          SMTSolver.push opt;
          if inv then (
            let templates_real = List.init depth (fun n -> TemplateTable.find (List.nth !template_tables_real n)) in 
            SMTSolver.add_real opt (MPRF_Invariants.non_increasing_constraint depth measure transition templates_real);
            SMTSolver.add_real opt (MPRF_Invariants.consecution_constraint depth measure transition templates_real);
            Program.entry_transitions logger program [transition]
            |> List.iter (fun trans ->  SMTSolver.add_real opt (MPRF_Invariants.initiation_constraint depth measure trans templates_real));
          ) else 
            SMTSolver.add opt (non_increasing_constraint depth measure transition);
          Stack.push transition non_increasing;
          backtrack ~inv:inv depth (steps_left - 1) (i + 1) opt scc non_increasing to_be_found measure program applied_cfr;
          ignore (Stack.pop non_increasing);
          SMTSolver.pop opt;
        done;
        try_decreasing ~inv:inv depth opt non_increasing to_be_found measure applied_cfr;
      )
    )

let compute_ ?(inv = false) measure applied_cfr program =
  program
  |> Program.sccs
  |> Enum.iter (fun scc ->
         try
           for depth = 1 to !maxDepth do
           if !numberOfGeneratedTemplates < depth then (
           compute_ranking_templates depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
           if inv then
            compute_ranking_templates_real depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
           numberOfGeneratedTemplates := depth);
           backtrack ~inv:inv
                     depth 
                     (TransitionSet.cardinal scc)
                     0
                     (SMTSolver.create ())
                     (Array.of_enum (TransitionSet.enum scc))
                     (Stack.create ())
                     (ref (TransitionSet.cardinal scc))
                     measure
                     program
                     applied_cfr;
          done; 
           scc
           |> TransitionSet.iter (fun t ->
                  if not (RankingTable.mem (ranking_table measure) t) then
                    Logger.(log logger WARN (fun () -> "no_mrf", ["measure", show_measure measure; "transition", Transition.to_id_string t]))
                ) 
         with Exit -> ()
        )

let compute_scc ?(inv = false) measure applied_cfr program scc =
  try
    for depth = 1 to !maxDepth do
    if !numberOfGeneratedTemplates < depth then (
      compute_ranking_templates depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    if inv then
      compute_ranking_templates_real depth (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    numberOfGeneratedTemplates := depth);
    backtrack ~inv:inv
              depth 
              (TransitionSet.cardinal scc)
              0
              (SMTSolver.create ())
              (Array.of_enum (TransitionSet.enum scc))
              (Stack.create ())
              (ref (TransitionSet.cardinal scc))
              measure
              program
              applied_cfr;
  done; 
    scc
    |> TransitionSet.iter (fun t ->
          if not (RankingTable.mem (ranking_table measure) t) then
            Logger.(log logger WARN (fun () -> "no_mrf", ["measure", show_measure measure; "transition", Transition.to_id_string t]))
        ) 
  with Exit -> ()
        

let find ?(inv = false) measure applied_cfr program transition =
  let execute () =
    if inv then (
      if MPRF_Invariants.TemplateTable.is_empty MPRF_Invariants.template_table_inv then
        MPRF_Invariants.compute_invariant_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list););    if RankingTable.is_empty (ranking_table measure) then
      compute_ ~inv:inv measure applied_cfr program;
    (try
       RankingTable.find_all (ranking_table measure) transition
     with Not_found -> [])
    |> List.rev
  in
  Logger.with_log logger Logger.DEBUG
    (fun () -> "find_mrf", ["measure", show_measure measure;
                                          "transition", Transition.to_id_string transition])
    ~result:(Util.enum_to_string to_string % List.enum)
    execute

let find_scc ?(inv = false) measure applied_cfr program transition scc =
    let execute () =
    if inv then (
      if MPRF_Invariants.TemplateTable.is_empty MPRF_Invariants.template_table_inv then
        MPRF_Invariants.compute_invariant_templates (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);); 
    if RankingTable.is_empty (ranking_table measure) then
      compute_scc ~inv:inv measure applied_cfr program scc;
    (try
      RankingTable.find_all (ranking_table measure) transition
    with Not_found -> [])
    |> List.rev
  in
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "find_mrf_scc", ["measure", show_measure measure;
                                                        "transition", Transition.to_id_string transition])
                  ~result:(Util.enum_to_string to_string % List.enum)
                  execute

(* Useful for testing*)
let reset () =
  cache#clear;
  (* MPRF_Invariants.cache#clear; *)
  numberOfGeneratedTemplates := 0;
  RankingTable.clear time_ranking_table;
  RankingTable.clear cost_ranking_table;
  if MPRF_Invariants.TemplateTable.is_empty MPRF_Invariants.template_table_inv then 
    MPRF_Invariants.TemplateTable.clear MPRF_Invariants.template_table_inv;
  List.iter (fun e -> TemplateTable.clear e) !template_tables;
  List.iter (fun e -> TemplateTable.clear e) !template_tables_real;