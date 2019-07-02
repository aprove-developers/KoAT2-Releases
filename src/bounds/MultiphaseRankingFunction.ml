open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes

(** Class is derived from RankingFunction.ml*)

module DummyRank = DummyRF.Make

type constraint_type = [ `Non_Increasing | `Decreasing] [@@deriving show, eq]

let maxDegree = ref 5

type mrf = (Location.t -> Polynomial.t) list

type t = {
  rank : mrf;
  decreasing : Transition.t;
  non_increasing : TransitionSet.t;
  degree : int;
}

let rank f = f.rank

let decreasing f = f.decreasing

let non_increasing f = TransitionSet.to_list f.non_increasing

let degree f = f.degree

(* output methods *)
let rank_to_string (locations: Location.t list) (content_to_string: ((Location.t -> 'a) list) * Location.t -> string) (rank: (Location.t -> 'a) list) =
  locations
  |> List.enum
  |> Util.enum_to_string (fun l -> Location.to_string l ^ ": " ^ content_to_string (rank ,l))

let polyList_to_string ((rank: (Location.t -> 'a) list) , (l : Location.t)) =
  rank
  |> List.enum
  |> Util.enum_to_string (fun p -> Polynomial.to_string(p l) ^ " ")

let only_rank_to_string {rank; decreasing; non_increasing; degree} =
  let locations = non_increasing |> TransitionSet.enum |> Program.locations |> List.of_enum |> List.unique ~eq:Location.equal in
  rank_to_string locations polyList_to_string rank

let to_string {rank; decreasing; non_increasing; degree} =
  "{multirank:" ^ only_rank_to_string {rank; decreasing; non_increasing; degree} ^ ";decreasing:" ^ Transition.to_id_string decreasing ^ "}"



let template_tables: ((ParameterPolynomial.t DummyRank.TemplateTable.t) list) ref= ref []

let list_init degree = 
  template_tables := (List.init degree (fun i -> DummyRank.TemplateTable.create 10))

let fresh_coeffs: Var.t list ref = ref []

let compute_ranking_templates (degree:int) (vars: VarSet.t) (locations: Location.t list): unit =
  let execute (i:int) =
    let ins_loc_prf location =
      (* Each location needs its own ranking template with different fresh variables *)
      let (parameter_poly, fresh_vars) = DummyRank.ranking_template vars in
      (location, parameter_poly, fresh_vars)
    in
    let templates = List.map ins_loc_prf locations in
    templates
    |> List.iter (fun (location,polynomial,_) -> DummyRank.TemplateTable.add (List.nth !template_tables i) location polynomial);
    templates
    |> List.map (fun (_,_,fresh_vars)-> fresh_vars)
    |> List.flatten
    |> (fun fresh_vars -> fresh_coeffs := fresh_vars)
  in
  for i = 0 to degree - 1 do
    Logger.with_log DummyRank.logger Logger.DEBUG
      (fun () -> "compute_ranking_templates_" ^ string_of_int i, [])
      ~result:(fun () ->
          (List.nth !template_tables i)
          |> DummyRank.TemplateTable.enum
          |> Util.enum_to_string (fun (location, polynomial) -> Location.to_string location ^ ": " ^ ParameterPolynomial.to_string polynomial)
        )
      (fun () -> execute i);
  done

let decreaser measure t =
  match measure with
  | `Cost -> TransitionLabel.cost t
  | `Time -> Polynomial.one


(** methods define properties of mrf *)

(* method for mrf and functions f_2 to f_d*)
let transition_constraint_ (template_table0,template_table1, measure, constraint_type, (l,t,l')): Formula.t =
  let template0 = DummyRank.TemplateTable.find template_table0 in
  let template1 = DummyRank.TemplateTable.find template_table1 in
  let poly = ParameterPolynomial.add (template0 l) (template1 l) in
  let atom =
    match constraint_type with
    | `Non_Increasing -> ParameterAtom.Infix.(poly >= ParameterPolynomial.substitute_f (DummyRank.as_parapoly t) (template1 l'))
    | `Decreasing -> ParameterAtom.Infix.(poly >= ParameterPolynomial.(ParameterPolynomial.of_polynomial (decreaser measure t) + substitute_f (DummyRank.as_parapoly t) (template1 l')))
  in
  ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
  |> Formula.mk

(* method for mrf and function f_1*)
let transition_constraint_1 (template_table1, measure, constraint_type, (l,t,l')): Formula.t =
  let template1 = DummyRank.TemplateTable.find template_table1 in
  let atom =
    match constraint_type with
      | `Non_Increasing -> ParameterAtom.Infix.(template1 l >= ParameterPolynomial.substitute_f (DummyRank.as_parapoly t) (template1 l'))
      | `Decreasing -> ParameterAtom.Infix.(template1 l >= ParameterPolynomial.(ParameterPolynomial.of_polynomial (decreaser measure t) + substitute_f (DummyRank.as_parapoly t) (template1 l')))
  in
  ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
  |> Formula.mk

(* method for mrf and function f_d*)
let transition_constraint_d (template_table1, measure, constraint_type, (l,t,l')): Formula.t =
  let template1 = DummyRank.TemplateTable.find template_table1 in
    match constraint_type with
    | `Non_Increasing -> Formula.mk_true
    | `Decreasing  -> (
      let atom = ParameterAtom.Infix.((template1 l)  >= ParameterPolynomial.of_polynomial (decreaser measure t)) in
        ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom
        |> Formula.mk)

(* use all three functions above combined*)
let transition_constraints_ degree (measure, constraint_type, (l,t,l')): Formula.t =
  let res = ref Formula.mk_true in
  for i = 1 to (degree - 1) do
    res := ((List.nth !template_tables (i - 1)), (List.nth !template_tables i), measure, constraint_type, (l,t,l'))
           |> transition_constraint_
           |> Formula.mk_and !res
  done;
  res := ((List.nth !template_tables 0), measure, constraint_type, (l,t,l'))
         |> transition_constraint_1
         |> Formula.mk_and !res;

  res := ((List.nth !template_tables (degree - 1)), measure, constraint_type, (l,t,l'))
         |> transition_constraint_d
         |> Formula.mk_and !res;
  !res

let transition_constraint degree = Util.memoize ~extractor:(Tuple3.map3 Transition.id) (transition_constraints_ degree)

let transitions_constraint degree measure (constraint_type: constraint_type) (transitions : Transition.t list): Formula.t =
  transitions
  |> List.map (fun t -> transition_constraint degree (measure, constraint_type, t))
  |> Formula.all


let non_increasing_constraint degree measure transition =
  transition_constraint degree (measure, `Non_Increasing, transition)

let non_increasing_constraints degree measure transitions =
  transitions_constraint degree measure `Non_Increasing (TransitionSet.to_list transitions)

let decreasing_constraint degree measure  transition =
  transition_constraint degree (measure, `Decreasing, transition)

(** A valuation is a function which maps from a finite set of variables to values *)

let rank_from_valuation degree (i:int) valuation location =
  location
  |> DummyRank.TemplateTable.find (List.nth !template_tables i)
  |> ParameterPolynomial.eval_coefficients (fun var -> DummyRank.Valuation.eval_opt var valuation |? OurInt.zero)

let make degree decreasing_transition non_increasing_transitions valuation  =
{
  rank = List.init degree (fun i -> rank_from_valuation degree i valuation);
  decreasing = decreasing_transition;
  non_increasing = non_increasing_transitions;
  degree = degree;
}

module RankingTable = Hashtbl.Make(struct include Transition let equal = Transition.same end)

let time_ranking_table: t RankingTable.t = RankingTable.create 10

let cost_ranking_table: t RankingTable.t = RankingTable.create 10

let ranking_table = function
  | `Time -> time_ranking_table
  | `Cost -> cost_ranking_table

let try_decreasing degree (opt: DummyRank.Solver.t) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: DummyRank.measure) =
  non_increasing
  |> Stack.enum
  |> Enum.filter (fun t -> not (RankingTable.mem (ranking_table measure) t))
  |> Enum.iter (fun decreasing ->
        Logger.(log DummyRank.logger DEBUG (fun () -> "try_decreasing", ["measure", DummyRank.show_measure measure;
                                                                "decreasing", Transition.to_id_string decreasing;
                                                                "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                                                                "degree", string_of_int degree]));
        DummyRank.Solver.push opt;
        DummyRank.Solver.add opt (decreasing_constraint degree measure decreasing);
        
        if DummyRank.Solver.satisfiable opt then (
          DummyRank.Solver.minimize_absolute opt !fresh_coeffs; (* Check if minimization is forgotten. *)
          DummyRank.Solver.model opt
          |> Option.map (make degree decreasing (non_increasing |> Stack.enum |> TransitionSet.of_enum))
          |> Option.may (fun ranking_function ->
              to_be_found := !to_be_found - 1;
              RankingTable.add (ranking_table measure) decreasing ranking_function;
              Logger.(log DummyRank.logger INFO (fun () -> "add_ranking_function", [
                  "measure", DummyRank.show_measure measure;
                  "decreasing", Transition.to_id_string decreasing;
                  "non_increasing", Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing);
                  "rank", only_rank_to_string ranking_function];))
            )
        );
        DummyRank.Solver.pop opt;
    );
  if !to_be_found <= 0 then
    raise Exit


let rec backtrack (steps_left: int) (index: int) (opt: DummyRank.Solver.t) (scc: Transition.t array) (non_increasing: Transition.t Stack.t) (to_be_found: int ref) (measure: DummyRank.measure) =
    for degree = 1 to !maxDegree do
    if DummyRank.Solver.satisfiable opt then (
      if steps_left == 0 then (
        try_decreasing degree opt non_increasing to_be_found measure
      ) else (
        for i=index to Array.length scc - 1 do
          let transition = Array.get scc i in
          DummyRank.Solver.push opt;
          DummyRank.Solver.add opt (non_increasing_constraint degree measure transition);
          Stack.push transition non_increasing;
          backtrack (steps_left - 1) (i + 1) opt scc non_increasing to_be_found measure;
          ignore (Stack.pop non_increasing);
          DummyRank.Solver.pop opt;
          try_decreasing degree opt non_increasing to_be_found measure;
        done;
      )
    )
  done

let compute_ measure program =
  program
  |> Program.sccs
  |> Enum.iter (fun scc ->
      try
        backtrack (TransitionSet.cardinal scc)
          0
          (DummyRank.Solver.create ())
          (Array.of_enum (TransitionSet.enum scc))
          (Stack.create ())
          (ref (TransitionSet.cardinal scc))
          measure;
        scc
        |> TransitionSet.iter (fun t ->
            if not (RankingTable.mem (ranking_table measure) t) then
              Logger.(log DummyRank.logger WARN (fun () -> "no_ranking_function", ["measure", DummyRank.show_measure measure; "transition", Transition.to_id_string t]))
          )
      with Exit -> ()
    )

let find ?(degree = 5) measure program transition =
  let execute () =
    (** or 2 or 3 or ... d *)
    if DummyRank.TemplateTable.is_empty (List.nth !template_tables 0) then
        compute_ranking_templates !maxDegree (Program.input_vars program) (program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list);
    if RankingTable.is_empty (ranking_table measure) then
      compute_ measure program;
    (try
       RankingTable.find_all (ranking_table measure) transition
     with Not_found -> [])
    |> List.rev
  in
  Logger.with_log DummyRank.logger Logger.DEBUG
    (fun () -> "find_ranking_functions", ["measure", DummyRank.show_measure measure;
                                          "transition", Transition.to_id_string transition])
    ~result:(Util.enum_to_string to_string % List.enum)
    execute


(* only for testing*)
let reset () =
  RankingTable.clear time_ranking_table;
  RankingTable.clear cost_ranking_table;
  for i = 0 to List.length !template_tables - 1 do
    DummyRank.TemplateTable.clear (List.nth !template_tables i)
  done