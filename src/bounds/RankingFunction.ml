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
  "{rank:" ^ only_rank_to_string {rank; decreasing; non_increasing} ^ "; decreasing:" ^ Transition.to_id_string decreasing ^ "; non_increasing: " ^ TransitionSet.to_string non_increasing ^ "}"

(*
  Return the update parameter polynomial and a set of constraints on this update.
  The constraints can for example encode the support of probability distributions.
*)
let as_parapoly label var: ParameterPolynomial.t * Constraint.t =
  match TransitionLabel.update label var with
  (** Correct? In the nondeterministic case we just make it deterministic? *)
  (** TODO This is not the correct encoding a nondeterministic update, but rather of an identity update *)
  | None -> ParameterPolynomial.of_var var, Constraint.mk_true
  | Some (TransitionLabel.UpdateElement.Poly p) -> ParameterPolynomial.of_polynomial p, Constraint.mk_true
  (** TODO is there a better way in the probabilistic case ? *)
  | Some (TransitionLabel.UpdateElement.Dist d) ->
      let var' = Var.fresh_id Var.Int () in
      ParameterPolynomial.of_var var', ProbDistribution.guard d var var'

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

module VarMap = Map.Make(Var)
let transition_constraint_ cache (measure, constraint_type, (l,t,l')): Formula.t =
  let template = TemplateTable.find (get_template_table cache) in
  let para_polys_with_constrs =
    ParameterPolynomial.vars (template l')
    |> fun vset ->
        VarSet.fold
          (fun v (vmap,cs) -> let (para_poly, c) = as_parapoly t v in VarMap.add v para_poly vmap, Constraint.mk_and c cs)
          vset (VarMap.empty, Constraint.mk_true)
  in
  let atom =
    match constraint_type with
    | `Non_Increasing ->
        ParameterAtom.Infix.(template l >= ParameterPolynomial.substitute_f (flip VarMap.find (Tuple2.first para_polys_with_constrs)) (template l')),
        Constraint.mk_and (TransitionLabel.guard t) (Tuple2.second para_polys_with_constrs)
    | `Decreasing ->
        ParameterAtom.Infix.(template l >= ParameterPolynomial.(of_polynomial (decreaser measure t) + substitute_f (flip VarMap.find (Tuple2.first para_polys_with_constrs)) (template l'))),
        Constraint.mk_and (TransitionLabel.guard t) (Tuple2.second para_polys_with_constrs)
    | `Bounded ->
        ParameterAtom.Infix.(template l >= ParameterPolynomial.of_polynomial (decreaser measure t)), TransitionLabel.guard t
  in
  atom
  |> fun (a,c) -> ParameterConstraint.farkas_transform c a
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
           Solver.minimize_absolute_old opt !fresh_coeffs;
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

(*
  For now we use the heuristic implementation below.
  It may be less accurate (although they perform similar on many examples) but is MASSIVELY faster,
  especially on graph with many general transitions where each of them consist of many non-general transitions
*)
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

module Dijkstra =
  Graph.Path.Dijkstra
    (TransitionGraph)
    (struct
      type edge = Transition.t
      type t = int
      let weight = const 1
      let compare = compare
      let add = (+)
      let zero = 0
    end)

module TransitionMap = Map.Make(
  struct
    type t = Transition.t
    let compare = Transition.compare_same
  end)

(* Compute the shortest path from a node to itself while not considering self-loops *)
let shortest_self_path_no_loop graph l =
  TransitionGraph.locations graph
  |> LocationSet.enum
  |> Enum.filter (not % Location.equal l)
  |> Enum.map
      (* Intermediate node not equal to l *)
      (fun v ->
          let composed_path () =
            let (p1, l1) = Dijkstra.shortest_path graph l v in
            let (p2, l2) = Dijkstra.shortest_path graph v l in
            Some (p1@p2, l1+l2)
          in
          try
            composed_path ()
          with Not_found -> None
      )
  |> Util.cat_maybes_enum
  |> List.of_enum

let next_candidates not_added_graph graph =
  let already_connected_locations = TransitionGraph.locations graph |> LocationSet.enum in
  (*
    We want to close circles as fast as possible. Hence for all pairs of already added locations
    l1 and l2, check how many transitions of the non added transitions must be added to obtain
    a path l1 -> l2.
  *)
  Enum.cartesian_product (Enum.clone already_connected_locations) (Enum.clone already_connected_locations)
  |> Enum.map
      (fun (l1,l2) ->
(*         Dijkstra does not deal with self-loops *)
        if not (Location.equal l1 l2) then
          try
            Some (Dijkstra.shortest_path not_added_graph l1 l2)
          with Not_found -> None
        else
          TransitionGraph.find_all_edges not_added_graph l1 l2
          |> fun ts ->  List.nth_opt ts 0
          |> fun o -> if Option.is_none o then List.nth_opt (shortest_self_path_no_loop not_added_graph l1) 0 else o
          |> Option.map (fun t -> List.singleton t, 1)
      )
  |> Util.cat_maybes_enum
  (* Which transition now is the begin of a path l1->l2? How long is a shortest such path? How many exist there?*)
  |> Enum.fold
      (fun tmap (path,length) ->
        let first_trans = List.hd path in
        TransitionMap.modify_def
          ([], Int.max_num) first_trans
          (fun (paths,length_shortest) -> path::paths, Int.min length length_shortest) tmap
      )
      TransitionMap.empty
  |> TransitionMap.enum
  |> List.of_enum
  |> List.sort
      (fun (gt1,(paths1,min_length1)) (gt,(paths2,min_length2))->
        compare (min_length1, List.length paths1) (min_length2, List.length paths2))
  |> List.map (fst % snd)
  |> List.flatten
  |> tap (fun l ->
        Logger.log logger Logger.DEBUG
        (fun () -> "next_candidates",
          ["candidates", Util.enum_to_string (Util.enum_to_string Transition.to_id_string) % List.enum @@ List.map List.enum l]
        )
      )

let close_next_cycle cache measure solver not_added_graph graph: TransitionSet.t =
  let all_paths = next_candidates not_added_graph graph in
  let rec close_circle path: bool * TransitionSet.t = match path with
    | []    -> true, TransitionSet.empty
    | e::es ->
        Solver.push solver;
        Solver.add solver (non_increasing_constraint cache measure e);
        if Solver.satisfiable solver then
          let (closed, non_inc_set) = close_circle es in
          Solver.pop solver;
          closed, TransitionSet.add e non_inc_set
        else
          (Solver.pop solver; false, TransitionSet.empty)
  in
  let rec choose_circle paths tset = match paths with
    | []    -> tset
    | p::ps ->
        let (closed_curr, tset_curr) = close_circle p in
        if closed_curr then
          tset_curr
        else
          let best_tset =
            if TransitionSet.cardinal tset >= TransitionSet.cardinal tset_curr then
              tset
            else
              tset_curr
          in
          choose_circle ps best_tset
  in
  choose_circle all_paths TransitionSet.empty
  |> tap (fun tset -> Logger.log logger Logger.DEBUG (fun () -> "close_next_cycle", ["add to non_inc_tset", TransitionSet.to_string tset]))
  (* Add Constraints to the Solver *)
  |> tap (
      TransitionSet.iter (Solver.add solver % non_increasing_constraint cache measure)
      )


let find_non_inc_set cache measure solver decreasing try_non_inc_set =
  let decr_graph =
    TransitionGraph.empty
    |> flip TransitionGraph.add_edge_e decreasing
  in
  let not_added_graph =
    TransitionGraph.empty
    |> TransitionSet.fold (flip TransitionGraph.add_edge_e) try_non_inc_set
  in
  let rec try_close_cycles n_a_g g =
    let next_non_inc_set = close_next_cycle cache measure solver n_a_g g in
    if TransitionSet.is_empty next_non_inc_set then
      TransitionGraph.transitions g
    else
      let n_a_g' = TransitionSet.fold (flip TransitionGraph.remove_edge_e) next_non_inc_set n_a_g in
      let g' = TransitionSet.fold (flip TransitionGraph.add_edge_e) next_non_inc_set g in
      try_close_cycles n_a_g' g'
  in

  try_close_cycles not_added_graph decr_graph

let compute_and_add_ranking_function cache measure all_trans decreasing: unit =
  let try_non_inc_set = TransitionSet.remove decreasing all_trans in
  let solver = Solver.create () in
  Solver.add solver (decreasing_constraint cache measure decreasing);
  Solver.add solver (bounded_constraint cache measure decreasing);
  let satinit = Solver.satisfiable solver in
  if Solver.satisfiable solver then
    let non_inc = find_non_inc_set cache measure solver decreasing try_non_inc_set in
    Solver.minimize_absolute_old solver !fresh_coeffs;
    Solver.model solver
    |> Option.map (make cache decreasing non_inc)
    |> Option.may (fun rank_func ->
      RankingTable.add (get_ranking_table measure cache) decreasing rank_func;
      Logger.(log logger INFO (fun () -> "add_ranking_function", [
                                    "measure", show_measure measure;
                                    "decreasing", Transition.to_id_string decreasing;
                                    "non_increasing", TransitionSet.to_string non_inc;
                                    "rank", only_rank_to_string rank_func]))
    )

let compute_ cache measure program =
  program
  |> Program.sccs
  |> Enum.iter (fun scc -> TransitionSet.iter (compute_and_add_ranking_function cache measure scc) scc)

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