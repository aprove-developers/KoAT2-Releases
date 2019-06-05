open Batteries
open Polynomials
open BoundsInst
open Formulas
open ProgramTypes

let logger = Logging.(get ExpSize)

type kind = [ `Lower | `Upper ] [@@deriving show]

let default_bound = RealBound.infinity

let max_detsizebound ((gt,l),var) get_sizebound =
  GeneralTransition.transitions gt
  |> TransitionSet.filter (Location.equal l % Transition.target)
  |> TransitionSet.to_list
  |> List.map (fun t -> get_sizebound (t,var) |> RealBound.of_intbound)
  |> List.enum
  |> RealBound.maximum

let formula_implied_by_formula formula1 formula2 =
  formula2
  (* Does formula1 always imply formula2 ? *)
  |> Formula.implies formula1
  |> Formula.neg
  (* Try to find a contra *)
  |> SMT.Z3Opt.unsatisfiable

let guard_holds_by_invariants t =
  let label = Transition.label t in
  label
  |> TransitionLabel.guard_without_invariants
  |> Formula.mk
  |> formula_implied_by_formula (TransitionLabel.invariants label |> Formula.mk)

module Traverse = Graph.Traverse.Bfs(TransitionGraph)
module ERV = ApproximationModules.ERV


(* (start_location, target_location, l) *)
let pr_cache: (Location.t * Location.t * Location.t, OurFloat.t option) Hashtbl.t = Hashtbl.create 10

let guard_enabled gt =
  let module Solver = SMT.Z3Opt in
  GeneralTransition.invariants gt
  |> Formula.mk
  |> Formula.implies (GeneralTransition.guard gt |> Formula.mk)
  |> fun f ->
      try Solver.tautology f
      with (Failure _) -> false

let only_one_gt_outgoing program gt =
  let start_loc = GeneralTransition.start gt in
  let gts = Program.generalized_transitions program in
  gts
  |> GeneralTransitionSet.filter (Location.equal start_loc % GeneralTransition.start)
  |> fun gtset -> GeneralTransitionSet.for_all
       ( fun gt ->
          let gtset_without_gt = GeneralTransitionSet.remove gt gtset in
          let rhs              =
            GeneralTransitionSet.fold
              (fun gtselem f -> Formula.mk_or f (GeneralTransition.guard gtselem |> Formula.mk))
              gtset_without_gt Formula.mk_false
          in
          formula_implied_by_formula (GeneralTransition.guard gt |> Formula.mk) (Formula.neg rhs)
       ) gtset

(** Returns the maximum of all incoming sizebounds applied to the local sizebound.
    Corresponds to 'SizeBounds for trivial SCCs':
    Here we add all incomin bounds on absolute values *)
let incoming_bound program get_sizebound_abs get_expsizebound (exp_upd_poly: RealBound.t) gt (pr_func: Location.t -> OurFloat.t option) =
  let execute () =
    let substitute_with_prevalues gtset =
      let prevalues_exp var =
        GeneralTransitionSet.to_list gtset
        |> List.map (fun gt' -> get_expsizebound ((gt',GeneralTransition.start gt),var))
        |> List.enum
        |> RealBound.sum
      in
      let prevalues var =
        GeneralTransitionSet.to_list gtset
        |> List.map (fun gt' -> max_detsizebound ((gt', GeneralTransition.start gt), var) get_sizebound_abs)
        |> List.enum |> RealBound.maximum
      in
      RealBound.appr_substitution_probabilistic_and_nonprobabilistic
        (* Propagate expected values if possible *)
        ~probabilistic:(prevalues_exp)
        ~nonprobabilistic:(prevalues) exp_upd_poly
    in
    let pre_gts = Program.pre_gt program gt in
    let substitute_with_pr_prevalues gtset (pr_func_unboxed: Location.t -> OurFloat.t option) =
      let pre_with_pr =
        GeneralTransitionSet.to_list gtset
        |> List.map (fun gt' -> pr_func_unboxed (GeneralTransition.start gt')
                                |> Option.map (fun pr -> (pr,gt')))
        |> Util.option_sequence
      in
      match pre_with_pr with
        | None -> None
        | Some pres_with_prs ->
            (* Check if we can subsitute the variables in the bounds due to linearity. If this is not possible
             * substitute the bound with deterministic upper size bounds *)
            let substitution_exp var =
              pres_with_prs
              |> List.fold_left
                   (fun b (pr,gt') -> RealBound.( b + ((of_constant pr) * (get_expsizebound ((gt',GeneralTransition.start gt),var) )) ))
                   RealBound.zero
            in
            let substitution var =
              pres_with_prs
              |> List.fold_left
                   (fun b (pr,gt') -> RealBound.( b + ((of_constant pr) * max_detsizebound ((gt',GeneralTransition.start gt),var) get_sizebound_abs) ))
                   RealBound.zero
            in
            Some (RealBound.appr_substitution_probabilistic_and_nonprobabilistic
              (* Propagate expected values if possible *)
              ~probabilistic:(substitution_exp)
              ~nonprobabilistic:(substitution) exp_upd_poly )
    in
    let pr_prevalues = substitute_with_pr_prevalues pre_gts pr_func in
    match pr_prevalues with
      | None ->
          pre_gts
          |> substitute_with_prevalues
      | Some bound ->
          bound

  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute highest incoming bound", ["exp_upd_poly", RealBound.to_string exp_upd_poly;
                                                                   "transition", GeneralTransition.to_string gt])
                  ~result:RealBound.to_string
                  execute


let every_path_enters_loc graph start_location middle_location target_location : bool=
  let graph_without_middle =
    graph
    |> (flip TransitionGraph.remove_vertex) middle_location
  in
  let reachable_locations =
    try Traverse.fold_component LocationSet.add LocationSet.empty graph_without_middle (start_location)
    (* fails if middle_location = start_location *)
    with e -> LocationSet.empty
  in
  (* If we cut out middle location and in the resulting graph target_location is still reachable then there is also path
   * in the original graph reaching target_location without entering middle_location*)
  if Location.equal middle_location start_location then
    true
  else
    not (LocationSet.mem target_location reachable_locations)

let gts_outgoing tset loc =
  GeneralTransitionSet.of_transitionset tset
  |> GeneralTransitionSet.filter (Location.equal loc % GeneralTransition.start)

let build_subgraph (graph : TransitionGraph.t) (loc : Location.t) (cut_location : Location.t): TransitionGraph.t =
  let cut_graph = TransitionGraph.remove_vertex graph cut_location in
  let reachable_locations =
    try Traverse.fold_component LocationSet.add LocationSet.empty cut_graph loc
    (* if loc and cut_location are the same an exception is thrown. However this should be valid, so we want to return an empty subgraph *)
    with e -> LocationSet.empty
  in
  let subgraph_transitions =
    TransitionGraph.transitions graph
    |> TransitionSet.filter ((flip LocationSet.mem) reachable_locations % Transition.src)
    (* cut target location *)
    |> TransitionSet.filter (not % Location.equal cut_location % Transition.target)
  in
  LocationSet.fold (fun l g -> TransitionGraph.add_vertex g l) reachable_locations TransitionGraph.empty
  |> TransitionSet.fold (fun t g -> TransitionGraph.add_edge_e g t) subgraph_transitions

(* This function is used to check whether there is only one gt going from the subgraph to the the target location.
 * Because the get pr function is recursively applied this check can not be done in subgraph criteria. Hence we check this when
 * recursively calling the get_pr function and reaching the terminating state.
 * Also we want to make sure, that all outgoing transitions of the outgoing gt reach the target location *)

let only_one_gt_going_to_loc orig_graph target_loc sub =
  let locations = TransitionGraph.locations sub in
  let transitions =
    TransitionGraph.generalized_transitions orig_graph
    |> GeneralTransitionSet.filter (fun gt -> LocationSet.mem (GeneralTransition.start gt) locations &&
                                              LocationSet.exists (Location.equal target_loc) (GeneralTransition.targets gt))
  in
  GeneralTransitionSet.for_all ((=) 1 % LocationSet.cardinal % GeneralTransition.targets) transitions &&
  GeneralTransitionSet.cardinal transitions = 1

let check_subgraphs_criteria program get_timebound orig_graph entry_loc target_loc subs =

  (* Check if for each location the disjunction of the outgoing transitions guard is always implied by the invariants. I.e. the
   * transitionsystem doesn't get stuck in the subgraphs *)
  let all_guards_tautology_in_invariants =
    let for_subgraph sub =
      let gts_from_loc l=
        TransitionGraph.generalized_transitions orig_graph
        |> GeneralTransitionSet.filter (Location.equal l % GeneralTransition.start)
      in
      sub
      |> TransitionGraph.locations
      |> LocationSet.to_list
      |> List.map (fun l -> (l,gts_from_loc l))
      |> List.map (Tuple2.map2 (List.map
                                 (Formula.mk % GeneralTransition.guard_without_invariants) % GeneralTransitionSet.to_list))
      |> List.map (Tuple2.map2 (List.fold_left Formula.mk_or Formula.mk_false))
      (* Is the disjunction of the invariants implied by the guard at the given location? *)
      |> List.map (fun (l, formula) -> formula_implied_by_formula (Program.invariant l program |> Formula.mk) formula)
      |> List.for_all identity
    in

    subs |> List.for_all for_subgraph
  in

  (* Check if all transitions in the subgraphs have finite runtime. This also prevents getting stuck in the subgraph *)
  let termination =
    subs
    |> List.for_all
        (fun subgraph ->
           TransitionGraph.transitions subgraph
           |> TransitionSet.for_all ((fun b -> not (Bound.is_infinity b|| Bound.is_minus_infinity b)) % get_timebound)
        )
  in

  (* Check that all subgraphs are pairwise disjoint *)
  let disjoint =
    (* Checks if p is disjoint to all others graphs in rest *)
    let helper_fun g rest =
      ListMonad.(rest >>= (fun g2 ->
                             let lset1 = TransitionGraph.locations g in
                             let lset2 = TransitionGraph.locations g2 in
                             LocationSet.disjoint lset1 lset2 |> pure) )
      |> List.for_all identity
    in

    let rec rec_procedure subgs =
      match subgs with
        | [] -> failwith "impossible"
        | [g] -> true
        | (g :: gs) -> helper_fun g gs && rec_procedure gs
    in
    rec_procedure subs
  in

  (* if a general transitions contains a transition leaving the subgraph towards the target location then the set of targets of
   * this general transition only contains the target location. Also there is only on gt leaving each subgraph and entering the
   * target location*)
  let all_trans_of_gt_going_to_loc =
    let for_subgraph subgraph =
      TransitionGraph.generalized_transitions subgraph
      |> GeneralTransitionSet.filter
           (TransitionSet.exists (fun (_,_,l') -> Location.equal l' target_loc) % GeneralTransition.transitions)
      |> GeneralTransitionSet.for_all ((=) 1 % LocationSet.cardinal % GeneralTransition.targets)
    in
    List.for_all for_subgraph subs
  in

  let criteria =
    [all_guards_tautology_in_invariants;termination;disjoint;
     all_trans_of_gt_going_to_loc]
  in
  List.for_all identity criteria

let subgraph_connected_with_loc (orig_graph: TransitionGraph.t) (subgraph: TransitionGraph.t) (loc : Location.t): bool =
  let locations = TransitionGraph.locations subgraph in
  let outgoing_trans = TransitionGraph.transitions orig_graph
                       |> TransitionSet.filter (fun (l,_,_) -> LocationSet.mem l locations)
  in
  outgoing_trans
  |> TransitionSet.filter (fun (_,_,l) -> Location.equal l loc)
  |> (<) 0 % TransitionSet.cardinal


(* Memorizes the already computed pr function values *)
let lookup_cache graph ((start_location, target_location, l): Location.t * Location.t * Location.t)  (f: unit -> Location.t -> OurFloat.t option) =
  match Hashtbl.find_option pr_cache (start_location, target_location,l) with
  | Some p -> p
  | None ->
      let pr_function = f () in
      TransitionGraph.locations graph
      |> LocationSet.iter (fun l' -> Hashtbl.add pr_cache (start_location, target_location,l') (pr_function l'));
      Hashtbl.find pr_cache (start_location, target_location,l)


let runtime_at_most_one get_timebound program l =
  Program.transitions program
  |> TransitionSet.filter (Location.equal l % Transition.target)
  |> TransitionSet.enum
  |> Enum.map get_timebound
  |> Bound.sum
  |> Bound.get_constant_option
  |> fun co ->
      match co with
      | Some a -> (OurInt.compare a (OurInt.of_int 2)) < 0
      | None -> false


let rec get_pr (program: Program.t) (graph: TransitionGraph.t) (start: Location.t) (locs: LocationSet.t) (loc: Location.t) get_timebound:
  (Location.t-> OurFloat.t option) =
  let subgraph_cardinals_combined transitions_and_graphs_list =
    List.fold_left (fun ctr (_,g) -> ctr + (LocationSet.cardinal (TransitionGraph.locations g) )) 0 transitions_and_graphs_list
  in

  (*Search for candidates of subgraphs *)
  let candidates =
    locs
    |> LocationSet.to_list
    (* We don't want to branch on locations that are the same as target locations. *)
    |> List.filter (not % Location.equal loc)
    (* Check if all incoming transitions non-prob time bound sum up to at most 1, i.e. Ct_g,l < 2 *)
    |> List.filter (runtime_at_most_one get_timebound program)
    |> List.filter (fun ml -> every_path_enters_loc graph start ml loc)
    |> List.map (fun l -> (l,gts_outgoing (TransitionGraph.transitions graph) l))
    (* only one outgoing transition supported *)
    |> List.filter (fun (_,gtset) -> GeneralTransitionSet.cardinal gtset = 1)
    (* more than one target for non-linear branching *)
    |> List.filter (fun (_,gtset) -> GeneralTransitionSet.to_list gtset |> List.hd |> GeneralTransition.targets |> ((<) 1 % LocationSet.cardinal) )
    (* Build subgraphs *)
    |> List.map (fun (l,gt_from_l) ->
        gt_from_l
        (* Exactly one outgoing gt as filtered above *)
        |> GeneralTransitionSet.any
        |> GeneralTransition.transitions |> TransitionSet.to_list
        (* Only one GeneralTransition here so the next call always works as intended *)
        |> List.map Transition.target
        (* filter duplicate locations *)
        |> LocationSet.of_list |> LocationSet.to_list
        |> List.map (fun l -> (l,build_subgraph graph l loc))
        (*check whether the location loc is reachable from the subgraph if it would not have been cut away in build subgraph *)
        |> List.filter (fun (_,subgraph) -> subgraph_connected_with_loc graph subgraph loc)
        (* l is the branching location *)
        |> fun x -> (l,x) )
    (* filter all candidate with empty subgraph list. *)
    |> List.filter (fun (_,sub_graph_list) -> (List.length sub_graph_list) > 0)
    (* Sort by cardinality of all subgraphs combined *)
    |> List.sort (fun (_,a) (_,b) -> (flip Int.compare) (subgraph_cardinals_combined a) (subgraph_cardinals_combined b))
    |> List.filter (fun (lentry,subs) -> List.map Tuple2.second subs |> check_subgraphs_criteria program get_timebound graph lentry loc)
  in

  (* Construction of the pr function *)
  match candidates with
  | [] -> const None
  | ((lentry,subs) :: _) ->
      (* Here the returned pr function starts *)
      (fun l ->
        let is_contained_in_sub =
          subs
          |> List.filter (LocationSet.mem l % TransitionGraph.locations % Tuple2.second)
          (* This list contains at most one element *)
          |> Util.safe_head
        in
        match is_contained_in_sub with
        | None -> None
        | Some (sub_start,subgraph) ->
            (* Starting the recursive call on the subgraph l is contained in
             * but first check if it was already memoized*)

            match Hashtbl.find_option pr_cache (start,loc,l) with
            | Some p -> p
            | None ->
                let rec_result =
                  lookup_cache graph (sub_start,loc,l) (fun () -> get_pr program graph sub_start (TransitionGraph.locations subgraph) loc get_timebound)
                in

                let rel_trans_outgoing =
                  TransitionGraph.generalized_transitions graph
                  |> GeneralTransitionSet.filter (Location.equal lentry % GeneralTransition.start)
                  |> GeneralTransitionSet.to_list
                  (* There is always exactly one outgoing gt *)
                  |> List.hd
                  |> GeneralTransition.transitions
                  |> TransitionSet.filter (fun t -> List.exists (Location.equal (Transition.target t) % Tuple2.first) subs)
                in
                let total_probability =
                  rel_trans_outgoing |> TransitionSet.to_list |> List.map (TransitionLabel.probability % Transition.label)
                  |> List.fold_left OurFloat.(+) OurFloat.zero
                in
                let probability_reaching_loc l =
                  rel_trans_outgoing |> TransitionSet.filter (Location.equal l % Transition.target)
                  |> TransitionSet.to_list |> List.map (TransitionLabel.probability % Transition.label)
                  |> List.fold_left (OurFloat.(+)) (OurFloat.of_float 0.0)
                in

                (* if the recursive call to get_pr returns None, then no subgraphs exists such that the pr function can be refined.
                 * Therefore we check here if there is only outgoing gt to the target location in the current subgraph*)
                let is_valid =
                  match rec_result with
                  | None -> only_one_gt_going_to_loc graph loc subgraph
                  | Some _ -> true
                in

                match (rec_result, is_valid) with
                | (_, false) -> None
                | (None,true) -> Some (OurFloat.(/) (probability_reaching_loc sub_start) (total_probability))
                | (Some p, true) -> Some OurFloat.((probability_reaching_loc sub_start / total_probability) * p) )

let print_pr_func graph pr_func =
  let print_option_float oflaot =
    match oflaot with
    | None   -> "None"
    | Some s -> s
  in
  TransitionGraph.locations graph |> LocationSet.to_list
  |> List.map
       (fun l ->
          (Location.to_string l, pr_func l |> Option.map (OurFloat.to_string) |> print_option_float))
  |> List.enum
  |> Util.enum_to_string (fun (l,p) -> l ^ ", " ^ p)

(** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'. *)
let compute program get_sizebound (get_expsizebound: (GeneralTransition.t * Location.t) -> Var.t -> RealBound.t) get_timebound ((gt,loc),var) =
  if not (only_one_gt_outgoing program gt) then
    default_bound
  else
    let graph = Program.graph program in
    let start_loc = Program.start program in
    let locations = TransitionGraph.locations graph in
    let transition_location = GeneralTransition.start gt in
    let pr_func = fun l -> lookup_cache graph
                             (start_loc,transition_location,l)
                             (fun () -> get_pr program graph start_loc locations transition_location get_timebound)
    in
    let exp_upd_poly = ExpLocalSizeBound.elsb program ((gt,loc),var) in

    let execute () =
      if Program.is_initial_gt program gt then
        exp_upd_poly
      else
        incoming_bound
          program
          get_sizebound
          (uncurry get_expsizebound) exp_upd_poly gt pr_func
    in Logger.with_log logger Logger.DEBUG
                         (fun () -> "compute expected trivial bound",
                                    [ "rv", ERV.to_id_string ((gt,loc),var); "pr_func", print_pr_func graph pr_func
                                    ; "elsb", RealBound.to_string exp_upd_poly])
                         ~result:RealBound.to_string
                         execute

let reset_cache =
  Hashtbl.clear pr_cache