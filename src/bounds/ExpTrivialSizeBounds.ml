open Batteries
open Polynomials
open BoundsInst
open Formulas
open ProgramTypes

let logger = Logging.(get Size)

type kind = [ `Lower | `Upper ] [@@deriving show]

let get_expected_value_propagation kind =
  match kind with
  | `Upper -> ExpLocalSizeBound.poly_is_concave_in_all_vars
  | `Lower -> ExpLocalSizeBound.poly_is_concave_in_all_vars

let get_bound_from_enum kind =
  match kind with
  | `Upper -> RealBound.maximum
  | `Lower -> RealBound.minimum

let max_detsizebound kind ((gt,l),var) get_sizebound =
  GeneralTransition.transitions gt
  |> TransitionSet.filter (Location.equal l % Transition.target)
  |> TransitionSet.to_list
  |> List.map (fun t -> get_sizebound kind (t,var) |> RealBound.of_intbound)
  |> List.enum
  |> get_bound_from_enum kind

let guard_holds_by_invariants t = 
  let label = Transition.label t in
  label
  |> TransitionLabel.guard_without_invariants
  |> Formula.mk
  (* Do the invariants always imply the guard to be true? *)
  |> Formula.implies (TransitionLabel.invariants label |> Formula.mk)
  |> Formula.neg
  (* Try to find a contra *)
  |> SMT.Z3Solver.unsatisfiable

module Traverse = Graph.Traverse.Bfs(TransitionGraph)
module GTRV = RVGTypes.Make_RV(struct
                                 include GeneralTransition
                                 let compare_equivalent = GeneralTransition.compare
                                 let compare_same = GeneralTransition.compare
                                 let equivalent = GeneralTransition.same
                               end)

(** Returns the maximum of all incoming sizebounds applied to the local sizebound.
    Corresponds to 'SizeBounds for trivial SCCs':
    S'(alpha) = max(S_l(alpha)(S(t',v_1),...,S(t',v_n)) for all t' in pre(t)) *)
let incoming_bound kind program get_sizebound get_expsizebound (exp_poly: RealPolynomial.t) gt (pr_func: Location.t -> OurFloat.t option) =
  let execute () =
    let substitute_with_prevalues gtset =
      let prevalues_exp kind var =
        GeneralTransitionSet.to_list gtset
        |> List.map (fun gt' -> get_expsizebound kind ((gt',GeneralTransition.start gt),var))
        |> List.enum
        |> get_bound_from_enum kind
      in
      let prevalues kind var =
        GeneralTransitionSet.to_list gtset
        |> List.map (fun gt' -> max_detsizebound kind ((gt', GeneralTransition.start gt), var) get_sizebound)
        |> List.enum |> get_bound_from_enum kind
      in
      if get_expected_value_propagation kind gt exp_poly then
        RealBound.appr_substitution kind ~lower:(prevalues_exp `Lower) ~higher:(prevalues_exp `Upper) (RealBound.of_poly exp_poly)
      else
        RealBound.appr_substitution kind ~lower:(prevalues `Lower) ~higher:(prevalues `Upper) (RealBound.of_poly exp_poly)
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
            (* Check if we can subsitute the variables in the bounds due to concavity/convexity. If this is not possible
             * substitute the bound with deterministic upper size bounds *)
            let substitution_exp kind var =
              pres_with_prs
              |> List.fold_left
                   (fun b (pr,gt') -> RealBound.( b + ((of_constant pr) * (get_expsizebound kind ((gt',GeneralTransition.start gt),var) )) ))
                   RealBound.zero
            in
            let substitution kind var =
              pres_with_prs
              |> List.fold_left
                   (fun b (pr,gt') -> RealBound.( b + ((of_constant pr) * max_detsizebound kind ((gt',GeneralTransition.start gt),var) get_sizebound) ))
                   RealBound.zero
            in
            if get_expected_value_propagation kind gt exp_poly then
              Some (RealBound.of_poly exp_poly
                    |> RealBound.appr_substitution
                         kind
                         ~lower:(substitution_exp `Lower)
                         ~higher:(substitution_exp `Upper) )
            else
              Some (RealBound.of_poly exp_poly
                    |> RealBound.appr_substitution
                         kind
                         ~lower:(substitution `Lower)
                         ~higher:(substitution `Upper) )
    in
    let pr_prevalues = substitute_with_pr_prevalues pre_gts pr_func in
    match pr_prevalues with
      | None ->
          pre_gts
          |> substitute_with_prevalues
      | Some (bound) ->
          bound
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute highest incoming bound", ["exp_poly", (RealBound.to_string % RealBound.of_poly) exp_poly;
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
  GeneralTransitionSet.from_transitionset tset
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
    TransitionGraph.transitions orig_graph
    |> GeneralTransitionSet.from_transitionset 
    |> GeneralTransitionSet.filter (fun gt -> LocationSet.mem (GeneralTransition.start gt) locations &&
                                              LocationSet.exists (Location.equal target_loc) (GeneralTransition.targets gt))
  in
  GeneralTransitionSet.for_all ((=) 1 % LocationSet.cardinal % GeneralTransition.targets) transitions &&
  GeneralTransitionSet.cardinal transitions = 1

let check_subgraphs_criteria get_timebound orig_graph entry_loc target_loc subs =
  let all_guards_tautology_in_invariants =
    subs
    |> List.for_all
         (TransitionSet.for_all 
           (guard_holds_by_invariants)
             
            % TransitionGraph.transitions)
  in

  let transitions_going_to_loc_guard_tautology = 
    let check_for_sub sub = 
      let locations = TransitionGraph.locations sub in
      let transitions = 
        TransitionGraph.transitions orig_graph
        |> TransitionSet.filter (fun (l,_,l') -> LocationSet.mem l locations && Location.equal l' target_loc)
      in
      TransitionSet.for_all guard_holds_by_invariants transitions

    in
    List.for_all check_for_sub subs
  in

  let termination =
    subs
    |> List.for_all
        (fun subgraph ->
           TransitionGraph.transitions subgraph
           |> TransitionSet.for_all ((fun b -> not (Bound.is_infinity b|| Bound.is_minus_infinity b)) % get_timebound)
        )
  in

  let no_interleaving =
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

  let all_trans_of_gt_going_to_loc =
    let for_subgraph subgraph =
      TransitionGraph.transitions subgraph
      |> GeneralTransitionSet.from_transitionset
      |> GeneralTransitionSet.filter
           (TransitionSet.exists (fun (_,_,l') -> Location.equal l' target_loc) % GeneralTransition.transitions)
      |> GeneralTransitionSet.for_all ((=) 1 % LocationSet.cardinal % GeneralTransition.targets)
    in
    List.for_all for_subgraph subs
  in

  let only_enter_through_entry_loc =
    let cut_graph = TransitionGraph.remove_vertex orig_graph entry_loc in
    let for_subgraph subgraph =
      let locations_in_sub = TransitionGraph.locations subgraph in
      let locations_notin_sub = LocationSet.diff (TransitionGraph.locations orig_graph) locations_in_sub
      in
      TransitionGraph.transitions cut_graph
      |> TransitionSet.filter (fun (l,_,l') -> LocationSet.mem l  locations_in_sub &&
                                               LocationSet.mem l' locations_notin_sub && not (Location.equal target_loc l'))
      |> TransitionSet.cardinal
      |> (=) 0
    in
    List.for_all for_subgraph subs
  in

  let target_loc_from_all_sub_locations_reachable = 
    let subgraph_with_added_target subgraph = 
      let subgraph_locs = TransitionGraph.locations subgraph in
      let transitions_to_add = 
        TransitionGraph.transitions orig_graph 
        |> TransitionSet.filter (fun (l,_,l') -> Location.equal l' target_loc && 
                                                 LocationSet.mem l subgraph_locs)
      in
      TransitionSet.fold (flip TransitionGraph.add_edge_e) transitions_to_add subgraph
    in

    let for_subgraph subgraph =
      let graph_check = subgraph_with_added_target subgraph in
      let reachable_locs start = 
        Traverse.fold_component LocationSet.add LocationSet.empty graph_check start
      in

      TransitionGraph.locations graph_check
      |> LocationSet.for_all (LocationSet.mem target_loc % reachable_locs)
    in

    List.for_all for_subgraph subs
  in

  let criteria =
    [all_guards_tautology_in_invariants;termination;no_interleaving;only_enter_through_entry_loc;
     all_trans_of_gt_going_to_loc; transitions_going_to_loc_guard_tautology; target_loc_from_all_sub_locations_reachable]
  in
  Printf.printf "criteria: %s\n" (Util.enum_to_string Bool.to_string (List.enum criteria));
  List.for_all identity criteria

let subgraph_connected_with_loc (orig_graph: TransitionGraph.t) (subgraph: TransitionGraph.t) (loc : Location.t): bool =
  let locations = TransitionGraph.locations subgraph in
  let outgoing_trans = TransitionGraph.transitions orig_graph
                       |> TransitionSet.filter (fun (l,_,_) -> LocationSet.mem l locations)
  in
  outgoing_trans
  |> TransitionSet.filter (fun (_,_,l) -> Location.equal l loc)
  |> (<) 0 % TransitionSet.cardinal


let rec get_pr (graph: TransitionGraph.t) (start: Location.t) (locs: LocationSet.t) (loc: Location.t) get_timebound:
  (Location.t-> OurFloat.t option) =
  let subgraph_cardinals_combined transitions_and_graphs_list =
    List.fold_left (fun ctr (_,g) -> ctr + (LocationSet.cardinal (TransitionGraph.locations g) )) 0 transitions_and_graphs_list
  in

  (*Search for candidates of subgraphs *)
  let candidates =
    locs
    |> tap (fun _ -> if Location.to_string loc = "k" then Printf.printf "\n\n\n\n")
    |> tap (Printf.printf "start candidates start: %s     loc: %s \n" (Location.to_string start) (Location.to_string loc) |> const)
    |> LocationSet.to_list
    (* We don't want to branch on locations that are the same as target locations. We can handle these without a pr func *)
    |> List.filter (not % Location.equal loc)
    |> List.filter (fun ml -> every_path_enters_loc graph start ml loc)
    |> tap (Printf.printf "every_path_enters: %s\n" % Util.enum_to_string Location.to_string % List.enum)
    |> List.map (fun l -> (l,gts_outgoing (TransitionGraph.transitions graph) l))
    (* only one outgoing transition supported *)
    |> List.filter (fun (_,gtset) -> GeneralTransitionSet.cardinal gtset = 1)
    (* more than one target for non-linear branching *)
    |> List.filter (fun (_,gtset) -> GeneralTransitionSet.to_list gtset |> List.hd |> GeneralTransition.targets |> ((<) 1 % LocationSet.cardinal) )
    (* Build subgraphs *)
    |> tap (Printf.printf "lset: %s\n" % LocationSet.to_string % LocationSet.of_list % List.map Tuple2.first)
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
    |> List.filter (fun (lentry,subs) -> List.map Tuple2.second subs |> check_subgraphs_criteria get_timebound graph lentry loc)
    |> tap (Printf.printf "final_length: %i\n" % List.length)
    |> tap (fun l -> let lenum = List.enum l in
                     Printf.printf "subgraphs: %s\n" (Util.enum_to_string (fun (candidate, sublist) -> Location.to_string
                     candidate ^ " " ^ Util.enum_to_string (fun (brl,sub) -> Location.to_string brl ^ ", " ^
                     TransitionGraph.to_string sub) (List.enum sublist)) lenum))
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
            (* Starting the recursive call on the subgraph l is contained in *)
            let rec_result = Printf.printf "Recurse\n"; (get_pr graph sub_start (TransitionGraph.locations subgraph) loc get_timebound) l in
            let rel_trans_outgoing =
              graph
              |> TransitionGraph.transitions
              |> GeneralTransitionSet.from_transitionset
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
            Printf.printf "Recursion done: is_valid = %b, loc = %s, l_entry = %s\n" is_valid (Location.to_string loc)
            (Location.to_string lentry);

            match (rec_result, is_valid) with
            | (_, false) -> None
            | (None,true) -> Some (OurFloat.(/) (probability_reaching_loc sub_start) (total_probability))
            | (Some p, true) -> Some OurFloat.((probability_reaching_loc sub_start / total_probability) * p) )

(* Memorize the calculated pr-functions to improve performance. *)
let memoize_pr_func = 
  Util.memoize ~extractor:identity (get_pr)

(** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'. *)
let compute kind program get_sizebound get_expsizebound get_timebound ((gt,loc),var) =
  let execute () =
    let graph = Program.graph program in
    let start_loc = Program.start program in
    let locations = TransitionGraph.locations graph in
    let transition_location = GeneralTransition.start gt in
    let pr_func = memoize_pr_func graph start_loc locations transition_location get_timebound in
    let exp_poly = ExpLocalSizeBound.exp_poly ((gt,loc),var) in
    let print_pr_func = 
      TransitionGraph.locations graph |> LocationSet.to_list 
      |> List.map (fun l -> (Location.to_string l, (Option.default (-1 |> OurFloat.of_int) (pr_func l)) |> OurFloat.to_string)) 
      |> List.enum
      |> Util.enum_to_string (fun (l,p) -> l ^ ", " ^ p)
    in
    Printf.printf "pr_func: %s\n" print_pr_func;
    if Program.is_initial_gt program gt then
      RealBound.of_poly exp_poly
    else
      incoming_bound
        kind program
        (fun kind (t,v) -> get_sizebound kind t v)
        (fun kind ((gt,l),var) -> get_expsizebound kind (gt,l) var) exp_poly gt pr_func
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute trivial bound", ["kind", show_kind kind;
                                                          "rv", GTRV.to_id_string (gt,var)])
                     ~result:RealBound.to_string
                     execute

