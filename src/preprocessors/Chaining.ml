open Batteries
open ProgramTypes

let logger = Logging.(get Preprocessor)

(** Adds transitions to the graph such that every predecessor of the location is correctly connected with every successor of the location,
    making the location obsolete. *)
let skip_location trans_id_counter location graph =
  let all_gts =
    GeneralTransitionSet.of_transitionset (TransitionGraph.transitions graph)
  in
  let incoming_gts =
    (* we have checked before that if a gt has target location then all its targets are location *)
    GeneralTransitionSet.filter (LocationSet.exists ((=) location) % GeneralTransition.targets) all_gts
  in
  let outgoing_gts = GeneralTransitionSet.filter ((=) location % GeneralTransition.start) all_gts in

  (incoming_gts, outgoing_gts)
  |> Tuple2.mapn (GeneralTransitionSet.enum)
  |> uncurry Enum.cartesian_product
  |> Enum.map
      (fun (gtin, gtout) ->
        let new_gt_id = TransitionLabel.get_unique_gt_id trans_id_counter () in
        Tuple2.mapn GeneralTransition.transitions (gtin, gtout)
        |> Tuple2.mapn TransitionSet.enum
        |> uncurry Enum.cartesian_product
        |> Enum.map (fun ((l,t,_),(_,t',l')) -> l, TransitionLabel.append trans_id_counter ~new_gt_id t t', l')
        |> tap (fun new_ts ->
            Logger.log logger Logger.DEBUG
              (fun () -> "chaining",
                [ "gtin", GeneralTransition.to_id_string gtin
                ; "gtout", GeneralTransition.to_id_string gtout
                ; "added_transitions", Util.enum_to_string Transition.to_id_string (Enum.clone new_ts)]
              )
          )
      )
  |> Enum.flatten
  |> (flip Program.add_transitions) graph

(** Returns if the specific location is chainable in the graph. *)
let chainable ~conservative graph location : bool =
  let open TransitionGraph in
  let module VarMap = Map.Make(Var) in
  let gts = GeneralTransitionSet.of_transitionset @@ TransitionGraph.transitions graph in
  let gts_to_loc =
    gts
    |> GeneralTransitionSet.filter (LocationSet.exists (Location.equal location) % GeneralTransition.targets)
  in

  (* Check if the update function is not probabilistic *)
  let check_update_nonprobabilistic tlist =
    tlist
    |> List.for_all (fun t -> TransitionLabel.update_map (Transition.label t)
                              |> VarMap.for_all (fun _ -> TransitionLabel.UpdateElement.is_polynomial))
  in

  (* For transitions t1,t2 of a gt t1 might enable the guard of a possible follow-up transition
      whereas t2 might disable it.
      For simple handling check if all incoming gts consist of only one transition *)
  let check_unique_transition_for_inc_gt =
    gts_to_loc
    |> GeneralTransitionSet.for_all ((=) 1 % TransitionSet.cardinal % GeneralTransition.transitions)
  in

  (* This is a heuristic to avoid merging (nested) loops. Two nested loops
    with a linear timebound each should not be merged into a single loop with quadratic bound
    (since we can not determine quadratic ranking functions atm)
   *)
  let scc_cond () =
    let sccs graph =
      let module SCC = Graph.Components.Make(TransitionGraph) in
      List.map LocationSet.of_list @@ SCC.scc_list graph
    in
    let init_loc_scc = List.find (LocationSet.mem location) @@ sccs graph in

    if LocationSet.cardinal init_loc_scc = 1 then true
    else
      TransitionGraph.remove_vertex graph location
      |> sccs
      |> not % List.exists (LocationSet.equal (LocationSet.remove location init_loc_scc))
  in

  let out_gt_degree () =
    GeneralTransitionSet.filter (Location.equal location % GeneralTransition.start) gts
    |> GeneralTransitionSet.cardinal
  in

  not (mem_edge graph location location)

  (* Necessary conditions *)
  && out_degree graph location >= 1
  && in_degree graph location >= 1
  && check_update_nonprobabilistic (pred_e graph location @ succ_e graph location)
  && check_unique_transition_for_inc_gt

  (* Conservative conditions *)
  && (not conservative || (
        scc_cond ()
        (* we want to avoid introducing to many transitions since this blows up our program *)
        && (in_degree graph location = 1 || out_gt_degree () = 1)
     ))

(** Performs a chaining step removing the location from the graph. *)
let chain trans_id_counter location graph : TransitionGraph.t =
  let skipped = skip_location trans_id_counter location graph in
  TransitionGraph.succ_e skipped location
  |> List.enum
  |> Enum.fold TransitionGraph.remove_edge_e skipped

let transform_graph ~conservative trans_id_counter (graph: TransitionGraph.t): TransitionGraph.t MaybeChanged.t =
  let try_chaining location maybe_changed_graph =
    let open MaybeChanged in
    maybe_changed_graph >>= (fun graph ->
      if chainable ~conservative graph location then (
        Logger.(log logger INFO (fun () -> "chaining", ["location", Location.to_string location]));
        changed (chain trans_id_counter location graph)
      ) else
        same graph)
  in TransitionGraph.fold_vertex try_chaining graph (MaybeChanged.same graph)
