open! OurBase

let cfr_logger = Logging.(get CFR)

(** This module computes all cycles in a graph. *)
module Cycles (PM : ProgramTypes.ProgramModules) = struct
  open PM

  let cycle_to_string cycle = Sequence.of_list cycle |> Util.sequence_to_string ~f:Location.to_string
  let cycles_to_string cycles = Sequence.of_list cycles |> Util.sequence_to_string ~f:cycle_to_string

  (* mutable state *)
  type state = { blocked : (Location.t, bool) Hashtbl.t; b_lists : (Location.t, Location.t list) Hashtbl.t }

  (* Finds all cycles in a graph, using the algorithm from Donald B. Johnson (1975).
     Use transition_cycles_for in order to get the cycles with transitions. *)
  let find_cycles graph =
    let all_locations = TransitionGraph.locations graph in

    (* Initialize hashtables to the number of locations in the graph *)
    let hashtbl_size = Set.length all_locations in

    (* Initial state for circuit, based on a set of locations *)
    let initial_state locations =
      let state =
        {
          blocked = Hashtbl.create ~size:hashtbl_size (module Location);
          b_lists = Hashtbl.create ~size:hashtbl_size (module Location);
        }
      in
      Set.iter
        ~f:(fun location ->
          Hashtbl.add_exn state.blocked ~key:location ~data:false;
          Hashtbl.add_exn state.b_lists ~key:location ~data:[])
        locations;
      state
    in

    (* Check if a location is blocked, doesn't mutate s *)
    let is_blocked s location = Option.value ~default:false (Hashtbl.find s.blocked location) in

    (* Get the b_list of a location, doesn't mutate s *)
    let b_list_of s location = Option.value ~default:[] (Hashtbl.find s.b_lists location) in

    (* Add a location v to the b_list of w, if not already present; mutates s *)
    let add_to_b_list s w v =
      let old_b_list = b_list_of s w in
      if not (List.mem ~equal:Location.equal old_b_list v) then
        Hashtbl.update s.b_lists w ~f:(fun _ -> v :: old_b_list)
    in

    (* Block a location, mutates s *)
    let block s location = Hashtbl.update s.blocked location ~f:(fun _ -> true) in

    (* Unblock a location, and clear b_lists, mutates s *)
    let rec unblock s location =
      if is_blocked s location then (
        Hashtbl.update s.blocked location ~f:(fun _ -> false);
        List.iter
          ~f:(fun l ->
            Logging.log ~level:Logger.DEBUG cfr_logger "johnson" (fun () ->
                [ ("UNBLOCK", Location.to_string l) ]);
            unblock s l)
          (Option.value ~default:[] (Hashtbl.find s.b_lists location));
        Hashtbl.update s.b_lists location ~f:(fun _ -> []))
    in

    (* Transform the path into a list of locations without mutating the path. *)
    let cycle_of = Stack.to_list in

    (* Initialize the empty path.  *)
    let path = Stack.create () in

    let rec circuit graph s prev_results start_location current_location =
      Stack.push path current_location;
      Logging.log cfr_logger ~level:Logger.DEBUG "johnson" (fun () ->
          [ ("BLOCK", Location.to_string current_location) ]);
      block s current_location;
      (* TransitionGraph.succ can contain repetitions *)
      let successors =
        TransitionGraph.succ graph current_location
        |> LocationSet.of_list
        |> tap (fun ls ->
               Logging.log cfr_logger ~level:Logger.DEBUG "johnson" (fun () ->
                   [
                     ("CURRENT", Location.to_string current_location); ("SUCCESSORS", LocationSet.to_string ls);
                   ]))
      in
      let closed, new_results =
        Set.fold
          ~f:(fun (closed_acc, cycles_acc) next_location ->
            if Location.equal next_location start_location then (
              let cycle = cycle_of path in
              Logging.log cfr_logger ~level:Logger.DEBUG "johnson" (fun () ->
                  [ ("FOUND_CYCLE", cycle_to_string cycle) ]);
              (* Found a cycle, add to results *)
              (true, cycle :: cycles_acc))
            else if not (is_blocked s next_location) then
              let inner_closed, inner_cycles_acc = circuit graph s cycles_acc start_location next_location in
              (closed_acc || inner_closed, inner_cycles_acc)
            else
              (closed_acc, cycles_acc))
          successors ~init:(false, prev_results)
      in

      if closed then
        unblock s current_location
      else
        TransitionGraph.iter_succ (fun w -> add_to_b_list s w current_location) graph current_location;

      let _ = Stack.pop path in
      (closed, new_results)
    in

    (* Locations are ordered; as is the LocationSet *)
    let _, results =
      Set.fold
        ~f:(fun (current_graph, prev_results) location ->
          Logging.log cfr_logger ~level:Logger.DEBUG "johnson" (fun () ->
              [ ("MIN_LOCATION", Location.to_string location) ]);
          (* The SCC containing the smallest location according to the ordering *)
          let min_scc_opt =
            List.find (TransitionGraph.sccs current_graph) ~f:(fun scc ->
                Set.mem (TransitionSet.locations scc) location)
          in
          Logging.log cfr_logger ~level:Logger.DEBUG "johnson" (fun () ->
              [ ("MIN_SCC", min_scc_opt |> Option.map ~f:TransitionSet.to_string |? "None") ]);
          (* the smallest location might be in a trivial scc and already filtered by TransitionGraph.sccs *)
          let new_results =
            match min_scc_opt with
            | Some scc ->
                let scc_graph = TransitionGraph.mk (Set.to_sequence scc)
                and scc_locations = TransitionSet.locations scc in
                let _closed, results =
                  circuit scc_graph (initial_state scc_locations) prev_results location location
                in
                results
            | None -> prev_results
          in
          (TransitionGraph.remove_vertex current_graph location, new_results))
        (TransitionGraph.locations graph) ~init:(graph, [])
    in
    Logging.log cfr_logger ~level:Logger.INFO "cycles" (fun () -> [ ("CYCLES", cycles_to_string results) ]);
    results


  (* Find all cycles in a given scc using the algorithm from Donald B. Johnson (1975) **)
  let find_cycles_scc graph scc =
    let scc_graph = Set.to_sequence scc |> TransitionGraph.mk in
    find_cycles scc_graph


  (* For a list of location cycles, this function creates the list of all transition cycles
      containing a cycle from the location cycles.

      Example:
      The graph G contains transitions
      (l0, t0, l1)
      (l1, t1, l2)
      (l1, t2, l2)
      (l2, t3, l1)

      The cycle detection `find_cycles` would only find the cycle [l1, l2].
      This function expands the (location) cycle [l1,l2] the the transition cycles
      [t1,t3], [t2,t3].
  *)
  let transition_cycles_from graph (loc_cycles : Location.t list list) =
    let transitions_betwen_locations src target =
      TransitionGraph.fold_succ_e
        (fun t ts ->
          if Location.equal (Transition.target t) target then
            t :: ts
          else
            ts)
        graph src []
    in

    let combine (transitions : Transition.t list) (suffixes : Transition.t list list) : Transition.t list list
        =
      List.fold
        ~f:(fun results suffix ->
          List.fold ~f:(fun results transition -> (transition :: suffix) :: results) ~init:[] transitions)
        ~init:[] suffixes
    in

    (* Computes for every step in the cycle the walkable transitions *)
    let rec transition_cycles (lh : Location.t) (loc_cycle : Location.t list) =
      match loc_cycle with
      | l1 :: l2 :: ls -> combine (transitions_betwen_locations l1 l2) (transition_cycles lh (l2 :: ls))
      | l1 :: [] ->
          (* start cycles with transitions from last location to head *)
          combine (transitions_betwen_locations l1 lh) [ [] ]
      | [] -> [ [] ]
    in

    List.fold
      ~f:(fun results loc_cycle -> List.append (transition_cycles (List.hd_exn loc_cycle) loc_cycle) results)
      ~init:[] loc_cycles


  (** Rotate a cycle so that its head is the first location *)
  let rotate head cycle =
    let rec rotate_ prefix suffix =
      Logging.log cfr_logger ~level:Logger.DEBUG "rotate" (fun () ->
          [ ("PREFIX", cycle_to_string prefix); ("SUFFIX", cycle_to_string suffix) ]);
      match suffix with
      | location :: suffix ->
          if location == head then
            (* attach prefix at the end *)
            List.append (location :: suffix) (List.rev prefix)
          else
            (* continue rotating *)
            rotate_ (location :: prefix) suffix
      | [] -> List.rev prefix
    in
    rotate_ [] cycle


  (** Returns the first location of each cycle. *)
  let loop_heads graph cycles =
    let cycle_head_from_cycle cycle =
      let num_of_pre_trans = List.length % TransitionGraph.pred graph in
      List.max_elt ~compare:(fun a b -> Int.compare (num_of_pre_trans a) (num_of_pre_trans b)) cycle
      |> Option.value_exn
    in
    let heads = List.map ~f:cycle_head_from_cycle cycles |> LocationSet.of_list in
    Logging.log ~level:Logger.INFO cfr_logger "loop_heads" (fun () ->
        [ ("loop_heads", LocationSet.to_string heads) ]);
    heads
end
