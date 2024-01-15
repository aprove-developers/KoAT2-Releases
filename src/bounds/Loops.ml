open! OurBase

let cfr_logger = Logging.(get CFR)

module Loops (PM : ProgramTypes.ProgramModules) = struct
  open PM

  let loop_to_string loop = Sequence.of_list loop |> Util.sequence_to_string ~f:Location.to_string
  let loops_to_string loops = Sequence.of_list loops |> Util.sequence_to_string ~f:loop_to_string

  (* mutable state *)
  type state = { blocked : (Location.t, bool) Hashtbl.t; b_lists : (Location.t, Location.t list) Hashtbl.t }

  (** Finds all loops in a graph, using the algorithm from Donald B. Johnson (1975)
  By itself this function is probably not very useful. Use transition_loops_for in
  order to get the loops with transitions.
  *)
  let find_loops graph =
    let all_locations = TransitionGraph.locations graph in

    (* initialize hashtables to the number of locations in the graph *)
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

    (* Transform the path into a list of locations, without mutating the path *)
    let loop_of = Stack.to_list in

    (* We start with an empty path *)
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
          ~f:(fun (closed_acc, loops_acc) next_location ->
            if Location.equal next_location start_location then (
              let loop = loop_of path in
              Logging.log cfr_logger ~level:Logger.DEBUG "johnson" (fun () ->
                  [ ("FOUND_LOOP", loop_to_string loop) ]);
              (* Found a loop, add to results *)
              (true, loop :: loops_acc))
            else if not (is_blocked s next_location) then
              let inner_closed, inner_loops_acc = circuit graph s loops_acc start_location next_location in
              (closed_acc || inner_closed, inner_loops_acc)
            else
              (closed_acc, loops_acc))
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
    Logging.log cfr_logger ~level:Logger.INFO "loops" (fun () -> [ ("LOOPS", loops_to_string results) ]);
    results


  (** Find all loops in a given scc using the algorithm from Donald B. Johnson (1975) **)
  let find_loops_scc graph scc =
    let scc_graph = Set.to_sequence scc |> TransitionGraph.mk in
    find_loops scc_graph


  (** For a list of location loops, this function creates the list of all transition loops
      containing a loop from the location loops.

      Example:
      The graph G contains transitions
      (l0, t0, l1)
      (l1, t1, l2)
      (l1, t2, l2)
      (l2, t3, l1)

      The loop detection `find_loops` would only find the loop [l1, l2].
      This function expands the (location) loop [l1,l2] the the transition loops
      [t1,t3], [t2,t3].

      *)
  let transition_loops_from graph (loc_loops : Location.t list list) =
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

    (* computes for every step in the loop the walkable transitions *)
    let rec transition_loops (lh : Location.t) (loc_loop : Location.t list) =
      match loc_loop with
      | l1 :: l2 :: ls -> combine (transitions_betwen_locations l1 l2) (transition_loops lh (l2 :: ls))
      | l1 :: [] ->
          (* start loops with transitions from last location to head *)
          combine (transitions_betwen_locations l1 lh) [ [] ]
      | [] -> [ [] ]
    in

    List.fold
      ~f:(fun results loc_loop -> List.append (transition_loops (List.hd_exn loc_loop) loc_loop) results)
      ~init:[] loc_loops


  (** Rotate a loop, so that its head is the first location *)
  let rotate head loop =
    let rec rotate_ prefix suffix =
      Logging.log cfr_logger ~level:Logger.DEBUG "rotate" (fun () ->
          [ ("PREFIX", loop_to_string prefix); ("SUFFIX", loop_to_string suffix) ]);
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
    rotate_ [] loop


  (** Returns the first locations of the loops *)
  let loop_heads graph loops =
    let loop_head_from_loop loop =
      let num_of_pre_trans = List.length % TransitionGraph.pred graph in
      List.max_elt ~compare:(fun a b -> Int.compare (num_of_pre_trans a) (num_of_pre_trans b)) loop
      |> Option.value_exn
    in
    List.map ~f:loop_head_from_loop loops |> LocationSet.of_list
end
