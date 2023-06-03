open Batteries

module Loops (PM : ProgramTypes.ProgramModules) = struct
  open PM

  (* mutable state *)
  type state = {
    blocked : (Location.t, bool) Hashtbl.t;
    b_lists : (Location.t, Location.t list) Hashtbl.t;
  }

  (** Finds all loops in a graph, using the algorithm from Donald B. Johnson (1975)
  By itself this function is probably not very useful. Use transition_loops_for in 
  order to get the loops with transitions.
  *)
  let find_loops graph =
    let all_locations = TransitionGraph.locations graph in

    (* initialize hashtables to the number of locations in the graph *)
    let hashtbl_size = all_locations |> LocationSet.enum |> Enum.hard_count in

    (* Initial state for circuit, based on a set of locations *)
    let initial_state locations =
      let state =
        {
          blocked = Hashtbl.create hashtbl_size;
          b_lists = Hashtbl.create hashtbl_size;
        }
      in
      LocationSet.iter
        (fun location ->
          Hashtbl.add state.blocked location true;
          Hashtbl.add state.b_lists location [])
        locations;
      state
    in

    (* Check if a location is blocked, doesn't mutate s *)
    let is_blocked s location =
      Option.default false (Hashtbl.find_option s.blocked location)
    in

    (* Get the b_list of a location, doesn't mutate s *)
    let b_list_of s location =
      Option.default [] (Hashtbl.find_option s.b_lists location)
    in

    (* Add a location v to the b_list of w, if not already present; mutates s *)
    let add_to_b_list s w v =
      let old_b_list = b_list_of s w in
      if not (List.mem v old_b_list) then
        Hashtbl.replace s.b_lists w (v :: old_b_list)
    in

    (* Block a location, mutates s *)
    let block s location = Hashtbl.replace s.blocked location true in

    (* Unblock a location, and clear b_lists, mutates s *)
    let rec unblock s location =
      if is_blocked s location then Hashtbl.replace s.blocked location false;
      List.iter (unblock s)
        (Option.default [] (Hashtbl.find_option s.b_lists location));
      Hashtbl.replace s.b_lists location []
    in

    (* Transform the path into a list of locations, without mutating the path *)
    let loop_of path = Stack.enum path |> Enum.fold (Fun.flip List.cons) [] in

    (* We start with an empty path *)
    let path = Stack.create () in

    let rec circuit graph s prev_results start_location current_location =
      Stack.push current_location path;
      block s current_location;
      let closed, new_results =
        TransitionGraph.fold_succ
          (fun next_location (closed_acc, loops_acc) ->
            if Location.equal next_location start_location then
              (* Found a loop, add to results *)
              (true, loop_of path :: loops_acc)
            else if not (is_blocked s next_location) then
              let inner_closed, inner_loops_acc =
                circuit graph s loops_acc start_location next_location
              in
              (closed_acc || inner_closed, inner_loops_acc)
            else (closed_acc, loops_acc))
          graph current_location (false, prev_results)
      in

      if closed then unblock s current_location
      else
        TransitionGraph.iter_succ
          (fun w -> add_to_b_list s w current_location)
          graph current_location;

      let _ = Stack.pop path in
      (closed, new_results)
    in

    (* Locations are ordered; as is the LocationSet *)
    let _, results =
      LocationSet.fold
        (fun location (current_graph, prev_results) ->
          (* The SCC containing the smallest location according to the ordering *)
          let min_scc_opt =
            List.find_opt
              (fun scc ->
                TransitionSet.locations scc |> LocationSet.mem location)
              (TransitionGraph.sccs current_graph)
          in
          (* the smallest location might be in a trivial scc and already filtered by TransitionGraph.sccs *)
          let new_results =
            match min_scc_opt with
            | Some scc ->
                let scc_graph = TransitionGraph.mk (TransitionSet.enum scc)
                and scc_locations = TransitionSet.locations scc in
                let _closed, results =
                  circuit scc_graph
                    (initial_state scc_locations)
                    prev_results location location
                in
                results
            | None -> prev_results
          in
          (TransitionGraph.remove_vertex current_graph location, new_results))
        (TransitionGraph.locations graph)
        (graph, [])
    in
    results

  (** Find all loops in a given scc using the algorithm from Donald B. Johnson (1975) **)
  let find_loops_scc graph scc =
    let scc_graph = TransitionSet.enum scc |> TransitionGraph.mk in
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
  let transition_loops_from graph (loc_loops: Location.t list list) = 
    let transitions_betwen_locations src target = TransitionGraph.fold_succ_e 
      (fun t ts -> if Location.equal (Transition.target t) target then t :: ts else ts) 
      graph src []
    in 

    let combine (transitions: Transition.t list) (suffixes: Transition.t list list): Transition.t list list = 
      List.fold (fun results suffix -> 
        List.fold (fun results transition -> (transition :: suffix) :: results) [] transitions
      ) [] suffixes
    in 

    (* computes for every step in the loop the walkable transitions *)
    let rec transition_loops (loc_loop: Location.t list) = match loc_loop with 
      | l1 :: l2 :: ls -> combine (transitions_betwen_locations l1 l2) (transition_loops (l2::ls))
      | l1 :: [] -> [[]]
      | [] -> [[]]
    in  

    List.fold (fun results loc_loop -> List.append (transition_loops loc_loop) results) [] loc_loops 

end
