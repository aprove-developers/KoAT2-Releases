open Batteries 
module PIP = ProbabilisticProgramModules

type pe_conf = { 
  abstraction_selection: [`FVS | `DFS |`ALL];
  k_encounters: int;
  compute_properties: [`Propagation];
  update_invariants: bool;
}

let default_config: pe_conf = {
  abstraction_selection = `FVS;
  k_encounters = 0;
  compute_properties = `Propagation;
  update_invariants = true;
}

  (* module Path = struct *) 
  (*   type t = PIP.Transition.t list *)
  (*   type result = *) 
  (*     | Open of t (1* The last transition does not point two a location already in the path *1) *)
  (*     | Closed of t (1* The last transition closes a loop *1) *)

  (*   exception NonConsecutiveTransition *)

  (*   let mk (first_step) = [first_step] *)

  (*   let locations path = List.fold_left *) 
  (*       ( fun ls transition -> PIP.LocationSet.add (PIP.Transition.target transition) ls) *) 
  (*       ( PIP.LocationSet.add (PIP.Transition.src (List.hd path)) PIP.LocationSet.empty) *)
  (*       path *) 

  (*   let walk_step (path: t) (step: PIP.Transition.t) = *) 
  (*     if (PIP.Transition.target (List.hd path)) == PIP.Transition.src step then *) 
  (*       if not (PIP.LocationSet.mem (PIP.Transition.target step) (locations path)) then *) 
  (*         Open (step :: path) *)
  (*       else *) 
  (*         Loop (step :: path) *)
  (*     else raise NonConsecutiveTransition *)

  (*   let backtrack (path: t) = match path with *)
  (*     | [] -> None *)
  (*     | _ :: xs -> Some xs *)
  (* end *)


module Traverse(PM: ProgramTypes.ProgramModules) = struct 
  open PM

  (* mutable state *)
  type state = {
    blocked: (Location.t, bool) Hashtbl.t;
    b_lists: (Location.t, Location.t list) Hashtbl.t;
  }

  let find_loops graph = 
    (** Finds all loops in a graph, using the algorithm from Donald B. Johnson (1975) **)

    let all_locations = TransitionGraph.locations graph in

    let hashtbl_size = all_locations |> LocationSet.enum |> Enum.hard_count in
    (** initialize hashtables to the number of locations in the graph **)

    let initial_state locations = 
      (** Initial state for circuit, based on a set of locations **)
      let state = {
        blocked = Hashtbl.create hashtbl_size;
        b_lists = Hashtbl.create hashtbl_size;
      } in
      LocationSet.iter (fun location -> 
        Hashtbl.add state.blocked location true;
        Hashtbl.add state.b_lists location [];
      ) locations;
      state
    in

    let is_blocked s location = Option.default false (Hashtbl.find_option s.blocked location) in
    (** Check if a location is blocked, doesn't mutate s **)

    let b_list_of s location = Option.default [] (Hashtbl.find_option s.b_lists location) in
    (** Get the b_list of a location, doesn't mutate s **)

    let add_to_b_list s w v = 
      (** Add a location v to the b_list of w, if not already present; mutates s **)
      let old_b_list = b_list_of s w in
      if not (List.mem v old_b_list) then
        Hashtbl.replace s.b_lists w (v :: old_b_list);
    in

    let block s location = Hashtbl.replace s.blocked location true; in 
    (** Block a location, mutates s **)

    let rec unblock s location = 
    (** Unblock a location, and clear b_lists, mutates s **)
      if is_blocked s location then
        Hashtbl.replace s.blocked location false;
        List.iter (unblock s) (Option.default [] (Hashtbl.find_option s.b_lists location));
        Hashtbl.replace s.b_lists location [];
    in 

    let loop_of path = Stack.enum path |> Enum.fold (Fun.flip List.cons) [] in 
    (** Transform the path into a list of locations, without mutating the path *)

    (* We start with an empty path *)
    let path = Stack.create () in

    let rec circuit graph s prev_results start_location current_location = 
      Stack.push current_location path;
      block s current_location;
      let (closed, new_results) = 
        TransitionGraph.fold_succ (fun next_location (closed_acc, loops_acc) ->
          if Location.equal next_location start_location then 
            (* Found a loop, add to results *)
            (true, (loop_of path) :: loops_acc)
          else if not (is_blocked s next_location) then 
            let (inner_closed, inner_loops_acc) = circuit graph s loops_acc start_location next_location in
            (closed_acc || inner_closed, inner_loops_acc)
          else (closed_acc, loops_acc)
        ) graph current_location (false, prev_results) 
      in

      if closed then (unblock s current_location)
      else TransitionGraph.iter_succ (fun w -> add_to_b_list s w current_location) graph current_location;

      let _ = Stack.pop path in
      (closed, new_results)
    in 
    
    (* Locations are ordered; as is the LocationSet *)
    let (_, results) = LocationSet.fold (fun location (current_graph, prev_results) -> 
      (* The SCC containing the smallest location according to the ordering *)
      let min_scc_opt = List.find_opt (fun scc -> 
            TransitionSet.locations scc 
            |> LocationSet.mem location 
          ) (TransitionGraph.sccs current_graph)
      in 
      (* the smallest location might be in a trivial scc and already filtered by TransitionGraph.sccs *)
      let new_results = match min_scc_opt with 
        | Some scc -> 
            let scc_graph = TransitionGraph.mk (TransitionSet.enum scc)
            and scc_locations = TransitionSet.locations scc in
            let (_closed, results) = circuit scc_graph (initial_state scc_locations) prev_results location location in
            results
        | None -> prev_results
      in 
      (TransitionGraph.remove_vertex current_graph location, new_results)
    ) (TransitionGraph.locations graph) (graph, [])
    in results

    let find_loops_scc graph scc = 
      (** Find all loops in a given scc using the algorithm from Donald B. Johnson (1975) **) 
      let scc_graph = TransitionSet.enum scc |> TransitionGraph.mk in
      find_loops scc_graph
end

(** 
    Apply control flow refinement via partial evaluation to a probabilistic program 
    log: logger
    conf: configuration
    program: program
    sccs: a list of strongly-conected components (SCC) in the program, wich shall be evaluated. Must not overlap!
 **)
(*let apply_prob_pe ~(log: BatLogger.log) ~(conf:pe_conf) (program: PIP.Program.t) (sccs: PIP.TransitionSet.t list)= *)
(*  BatEnum.fold (fun scc -> *)
(*    let scc_list = PIP.TransitionSet.to_list scc in *)
(*    let incoming_transitions = PIP.Program.entry_transitions log scc_list *)
(*    and outgoing_transitions = PIP.Program.outgoing_transitions log scc_list in *) 
(*    and loops = *) 
(*    let evaluated_scc = pe_on_scc program incoming_transitions outgoing_transitions *)
(*    (1* *)
(*    - compute new transitions *)
(*    - delete old transitions from program *)
(*    - add new_transitions to program *)
(*    - wrap in maybe changed *)
(*    *1) *)
(*    pe_on_scc incoming_transitions *)
(*  ) sccs *)
