open! OurBase

let cfr_logger = Logging.(get CFR)

let log ?(level = Logger.INFO) method_name data =
  Logger.log cfr_logger level (fun () -> (method_name, data ()))


module Unfolding
    (PM : ProgramTypes.ProgramModules)
    (A : GenericProgram_.Adapter
           with type update_element = PM.UpdateElement.t
            and type transition = PM.Transition.t) =
struct
  open GenericProgram_.OverApproximationUtils (A)
  open Polyhedrons

  (** [initial_guard_polyh am constr guard] computes a polyhedron overapproximating satisfying assignments for the conjunction of [constr] and [guard] *)
  let initial_guard_polyh am constr guard =
    Guard.mk_and constr guard
    |> tap (fun p ->
           log ~level:Logger.DEBUG "unfold" (fun () -> [ ("INITIAL", p |> Guard.to_string ~pretty:true) ]))
    |> ApronInterface.Koat2Apron.constraint_to_polyh am


  (** Unfolds the guard with after applying the given update.
      Returns [None] if the guard is unsatisfiable. *)
  let unfold_update am initial_polyh program_vars update =
    let update_approx, update_guard = overapprox_update update in
    let update_temp_vars =
      [ Guard.vars update_guard; Polyhedrons.vars_in_update update_approx ]
      |> VarSet.union_list
      |> Set.filter ~f:(fun v -> not (Set.mem program_vars v))
    in
    let update_program_vars =
      [ Guard.vars update_guard; Polyhedrons.vars_in_update update_approx ]
      |> VarSet.union_list
      |> Set.filter ~f:(fun v -> Set.mem program_vars v)
    in

    initial_polyh
    |> Polyhedrons.add_vars_to_polyh am (Set.union update_program_vars update_temp_vars)
    |> tap (fun p ->
           log ~level:Logger.DEBUG "unfold" (fun () ->
               [
                 ( "WITH_UPDATE_VARS",
                   ApronInterface.Apron2Koat.polyh_to_constraint am p |> Guard.to_string ~pretty:true );
               ]))
    |> Polyhedrons.intersect_constraint am update_guard
    |> tap (fun p ->
           log ~level:Logger.DEBUG "unfold" (fun () ->
               [
                 ( "INITIAL_POLYH",
                   ApronInterface.Apron2Koat.polyh_to_constraint am initial_polyh
                   |> Guard.to_string ~pretty:true );
                 ( "WITH_UPDATE_GUARD",
                   ApronInterface.Apron2Koat.polyh_to_constraint am p |> Guard.to_string ~pretty:true );
               ]))
    |> Polyhedrons.update_polyh am update_approx
    |> tap (fun p ->
           log ~level:Logger.DEBUG "unfold" (fun () ->
               [
                 ( "UPDATE",
                   Map.to_sequence update_approx
                   |> Util.sequence_to_string ~f:(fun (x, p) ->
                          Printf.sprintf "%s = %s" (Var.to_string ~pretty:true x)
                            (Polynomials.Polynomial.to_string_pretty p)) );
                 ( "WITH_UPDATE",
                   ApronInterface.Apron2Koat.polyh_to_constraint am p |> Guard.to_string ~pretty:true );
               ]))
    |> Polyhedrons.project_polyh am program_vars
    |> tap (fun p ->
           log ~level:Logger.DEBUG "unfold" (fun () ->
               [
                 ("PROGRAM_VARS", VarSet.to_string program_vars);
                 ( "PROJECTED",
                   ApronInterface.Apron2Koat.polyh_to_constraint am p |> Guard.to_string ~pretty:true );
               ]))
    |> ApronInterface.Apron2Koat.polyh_to_constraint am
end

module PartialEvaluation
    (PM : ProgramTypes.ProgramModules)
    (Adapter : GenericProgram_.Adapter
                 with type update_element = PM.UpdateElement.t
                  and type transition = PM.Transition.t
                  and type program = PM.Program.t
                  and type transition_graph = PM.TransitionGraph.t) =
struct
  module Abstraction = Abstraction.PropertyBasedAbstraction (PM) (Adapter)
  module Version = Version.Version (Abstraction)
  module VersionSet = MakeSetCreators0 (Version)
  open PM
  open Unfolding (PM) (Adapter)
  open Cycles
  open Cycles (PM)
  open GraphUtils.GraphUtils (PM)

  (** The call [generate_version_location_name program_locations index_table version] generates a new location name by appending the string [_vi] to the location of [version] where [i] acts as an index.
      The hashtable [index_table] maps locations to the last {i used} index.

     Currently, there is no nice way of renaming locations.
     1. We reuse the original name, if the location is unique in the pe_graph,
        that happens when the location was not part of the scc.
     2. We add a suffix _vi (where i acts as an index) if the location was multiplied (part of the scc), and l_i
        is not already present
     3. Increment i until a unique name is found.

     There is certainly a better solution to this problem, but this will work
     for now.

     TODO: rewrite, and probably move to Location module.
  *)
  let generate_version_location_name all_original_locations index_table version =
    let location = Version.location version in
    let rec get_next_location () =
      let next_index =
        Hashtbl.update_and_return index_table location ~f:(Option.value_map ~default:1 ~f:(( + ) 1))
      in
      let next_location =
        Printf.sprintf "%s_v%i" (Location.to_string location) next_index |> Location.of_string
      in
      if Set.mem all_original_locations next_location then
        get_next_location ()
      else
        next_location
    in

    if Version.is_true version then
      location
    else
      get_next_location ()


  let evaluate_component config component program_vars program_start graph =
    let am = Ppl.manager_alloc_loose () in

    let entry_transitions = entry_transitions graph component
    and exit_transitions = exit_transitions graph component in
    let entry_versions =
      let from_entry_trans =
        Set.fold
          ~f:(fun entry_versions entry_transition ->
            let entry_location = Transition.target entry_transition in
            Set.add entry_versions (Version.mk_true entry_location))
          entry_transitions ~init:VersionSet.empty
      in
      if Set.mem (TransitionSet.locations component) program_start then
        Set.add from_entry_trans (Version.mk_true program_start)
      else
        from_entry_trans
    in

    let generate_version_location_name =
      let program_locations = TransitionGraph.locations graph in
      let index_table = Hashtbl.create (module Location) in
      fun version -> generate_version_location_name program_locations index_table version
    in
    let version_location_tbl = Hashtbl.create (module Version) in

    let abstr_ctx = Abstraction.mk_from_heuristic_scc config graph component program_vars in

    let evaluate_version current_version =
      let evaluate_transition src_polyh transition =
        let src_loc, label, target_loc = transition in
        assert (Location.equal src_loc (Version.location current_version));
        if Set.mem exit_transitions transition then
          `ExitTransition (Version.mk_true target_loc)
        else
          let update = TransitionLabel.update_map label in
          let unfolded_constr = unfold_update am src_polyh program_vars update in
          let abstracted = Abstraction.abstract abstr_ctx target_loc unfolded_constr in
          `EvaluatedTransition (Version.mk target_loc abstracted)
      in
      let evaluate_grouped_transition grouped_transition :
          (Adapter.grouped_transition * Version.t List.t) Option.t =
        let src_constr = current_version |> Version.abstracted |> Abstraction.to_guard in
        let guard = Adapter.guard_of_grouped_transition grouped_transition in
        let src_polyh = initial_guard_polyh am src_constr guard in
        if Apron.Abstract1.is_bottom am src_polyh then
          (* Version + Transition guard is UNSAT *)
          None
        else
          let version_start_loc =
            Hashtbl.find_or_add version_location_tbl current_version ~default:(fun () ->
                generate_version_location_name current_version)
          in
          let next_versions = ref [] in
          let evaluated_grouped_transition =
            grouped_transition
            |> Adapter.copy_and_modify_grouped_transition ~new_start:version_start_loc
                 ~add_invariant:src_constr ~redirect:(fun trans ->
                   match evaluate_transition src_polyh trans with
                   | `ExitTransition target_version ->
                       (* Here, we exit the component hence we go to the original location *)
                       Version.location target_version
                   | `EvaluatedTransition next_version -> (
                       match Hashtbl.find version_location_tbl next_version with
                       | Some target_location ->
                           (* We have already seen next_version *)
                           target_location
                       | None ->
                           let target_location = generate_version_location_name next_version in
                           Hashtbl.add_exn version_location_tbl ~key:next_version ~data:target_location;
                           next_versions := next_version :: !next_versions;
                           target_location))
          in
          Some (evaluated_grouped_transition, !next_versions)
      in
      let refined_outgoing_grouped_transitions, next_versionss =
        Adapter.outgoing_grouped_transitions graph (Version.location current_version)
        |> Sequence.map ~f:evaluate_grouped_transition
        |> Sequence.filter_opt |> Sequence.to_list |> List.unzip
      in
      (refined_outgoing_grouped_transitions, List.concat next_versionss)
    in

    let version_stack_to_string =
      Util.sequence_to_string ~f:Version.to_string_pretty % Sequence.of_list % Stack.to_list
    in

    let evaluate_versions_till_fixedpoint remaining_versions =
      let rec evaluate_ refined_grouped_transitions already_evaluated_versions =
        match Stack.pop remaining_versions with
        | None -> refined_grouped_transitions
        | Some next_version when Set.mem already_evaluated_versions next_version ->
            log "evaluate_versions_till_fixedpoint.already_evaluated" (fun () ->
                [
                  ("version", Version.to_string_pretty next_version);
                  ("remaining:", version_stack_to_string remaining_versions);
                ]);
            evaluate_ refined_grouped_transitions already_evaluated_versions
        | Some next_version ->
            let new_grouped_transitions, new_versions = evaluate_version next_version in
            log "evaluate_versions_till_fixedpoint" (fun () ->
                [
                  ("version", Version.to_string_pretty next_version);
                  ( "new_versions",
                    Util.sequence_to_string ~f:Version.to_string_pretty (Sequence.of_list new_versions) );
                  ("remaining:", version_stack_to_string remaining_versions);
                ]);

            let new_grouped_transitions =
              Set.of_list (Set.comparator_s Adapter.empty_grouped_transition_set) new_grouped_transitions
            in
            List.iter ~f:(fun version -> Stack.push remaining_versions version) new_versions;
            evaluate_
              (Set.union refined_grouped_transitions new_grouped_transitions)
              (Set.add already_evaluated_versions next_version)
      in
      evaluate_ Adapter.empty_grouped_transition_set VersionSet.empty
    in
    Stack.of_list (Set.to_list entry_versions)
    |> evaluate_versions_till_fixedpoint
    |> tap (fun _ ->
           log "evaluate_component" (fun () ->
               [
                 ( "version_location_tbl",
                   Hashtbl.to_alist version_location_tbl
                   |> List.map ~f:(fun (ver, loc) -> (loc, ver))
                   |> List.sort ~compare:(fun (loc1, _) (loc2, _) -> Location.compare loc1 loc2)
                   |> Sequence.of_list
                   |> Util.sequence_to_string ~f:(fun (loc, ver) ->
                          Location.to_string loc ^ ": " ^ Version.to_string_pretty ver) );
               ]))


  let evaluate_component_in_program config component program_vars program_start graph =
    let exec () =
      let all_grouped_transitions_without_component =
        let all_grouped_transitions = Adapter.all_grouped_transitions_of_graph graph in
        let all_grouped_transitions_in_component =
          Set.map
            (Set.comparator_s Adapter.empty_grouped_transition_set)
            ~f:Adapter.grouped_transition_of_transition component
        in
        Set.diff all_grouped_transitions all_grouped_transitions_in_component
      in
      let all_refined_grouped_transitions =
        evaluate_component config component program_vars program_start graph
      in
      Set.union all_grouped_transitions_without_component all_refined_grouped_transitions
      |> Adapter.create_new_program program_start
    in
    Logger.with_log cfr_logger Logger.INFO
      (fun () ->
        ( "evaluate_component_in_program",
          [
            ("component", TransitionSet.to_id_string_pretty component);
            ("program_vars", VarSet.to_string ~pretty:true program_vars);
          ] ))
      exec


  let evaluate_program config program =
    let program_vars =
      Program.input_vars program
      |> tap (fun x -> log "pe" (fun () -> [ ("PROGRAM_VARS", VarSet.to_string x) ]))
    in
    let pe_prog =
      evaluate_component_in_program config (Program.transitions program) program_vars (Program.start program)
        (Program.graph program)
    in

    assert (VarSet.equal (Program.input_vars program) (Program.input_vars pe_prog));
    pe_prog


  let apply_sub_scc_cfr config (non_linear_transitions : TransitionSet.t) program =
    let program_vars =
      Program.input_vars program
      |> tap (fun x -> log "pe" (fun () -> [ ("PROGRAM_VARS", VarSet.to_string x) ]))
    in
    let orig_graph = Program.graph program in

    let pe_prog =
      let find_smallest_cycle transition =
        let src, _, target = transition in
        let shortest_path, _length = Djikstra.shortest_path orig_graph target src in
        transition :: shortest_path |> TransitionSet.of_list
      in

      let find_parallel_transitions transition =
        let src, _, target = transition in
        TransitionGraph.find_all_edges orig_graph src target |> TransitionSet.of_list
      in

      let component =
        non_linear_transitions
        |> Set.fold
             ~f:(fun cycles transition -> Set.union cycles (find_smallest_cycle transition))
             ~init:TransitionSet.empty
        |> Set.fold
             ~f:(fun parallel transition -> Set.union parallel (find_parallel_transitions transition))
             ~init:TransitionSet.empty
      in
      evaluate_component_in_program config component program_vars (Program.start program) orig_graph
    in

    log ~level:INFO "pe" (fun () -> [ ("PE", Program.to_string pe_prog) ]);
    pe_prog
end

module ClassicPartialEvaluation = PartialEvaluation (ProgramModules) (ProgramModules.ClassicAdapter)

module ProbabilisticPartialEvaluation =
  PartialEvaluation (ProbabilisticProgramModules) (ProbabilisticProgramModules.ProbabilisticAdapter)
