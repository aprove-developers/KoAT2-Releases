open! OurBase
open Constraints
open Formulas

module PreAdapterNonRec
    (TL : ProgramTypes.DefaultTransitionLabel)
    (T : ProgramTypes.Transition
           with type transition_label = TL.t
            and type transition_label_comparator_witness = TL.comparator_witness)
    (G : ProgramTypes.TransitionGraph
           with type transition_label = TL.t
            and type transition_label_comparator_witness = TL.comparator_witness) =
struct
  type transition_label = TL.t
  type transition_label_comparator_witness = TL.comparator_witness
  type transition = Location.t * TL.t * Location.t
  type transition_comparator_witness = T.comparator_witness
  type transition_set = (transition, transition_comparator_witness) Set.t
  type transition_graph = G.t
  type t = (transition_label, transition_label_comparator_witness, transition_graph) GenericProgram_.t

  open GenericProgram_

  let with_pre_cache program = Atomically.run_atomically program.pre_cache

  let compute_pre program (l, t, l') =
    let is_satisfiable f =
      try SMT.Z3Solver.satisfiable f with
      | SMT.SMTFailure _ ->
          true (* thrown if solver does not know a solution due to e.g. non-linear arithmetic *)
    in
    l |> G.pred_e program.graph |> Sequence.of_list
    |> Sequence.filter ~f:(fun (_, t', _) ->
           TL.chain_guards t' t
           |> is_satisfiable % Formula.mk % Constraint.drop_nonlinear (* such that Z3 uses QF_LIA*))


  let pre_lazy program trans =
    let res = with_pre_cache program @@ fun pre_cache -> Hashtbl.find pre_cache trans in
    match res with
    | Some tset -> Set.to_sequence tset
    | None -> compute_pre program trans


  let pre program trans =
    with_pre_cache program @@ fun pre_cache ->
    match Hashtbl.find pre_cache trans with
    | Some tset -> tset
    | None ->
        let tset = Set.of_sequence (module T) (compute_pre program trans) in
        Hashtbl.add_exn pre_cache ~key:trans ~data:tset;
        tset


  module TransitionSet = Transition_.TransitionSetOver (T)

  let entry_transitions program transitions =
    let transitions_set = TransitionSet.of_list transitions in
    let all_possible_pre_transitions = transitions |> List.map ~f:(pre program) |> TransitionSet.union_list in
    Set.diff all_possible_pre_transitions transitions_set |> Set.to_list


  (** All entry transitions of the given transitions.
            These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  let entry_transitions_with_logger logger program (transitions : T.t list) : T.t List.t =
    entry_transitions program transitions
    |> tap (fun transitions ->
           Logger.log logger Logger.DEBUG (fun () ->
               ( "entry_transitions",
                 [ ("result", transitions |> Sequence.of_list |> Util.sequence_to_string ~f:T.to_id_string) ]
               )))
end

module PreAdapter = struct
  module Transition = Transition_.MakeClassical (TransitionLabel_)
  module TransitionSet = Transition_.TransitionSetOver (Transition)
  module PreAdapter = PreAdapterNonRec (TransitionLabel_) (Transition) (TransitionGraph_)

  type transition_label = TransitionLabel_.t
  type transition_label_comparator_witness = TransitionLabel_.comparator_witness
  type transition = Transition.t
  type transition_comparator_witness = Transition.comparator_witness
  type transition_set = TransitionSet.t
  type transition_graph = TransitionGraph_.t
  type t = (transition_label, transition_label_comparator_witness, transition_graph) GenericProgram_.t

  let rec_trans program l =
    let open GenericProgram_ in
    Set.filter (TransitionGraph_.transitions program.graph) ~f:(fun t ->
        Set.mem (Set.map (module Location) ~f:VarRec.return_loc (Transition.rec_vars t)) l)


  let compute_pre program ((l, _, _) as t) =
    Sequence.append (PreAdapter.compute_pre program t) (Set.to_sequence @@ rec_trans program l)


  let pre_lazy program ((l, _, _) as t) =
    Sequence.append (PreAdapter.pre_lazy program t) (Set.to_sequence @@ rec_trans program l)


  let pre program ((l, _, _) as t) = Set.union (PreAdapter.pre program t) (rec_trans program l)

  let entry_transitions program transitions =
    let transitions_set = TransitionSet.of_list transitions in
    let all_possible_pre_transitions = transitions |> List.map ~f:(pre program) |> TransitionSet.union_list in
    Set.diff all_possible_pre_transitions transitions_set |> Set.to_list


  (** All entry transitions of the given transitions.
         These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  let entry_transitions_with_logger logger program (transitions : Transition.t list) : Transition.t List.t =
    entry_transitions program transitions
    |> tap (fun transitions ->
           Logger.log logger Logger.DEBUG (fun () ->
               ( "entry_transitions_rec",
                 [
                   ( "result",
                     transitions |> Sequence.of_list |> Util.sequence_to_string ~f:Transition.to_id_string );
                 ] )))
end
