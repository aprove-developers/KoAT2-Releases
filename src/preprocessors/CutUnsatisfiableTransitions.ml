open OurBase
(** Implemenation of a preprocessor which removes all unsatisfiable transitions. *)

open Formulas

(** This preprocessor removes all unsatisfiable transitions from the graph.
    Those transitions can never be part of an evaluation.
    Note that it only removes the specific transitions.
    After the transformation the graph might contain unreachable locations, and even locations that are not connected to any transition. *)

module Make (M : ProgramTypes.ProgramModules) = struct
  open M

  (** Logger Preprocessor *)
  let logger = Logging.(get Preprocessor)

  (** Returns a set of transistions which have a conflicting guard (e.g. 0 > 0). *)
  let unsatisfiable_transitions program graph : TransitionSet.t =
    let combine (l, t, l') set =
      if Program.is_initial_location program l then
        if SMT.Z3Solver.unsatisfiable (Formula.mk (TransitionLabel.guard t)) then
          Base.Set.add set (l, t, l')
        else
          set
      else
        (* There needs to be a transition distinct from (l,t,l') to enter (l,t,l') *)
        (* Note that the enum returned by Program.pre is lazy. Hence, we only have to compute the first value of this enum *)
        let intrans =
          Sequence.filter ~f:(not % Transition.equal (l, t, l')) @@ Program.pre_lazy program (l, t, l')
        in
        if Sequence.is_empty intrans then
          Base.Set.add set (l, t, l')
        else
          set
    in
    TransitionGraph.fold_edges_e combine graph TransitionSet.empty


  (** Returns program without unsatisfiable transitions. *)
  let transform_program program =
    let unsatisfiable_transitions = unsatisfiable_transitions program (Program.graph program) in
    if Base.Set.is_empty unsatisfiable_transitions then
      MaybeChanged.same program
    else
      let removed =
        Logger.(
          log logger INFO (fun () ->
              ( "cut_unsatisfiable_transitions",
                [ ("transitions", TransitionSet.to_id_string_pretty unsatisfiable_transitions) ] )));
        ProofOutput.add_str_paragraph_to_proof (fun () ->
            "Cut unsatisfiable transition " ^ TransitionSet.to_id_string_pretty unsatisfiable_transitions);
        Program.remove_unsatisfiable_transitions program unsatisfiable_transitions
      in
      MaybeChanged.changed removed
end

include Make (ProgramModules)
