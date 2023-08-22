open! OurBase

module UnliftedTimeBound = struct
  type ('trans, 'bound, 'trans_cmp_wit) compute_proof_ =
    get_timebound:('trans -> 'bound) ->
    get_sizebound:('trans -> Var.t -> 'bound) ->
    ('trans, 'bound, 'trans_cmp_wit) Map.t ->
    'bound ->
    Formatter.format ->
    FormattedString.t

  type ('trans, 'bound, 'trans_cmp_wit) unlifted_time_bound = {
    measure_decr_transitions : ('trans, 'trans_cmp_wit) Set.t;
    entry_transitions_measure : ('trans, 'bound, 'trans_cmp_wit) Map.t;
    compute_proof : ('trans, 'bound, 'trans_cmp_wit) compute_proof_;
  }

  module Make (PM : ProgramTypes.ProgramModules) (B : BoundType.Bound) = struct
    open PM

    let measure_decr_transitions t = t.measure_decr_transitions

    type t = (Transition.t, B.t, Transition.comparator_witness) unlifted_time_bound
    type compute_proof = (Transition.t, B.t, Transition.comparator_witness) compute_proof_

    let entry_transitions_measure t = t.entry_transitions_measure

    let mk ~measure_decr_transitions ?(compute_proof = None) entry_transitions_measure : t =
      let compute_proof =
        Option.value compute_proof ~default:(fun ~get_timebound ~get_sizebound _ _ _ -> FormattedString.Empty)
      in
      { measure_decr_transitions; entry_transitions_measure; compute_proof }


    let mk_from_program logger ~handled_transitions ~measure_decr_transitions ?(compute_proof = None) program
        measure_from_entry_trans : t =
      let entry_transitions_measure =
        Program.entry_transitions_with_logger logger program (Set.to_list handled_transitions)
        |> List.map ~f:(fun t -> (t, measure_from_entry_trans t))
        |> Map.of_alist_exn (module Transition)
      in
      mk ~measure_decr_transitions ~compute_proof entry_transitions_measure


    let lift_and_get_proof ~get_timebound ~get_sizebound (t : t) =
      let res =
        Map.to_sequence t.entry_transitions_measure
        |> Sequence.map ~f:(fun ((l, t, l'), measure) ->
               let timebound = get_timebound (l, t, l') in
               let overappr_measure = B.substitute_f (get_sizebound (l, t, l')) measure in
               B.mul timebound overappr_measure)
        |> B.sum
      in
      (res, t.compute_proof ~get_timebound ~get_sizebound t.entry_transitions_measure res)


    let lift ~get_sizebound ~get_timebound t =
      Tuple2.first (lift_and_get_proof ~get_sizebound ~get_timebound t)
  end
end
