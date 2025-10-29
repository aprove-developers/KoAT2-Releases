open! OurBase

module UnliftedTimeBound = struct
  type ('trans, 'modifier, 'bound, 'trans_cmp_wit) compute_proof_ =
    get_timebound:('trans -> 'bound) ->
    get_sizebound:('modifier -> Var.t -> 'bound) ->
    ('trans, 'bound, 'trans_cmp_wit) Map.t ->
    'bound ->
    Formatter.format ->
    FormattedString.t

  type ('trans, 'modifier, 'bound, 'trans_cmp_wit) unlifted_time_bound = {
    measure_decr_transitions : ('trans, 'trans_cmp_wit) Set.t;
    entry_transitions_measure : ('trans, 'bound, 'trans_cmp_wit) Map.t;
    entry_transitions_measure_fcs : ('trans, 'bound * VarFunctionCallSet.t, 'trans_cmp_wit) Map.t;
    compute_proof : ('trans, 'modifier, 'bound, 'trans_cmp_wit) compute_proof_;
  }

  module Make (PM : ProgramTypes.ClassicalProgramModules) (B : BoundType.Bound) = struct
    open PM

    let measure_decr_transitions t = t.measure_decr_transitions

    type t = (Transition.t, RV.modifier, B.t, Transition.comparator_witness) unlifted_time_bound
    type compute_proof = (Transition.t, RV.modifier, B.t, Transition.comparator_witness) compute_proof_

    let entry_transitions_measure t = t.entry_transitions_measure

    let mk ~measure_decr_transitions ?(compute_proof = None) entry_transitions_measure : t =
      let compute_proof =
        Option.value compute_proof ~default:(fun ~get_timebound ~get_sizebound _ _ _ -> FormattedString.Empty)
      in
      {
        measure_decr_transitions;
        entry_transitions_measure;
        entry_transitions_measure_fcs = Map.empty (module Transition);
        compute_proof;
      }


    let mk_fcs ~measure_decr_transitions ?(compute_proof = None) entry_transitions_measure
        entry_transitions_measure_fcs : t =
      let compute_proof =
        Option.value compute_proof ~default:(fun ~get_timebound ~get_sizebound _ _ _ -> FormattedString.Empty)
      in
      { measure_decr_transitions; entry_transitions_measure; entry_transitions_measure_fcs; compute_proof }


    let mk_from_program logger ~handled_transitions ~measure_decr_transitions ?(compute_proof = None) program
        measure_from_entry_trans : t =
      let entry_transitions_measure =
        Program.entry_transitions_without_function_calls_with_logger logger program (Set.to_list handled_transitions)
        |> List.map ~f:(fun t -> (t, measure_from_entry_trans t))
        |> Map.of_alist_exn (module Transition)
      in
      mk ~measure_decr_transitions ~compute_proof entry_transitions_measure


    let mk_from_program_fcs logger ~handled_transitions ~measure_decr_transitions ?(compute_proof = None)
        program measure_from_entry_trans measure_from_entry_trans_fcs : t =
      let entry_transitions_measure =
        Program.entry_transitions_without_function_calls_with_logger logger program (Set.to_list handled_transitions)
        |> List.map ~f:(fun t -> (t, measure_from_entry_trans t))
        |> Map.of_alist_exn (module Transition)
      in
      let entry_transitions_measure_fcs =
        Program.entry_transitions_only_rec program (Set.to_list handled_transitions)
        |> List.map ~f:(fun t -> (t, measure_from_entry_trans_fcs t))
        |> Map.of_alist_exn (module Transition)
      in
      mk_fcs ~measure_decr_transitions ~compute_proof entry_transitions_measure entry_transitions_measure_fcs


    let lift_and_get_proof ~get_timebound ~get_sizebound (t : t) =
      let res_trans =
        Map.to_sequence t.entry_transitions_measure
        |> Sequence.map ~f:(fun (t', measure) ->
               let timebound = get_timebound t' in
               let overappr_measure = B.substitute_f (get_sizebound (RV.modifier_of_transition t')) measure in
               B.mul timebound overappr_measure)
        |> B.sum
      in
      let res_fcs =
        Map.to_sequence t.entry_transitions_measure_fcs
        |> Sequence.map ~f:(fun (t', (measure, fcs)) ->
               let timebound = get_timebound t' in
               let overappr_measure =
                 B.sum
                 @@ Sequence.map
                      ~f:(fun fc -> B.substitute_f (get_sizebound (RV.modifier_of_function_call fc)) measure)
                      (Set.to_sequence fcs)
               in
               B.mul timebound overappr_measure)
        |> B.sum
      in
      B.
        ( res_trans + res_fcs,
          t.compute_proof ~get_timebound ~get_sizebound t.entry_transitions_measure (res_trans + res_fcs) )


    let lift ~get_sizebound ~get_timebound t =
      Tuple2.first (lift_and_get_proof ~get_sizebound ~get_timebound t)
  end
end
