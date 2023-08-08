open OurBase

module UnliftedTimeBound = struct
  type ('trans,'bound,'trans_cmp_wit) unlifted_time_bound =
      { handled_transitions:       ('trans,'trans_cmp_wit) Set.t
      ; measure_decr_transitions:  ('trans,'trans_cmp_wit) Set.t
      ; entry_transitions_measure: ('trans,'bound,'trans_cmp_wit) Map.t
      ; hook: get_timebound:('trans -> 'bound)
              -> get_sizebound:('trans -> Var.t -> 'bound)
              -> ('trans,'bound,'trans_cmp_wit) Map.t
              -> 'bound
              -> unit
      }

  module Make(PM: ProgramTypes.ProgramModules)(B: BoundType.Bound) = struct
    open PM

    let measure_decr_transitions t =  t.measure_decr_transitions

    type t = (Transition.t, B.t, Transition.comparator_witness) unlifted_time_bound

    let mk ~handled_transitions ~measure_decr_transitions ?(hook=None) entry_transitions_measure: t =
      let hook = match hook with
        | None -> fun ~get_timebound ~get_sizebound _ _ -> ()
        | Some h -> h
      in
      { handled_transitions; measure_decr_transitions; entry_transitions_measure; hook; }

    let mk_from_program logger ~handled_transitions ~measure_decr_transitions ?(hook=None) program measure_from_entry_trans: t =
      let entry_transitions_measure =
        Program.entry_transitions_with_logger logger program (Set.to_list handled_transitions)
        |> List.map ~f:(fun t -> t, measure_from_entry_trans t)
        |> Map.of_alist_exn (module Transition)
      in
      mk ~handled_transitions ~measure_decr_transitions ~hook entry_transitions_measure

    let lift_and_get_hook ~get_timebound ~get_sizebound (t:t) =
      let res =
        Map.to_sequence t.entry_transitions_measure
        |> Base.Sequence.map ~f:(fun ((l,t,l'), measure) ->
            let timebound        = get_timebound (l,t,l') in
            let overappr_measure = B.substitute_f (get_sizebound (l,t,l')) measure in
            B.mul timebound overappr_measure
          )
        |> B.sum
      in
      res, fun () -> t.hook ~get_timebound ~get_sizebound t.entry_transitions_measure res

    let lift ~get_sizebound ~get_timebound t =
      Tuple2.first (lift_and_get_hook ~get_sizebound ~get_timebound t)

  end

end
