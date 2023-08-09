open OurBase
(** Implemenation of a preprocessor which performs chaining on the TransitionGraph. *)

open ProgramModules

(** Implemenation of a preprocessor which performs chaining on the TransitionGraph. Adds transitions to the graph such that every predecessor of the location is correctly connected with every successor of the location, making the location obsolete. *)

(** Logger Preprocessor *)
let logger = Logging.(get Preprocessor)

(** Adds transitions to the graph such that every predecessor of the location is correctly connected with every successor of the location,
    making the location obsolete. *)
let skip_location location graph =
  TransitionGraph.(pred_e, succ_e)
  |> Tuple2.mapn (fun f -> f graph location)
  |> Tuple2.mapn Sequence.of_list |> uncurry Sequence.cartesian_product
  |> Sequence.map ~f:(fun ((l, t, l1), (l'1, t', l')) ->
         let chained = (l, TransitionLabel.append t t', l') in
         ProofOutput.add_str_paragraph_to_proof (fun () ->
             "Chain transitions "
             ^ Transition.to_id_string_pretty (l, t, l1)
             ^ " and "
             ^ Transition.to_id_string_pretty (l'1, t', l')
             ^ " to "
             ^ Transition.to_id_string_pretty chained);
         chained)
  |> (flip TransitionGraph.add_transitions) graph


(** Returns true if the specific location is chainable in the graph. *)
let chainable ?(conservative = false) graph location : bool =
  let open TransitionGraph in
  if conservative then
    (not (mem_edge graph location location))
    && out_degree graph location == 1
    && in_degree graph location == 1
  else
    (not (mem_edge graph location location))
    && out_degree graph location >= 1
    && in_degree graph location >= 1


(** Performs a chaining step removing the location from the graph. *)
let chain location graph : TransitionGraph.t =
  let skipped = skip_location location graph in
  TransitionGraph.succ_e skipped location |> List.fold ~f:TransitionGraph.remove_edge_e ~init:skipped


(** Performs chaining on the TransitionGraph. *)
let transform_graph ?(conservative = false) ?(scc = None) (graph : TransitionGraph.t) :
    TransitionGraph.t MaybeChanged.t =
  let try_chaining location maybe_changed_graph =
    let open MaybeChanged in
    maybe_changed_graph >>= fun graph ->
    if
      (Option.is_none scc || Set.mem (TransitionSet.locations (Option.value_exn scc)) location)
      && chainable ~conservative graph location
    then (
      Logger.(log logger INFO (fun () -> ("chaining", [ ("location", Location.to_string location) ])));
      changed (chain location graph))
    else
      same graph
  in
  TransitionGraph.fold_vertex try_chaining graph (MaybeChanged.same graph)
