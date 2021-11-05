(** Implemenation of a preprocessor which performs chaining on the TransitionGraph. *)
open Batteries
open ProgramTypes

(** Implemenation of a preprocessor which performs chaining on the TransitionGraph. Adds transitions to the graph such that every predecessor of the location is correctly connected with every successor of the location, making the location obsolete. *)

(** Logger Preprocessor *)
let logger = Logging.(get Preprocessor)

(** Adds transitions to the graph such that every predecessor of the location is correctly connected with every successor of the location,
    making the location obsolete. *)
let skip_location location graph =
  TransitionGraph.(pred_e, succ_e)
  |> Tuple2.mapn (fun f -> f graph location)
  |> Tuple2.mapn List.enum
  |> uncurry Enum.cartesian_product
  |> Enum.map (fun ((l,t,l1), (l'1,t',l')) ->
        let chained = (l, TransitionLabel.append t t', l') in
        ProofOutput.add_str_paragraph_to_proof
          (fun () -> "Chain transitions "^Transition.to_id_string (l,t,l1)^" and "^
                      Transition.to_id_string (l'1,t',l')^" to "^Transition.to_id_string chained);
        chained
     )
  |> (flip Program.add_transitions) graph

(** Returns true if the specific location is chainable in the graph. *)
let chainable graph location : bool =
  let open TransitionGraph in
  not (mem_edge graph location location)
  && out_degree graph location >= 1
  && in_degree graph location >= 1

(** Performs a chaining step removing the location from the graph. *)
let chain location graph : TransitionGraph.t =
  let skipped = skip_location location graph in
  TransitionGraph.succ_e skipped location
  |> List.enum
  |> Enum.fold TransitionGraph.remove_edge_e skipped

(** Performs chaining on the TransitionGraph. *)
let transform_graph scc (graph: TransitionGraph.t): TransitionGraph.t MaybeChanged.t =
  let try_chaining location maybe_changed_graph =
    let open MaybeChanged in
    maybe_changed_graph >>= (fun graph ->
      if LocationSet.mem location (TransitionSet.locations scc) && chainable graph location then (
        Logger.(log logger INFO (fun () -> "chaining", ["location", Location.to_string location]));
        changed (chain location graph)
      ) else
        same graph)
  in TransitionGraph.fold_vertex try_chaining graph (MaybeChanged.same graph)

let lift_to_program scc transform program =
  MaybeChanged.(transform scc (Program.graph program) >>= (fun graph -> same (Program.map_graph (fun _ -> graph) program)))