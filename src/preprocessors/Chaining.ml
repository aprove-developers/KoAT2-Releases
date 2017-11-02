open Batteries
open Program.Types
   
(** Adds transitions to the graph such that every predecessor of the location is correctly connected with every successor of the location,
    making the location obsolete. *)
let skip_location location graph =
  TransitionGraph.(pred_e, succ_e)
  |> Tuple2.mapn (fun f -> f graph location)
  |> Tuple2.mapn List.enum
  |> uncurry Enum.cartesian_product
  |> Enum.map (fun ((l,t,_), (_,t',l')) -> (l, TransitionLabel.append t t', l'))
  |> (flip Program.add_transitions) graph

(** Returns if the specific location is chainable in the graph. *)
let chainable graph location : bool =
  let open TransitionGraph in
  not (mem_edge graph location location)
  && out_degree graph location >= 1
  && in_degree graph location >= 1

(** Performs a chaining step removing the location from the graph. *)
let chain location graph : TransitionGraph.t =
  TransitionGraph.remove_vertex (skip_location location graph) location

let transform_graph (graph: TransitionGraph.t): TransitionGraph.t MaybeChanged.t =
  let try_chaining location maybe_changed_graph =
    let open MaybeChanged in
    maybe_changed_graph >>= (fun graph ->
      if chainable graph location then
        changed (chain location graph)
      else
        same graph)
  in TransitionGraph.fold_vertex try_chaining graph (MaybeChanged.same graph)
