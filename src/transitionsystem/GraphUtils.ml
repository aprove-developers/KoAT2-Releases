open! OurBase

module GraphUtils (PM : ProgramTypes.ProgramModules) = struct
  open PM

  module TransitionGraphWeight (Value : PolyTypes.Ring) = struct
    type t = Value.t
    type edge = PM.TransitionGraph.E.t

    let weight (x : edge) = Value.one
    let compare x y = 0
    let add x y = Value.add x y
    let zero = Value.zero
  end

  module Djikstra = Graph.Path.Dijkstra (PM.TransitionGraph) (TransitionGraphWeight (OurInt))

  let entry_transitions graph scc =
    TransitionSet.locations scc |> Set.to_sequence
    |> Sequence.concat_map ~f:(fun l -> TransitionGraph.pred_e graph l |> Sequence.of_list)
    |> Sequence.filter ~f:(fun t -> not (Set.mem scc t))
    |> TransitionSet.of_sequence


  let exit_transitions graph scc =
    TransitionSet.locations scc |> Set.to_sequence
    |> Sequence.concat_map ~f:(fun l -> TransitionGraph.succ_e graph l |> Sequence.of_list)
    |> Sequence.filter ~f:(fun t -> not (Set.mem scc t))
    |> TransitionSet.of_sequence
end
