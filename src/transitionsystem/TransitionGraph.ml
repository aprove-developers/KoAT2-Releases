open Batteries

module TransitionGraphOver(L : ProgramTypes.Location) = struct
  module Location = L
  module Transition = Transition.TransitionOver(L)
  module LocationSet = Transition.LocationSet
  module TransitionSet = Transition.TransitionSet

  let test: TransitionSet.locationSet = Transition.LocationSet.empty

  module GraphModule = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(struct include TransitionLabel let compare = compare_same end)

  include GraphModule

  let add_locations locations graph = Enum.fold add_vertex graph locations

  let add_transitions transitions graph = Enum.fold add_edge_e graph transitions

  let mk transitions = empty |> add_transitions transitions

  let locations graph = fold_vertex LocationSet.add graph LocationSet.empty

  let transitions graph = fold_edges_e TransitionSet.add graph TransitionSet.empty

  let map_transitions f t =
    let module MapModule = Graph.Gmap.Edge(GraphModule)(struct include GraphModule let empty () = empty end) in
    MapModule.map f t

  let map_labels f = map_transitions (fun(l,t,l') -> (l,f t,l'))

  let loc_transitions graph locations =
    transitions graph
    |> TransitionSet.filter (fun (l,_,l') ->
           List.mem_cmp Location.compare l locations
           && List.mem_cmp Location.compare l' locations)

  module Equivalence_TransitionSet = Set.Make(struct include Transition let compare = Transition.compare_equivalent end)

  let equivalent graph1 graph2 =
    LocationSet.equal (locations graph1) (locations graph2)
    && Equivalence_TransitionSet.equal (graph1 |> transitions |> TransitionSet.enum |> Equivalence_TransitionSet.of_enum)
         (graph2 |> transitions |> TransitionSet.enum |> Equivalence_TransitionSet.of_enum)

  let replace_edge_e old_transition new_transition graph =
    add_edge_e (remove_edge_e graph old_transition) new_transition

  let add_invariant location invariant graph =
    location
    |> succ_e graph (* An invariant holds before the execution of the successor transitions *)
    |> List.fold_left (fun result transition ->
           replace_edge_e transition (Transition.add_invariant invariant transition) result
         ) graph

end

include TransitionGraphOver(Location)
