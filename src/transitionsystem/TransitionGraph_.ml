open Batteries

module Make_(T: ProgramTypes.Transition)
            (L: ProgramTypes.Location with type t = T.location)
            (G: Graph.Sig.P with type V.t = L.t
                             and type V.label = L.t
                             and type E.t = L.t * T.transition_label * L.t
                             and type E.label = T.transition_label) = struct
  type location = L.t
  type location_set = Set.Make(L).t
  type transition_label = T.transition_label
  type transition = T.t
  type transition_set = Transition_.TransitionSetOver(T)(L).t

  module Location = L
  module LocationSet = Set.Make(L)
  module TransitionSet = Transition_.TransitionSetOver(T)(L)

  include G

  let add_locations locations graph = Enum.fold add_vertex graph locations

  let add_transitions transitions graph = Enum.fold add_edge_e graph transitions

  let mk transitions = empty |> add_transitions transitions

  let locations graph = fold_vertex LocationSet.add graph LocationSet.empty

  let transitions graph = fold_edges_e TransitionSet.add graph TransitionSet.empty

  let map_transitions f t =
    let module MapModule = Graph.Gmap.Edge(G)(struct include G let empty () = empty end) in
    MapModule.map f t

  let map_labels f = map_transitions (fun(l,t,l') -> (l,f t,l'))

  let loc_transitions graph locations =
    transitions graph
    |> TransitionSet.filter (fun (l,_,l') ->
           List.mem_cmp Location.compare l locations
           && List.mem_cmp Location.compare l' locations)

  let equivalent graph1 graph2 =
    let module Equivalence_TransitionSet =
      Set.Make(struct include T let compare = T.compare_equivalent end)
    in
    LocationSet.equal (locations graph1) (locations graph2)
    && Equivalence_TransitionSet.equal (graph1 |> transitions |> TransitionSet.enum |> Equivalence_TransitionSet.of_enum)
         (graph2 |> transitions |> TransitionSet.enum |> Equivalence_TransitionSet.of_enum)

  let replace_edge_e old_transition new_transition graph =
    add_edge_e (remove_edge_e graph old_transition) new_transition

  let add_invariant location invariant graph =
    location
    |> succ_e graph (* An invariant holds before the execution of the successor transitions *)
    |> List.fold_left (fun result transition ->
           replace_edge_e transition (T.add_invariant invariant transition) result
         ) graph

end

module TransitionGraphOverLocation(L: ProgramTypes.Location) =
  Make_(Transition_.TransitionOver(TransitionLabel_)(L)) (L)
       (Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(L)(TransitionLabel_))

include TransitionGraphOverLocation(Location)
