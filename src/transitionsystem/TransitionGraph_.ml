open Batteries

module Make_(T: ProgramTypes.Transition)
            (L: ProgramTypes.Location with type t = T.location)
            (G: Graph.Sig.P with type V.t = L.t
                             and type V.label = L.t
                             and type E.t = L.t * T.transition_label * L.t
                             and type E.label = T.transition_label) = struct


  type location = L.t
  type location_set = Location.LocationSetOver(L).t
  type transition_label = T.transition_label
  type transition = T.t
  type transition_set = (T.t, T.comparator_witness) Base.Set.t

  module TransitionSet = Transition_.TransitionSetOver(T)(L)
  module Location = L

  include G

  let add_locations locations graph =
    Base.(Sequence.fold ~f:add_vertex ~init:graph locations)

  let add_transitions transitions graph =
    Base.(Sequence.fold ~f:add_edge_e ~init:graph transitions)

  let mk transitions = empty |> add_transitions transitions

  let locations graph = fold_vertex (flip Base.Set.add) graph (Base.Set.empty (module L))

  let transitions graph = fold_edges_e (flip Base.Set.add) graph TransitionSet.empty

  let map_transitions f t =
    let module MapModule = Graph.Gmap.Edge(G)(struct include G let empty () = empty end) in
    MapModule.map f t

  let map_labels f = map_transitions (fun(l,t,l') -> (l,f t,l'))

  let loc_transitions graph locations =
    transitions graph
    |> Base.Set.filter ~f:(fun (l,_,l') ->
           List.mem_cmp Location.compare l locations
           && List.mem_cmp Location.compare l' locations)

  let equivalent graph1 graph2 =
    let module Equivalence_TransitionSet = struct
      include T
      include Base.Comparator.Make(struct include T let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque let compare = T.compare_equivalent end)
      end
    in
    Base.Set.equal (locations graph1) (locations graph2)
    && Base.Set.equal (graph1 |> transitions |> Base.Set.to_sequence |> Base.Set.of_sequence (module Equivalence_TransitionSet))
         (graph2 |> transitions |> Base.Set.to_sequence |> Base.Set.of_sequence (module Equivalence_TransitionSet))

  let replace_edge_e old_transition new_transition graph =
    add_edge_e (remove_edge_e graph old_transition) new_transition

  let add_invariant location invariant graph =
    location
    |> succ_e graph (* An invariant holds before the execution of the successor transitions *)
    |> List.fold_left (fun result transition ->
           replace_edge_e transition (T.add_invariant invariant transition) result
         ) graph

  let sccs graph =
    let module SCC = Graph.Components.Make(G) in
    List.map (loc_transitions graph) @@ SCC.scc_list graph
    |> List.filter (not % Base.Set.is_empty)

  let sccs_ trans =
    let graph = add_transitions trans empty in
    sccs graph
end

module TransitionGraphOverLocation(L: ProgramTypes.Location) =
  Make_(Transition_.TransitionOver(TransitionLabel_)(L)) (L)
       (Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(L)(TransitionLabel_))

include TransitionGraphOverLocation(Location)
