open! OurBase

module Make_
    (T : ProgramTypes.Transition)
    (L : ProgramTypes.Location
           with type t = T.location
            and type comparator_witness = T.location_comparator_witness)
    (G : Graph.Sig.P
           with type V.t = L.t
            and type V.label = L.t
            and type E.t = L.t * T.transition_label * L.t
            and type E.label = T.transition_label) =
struct
  type location = L.t
  type location_comparator_witness = L.comparator_witness
  type location_set = (location, location_comparator_witness) Set.t
  type transition_label = T.transition_label
  type transition_label_comparator_witness = T.transition_label_comparator_witness
  type transition = T.t

  type transition_comparator_witness =
    (transition_label_comparator_witness, location_comparator_witness) TransitionComparator.comparator_witness

  type transition_set = (transition, transition_comparator_witness) Set.t

  module TransitionSet = Transition_.TransitionSetOver (T) (L)
  module Location = L
  include G

  let add_locations locations graph = Sequence.fold ~f:add_vertex ~init:graph locations
  let add_transitions transitions graph = Sequence.fold ~f:add_edge_e ~init:graph transitions
  let mk transitions = empty |> add_transitions transitions
  let locations graph = fold_vertex (flip Set.add) graph (Set.empty (module L))
  let transitions graph = fold_edges_e (flip Set.add) graph TransitionSet.empty

  let map_transitions f t =
    let module MapModule =
      Graph.Gmap.Edge
        (G)
        (struct
          include G

          let empty () = empty
        end)
    in
    MapModule.map f t


  let map_labels f = map_transitions (fun (l, t, l') -> (l, f t, l'))

  let loc_transitions graph locations =
    transitions graph
    |> Set.filter ~f:(fun (l, _, l') ->
           List.mem ~equal:Location.equal locations l && List.mem ~equal:Location.equal locations l')


  let equivalent graph1 graph2 =
    let module Equivalence_TransitionSet = struct
      include T

      include Comparator.Make (struct
        include T

        let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
        let compare = T.compare_equivalent
      end)
    end in
    Set.equal (locations graph1) (locations graph2)
    && Set.equal
         (graph1 |> transitions |> Set.to_sequence |> Set.of_sequence (module Equivalence_TransitionSet))
         (graph2 |> transitions |> Set.to_sequence |> Set.of_sequence (module Equivalence_TransitionSet))


  let replace_edge_e old_transition new_transition graph =
    add_edge_e (remove_edge_e graph old_transition) new_transition


  let sccs graph =
    let module SCC = Graph.Components.Make (G) in
    List.map ~f:(loc_transitions graph) @@ SCC.scc_list graph |> List.filter ~f:(not % Set.is_empty)


  let sccs_from_sequence trans =
    let graph = add_transitions trans empty in
    sccs graph
end

module TransitionGraphOverLocation (L : ProgramTypes.Location) =
  Make_
    (Transition_.Make (TransitionLabel_) (L)) (L)
    (Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (L) (TransitionLabel_))

include TransitionGraphOverLocation (Location)
