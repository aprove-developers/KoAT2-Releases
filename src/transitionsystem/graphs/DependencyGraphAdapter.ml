open! OurBase

module type Adapter = sig
  type transition_label
  type transition_graph

  val mk_from_graph : transition_graph -> DependencyGraph.DependencyGraph.t
end

module ClassicAdapter = struct
  type transition_label = TransitionLabel_.t
  type transition_graph = TransitionGraph_.t

  let mk_from_graph graph =
    let open DependencyGraph in
    TransitionGraph_.fold_edges_e
      (fun (l, t, l') g ->
        (* Add all recursive jumps λ an edge l -> λ and an edge l'' -> l iff l'' is a return location and reachable from λ *)
        Set.fold (TransitionLabel_.rec_vars t)
          ~f:(fun g v ->
            let return_loc = VarFunctionCall.return_loc v in
            let reachable_locs = TransitionGraph_.reachable_locations graph return_loc in
            Set.fold reachable_locs
              ~f:(fun g l'' -> DependencyGraph.add_edge g l'' l)
              ~init:(DependencyGraph.add_edge g l return_loc))
          ~init:(DependencyGraph.add_edge g l l'))
      graph DependencyGraph.empty
end
