open! OurBase

module DependencyGraph = struct
  include Graph.Persistent.Digraph.ConcreteBidirectional (Location)

  let sccs_locs graph =
    let module SCC = Graph.Components.Make (Graph.Persistent.Digraph.ConcreteBidirectional (Location)) in
    SCC.scc_list graph |> List.map ~f:LocationSet.of_list


  let sccs graph =
    let module SCC = Graph.Components.Make (Graph.Persistent.Digraph.ConcreteBidirectional (Location)) in
    SCC.scc graph
end
