module DependencyGraph : sig
  include module type of Graph.Persistent.Digraph.ConcreteBidirectional (Location)
  open Graph.Persistent.Digraph.ConcreteBidirectional(Location)

  val sccs_locs : t -> LocationSet.t list
  val sccs : t -> int * (Location.t -> int)
end
