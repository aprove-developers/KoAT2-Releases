open Batteries
open Program.Types
   
(** This preprocessor cuts all unreachable locations (and all transitions connected to them) from the program. *)

module LocationSet = Set.Make(Location)

(** Returns a set of all locations which are reachable from the given start location. *)
let reachable_locations graph start : LocationSet.t  =
  let module Traverse = Graph.Traverse.Bfs(TransitionGraph) in
  Traverse.fold_component LocationSet.add LocationSet.empty graph start

let unreachable_locations graph start : LocationSet.t =
  LocationSet.diff (TransitionGraph.locations graph) (reachable_locations graph start)

let transform_program program = 
  let unreachable_locations = unreachable_locations (Program.graph program) (Program.start program) in
  if LocationSet.is_empty unreachable_locations then
    MaybeChanged.same program
  else
    MaybeChanged.changed (LocationSet.fold (flip Program.remove_location) unreachable_locations program)
