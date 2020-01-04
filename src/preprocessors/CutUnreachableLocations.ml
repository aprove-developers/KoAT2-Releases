(** Implemenation of a preprocessor which removes all unreachable locations. *)
open Batteries
open ProgramTypes
   
(** This preprocessor cuts all unreachable locations (and all transitions connected to them) from the program. *)

let logger = Logging.(get Preprocessor)
   
module LocationSet = Set.Make(Location)

(** Returns a set of all locations which are reachable from the given start location. *)
let reachable_locations graph start : LocationSet.t  =
  let module Traverse = Graph.Traverse.Bfs(TransitionGraph) in
  Traverse.fold_component LocationSet.add LocationSet.empty graph start

(** Returns a set of all locations which are unreachable from the given start location. *)
let unreachable_locations graph start : LocationSet.t =
  LocationSet.diff (TransitionGraph.locations graph) (reachable_locations graph start)

(** Returns program without unreachable locations and without all related transitions. *)
let transform_program program = 
  let unreachable_locations = unreachable_locations (Program.graph program) (Program.start program) in
  if LocationSet.is_empty unreachable_locations then
    MaybeChanged.same program
  else
    let remove location program =
      Logger.(log logger INFO (fun () -> "cut_unreachable_locations", ["location", Location.to_string location]));
      Program.remove_location program location
    in
    MaybeChanged.changed (LocationSet.fold remove unreachable_locations program)
