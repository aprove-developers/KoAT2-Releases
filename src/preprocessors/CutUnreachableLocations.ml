(** Implemenation of a preprocessor which removes all unreachable locations. *)
open Batteries

(** This preprocessor cuts all unreachable locations (and all transitions connected to them) from the program. *)

module Make(M: ProgramTypes.ProgramModules) = struct
  (** Logger Preprocessor *)
  let logger = Logging.(get Preprocessor)

  (** Returns a set of all locations which are reachable from the given start location. *)
  let reachable_locations graph start : M.LocationSet.t  =
    let module Traverse = Graph.Traverse.Bfs(M.TransitionGraph) in
    Traverse.fold_component M.LocationSet.add M.LocationSet.empty graph start

  (** Returns a set of all locations which are unreachable from the given start location. *)
  let unreachable_locations program start : M.LocationSet.t =
    let graph = M.Program.graph program in
    M.LocationSet.diff (M.Program.locations program) (reachable_locations graph start)

  (** Returns program without unreachable locations and without all related transitions. *)
  let transform_program (program: M.Program.t) =
    let unreachable_locations = unreachable_locations program (M.Program.start program) in
    if M.LocationSet.is_empty unreachable_locations then
    MaybeChanged.same program
  else (
    ProofOutput.add_str_paragraph_to_proof
        (fun () -> "Cut unreachable locations "^Util.enum_to_string M.Location.to_string (M.LocationSet.enum unreachable_locations)^
                 " from the program graph");
    let remove location program =
        Logger.(log logger INFO (fun () -> "cut_unreachable_locations", ["location", M.Location.to_string location]));
        M.Program.remove_location program location
    in
      MaybeChanged.changed (M.LocationSet.fold remove unreachable_locations program))
end

include Make(ProgramModules)
