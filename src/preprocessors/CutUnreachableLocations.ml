open! OurBase
(** Implemenation of a preprocessor which removes all unreachable locations. *)

(** This preprocessor cuts all unreachable locations (and all transitions connected to them) from the program. *)

module Make (M : ProgramTypes.ProgramModules) = struct
  (** Logger Preprocessor *)
  let logger = Logging.(get Preprocessor)

  (** Returns a set of all locations which are reachable from the given start location. *)
  let reachable_locations graph start : LocationSet.t =
    let module Traverse = Graph.Traverse.Bfs (M.TransitionGraph) in
    Traverse.fold_component (flip Set.add) LocationSet.empty graph start


  (** Returns a set of all locations which are unreachable from the given start location. *)
  let unreachable_locations program start : LocationSet.t =
    let graph = M.Program.graph program in
    if Set.is_empty (M.Program.locations program) then
      LocationSet.empty
    else
      Set.diff (M.Program.locations program) (reachable_locations graph start)


  (** Returns program without unreachable locations and without all related transitions. *)
  let transform_program (program : M.Program.t) =
    let unreachable_locations = unreachable_locations program (M.Program.start program) in
    if Set.is_empty unreachable_locations then
      MaybeChanged.same program
    else (
      ProofOutput.add_str_paragraph_to_proof (fun () ->
          "Cut unreachable locations "
          ^ Util.sequence_to_string ~f:Location.to_string (Set.to_sequence unreachable_locations)
          ^ " from the program graph");
      let remove program location =
        Logger.(
          log logger INFO (fun () ->
              ("cut_unreachable_locations", [ ("location", Location.to_string location) ])));
        M.Program.remove_location program location
      in
      MaybeChanged.changed (Set.fold ~f:remove unreachable_locations ~init:program))
end

include Make (ProgramModules)
