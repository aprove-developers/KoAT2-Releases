open Batteries

(** Provides all module types related to preprocessors *)

type subject = Program.t * Approximation.t

(** Transforms the transition graph in an equivalent form, which is more suitable for the upcoming computations. *)
type preprocessor = subject -> subject
   
(* Applies each preprocessor exactly one time. *)
let preprocess (preprocessors: preprocessor list) (subject: subject) =
  List.fold_left (fun s preprocessor -> preprocessor s) subject preprocessors

module CutUnreachable =
  struct
    module LocationSet = Set.Make(Program.Location)
                       
    (** Returns a set of all locations which are reachable from the given start location. *)
    let reachable_locations graph start : LocationSet.t  =
      let module Traverse = Graph.Traverse.Bfs(Program.TransitionGraph) in
      Traverse.fold_component LocationSet.add LocationSet.empty graph start

    let locations graph : LocationSet.t =
      Program.TransitionGraph.fold_vertex LocationSet.add graph LocationSet.empty

    let unreachable_locations graph start : LocationSet.t =
      LocationSet.diff (locations graph) (reachable_locations graph start)

    let transform: preprocessor =
      Tuple2.map1 (fun program -> 
          let unreachable_locations = unreachable_locations (Program.graph program) (Program.start program) in
          LocationSet.fold (flip Program.remove_location) unreachable_locations program
        )
  end
