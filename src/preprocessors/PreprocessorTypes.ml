open Batteries

(** Provides all module types related to preprocessors *)

type subject = Program.t * Approximation.t

(** Transforms the transition graph in an equivalent form, which is more suitable for the upcoming computations. *)
type preprocessor = subject -> subject
   
(* Applies each preprocessor exactly one time. *)
let preprocess (preprocessors: preprocessor list) (subject: subject) =
  List.fold_left (fun s preprocessor -> preprocessor s) subject preprocessors

module type Preprocessor =
  sig
    val transform: preprocessor
  end

(** This preprocessor cuts all unreachable locations (and all transitions connected to them) from the program. *)
module CutUnreachable =
  struct
    module LocationSet = Set.Make(Program.Location)
                       
    (** Returns a set of all locations which are reachable from the given start location. *)
    let reachable_locations graph start : LocationSet.t  =
      let module Traverse = Graph.Traverse.Bfs(Program.TransitionGraph) in
      Traverse.fold_component LocationSet.add LocationSet.empty graph start

    let unreachable_locations graph start : LocationSet.t =
      LocationSet.diff (Program.TransitionGraph.locations graph) (reachable_locations graph start)

    let transform_program program = 
      let unreachable_locations = unreachable_locations (Program.graph program) (Program.start program) in
      LocationSet.fold (flip Program.remove_location) unreachable_locations program
      
    let transform: preprocessor =
      Tuple2.map1 transform_program
  end

(** This preprocessor infers for all transitions which are not part of an scc a time bound of their cost.
    Those transitions can only be executed once and preprocessing might increase performance and also might lead to better bounds. *)
module TrivialTimeBounds =
  struct
    module SCC = Graph.Components.Make(Program.TransitionGraph)

    let transform (program, appr) =
      let graph = Program.graph program in
      let (_, scc_number) = SCC.scc graph in
      let may_improve l1 l2 appr =
        if scc_number l1 = scc_number l2 then
          appr
        else Approximation.add_timebound Bound.one (Program.TransitionGraph.find_edge graph l1 l2) appr in
      (program, Program.TransitionGraph.fold_edges may_improve graph appr)
    
  end
