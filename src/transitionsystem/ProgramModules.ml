open Batteries

(** Modules relevant for working with programs *)
module ProgramModulesOver(L: ProgramTypes.Location) = struct
  module Program = Program.ProgramOver(L)
  module TransitionGraph = Program.TransitionGraph
  module Transition = Program.Transition
  module TransitionSet = Program.TransitionSet
  module Location = L
  module LocationSet = Set.Make(Location)
end

(* here we can not simply use include ProgramModulesOver(Location) since we rely on the specialized versions *)
module Program = Program
module TransitionGraph = Program.TransitionGraph
module Transition = Program.Transition
module TransitionSet = Program.TransitionSet
module Location = Location
module LocationSet = Program.LocationSet

let id: type a. a -> a = fun a->a
let test: LocationSet.t -> TransitionSet.locationSet = id
