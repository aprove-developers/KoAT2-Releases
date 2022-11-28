open Batteries

(** Modules relevant for working with programs *)
module ProgramModulesOver(L: ProgramTypes.Location) = struct
  module Program = Program.ProgramOver(L)
  module TransitionGraph = TransitionGraph.TransitionGraphOver(L)
  module TransitionSet = Transition.TransitionSetOver(L)
  module Transition = Transition.TransitionOver(L)
  module Location = L
  module LocationSet = Set.Make(Location)
end

(* here we can not simply use include ProgramModulesOver(Location) since we rely on the specialized versions *)
module Program = Program
module TransitionGraph = TransitionGraph
module TransitionSet = Transition.TransitionSetOver(Location)
module Transition = Transition
module Location = Location
module LocationSet = Set.Make(Location)
