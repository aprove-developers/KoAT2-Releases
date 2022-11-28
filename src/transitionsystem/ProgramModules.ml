open Batteries

(** Modules relevant for working with programs *)
module ProgramModulesOver(L: ProgramTypes.Location) = struct
  module Location = L
  module LocationSet = Set.Make(L)
  module TransitionLabel = TransitionLabel

  module TransitionSet = Transition.TransitionSetOver(Transition.TransitionOver(TransitionLabel)(L))(L)
  module TransitionGraph = TransitionGraph.TransitionGraphOverLocation(L)
  module Transition = Transition.TransitionOver(TransitionLabel)(L)

  module Program = Program.ProgramOverLocation(L)
end

(* here we can not simply use include ProgramModulesOver(Location) since we rely on the specialized versions *)
module Program = Program
module TransitionGraph = TransitionGraph
module TransitionLabel = TransitionLabel
module TransitionSet = Transition.TransitionSetOver(Transition.TransitionOver(TransitionLabel)(Location))(Location)
module Transition = Transition
module LocationSet = Set.Make(Location)
module Location = Location
