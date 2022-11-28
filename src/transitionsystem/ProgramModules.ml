open Batteries

(** Modules relevant for working with programs *)
module ProgramModulesOver(L: ProgramTypes.Location): ProgramTypes.ProgramModules = struct
  module Location = L
  module LocationSet = Set.Make(L)
  module UpdateElement = Polynomials.Polynomial
  module TransitionLabel = TransitionLabel_

  module TransitionSet = Transition_.TransitionSetOver(Transition_.TransitionOver(TransitionLabel)(L))(L)
  module TransitionGraph = TransitionGraph_.TransitionGraphOverLocation(L)
  module Transition = Transition_.TransitionOver(TransitionLabel)(L)

  module Program = Program_.ProgramOverLocation(L)
end

(* here we can not simply use include ProgramModulesOver(Location) since we rely on the specialized versions *)
module Program = Program_
module UpdateElement = Polynomials.Polynomial
module TransitionGraph = TransitionGraph_
module TransitionLabel = TransitionLabel_
module TransitionSet = Transition_.TransitionSetOver(Transition_.TransitionOver(TransitionLabel_)(Location))(Location)
module Transition = Transition_
module LocationSet = Set.Make(Location)
module Location = Location
