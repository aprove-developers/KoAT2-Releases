open Batteries

(** Modules relevant for working with programs *)
module ProgramModulesOver(L: ProgramTypes.Location): ProgramTypes.ProgramModules = struct
  module LocationSet = Location.LocationSetOver(L)
  module Location = L
  module UpdateElement = Polynomials.Polynomial
  module TransitionLabel = TransitionLabel_

  module Transition = Transition_.TransitionOver(TransitionLabel)(L)
  module TransitionSet = Transition_.TransitionSetOver(Transition)(L)
  module TransitionGraph = TransitionGraph_.TransitionGraphOverLocation(L)

  module Program = Program_.ProgramOverLocation(L)

  module RV = RVGTypes.MakeRV(TransitionLabel)(Transition)
end

(* here we can not simply use include ProgramModulesOver(Location) since we rely on the specialized versions *)
module Program = Program_
module UpdateElement = Polynomials.Polynomial
module TransitionGraph = TransitionGraph_
module TransitionLabel = TransitionLabel_
module Transition = Transition_
module TransitionSet = Transition_.TransitionSetOver(Transition)(Location)
module LocationSet = Location.LocationSetOver(Location)
module Location = Location
module RV = RVGTypes.MakeRV(TransitionLabel)(Transition)
