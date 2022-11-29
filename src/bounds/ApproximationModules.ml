open Batteries
open ProgramModules

module MakeTransitionApproximation(PM: ProgramTypes.ProgramModules) =
  TransitionApproximationType.Make(OurInt)(PM)


module MakeSizeApproximation(PM: ProgramTypes.ProgramModules) =
  SizeApproximationType.Make(OurInt)(RVGTypes.MakeRV(PM.TransitionLabel)(PM.Transition))
