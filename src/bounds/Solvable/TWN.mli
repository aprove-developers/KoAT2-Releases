open BoundsInst
open ProgramModules

type configuration = NoTransformation | Transformation
module Make(PM: ProgramTypes.ClassicalProgramModules): sig
  open PM

  module Approximation : module type of  Approximation.MakeForClassicalAnalysis(PM)

  val time_bound : ?relax_loops:bool -> configuration -> Transition.t -> TransitionSet.t -> Program.t -> Approximation.t -> Bound.t

  val terminates : ?relax_loops:bool ->  configuration -> Transition.t -> TransitionSet.t -> Program.t -> Approximation.t -> bool

  val reset_cfr : unit -> unit
end
