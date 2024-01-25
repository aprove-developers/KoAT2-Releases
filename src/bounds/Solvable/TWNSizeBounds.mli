module Make (PM : ProgramTypes.ClassicalProgramModules) : sig
  module Approximation : module type of Approximation.MakeForClassicalAnalysis (Bounds.Bound) (PM)

  val improve : PM.Program.t -> ?scc:PM.TransitionSet.t option -> Approximation.t -> Approximation.t
  val reset_cfr : unit -> unit
end
