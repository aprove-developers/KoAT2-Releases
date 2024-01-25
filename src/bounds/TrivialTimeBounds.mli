open! OurBase

module Classical (Bound : BoundType.Bound) : sig
  open ProgramModules

  type appr = Approximation.MakeForClassicalAnalysis(Bound)(ProgramModules).t

  val compute : Program.t -> appr -> appr
  (** This function infers for all transitions which are not part of an scc a time bound of one.
      Those transitions can only be executed once and preprocessing might increase performance and also might lead to better bounds. *)
end

module Probabilistic : sig
  open ProbabilisticProgramModules

  val compute : Program.t -> Approximation.Probabilistic.apprs -> Approximation.Probabilistic.apprs
end

include module type of Classical (Bounds.Bound)
