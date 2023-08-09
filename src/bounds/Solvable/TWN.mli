open Bounds
open ProgramModules

type configuration = [ `NoTransformation | `Transformation ]

module Make (PM : ProgramTypes.ClassicalProgramModules) : sig
  open PM
  module Approximation : module type of Approximation.MakeForClassicalAnalysis (PM)

  val time_bound :
    configuration ->
    Transition.t ->
    TransitionSet.t ->
    Program.t ->
    Approximation.t ->
    ( Transition.t,
      Bound.t,
      Transition.comparator_witness )
    UnliftedBounds.UnliftedTimeBound.unlifted_time_bound
    Option.t

  val terminates : configuration -> Transition.t -> TransitionSet.t -> Program.t -> Approximation.t -> bool
  val reset_cfr : unit -> unit
end
