open Bounds
open ProgramModules

type configuration = [ `NoTransformation | `Transformation ]

module Make (PM : ProgramTypes.ClassicalProgramModules) : sig
  open PM
  module Approximation : module type of Approximation.MakeForClassicalAnalysis (PM)

  type twn_loop = SimpleCycle.Make(PM).twn_loop

  val handled_transitions : twn_loop -> TransitionSet.t

  val find_all_possible_loops_for_scc :
    configuration -> TransitionSet.t -> Program.t -> twn_loop ProofOutput.LocalProofOutput.with_proof list

  val finite_bound_possible_if_twn_terminates :
    get_timebound:(Transition.t -> Bound.t) ->
    get_sizebound:(Transition.t -> Var.t -> Bound.t) ->
    twn_loop ->
    bool
  (** Under the assumption that the runtime of the TWN Loop itself is bounded would it be possible to compute new global time bounds? *)

  val to_unlifted_bounds :
    twn_loop ProofOutput.LocalProofOutput.with_proof ->
    ( Transition.t,
      Bound.t,
      Transition.comparator_witness )
    UnliftedBounds.UnliftedTimeBound.unlifted_time_bound

  val terminates : configuration -> Transition.t -> TransitionSet.t -> Program.t -> Approximation.t -> bool
  val reset_cfr : unit -> unit
end
