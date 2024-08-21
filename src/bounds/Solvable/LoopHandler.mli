module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) : sig
  open PM
  module Approximation : module type of Approximation.MakeForClassicalAnalysis (Bound) (PM)

  type loop = SimpleCycle.Make(Bound)(PM).loop

  val handled_transitions : loop -> TransitionSet.t

  module Loop : module type of Loop.Make (Bound) (PM)

  val find_all_possible_loops_for_scc :
    (Loop.t -> bool) -> TransitionSet.t -> Program.t -> loop ProofOutput.LocalProofOutput.with_proof list

  val finite_bound_possible_if_terminating :
    get_timebound:(Transition.t -> Bound.t) ->
    get_sizebound:(Transition.t -> Var.t -> Bound.t) ->
    loop ->
    bool
  (** Under the assumption that the runtime of the loop itself is bounded would it be possible to compute new global time bounds? *)

  val finite_bound_possible_if_terminating_with_combined_bounds :
    get_combined_bounds:(Transition.t -> Bound.t * (Var.t -> Bound.t)) -> loop -> bool
  (** Similar to [finite_bound_possible_if_twn_terminates] but allows for more choice when obtaining the bounds. *)

  val to_unlifted_bounds :
    ?twnlog:bool ->
    ?unsolvable:bool ->
    loop ProofOutput.LocalProofOutput.with_proof ->
    ( Transition.t,
      Bound.t,
      Transition.comparator_witness )
    UnliftedBounds.UnliftedTimeBound.unlifted_time_bound
end
