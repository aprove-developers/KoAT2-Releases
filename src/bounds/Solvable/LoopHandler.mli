module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) : sig
  open PM
  module Approximation : module type of Approximation.MakeForClassicalAnalysis (Bound) (PM)

  module TWN_Complexity :
      module type of TWN_Complexity.Make (Bound) (PM.TransitionLabel.TransitionLabelNonFunctionCall)

  type loop = Transition.t list * (Transition.t * TWN_Complexity.local_bound Lazy.t) list
  (** A cycle together with the local bounds of all its entry transitions. Each local bound is computed at most once and only on demand. *)

  val handled_transitions : loop -> TransitionSet.t

  module Loop : module type of Loop.Make (Bound) (PM.TransitionLabel.TransitionLabelNonFunctionCall)

  val find_all_possible_loops_for_scc :
    ?twnlog:bool ->
    ?unsolvable:bool ->
    (Loop.t -> bool) ->
    TransitionSet.t ->
    Program.t ->
    loop ProofOutput.LocalProofOutput.with_proof list

  val finite_bound_possible_if_terminating :
    get_timebound:(Transition.t -> Bound.t) -> get_sizebound:(RV.modifier -> Var.t -> Bound.t) -> loop -> bool
  (** Under the assumption that the runtime of the loop itself is bounded would it be possible to compute new global time bounds?
      This requires finite time bounds for all entry transitions and finite size bounds for the variables occurring in the corresponding local bounds. *)

  val finite_bound_possible_if_terminating_with_combined_bounds :
    get_combined_bounds:(Transition.t -> Bound.t * (Var.t -> Bound.t)) -> loop -> bool
  (** Similar to [finite_bound_possible_if_twn_terminates] but allows for more choice when obtaining the bounds. *)

  val to_unlifted_bounds :
    loop ProofOutput.LocalProofOutput.with_proof ->
    ( Transition.t,
      RV.modifier,
      Bound.t,
      Transition.comparator_witness )
    UnliftedBounds.UnliftedTimeBound.unlifted_time_bound
end
