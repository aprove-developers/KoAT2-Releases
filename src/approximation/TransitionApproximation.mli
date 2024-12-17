open! OurBase

module MakeDefaultApproximableTransition (PM : ProgramTypes.ProgramModules) :
  ApproximationTypes.ApproximableTransitionType
    with type program = PM.Program.t
     and type t = PM.Transition.t
     and type comparator_witness = PM.Transition.comparator_witness

type ('trans, 'bound, 'trans_cmp_wit) transition_approximation_t
(** The type of transition approximations *)

module Make (B : BoundType.Bound) (T : ApproximationTypes.ApproximableTransitionType) :
  ApproximationTypes.TransitionApproximationType
    with type bound = B.t
     and type transition = T.t
     and type program = T.program
     and type t = (T.t, B.t, T.comparator_witness) transition_approximation_t
