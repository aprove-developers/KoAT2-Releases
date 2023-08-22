open! OurBase
open ProbabilisticProgramModules
open Approximation.Probabilistic

type twn_state

val empty_twn_state : twn_state

val initial_twn_state :
  TWN.configuration -> Program.t -> GeneralTransitionSet.t (** The scc to be analysed *) -> twn_state

val twn_state_to_string : twn_state -> string

val improve :
  twn:twn_state ref * TWN.configuration Option.t
    (** The twn state will be modified during the execution of this method! Always pass on the same ref to avoid duplicated work *) ->
  mprf_depth:int Option.t ->
  Program.t (** The program *) ->
  GeneralTransitionSet.t (** The scc to be analysed *) ->
  ClassicalApproximation.t * ExpApproximation.t ->
  ExpApproximation.t MaybeChanged.t
(** Perform a timebound analysis step by lifting classical analysis methods *)
