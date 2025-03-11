open! OurBase

(** Adapter for Partial Evaluation to work with different kinds of programs *)
module type PEAdapter = sig
  type update_element
  type transition
  type transition_graph
  type grouped_transition
  type grouped_transition_cmp_wit
  type program
  type approx = Polynomials.Polynomial.t * Guard.t

  val overapprox_indeterminates : update_element -> approx
  val outgoing_grouped_transitions : transition_graph -> Location.t -> grouped_transition Sequence.t
  val empty_grouped_transition_set : (grouped_transition, grouped_transition_cmp_wit) Set.t
  val guard_of_grouped_transition : grouped_transition -> Guard.t

  val all_grouped_transitions_of_graph :
    transition_graph -> (grouped_transition, grouped_transition_cmp_wit) Set.t

  val grouped_transition_of_transition : transition -> grouped_transition

  val copy_and_modify_grouped_transition :
    new_start:Location.t ->
    add_invariant:Guard.t ->
    redirect:(transition -> Location.t) ->
    grouped_transition ->
    grouped_transition

  val create_new_program : Location.t -> (grouped_transition, grouped_transition_cmp_wit) Set.t -> program
end
