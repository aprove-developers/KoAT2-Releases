open! OurBase

type (!'trans_label, !'trans_label_cmp_wit) pre_cache =
  ( Location.t * 'trans_label * Location.t,
    ( Location.t * 'trans_label * Location.t,
      'trans_label_cmp_wit TransitionComparator.comparator_witness )
    Set.t )
  Hashtbl.t
(** This allows us to show type equality of programs created with Make outside of this module.
    For instance in the case of probabilistic programs we want the abstract type t of probabilistic programs and overapproximated probabilistic programs to coincide *)

(* This should not be exposed. Move to Program_ ?  How can we achieve this without having the cyclic dependency Program -> ProgramTypes -> Program.GenericProgram *)
type (!'trans_label, !'trans_label_cmp_wit, !'graph) t = {
  start : Location.t;
  graph : 'graph;
  pre_cache : ('trans_label, 'trans_label_cmp_wit) pre_cache Atomically.t;
}

(** Generic adapter for program modules with generic overapproximation *)
module type Adapter = sig
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
