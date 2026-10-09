open! OurBase

type 'trans_label transition = Location.t * 'trans_label * Location.t

type (!'trans_label, !'trans_label_cmp_wit) transition_set =
  ('trans_label transition, 'trans_label_cmp_wit TransitionComparator.comparator_witness) Set.t

type (!'trans_label, !'trans_label_cmp_wit) pre_cache =
  ('trans_label transition, ('trans_label, 'trans_label_cmp_wit) transition_set) Hashtbl.t
(** This allows us to show type equality of programs created with Make outside of this module.
    For instance in the case of probabilistic programs we want the abstract type t of probabilistic programs and overapproximated probabilistic programs to coincide *)

(* This should not be exposed. Move to Program_ ?  How can we achieve this without having the cyclic dependency Program -> ProgramTypes -> Program.GenericProgram *)
type (!'trans_label, !'trans_label_cmp_wit, !'graph) t = {
  start : Location.t;
  graph : 'graph;
  return_locations : LocationSet.t;
  dependency_graph : DependencyGraph.DependencyGraph.t;
  transitions_with_function_calls : ('trans_label, 'trans_label_cmp_wit) transition_set;
      (** Derived from [graph] like [dependency_graph], as [pre] needs it for every query. *)
  pre_cache : ('trans_label, 'trans_label_cmp_wit) pre_cache Atomically.t;
}
