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
