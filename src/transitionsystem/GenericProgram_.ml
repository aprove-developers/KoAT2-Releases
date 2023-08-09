open OurBase

type (!'trans_label, !'trans_label_cmp_wit, !'loc, !'loc_cmp_wit) pre_cache =
  ( 'loc * 'trans_label * 'loc,
    ( 'loc * 'trans_label * 'loc,
      ('trans_label_cmp_wit, 'loc_cmp_wit) TransitionComparator.comparator_witness )
    Set.t )
  Hashtbl.t
(** This allows us to show type equality of programs created with Make outside of this module.
    For instance in the case of probabilistic programs we want the abstract type t of probabilistic programs and overapproximated probabilistic programs to coincide *)

type (!'trans_label, !'trans_label_cmp_wit, !'loc, !'loc_cmp_wit, !'graph) t = {
  start : 'loc;
  graph : 'graph;
  pre_cache : ('trans_label, 'trans_label_cmp_wit, 'loc, 'loc_cmp_wit) pre_cache Atomically.t;
}
