open! OurBase

(** TransitionComparators only depend on Label & Location Comparators *)
include Comparator.Derived (struct
  type !'trans_label t = Location.t * 'trans_label * Location.t

  let compare label_compare (_, label1, _) (_, label2, _) = label_compare label1 label2
  let sexp_of_t _ = Sexplib0.Sexp_conv.sexp_of_opaque
end)
