open OurBase

(** TransitionComparators only depend on Label & Location Comparators *)
include Comparator.Derived2 (struct
  type (!'trans_label, !'loc) t = 'loc * 'trans_label * 'loc

  let compare label_compare loc_compar (_, label1, _) (_, label2, _) = label_compare label1 label2
  let sexp_of_t _ _ = Sexplib0.Sexp_conv.sexp_of_opaque
end)
