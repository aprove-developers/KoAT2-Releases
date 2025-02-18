open! OurBase

(** RVComparators only depend on Modifier & Variable Comparators *)
include Comparator.Derived2 (struct
  type (!'modifier, !'var) t = 'modifier * 'var

  let compare modifier_compare var_compare (m1, v1) (m2, v2) =
    let t_compare = modifier_compare m1 m2 in
    if t_compare = 0 then
      var_compare v1 v2
    else
      t_compare


  let sexp_of_t _ _ = Sexplib0.Sexp_conv.sexp_of_opaque
end)
