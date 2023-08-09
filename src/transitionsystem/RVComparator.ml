open OurBase

(** RVComparators only depend on Transition & Variable Comparators *)
include Comparator.Derived2 (struct
  type (!'trans, !'var) t = 'trans * 'var

  let compare trans_compare var_compare (t1, v1) (t2, v2) =
    let t_compare = trans_compare t1 t2 in
    if t_compare = 0 then
      var_compare v1 v2
    else
      t_compare


  let sexp_of_t _ _ = Sexplib0.Sexp_conv.sexp_of_opaque
end)
