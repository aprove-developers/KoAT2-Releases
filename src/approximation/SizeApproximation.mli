open! OurBase

type ('rvtuple_, 'bound, 'rvtuple__cmp_wit) size_approximation_t

module Make (B : ApproximationTypes.ApproximableBoundType) (RV : ProgramTypes.RV) :
  ApproximationTypes.SizeApproximationType
    with type bound = B.t
     and type rv = RV.t
     and type t = (RV.t, B.t, RV.comparator_witness) size_approximation_t
