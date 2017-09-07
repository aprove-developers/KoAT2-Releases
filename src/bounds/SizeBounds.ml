open Batteries

module Make(A : BoundTypes.Approximation) =
  struct

    module Approximation_ = A

    type classification = Equality | Constant | ScaledSum | Unbound

    let classify (poly: A.bound): classification =
      raise (Failure "Not yet implemented")
                                                          
    let improve graph appr =
      raise (Failure "Not yet implemented")

  end
