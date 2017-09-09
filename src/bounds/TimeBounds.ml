open Batteries

module Make(A : BoundTypes.Approximation) =
  struct

    module Approximation_ = A
    module Program_ = Approximation_.Program_

    let improve graph appr =
      raise (Failure "Not yet implemented")

  end
