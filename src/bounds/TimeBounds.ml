open Batteries

module Make(A : BoundTypes.Approximation) =
  struct

    module Approximation_ = A
    module Program_ = Approximation_.Program_

    type prf = Program_.Location.t -> Program_.Constraint_.Atom_.Polynomial_.t

    (* Finds a suitable prf which decreases at least one transition and does not increase any transition. *)
    let find_prf (program: Program_.t): prf =
      raise (Failure "Not yet implemented")

    (* Transforms the prf to a monotonic function. *)
    let monotonize_prf (prf: prf): prf =
      raise (Failure "Not yet implemented")

    let improve graph appr =
      raise (Failure "Not yet implemented")

  end
