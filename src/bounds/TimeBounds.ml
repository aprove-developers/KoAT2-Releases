open Batteries

module Make(A : BoundTypes.Approximation) =
  struct

    module Approximation_ = A

    type prf = Approximation_.TransitionGraph_.Location_.t -> Approximation_.TransitionGraph_.Transition_.Constraint_.Atom_.Polynomial_.t

    (* Finds a suitable prf which decreases at least one transition and does not increase any transition. *)
    let find_prf (graph: Approximation_.TransitionGraph_.t): prf =
      raise (Failure "Not yet implemented")

    (* Transforms the prf to a monotonic function. *)
    let monotonize_prf (prf: prf): prf =
      raise (Failure "Not yet implemented")
                          
    let improve graph appr =
      raise (Failure "Not yet implemented")

  end
