open Batteries

module Make(P : ProgramTypes.Program) =
  struct
    module Program_ = P
    module Polynomial_ = Program_.Constraint_.Polynomial_

    type t = {
        pol : Program_.Location.t -> Polynomial_.t;
        strictly_decreasing : Program_.Transition.t list;
        non_increasing : Program_.Transition.t list;
      }

    let strictly_decreasing f = f.strictly_decreasing

    let non_increasing f = f.non_increasing

    let find program =
      raise (Failure "Not yet implemented")

    let monotonize f =
      raise (Failure "Not yet implemented")

  end
