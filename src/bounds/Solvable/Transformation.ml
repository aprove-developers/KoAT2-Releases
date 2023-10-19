open Batteries
module ScaledMonomial = ScaledMonomials.Make (OurInt)
module Monomial = Monomials.Make (OurInt)

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open! PM
  module Automorphism = Automorphism.Automorphism (Bound)
  module Loop = Loop.Make (Bound) (PM)
  module Check_Solvable = Check_Solvable.Make (Bound) (ProgramModules)

  exception NotTransformable

  (** [transform_linearly loop] first checks whether a loop is in twn-form.
      Then it tries to find a transformation using the Jordan normal form (by calling sympy). *)
  let transform_linearly (loop : Loop.t) =
    match Check_Solvable.check_solvable loop with
    | None -> raise NotTransformable
    | Some blocks ->
        if List.for_all (fun block -> List.length block = 1) blocks then
          (loop, Some Automorphism.identity)
        else
          (loop, Some Automorphism.identity)


  let transform transformation_type (loop : Loop.t) =
    match transformation_type with
    | `NoTransformation -> (loop, Some Automorphism.identity)
    | `TWNTransform -> transform_linearly loop
end
