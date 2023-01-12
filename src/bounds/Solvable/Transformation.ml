open Automorphism
open Batteries
open ProgramTypes
open Formulas
open Polynomials
open ProgramModules

module ScaledMonomial = ScaledMonomials.Make(OurInt)
module Monomial = Monomials.Make(OurInt)

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Loop = SimpleCycle.Loop(PM)

  module Check_Solvable = Check_Solvable.Make(ProgramModules)

  (** [transform_linearly loop] first checks whether a loop is in twn-form.
      Then it tries to find a transformation using the Jordan normal form (by calling sympy). *)
  let transform_linearly (loop: Loop.t) =
    match Check_Solvable.check_solvable loop with
      | None -> None
      | Some blocks ->
        if List.for_all (fun block -> List.length block = 1) blocks then
          Some (loop, Automorphism.identity)
        else
          Some (loop, Automorphism.identity)

  let transform (loop: Loop.t) = function
    | `TWN -> Some (loop, Automorphism.identity)
    | `TWN_LINEAR -> transform_linearly loop

end
