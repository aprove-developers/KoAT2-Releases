(** Provides default implementations of formulas.*)
open Batteries
open Constraints
   

(** Constructs a default formula using a list of atoms each comparing two polynomials *)
module FormulaOver(C : ConstraintTypes.Constraint) : ConstraintTypes.Formula
       with type constr = C.t
        and type atom = C.atom
        and type polynomial = C.polynomial
        and type value = C.value

module Formula :
sig
  include module type of FormulaOver(Constraint)

  (** TODO doc *)
  val max_of_occurring_constants : t -> OurInt.t

  (* Add operations specific to polynomial formula here if needed *)
end

module ParameterFormula :
sig
  include module type of FormulaOver(ParameterConstraint)

  (* Add operations specific to parameter polynomial formula here if needed *)
end
