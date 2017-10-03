open Batteries
open Constraints
   
(** Provides default implementations of a constraint *)

(** Constructs a default constraint using a list of atoms each comparing two polynomials *)
module FormulaOver(C : ConstraintTypes.Constraint) : ConstraintTypes.Formula
       with type constr = C.t
        and type atom = C.atom
        and type polynomial = C.polynomial
        and type value = C.value
        and module C = C

module Formula :
sig
  include module type of FormulaOver(Constraint)

  (* Add operations specific to polynomial formula here if needed *)
end

module ParameterFormula :
sig
  include module type of FormulaOver(ParameterConstraint)

  (* Add operations specific to parameter polynomial formula here if needed *)
end
