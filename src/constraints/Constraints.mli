open! OurBase
(** Provides default implementations of constraints. *)

open Atoms

(** Provides default implementations of constraints, i.e., a conjunction of atoms. *)

(** Constructs a default constraint using a list of atoms each comparing two polynomials. *)
module ConstraintOver (A : ConstraintTypes.Atom) :
  ConstraintTypes.Constraint
    with type polynomial = A.polynomial
     and type value = A.value
     and type atom = A.t
     and type t = A.t list

(** Provides an implementation of constraints consisting of atoms over polynomials. *)
module Constraint : sig
  include module type of ConstraintOver (Atom)

  val drop_nonlinear : t -> t
  (** Drops all nonlinear atoms from the constraints. Example: (a > 0 && b^2 < 2) gets transformed to (a > 0) *)

  (** The result of the following drop methods is not equivalent to the input constraint.
      But each satisfying valuation of the input constraint is still a model of the new constraint. *)

  val get_coefficient_vector : Var.t -> t -> value list
  (** Returns the row of all coefficients of a variable in a constraint, i.e., used for farkas quantor elimination. *)

  val get_matrix : Var.t list -> t -> value list list
  (** Returns the matrix of all coefficients of a variable from a set of variables in a constraint, i.e., used for farkas quantor elimination. *)

  val get_constant_vector : t -> value list
  (** Returns the row of all constants in a constraint, i.e., used for farkas quantor elimination. *)

  val dualise : Var.t list -> polynomial list list -> polynomial list -> t
  (** TODO doc *)

  val max_of_occurring_constants : t -> OurInt.t
  (** TODO doc *)

  val remove_strict : t -> t
  val simplify : t -> t

  (* Add operations specific to polynomial constraints here if needed *)
end

module RationalConstraint : sig
  include module type of ConstraintOver (RationalAtom)

  val max_of_occurring_constants : t -> OurRational.t

  (* Add operations specific to polynomial constraints here if needed *)
  val of_intconstraint : Constraint.t -> t
end

(** Provides an implementation of constraints consisting of atoms over parameter polynomials. *)
module ParameterConstraintOver (Value : PolyTypes.Ring) : sig
  include module type of ConstraintOver (ParameterAtomOver (Value))

  type unparametrised_constraint = ConstraintOver(AtomOver(Polynomials.PolynomialOver(Value))).t

  val of_constraint : unparametrised_constraint -> t

  val farkas_transform : t -> atom -> unparametrised_constraint
  (** Applies Farkas-Transformation on a Constraint and a ParameterAtom. *)
end

module ParameterConstraint : module type of ParameterConstraintOver (OurInt)

module RationalParameterConstraint : sig
  include module type of ParameterConstraintOver (OurRational)

  val of_intconstraint : Constraint.t -> t
end
