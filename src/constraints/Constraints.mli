open! OurBase

(** Provides implementations of constraints, i.e., conjunctions of atoms. *)

(** Provides an implementation of constraints consisting of atoms over polynomials. *)
module Constraint : sig
  include
    ConstraintTypes.Constraint
      with type polynomial = Polynomials.Polynomial.t
       and type value = OurInt.t
       and type atom = Atoms.Atom.t
       and type t = Atoms.Atom.t List.t

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
  val to_set : t -> (Atoms.Atom.t, Atoms.Atom.comparator_witness) Set.t
  val of_set : (Atoms.Atom.t, Atoms.Atom.comparator_witness) Set.t -> t

  (* Add operations specific to polynomial constraints here if needed *)
end

module RationalConstraint : sig
  include
    ConstraintTypes.Constraint
      with type polynomial = Polynomials.RationalPolynomial.t
       and type value = OurRational.t
       and type atom = Atoms.RationalAtom.t

  val max_of_occurring_constants : t -> OurRational.t

  (* Add operations specific to polynomial constraints here if needed *)
  val of_intconstraint : Constraint.t -> t
end

module ParameterConstraint :
  ConstraintTypes.ParameterConstraint
    with type polynomial = Polynomials.ParameterPolynomial.t
     and type value = Polynomials.Polynomial.t
     and type atom = Atoms.ParameterAtom.t
     and type unparametrised_constraint = Constraint.t

module RationalParameterConstraint : sig
  include
    ConstraintTypes.ParameterConstraint
      with type polynomial = Polynomials.RationalParameterPolynomial.t
       and type value = Polynomials.RationalPolynomial.t
       and type atom = Atoms.RationalParameterAtom.t
       and type unparametrised_constraint = RationalConstraint.t

  val of_intconstraint : Constraint.t -> t
end
