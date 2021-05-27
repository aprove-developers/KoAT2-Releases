(** Provides default implementations of atoms. *)
open Batteries
open Polynomials
open BoundsInst

(** Provides default implementations of atoms, i.e., for two objects q and p (e.g. polynomials, bounds), and a comparator ~, the expression  q ~ p is an atom. *)

(** Constructs a default constraint using a simple record *)
module AtomOver(P : ConstraintTypes.Atomizable) : ConstraintTypes.Atom
       with type polynomial = P.t
        and type value = P.value
        and module P = P
        (** Provides default implementations of atoms, i.e., for two objects q and p (e.g. polynomials, bounds), and a comparator ~, the expression q ~ p is an atom. *)


(** Provides an implementation of atoms over polynomials. *)
module Atom :
sig
  include module type of AtomOver(Polynomial)

  (** TODO doc*)
  val max_of_occurring_constants : t -> OurInt.t

  (** Returns the single right hand side constant of the atom. Only works as intended for comparison with <= or integer atoms *)
  val get_constant : t -> value

  (* Only works as intended for integer atoms *)
  val remove_strict : t -> t

  (* Add operations specific to polynomial atoms here if needed *)
end

(** Provides an implementation of atoms over parameter polynomials. *)
module ParameterAtom :
sig
  include module type of AtomOver(ParameterPolynomial)

  (** Returns the coefficient of a variable which is normalised to the lhs. *)
  val get_coefficient : Var.t -> t -> value

  (** Returns the single right hand side constant of the atom. *)
  val get_constant : t -> value

  val remove_strict : t -> t
  (* Add operations specific to parameter atoms here if needed *)
end

module RealAtom :
sig
  include module type of AtomOver(RealPolynomial)

  (** Returns if both polynomials are linear. *)
  val is_linear : t -> bool

  (** Returns the coefficient of a variable which is normalised to the lhs. *)
  val get_coefficient : Var.t -> t -> value

  val max_of_occurring_constants : t -> OurFloat.t

  (* Add operations specific to polynomial atoms here if needed *)
  val of_intatom : Atom.t -> t
end

module RealParameterAtom :
sig
  include module type of AtomOver(RealParameterPolynomial)

  (** Returns the coefficient of a variable which is normalised to the lhs. *)
  val get_coefficient : Var.t -> t -> value

  (** Returns the single right hand side constant of the atom. *)
  val get_constant : t -> value

  (* Add operations specific to parameter atoms here if needed *)
end
