(** Provides default implementations of atoms. *)
open Batteries
open Polynomials
   
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

  (* Add operations specific to parameter atoms here if needed *)
end

(** Provides an implementation of atoms over bounds. *)
module BoundAtom :
sig
  include module type of AtomOver(Bound)

  (* Add operations specific to polynomial atoms here if needed *)
end