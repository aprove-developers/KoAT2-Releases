open Batteries
open Polynomials
   
(** Provides default implementations of an atom *)

(** Constructs a default constraint using a simple record *)
module AtomOver(P : PolyTypes.Polynomial) : ConstraintTypes.Atom
       with type polynomial = P.t
        and type value = P.value
        and module P = P

module Atom :
sig
  include module type of AtomOver(Polynomial)

  (* Add operations specific to polynomial atoms here if needed *)
end

module ParameterAtom :
sig
  include module type of AtomOver(ParameterPolynomial)

  (* Add operations specific to parameter atoms here if needed *)
end
