(** Provides default module to handle programs. *)
open Batteries
open Polynomials
open Constraints
(** Provides default module to handle programs. *)

module ProgramOver(L: ProgramTypes.Location) : sig
  include ProgramTypes.Program
    with module Location = L
    and type LocationSet.elt = L.t
    and module Transition.Location = L
    and type Transition.t = L.t * TransitionLabel.t * L.t
    and type TransitionSet.elt = L.t * TransitionLabel.t * L.t
end

include module type of ProgramOver(Location)

(** TODO doc *)
val rename : t -> t

(** KoAT does not support recursion yet *)
exception RecursionNotSupported
