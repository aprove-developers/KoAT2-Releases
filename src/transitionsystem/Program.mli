(** Provides default module to handle programs. *)
open Batteries
open Polynomials
open Constraints
(** Provides default module to handle programs. *)

module ProgramOver(L: ProgramTypes.Location) : sig
  include ProgramTypes.Program
    with type location = L.t
    and type LocationSet.elt = L.t
    and type Transition.location = L.t
    and type Transition.t = L.t * TransitionLabel.t * L.t
    and type TransitionSet.elt = L.t * TransitionLabel.t * L.t
end

include ProgramTypes.Program
  with type location = Location.t
  and type LocationSet.elt = Location.t
  and type Transition.location = Location.t
  and type Transition.t = Location.t * TransitionLabel.t * Location.t
  and type TransitionSet.elt = Location.t * TransitionLabel.t * Location.t

(** TODO doc *)
val rename : t -> t

(** KoAT does not support recursion yet *)
exception RecursionNotSupported
