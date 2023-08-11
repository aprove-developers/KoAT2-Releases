open OurBase
(** Implementation of local size-bounds. *)

open Bounds
open Formulas
open Polynomials
open ProgramTypes

val c_range : Formula.t -> int
(** Estimate range of contant from guard formula *)

module Make
    (TL : ProgramTypes.TransitionLabel with type update_element = Polynomial.t)
    (T : ProgramTypes.Transition with type transition_label = TL.t)
    (P : ProgramTypes.Program with type transition_label = TL.t) : sig
  type t
  (** LocalSizeBounds are of the form factor * (constant + sum [x1;...;xn]) *)

  val mk : ?s:int -> ?c:int -> string list -> t
  (** Constructs a local size bound with the variables specified as string list*)

  val equal : t -> t -> bool
  (** Returns if the templated bounds represent the same bound. *)

  val factor : t -> int
  (** Returns the factor of the local sizebound. Raises unbounded, if the local size bound is unbounded*)

  val constant : t -> int
  (** Returns the constant of the local sizebound. Raises unbounded, if the local size bound is unbounded*)

  val vars : t -> VarSet.t
  (** Returns a set of of variables which affect the local sizebound *)

  val is_constant : t -> bool
  (** Is the LocalSizeBound constant, i.e. does it contain no variables? *)

  val to_string : t -> string
  (** Converts the templated bound to a string. *)

  val as_bound : t -> Bound.t
  (** Converts the templated bound to an actual (finite) bound. *)

  val option_lsb_as_bound : t option -> Bound.t

  val find_bound : VarSet.t -> Var.t -> Formula.t -> int -> (t * bool Lazy.t) option
  (** Tries to find a templated bound of any of the defined templates. The first vars corresponds to
  * the variabels that may occur in the lsb.
  * The bool Lazy.t value indiciates if this lsb is of the equality type *)

  val from_update_poly : VarSet.t -> Var.t -> Polynomial.t -> (t * bool Lazy.t) option
  (** Construct a local size bound directly from the update expression *)

  val compute_bound : VarSet.t -> T.t -> Var.t -> (t * bool Lazy.t) option

  val sizebound_local : P.t -> T.t -> Var.t -> t Option.t
  (** Returns a local sizebound of the specified kind for the variable of the transition.
      A local sizebound is expressed in relation to the values directly before executing the transition. *)
end

include module type of Make (TransitionLabel_) (Transition_) (Program_)
