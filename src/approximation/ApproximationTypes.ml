open! OurBase

(** Module type of transitions for which we can create (time/cost) approximations *)
module type ApproximableTransitionType = sig
  type program
  type t

  val id : t -> int
  val to_id_string : t -> string
  val compare : t -> t -> int
  val all_from_program : program -> t Sequence.t
  val ids_to_string : ?pretty:bool -> t -> string

  include Comparator.S with type t := t
end

(** Abstracts TransitionApproximation so that it can be used to handle normal transitions with integer bounds and general
 * transitions with real bounds*)
module type TransitionApproximationType = sig
  type bound
  type transition
  type program
  type t

  val empty : string -> t
  val get : t -> transition -> bound

  val sum : t -> program -> bound
  (** Returns a timebound for the execution of all given transitions *)

  val filter_transitions : (transition -> Bool.t) -> t -> t
  val add : ?simplifyfunc:(bound -> bound) -> bound -> transition -> t -> t
  val all_bounded : t -> transition Sequence.t -> bool
  val to_formatted : ?pretty:bool -> ?termination_only:bool -> transition list -> t -> FormattedString.t
  val to_string : ?termination_only:bool -> transition list -> t -> string
  val to_sequence : t -> (transition * bound) Sequence.t
end

module type SizeApproximationType = sig
  type bound
  type rv
  type t

  val empty : t
  val get : t -> rv -> bound
  val filter_rvs : (rv -> bool) -> t -> t
  val add : ?simplifyfunc:(bound -> bound) -> bound -> rv -> t -> t
  val add_all : ?simplifyfunc:(bound -> bound) -> bound -> rv list -> t -> t
  val to_formatted : ?pretty:bool -> t -> FormattedString.t
  val to_string : t -> string
  val to_sequence : t -> (rv * bound) Sequence.t
  val of_sequence : (rv * bound) Sequence.t -> t
end
